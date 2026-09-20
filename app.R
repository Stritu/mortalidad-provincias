library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(bslib)
library(plotly)

# ------------------------------------------------------------------------------
# Helpers de lectura y limpieza (optimizados con data.table::fread)
# ------------------------------------------------------------------------------
leer_csv_flexible <- function(path) {
  # europa.csv usa coma (autodetección de fread); poblacion_europa usa ";".
  dt <- tryCatch(
    data.table::fread(path, encoding = "UTF-8", showProgress = FALSE),
    error = function(e) NULL
  )
  if (!is.null(dt) && ncol(dt) > 1) return(dt)
  data.table::fread(path, sep = ";", encoding = "Latin-1", showProgress = FALSE)
}

leer_tabla_semicolon <- function(path, col_char = NULL) {
  # causas_defunciones.csv, funciones.csv, poblacion.csv usan ;
  # FIX: la columna de valor se lee como texto ("12.460" son miles). Si fread
  # la convierte a número (12.46) se pierde el cero y numero_limpio ya no
  # puede recuperar los miles: rents/conteos acabados en 0 se dividían /1000.
  cc <- if (!is.null(col_char)) stats::setNames(rep("character", length(col_char)), col_char) else NULL
  data.table::fread(path, sep = ";", encoding = "Latin-1", header = FALSE, showProgress = FALSE,
        colClasses = cc)
}

# ------------------------------------------------------------------------------
# Caché en disco para las secciones pesadas (OPT máxima)
# ------------------------------------------------------------------------------
# Se regenera sola si cambia el CSV fuente (fecha/tamaño) o CACHE_VERSION.
# v3: lectura de valores como texto (fix miles "12.460"->12460, antes 12.46).
CACHE_VERSION <- "v3"

cache_cargar <- function(fuentes, rds, sin_fuente_ok = FALSE) {
  if (!file.exists(rds)) return(NULL)
  guardado <- tryCatch(readRDS(rds), error = function(e) NULL)
  if (!is.list(guardado) || !identical(guardado$version, CACHE_VERSION)) return(NULL)
  info <- file.info(fuentes)
  if (any(is.na(info$mtime)) || any(is.na(info$size))) {
    # Sin fichero fuente (p. ej. despliegue, donde viajan solo las cachés):
    # se acepta avisando. La frescura la garantiza haberla generado en local.
    if (isTRUE(sin_fuente_ok)) {
      message("Fuente ausente (", paste(fuentes, collapse = ", "), "): usando caché ", rds, ".")
      return(guardado$datos)
    }
    return(NULL)
  }
  if (!identical(as.character(info$mtime), guardado$src_mtime)) return(NULL)
  if (!identical(as.character(info$size), guardado$src_size)) return(NULL)
  guardado$datos
}

cache_guardar <- function(datos, fuentes, rds) {
  dir.create(dirname(rds), showWarnings = FALSE, recursive = TRUE)
  info <- file.info(fuentes)
  saveRDS(
    list(version = CACHE_VERSION,
         src_mtime = as.character(info$mtime),
         src_size = as.character(info$size),
         datos = datos),
    rds
  )
  invisible(datos)
}

numero_limpio <- function(x) {
  # Limpieza base (signos, letras, espacios).
  x <- stringi::stri_replace_all_regex(as.character(x), "[^0-9,.-]", "")
  x[x %in% c("", ".", "..", ",")] <- NA_character_
  # Con punto Y coma ("1.234,56") el punto es miles, como antes.
  ambos <- grepl(".", x, fixed = TRUE) & grepl(",", x, fixed = TRUE)
  ambos[is.na(ambos)] <- FALSE
  x[ambos] <- gsub(".", "", x[ambos], fixed = TRUE)
  # Punto solitario: miles si grupos exactos de 3 ("7.503"->7503),
  # decimal en otro caso ("539.79"->539.79; antes se leía 53979).
  es_miles <- grepl("^[1-9][0-9]{0,2}(\\.[0-9]{3})+$", x)
  es_miles[is.na(es_miles)] <- FALSE
  x[es_miles] <- gsub(".", "", x[es_miles], fixed = TRUE)
  # Resto: la coma es decimal.
  x[!es_miles] <- gsub(",", ".", x[!es_miles], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# ==============================================================================
# 0. DEFINICIÓN DE CONSTANTES Y FUNCIONES AUXILIARES
# ==============================================================================

limpiar_texto <- function(x) {
  # OPT: una sola pasada stringi (vectorize_all=FALSE aplica los pares en el
  # mismo orden secuencial que los 7 gsub originales).
  stringi::stri_replace_all_regex(
    as.character(x),
    c("Ã¡|á|Ã\u0081", "Ã©|é", "Ã\u00ad|í", "Ã³|ó", "Ãº|ú", "Ã±|ñ"),
    c("á", "é", "í", "ó", "ú", "ñ"),
    vectorize_all = FALSE
  )
}

normalizar_provincias <- function(col) {
  # OPT: una sola pasada stringi en lugar de 7 gsub secuenciales.
  col <- stringi::stri_replace_all_regex(
    as.character(col),
    c("Ã\u0081", "Ã¡", "Ã\u00ad|Ã­", "Ã³", "Ã©", "Ãº", "Ã±"),
    c("Á", "á", "í", "ó", "é", "ú", "ñ"),
    vectorize_all = FALSE
  )
  col <- trimws(col)
  
  case_match(
    col,
    "Coruña, A"               ~ "A Coruña",
    "Coruña (A)"              ~ "A Coruña",
    "La Coruña"               ~ "A Coruña",
    "A Coruna"                ~ "A Coruña",
    "Araba/Álava"             ~ "Álava",
    "Araba / Álava"           ~ "Álava",
    "Alicante/Alacant"        ~ "Alicante",
    "Balears, Illes"          ~ "Baleares",
    "Illes Balears"           ~ "Baleares",
    "Castellón/Castelló"      ~ "Castellón",
    "Ciudad Real"             ~ "Ciudad Real",
    "Gipuzkoa"                ~ "Guipúzcoa",
    "Rioja, La"               ~ "La Rioja",
    "Rioja"                   ~ "La Rioja",
    "Palmas, Las"             ~ "Las Palmas",
    "Santa Cruz de Tenerife"  ~ "Santa Cruz de Tenerife",
    "Valencia/València"       ~ "Valencia",
    "Valencia/ValÃ¨ncia"      ~ "Valencia",
    "Bizkaia"                 ~ "Vizcaya",
    .default = col
  )
}

# ==============================================================================
# 1. CARGA Y PROCESAMIENTO DE DATOS
# ==============================================================================

# ==============================================================================
# FUNCIONES AUXILIARES GLOBALES (siempre disponibles, incluso con caché)
# ==============================================================================

normalizar_clave_pais_europa <- function(x) {
  y <- trimws(as.character(x))
  y <- case_when(
    y %in% c("Türkiye", "Turkey") ~ "Turkey",
    y %in% c("Czechia", "Czech Republic") ~ "Czechia",
    y %in% c("Moldova", "Republic of Moldova", "Moldova (Republic of)") ~ "Moldova",
    y %in% c("North Macedonia", "Republic of North Macedonia") ~ "North Macedonia",
    y %in% c("Bosnia and Herzegovina", "Bosnia & Herzegovina") ~ "Bosnia and Herzegovina",
    y %in% c("Russia", "Russian Federation") ~ "Russia",
    y %in% c("Vatican City", "Holy See") ~ "Vatican",
    TRUE ~ y
  )
  y
}

normalizar_nombre_columna <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]", "", x)
}

# OPT: Europa en disco (CSV ~25 MB + agregación costosa). En acierto se
# reutilizan los objetos y se omiten lecturas/cálculos (guardas !EUROPA_OK).
.ftes_europa <- c("europa.csv", {
  .cand <- c("poblacion_europa.csv", "poblacion_europa")
  .ex <- .cand[file.exists(.cand)]
  if (length(.ex)) .ex[[1]] else character(0)
})
europa_disk <- cache_cargar(.ftes_europa, file.path("cache", "cache_europa.rds"), sin_fuente_ok = TRUE)
EUROPA_OK <- !is.null(europa_disk)
if (EUROPA_OK) {
  europa_agrupada <- europa_disk$agrupada
  poblacion_europa <- europa_disk$poblacion
}

# Carga de Eurostat. Se prueba CSV estándar, CSV2 y finalmente read.csv.
if (!EUROPA_OK) df_europa_raw <- leer_csv_flexible("europa.csv")

cols_europa <- c(
  "sex",
  "International Statistical Classification of Diseases and Related Health Problems (ICD-10)",
  "Geopolitical entity (reporting)",
  "TIME_PERIOD",
  "OBS_VALUE",
  "Place of residence"
)

if (!EUROPA_OK && all(cols_europa %in% colnames(df_europa_raw))) {
  europa <- df_europa_raw %>% select(all_of(cols_europa))
  
  df_clean <- europa %>%
    filter(`Place of residence` == "All deaths reported in the country" | is.na(`Place of residence`)) %>%
    transmute(
      sexo = as.character(sex),
      causa_icd10 = as.character(`International Statistical Classification of Diseases and Related Health Problems (ICD-10)`),
      pais = as.character(`Geopolitical entity (reporting)`),
      anio = as.numeric(TIME_PERIOD),
      defunciones = numero_limpio(OBS_VALUE)
    ) %>%
    mutate(
      sexo = case_when(
                    sexo %in% c("Males", "Male", "M") ~ "Hombres",
                    sexo %in% c("Females", "Female", "F") ~ "Mujeres",
                    sexo %in% c("Both sexes", "Total", "T", "TOT") ~ "Ambos",
                    TRUE ~ sexo),
      anio = as.numeric(anio),
      defunciones = as.numeric(defunciones)
    ) %>%
    filter(is.finite(anio), is.finite(defunciones), !is.na(pais), pais != "")
} else {
  df_clean <- data.frame(
    sexo = character(), causa_icd10 = character(), pais = character(),
    anio = numeric(), defunciones = numeric(), stringsAsFactors = FALSE
  )
}

agregados_europa_excluir <- c(
  "European Union - 27 countries (from 2020)",
  "88 countries"
)

if (!EUROPA_OK) df_clean <- df_clean %>%
  filter(!pais %in% agregados_europa_excluir,
         !grepl("^European Union\\b", pais, ignore.case = TRUE),
         !grepl("^88 countries\\b", pais, ignore.case = TRUE))

# Traducciones para la interfaz
traducir_pais <- function(x) {
  map <- c(
    "Austria" = "Austria", "Belgium" = "Bélgica", "Bulgaria" = "Bulgaria",
    "Croatia" = "Croacia", "Cyprus" = "Chipre", "Czechia" = "Chequia",
    "Czech Republic" = "República Checa", "Denmark" = "Dinamarca", "Estonia" = "Estonia",
    "Finland" = "Finlandia", "France" = "Francia", "Germany" = "Alemania",
    "Greece" = "Grecia", "Hungary" = "Hungría", "Iceland" = "Islandia",
    "Ireland" = "Irlanda", "Italy" = "Italia", "Latvia" = "Letonia",
    "Liechtenstein" = "Liechtenstein", "Lithuania" = "Lituania", "Luxembourg" = "Luxemburgo",
    "Malta" = "Malta", "Netherlands" = "Países Bajos", "Norway" = "Noruega",
    "Poland" = "Polonia", "Portugal" = "Portugal", "Romania" = "Rumanía",
    "Slovakia" = "Eslovaquia", "Slovenia" = "Eslovenia", "Spain" = "España",
    "Sweden" = "Suecia", "Switzerland" = "Suiza", "Türkiye" = "Turquía",
    "Turkey" = "Turquía", "United Kingdom" = "Reino Unido", "North Macedonia" = "Macedonia del Norte",
    "Montenegro" = "Montenegro", "Serbia" = "Serbia", "Albania" = "Albania",
    "Bosnia and Herzegovina" = "Bosnia y Herzegovina", "Kosovo" = "Kosovo",
    "Moldova" = "Moldavia", "Ukraine" = "Ucrania", "Belarus" = "Bielorrusia",
    "Georgia" = "Georgia", "Armenia" = "Armenia", "Azerbaijan" = "Azerbaiyán"
  )
  ifelse(x %in% names(map), unname(map[x]), x)
}

# Traducción completa de las causas que aparecen en el CSV europeo.
# Se conserva una traducción explícita para no depender de traducciones parciales.
# OPT: tabla construida UNA sola vez a nivel global (antes se reconstruía en cada llamada).
causas_lookup <- c(
  "Accidental drowning and submersion" = "Ahogamiento y sumersión accidentales",
  "Accidental poisoning by and exposure to noxious substances" = "Envenenamiento accidental y exposición a sustancias nocivas",
  "Accidents (V01-X59, Y85, Y86)" = "Accidentes (V01-X59, Y85, Y86)",
  "Acute myocardial infarction including subsequent myocardial infarction" = "Infarto agudo de miocardio, incluido el infarto posterior",
  "Alzheimer disease" = "Enfermedad de Alzheimer",
  "Assault" = "Agresión",
  "Asthma and status asthmaticus" = "Asma y estado asmático",
  "Cerebrovascular diseases" = "Enfermedades cerebrovasculares",
  "Certain conditions originating in the perinatal period (P00-P96)" = "Ciertas afecciones originadas en el período perinatal (P00-P96)",
  "Certain infectious and parasitic diseases (A00-B99)" = "Ciertas enfermedades infecciosas y parasitarias (A00-B99)",
  "Chronic liver disease" = "Enfermedad hepática crónica",
  "Chronic liver disease (excluding alcoholic and toxic liver disease)" = "Enfermedad hepática crónica (excluidas las enfermedades hepáticas alcohólicas y tóxicas)",
  "Chronic lower respiratory diseases" = "Enfermedades respiratorias crónicas de las vías inferiores",
  "Chronic viral hepatitis B and C" = "Hepatitis víricas crónicas B y C",
  "Congenital malformations, deformations and chromosomal abnormalities (Q00-Q99)" = "Malformaciones, deformaciones y anomalías cromosómicas congénitas (Q00-Q99)",
  "COVID-19, other" = "COVID-19, otros casos",
  "COVID-19, virus identified" = "COVID-19, virus identificado",
  "COVID-19, virus not identified" = "COVID-19, virus no identificado",
  "Dementia" = "Demencia",
  "Diabetes mellitus" = "Diabetes mellitus",
  "Diseases of kidney and ureter" = "Enfermedades del riñón y del uréter",
  "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism" = "Enfermedades de la sangre y de los órganos hematopoyéticos y ciertos trastornos del mecanismo inmunitario",
  "Diseases of the circulatory system (I00-I99)" = "Enfermedades del sistema circulatorio (I00-I99)",
  "Diseases of the digestive system (K00-K93)" = "Enfermedades del sistema digestivo (K00-K93)",
  "Diseases of the genitourinary system (N00-N99)" = "Enfermedades del sistema genitourinario (N00-N99)",
  "Diseases of the musculoskeletal system and connective tissue (M00-M99)" = "Enfermedades del sistema musculoesquelético y del tejido conjuntivo (M00-M99)",
  "Diseases of the nervous system and the sense organs (G00-H95)" = "Enfermedades del sistema nervioso y de los órganos de los sentidos (G00-H95)",
  "Diseases of the respiratory system (J00-J99)" = "Enfermedades del sistema respiratorio (J00-J99)",
  "Diseases of the skin and subcutaneous tissue (L00-L99)" = "Enfermedades de la piel y del tejido subcutáneo (L00-L99)",
  "Drug dependence, toxicomania (F11-F16, F18-F19)" = "Dependencia de drogas y toxicomanía (F11-F16, F18-F19)",
  "Endocrine, nutritional and metabolic diseases (E00-E90)" = "Enfermedades endocrinas, nutricionales y metabólicas (E00-E90)",
  "Event of undetermined intent" = "Suceso de intención indeterminada",
  "External causes of morbidity and mortality (V01-Y89)" = "Causas externas de morbilidad y mortalidad (V01-Y89)",
  "Falls" = "Caídas",
  "Hodgkin disease and lymphomas" = "Enfermedad de Hodgkin y linfomas",
  "Human immunodeficiency virus [HIV] disease" = "Enfermedad por virus de la inmunodeficiencia humana (VIH)",
  "Ill-defined and unknown causes of mortality" = "Causas de mortalidad mal definidas o desconocidas",
  "Influenza (including swine flu)" = "Gripe (incluida la gripe porcina)",
  "Intentional self-harm" = "Lesiones autoinfligidas intencionalmente",
  "Ischaemic heart diseases" = "Enfermedades isquémicas del corazón",
  "Leukaemia" = "Leucemia",
  "Malignant melanoma of skin" = "Melanoma maligno de la piel",
  "Malignant neoplasm of bladder" = "Tumor maligno de la vejiga",
  "Malignant neoplasm of brain and central nervous system" = "Tumor maligno del cerebro y del sistema nervioso central",
  "Malignant neoplasm of breast" = "Tumor maligno de la mama",
  "Malignant neoplasm of cervix uteri" = "Tumor maligno del cuello uterino",
  "Malignant neoplasm of colon, rectosigmoid junction, rectum, anus and anal canal" = "Tumor maligno de colon, unión rectosigmoidea, recto, ano y conducto anal",
  "Malignant neoplasm of kidney, except renal pelvis" = "Tumor maligno del riñón, excepto pelvis renal",
  "Malignant neoplasm of larynx" = "Tumor maligno de la laringe",
  "Malignant neoplasm of lip, oral cavity, pharynx" = "Tumor maligno del labio, cavidad oral y faringe",
  "Malignant neoplasm of liver and intrahepatic bile ducts" = "Tumor maligno del hígado y de las vías biliares intrahepáticas",
  "Malignant neoplasm of oesophagus" = "Tumor maligno del esófago",
  "Malignant neoplasm of other parts of uterus" = "Tumor maligno de otras partes del útero",
  "Malignant neoplasm of ovary" = "Tumor maligno del ovario",
  "Malignant neoplasm of pancreas" = "Tumor maligno del páncreas",
  "Malignant neoplasm of prostate" = "Tumor maligno de la próstata",
  "Malignant neoplasm of stomach" = "Tumor maligno del estómago",
  "Malignant neoplasm of thyroid gland" = "Tumor maligno de la glándula tiroides",
  "Malignant neoplasm of trachea, bronchus and lung" = "Tumor maligno de tráquea, bronquios y pulmón",
  "Malignant neoplasms (C00-C97)" = "Tumores malignos (C00-C97)",
  "Mental and behavioural disorders (F00-F99)" = "Trastornos mentales y del comportamiento (F00-F99)",
  "Mental and behavioural disorders due to use of alcohol" = "Trastornos mentales y del comportamiento debidos al consumo de alcohol",
  "Neoplasms" = "Tumores",
  "Non-malignant neoplasms (benign and uncertain)" = "Tumores no malignos (benignos y de comportamiento incierto)",
  "Other accidents (W20-W64, W75-X39, X50-X59, Y86)" = "Otros accidentes (W20-W64, W75-X39, X50-X59, Y86)",
  "Other diseases of the circulatory system (remainder of I00-I99)" = "Otras enfermedades del sistema circulatorio (resto de I00-I99)",
  "Other diseases of the digestive system (remainder of K00-K93)" = "Otras enfermedades del sistema digestivo (resto de K00-K93)",
  "Other diseases of the genitourinary system (remainder of N00-N99)" = "Otras enfermedades del sistema genitourinario (resto de N00-N99)",
  "Other diseases of the musculoskeletal system and connective tissue (remainder of M00-M99)" = "Otras enfermedades del sistema musculoesquelético y del tejido conjuntivo (resto de M00-M99)",
  "Other diseases of the nervous system and the sense organs (remainder of G00-H95)" = "Otras enfermedades del sistema nervioso y de los órganos de los sentidos (resto de G00-H95)",
  "Other diseases of the respiratory system (remainder of J00-J99)" = "Otras enfermedades del sistema respiratorio (resto de J00-J99)",
  "Other endocrine, nutritional and metabolic diseases (remainder of E00-E90)" = "Otras enfermedades endocrinas, nutricionales y metabólicas (resto de E00-E90)",
  "Other external causes of morbidity and mortality (remainder of V01-Y89)" = "Otras causas externas de morbilidad y mortalidad (resto de V01-Y89)",
  "Other heart diseases" = "Otras enfermedades del corazón",
  "Other infectious and parasitic diseases (remainder of A00-B99)" = "Otras enfermedades infecciosas y parasitarias (resto de A00-B99)",
  "Other ischaemic heart diseases" = "Otras enfermedades isquémicas del corazón",
  "Other lower respiratory diseases" = "Otras enfermedades respiratorias de las vías inferiores",
  "Other malignant neoplasm of lymphoid, haematopoietic and related tissue" = "Otros tumores malignos del tejido linfoide, hematopoyético y relacionados",
  "Other malignant neoplasms (remainder of C00-C97)" = "Otros tumores malignos (resto de C00-C97)",
  "Other mental and behavioural disorders (remainder of F00-F99)" = "Otros trastornos mentales y del comportamiento (resto de F00-F99)",
  "Other symptoms, signs and abnormal clinical and laboratory findings (remainder of R00-R99)" = "Otros síntomas, signos y hallazgos anormales clínicos y de laboratorio (resto de R00-R99)",
  "Parkinson disease" = "Enfermedad de Parkinson",
  "Pneumonia" = "Neumonía",
  "Pregnancy, childbirth and the puerperium (O00-O99)" = "Embarazo, parto y puerperio (O00-O99)",
  "Rheumatoid arthritis and arthrosis (M05-M06,M15-M19)" = "Artritis reumatoide y artrosis (M05-M06, M15-M19)",
  "Sudden infant death syndrome" = "Síndrome de muerte súbita del lactante",
  "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)" = "Síntomas, signos y hallazgos anormales clínicos y de laboratorio, no clasificados en otra parte (R00-R99)",
  "Total" = "Total",
  "Transport accidents (V01-V99, Y85)" = "Accidentes de transporte (V01-V99, Y85)",
  "Tuberculosis" = "Tuberculosis",
  "Ulcer of stomach, duodenum and jejunum" = "Úlcera de estómago, duodeno y yeyuno",
  "Viral hepatitis and sequelae of viral hepatitis" = "Hepatitis vírica y secuelas de la hepatitis vírica"
)

traducir_causa <- function(x) {
  y <- stringr::str_squish(as.character(x))
  idx <- match(y, names(causas_lookup))
  out <- unname(causas_lookup[idx])
  out[is.na(out)] <- y[is.na(out)]
  out
}

# ------------------------------------------------------------------------------
# AGRUPACIÓN DE CAUSAS EUROSTAT
# ------------------------------------------------------------------------------
# Cada causa original se asigna a un grupo amplio. Para grupos jerárquicos,
# si Eurostat proporciona la categoría madre, se usa esa cifra y NO se suman
# también sus subcategorías. Así se evita el doble conteo.
clasificar_causa_europa <- function(causa) {
  x <- stringr::str_squish(tolower(as.character(causa)))
  
  dplyr::case_when(
    x == "total" ~ "Total",
    stringr::str_detect(x, "neoplasm|malignant melanoma|hodgkin disease|lymphoma|leukaemia") ~ "Tumores",
    stringr::str_detect(x, "circulatory|heart disease|myocardial infarction|cerebrovascular|ischaemic heart") ~ "Enfermedades cardiovasculares",
    stringr::str_detect(x, "covid-19") ~ "COVID-19",
    stringr::str_detect(x, "respiratory|asthma|pneumonia|influenza") ~ "Enfermedades respiratorias",
    stringr::str_detect(x, "digestive|liver disease|hepatitis|ulcer of stomach") ~ "Enfermedades digestivas",
    stringr::str_detect(x, "genitourinary|kidney and ureter") ~ "Enfermedades genitourinarias",
    stringr::str_detect(x, "nervous system|sense organs|parkinson|dementia|alzheimer") ~ "Enfermedades neurológicas y de los órganos de los sentidos",
    stringr::str_detect(x, "endocrine|nutritional|metabolic|diabetes") ~ "Enfermedades endocrinas, nutricionales y metabólicas",
    stringr::str_detect(x, "mental and behavioural|mental and behavioral|drug dependence|toxicomania|alcohol") ~ "Trastornos mentales y del comportamiento",
    stringr::str_detect(x, "infectious and parasitic|tuberculosis|human immunodeficiency") ~ "Enfermedades infecciosas y parasitarias",
    stringr::str_detect(x, "musculoskeletal|rheumatoid arthritis|arthrosis") ~ "Enfermedades musculoesqueléticas",
    stringr::str_detect(x, "skin and subcutaneous") ~ "Enfermedades de la piel y del tejido subcutáneo",
    stringr::str_detect(x, "blood and blood-forming|immune mechanism") ~ "Enfermedades de la sangre y del sistema inmunitario",
    stringr::str_detect(x, "congenital") ~ "Malformaciones y anomalías congénitas",
    stringr::str_detect(x, "perinatal|sudden infant death") ~ "Afecciones del período perinatal",
    stringr::str_detect(x, "pregnancy|childbirth|puerperium") ~ "Embarazo, parto y puerperio",
    stringr::str_detect(x, "external causes|accident|assault|poisoning|falls|drowning|self-harm|undetermined intent") ~ "Causas externas",
    stringr::str_detect(x, "ill-defined|unknown causes|symptoms, signs") ~ "Síntomas, signos y causas mal definidas",
    TRUE ~ "Otras causas"
  )
}

# Causa madre preferida para cada grupo. Si existe, prevalece sobre sus
# subcausas. En tumores se prioriza "Neoplasms", que engloba los tumores.
fuentes_preferidas_europa <- list(
  "Total" = c("Total"),
  "Tumores" = c("Neoplasms", "Malignant neoplasms (C00-C97)", "Non-malignant neoplasms (benign and uncertain)"),
  "Enfermedades cardiovasculares" = c("Diseases of the circulatory system (I00-I99)"),
  "COVID-19" = character(0),
  "Enfermedades respiratorias" = c("Diseases of the respiratory system (J00-J99)"),
  "Enfermedades digestivas" = c("Diseases of the digestive system (K00-K93)"),
  "Enfermedades genitourinarias" = c("Diseases of the genitourinary system (N00-N99)"),
  "Enfermedades neurológicas y de los órganos de los sentidos" = c("Diseases of the nervous system and the sense organs (G00-H95)"),
  "Enfermedades endocrinas, nutricionales y metabólicas" = c("Endocrine, nutritional and metabolic diseases (E00-E90)"),
  "Trastornos mentales y del comportamiento" = c("Mental and behavioural disorders (F00-F99)"),
  "Enfermedades infecciosas y parasitarias" = c("Certain infectious and parasitic diseases (A00-B99)"),
  "Enfermedades musculoesqueléticas" = c("Diseases of the musculoskeletal system and connective tissue (M00-M99)"),
  "Enfermedades de la piel y del tejido subcutáneo" = c("Diseases of the skin and subcutaneous tissue (L00-L99)"),
  "Enfermedades de la sangre y del sistema inmunitario" = c("Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism"),
  "Malformaciones y anomalías congénitas" = c("Congenital malformations, deformations and chromosomal abnormalities (Q00-Q99)"),
  "Afecciones del período perinatal" = c("Certain conditions originating in the perinatal period (P00-P96)"),
  "Embarazo, parto y puerperio" = c("Pregnancy, childbirth and the puerperium (O00-O99)"),
  "Causas externas" = c("External causes of morbidity and mortality (V01-Y89)"),
  "Síntomas, signos y causas mal definidas" = c("Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)")
)

construir_europa_agrupada <- function(data) {
  if (!nrow(data)) {
    return(tibble::tibble(
      sexo = character(), pais = character(), anio = numeric(),
      causa_grupo = character(), defunciones = numeric()
    ))
  }
  
  # OPT: clasificar cada causa ÚNICA una sola vez y reutilizar por lookup
  # (antes se ejecutaban ~20 str_detect por cada fila del dataset).
  causas_unicas_cache <- unique(as.character(data$causa_icd10))
  clasif_cache <- stats::setNames(
    vapply(causas_unicas_cache, function(cc) clasificar_causa_europa(cc)[1], character(1)),
    causas_unicas_cache
  )
  data <- data %>% mutate(causa_grupo = unname(clasif_cache[as.character(causa_icd10)]))
  
  # Primero agregamos cada causa original por sexo, país y año.
  base <- data %>%
    group_by(sexo, pais, anio, causa_grupo, causa_icd10) %>%
    summarise(defunciones = sum(defunciones, na.rm = TRUE), .groups = "drop")
  
  base %>%
    group_by(sexo, pais, anio, causa_grupo) %>%
    summarise(
      defunciones = {
        presentes <- fuente <- fuentes_preferidas_europa[[dplyr::first(causa_grupo)]]
        presentes <- if (length(fuente)) fuente[fuente %in% causa_icd10] else character(0)
        if (length(presentes)) {
          # Si hay varias fuentes candidatas, se utiliza la primera existente.
          sum(defunciones[causa_icd10 == presentes[1]], na.rm = TRUE)
        } else {
          # Solo sumamos subcausas cuando no hay una categoría madre disponible.
          sum(defunciones, na.rm = TRUE)
        }
      },
      .groups = "drop"
    ) %>%
    filter(is.finite(defunciones))
}

if (!EUROPA_OK) europa_agrupada <- construir_europa_agrupada(df_clean)

# ------------------------------------------------------------------------------
# POBLACIÓN EUROPEA Y ESTANDARIZACIÓN DE DEFUNCIONES
# ------------------------------------------------------------------------------
# El archivo puede llamarse "poblacion_europa.csv" o "poblacion_europa".
if (!EUROPA_OK) {
candidatos_poblacion_eu <- c("poblacion_europa.csv", "poblacion_europa")
archivos_existentes_eu <- candidatos_poblacion_eu[file.exists(candidatos_poblacion_eu)]
archivo_poblacion_eu <- if (length(archivos_existentes_eu)) archivos_existentes_eu[[1]] else NA_character_

if (!is.na(archivo_poblacion_eu) && nzchar(archivo_poblacion_eu)) {
  # Este archivo se puede leer aunque no tenga extensión .csv.
  # Usamos primero coma y, si queda una sola columna, probamos punto y coma.
  df_p_eu_raw <- tryCatch(
    read.csv(archivo_poblacion_eu, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(df_p_eu_raw) || ncol(df_p_eu_raw) <= 1) {
    df_p_eu_raw <- tryCatch(
      read.csv2(archivo_poblacion_eu, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
      error = function(e) NULL
    )
  }
  if (is.null(df_p_eu_raw)) {
    stop(paste0("No se pudo leer el archivo de población europea: ", archivo_poblacion_eu))
  }
  nombres_p <- names(df_p_eu_raw)
  nombres_p_norm <- normalizar_nombre_columna(nombres_p)
  
  # Detectamos por nombre y, como respaldo, por las posiciones del CSV que indicaste.
  idx_pais <- which(grepl("geopolitical.*entity.*reporting|geopoliticalentityreporting", nombres_p_norm))[1]
  idx_anio <- which(grepl("time.*period|timeperiod", nombres_p_norm))[1]
  idx_valor <- which(grepl("obs.*value|obsvalue|population", nombres_p_norm))[1]
  
  if ((is.na(idx_pais) || is.na(idx_anio) || is.na(idx_valor)) && ncol(df_p_eu_raw) >= 12) {
    idx_pais <- if (is.na(idx_pais)) 9 else idx_pais
    idx_anio <- if (is.na(idx_anio)) 10 else idx_anio
    idx_valor <- if (is.na(idx_valor)) 12 else idx_valor
  }
  
  indices_validos <- all(c(idx_pais, idx_anio, idx_valor) >= 1) &&
    all(c(idx_pais, idx_anio, idx_valor) <= ncol(df_p_eu_raw))
  
  if (indices_validos) {
    poblacion_europa <- tibble::tibble(
      pais_pob = as.character(df_p_eu_raw[[idx_pais]]),
      anio = suppressWarnings(as.numeric(df_p_eu_raw[[idx_anio]])),
      poblacion = numero_limpio(df_p_eu_raw[[idx_valor]])
    ) %>%
      mutate(pais_key = normalizar_clave_pais_europa(pais_pob)) %>%
      filter(
        anio %in% unique(df_clean$anio),
        is.finite(anio),
        is.finite(poblacion),
        poblacion > 0
      )
    
    # Solo países presentes en el dataset de defunciones del estudio.
    paises_estudio_key <- unique(normalizar_clave_pais_europa(df_clean$pais))
    poblacion_europa <- poblacion_europa %>%
      filter(pais_key %in% paises_estudio_key) %>%
      group_by(pais_key, anio) %>%
      summarise(poblacion = max(poblacion, na.rm = TRUE), .groups = "drop")
  } else {
    stop(
      paste0(
        "No se pudieron identificar país/año/población en '", archivo_poblacion_eu,
        "'. Columnas detectadas: ", paste(names(df_p_eu_raw), collapse = ", ")
      )
    )
  }
} else {
  stop("No se encontró poblacion_europa.csv ni poblacion_europa en la carpeta de la aplicación.")
}
} # fin !EUROPA_OK (población europea)

if (!EUROPA_OK) {
europa_agrupada <- europa_agrupada %>%
  mutate(pais_key = normalizar_clave_pais_europa(pais)) %>%
  left_join(poblacion_europa, by = c("pais_key", "anio")) %>%
  mutate(
    tasa_100k = if_else(poblacion > 0, defunciones / poblacion * 100000, NA_real_)
  ) %>%
  filter(is.finite(defunciones), is.finite(tasa_100k)) %>%
  # OPT: normalización de sexo calculada UNA vez aquí (antes se repetía
  # con case_when en datos_modelo_europa y datos_europa_ranking_causas).
  mutate(sexo_norm = case_when(
    sexo %in% c("Hombres", "M", "Male", "Males") ~ "Hombres",
    sexo %in% c("Mujeres", "F", "Female", "Females") ~ "Mujeres",
    TRUE ~ as.character(sexo)
  ))
} # fin !EUROPA_OK (enriquecimiento)
if (!EUROPA_OK) cache_guardar(list(agrupada = europa_agrupada, poblacion = poblacion_europa),
                              .ftes_europa, file.path("cache", "cache_europa.rds"))
causas_europa_raw <- c(
  "Total",
  "Tumores",
  "Enfermedades cardiovasculares",
  "Enfermedades respiratorias",
  "COVID-19",
  "Enfermedades digestivas",
  "Enfermedades genitourinarias",
  "Enfermedades neurológicas y de los órganos de los sentidos",
  "Enfermedades endocrinas, nutricionales y metabólicas",
  "Trastornos mentales y del comportamiento",
  "Enfermedades infecciosas y parasitarias",
  "Enfermedades musculoesqueléticas",
  "Enfermedades de la piel y del tejido subcutáneo",
  "Enfermedades de la sangre y del sistema inmunitario",
  "Malformaciones y anomalías congénitas",
  "Afecciones del período perinatal",
  "Embarazo, parto y puerperio",
  "Causas externas",
  "Síntomas, signos y causas mal definidas",
  "Otras causas"
)
causas_europa_raw <- intersect(causas_europa_raw, unique(europa_agrupada$causa_grupo))
sexos_europa_raw <- sort(unique(europa_agrupada$sexo))
paises_europa_raw <- sort(unique(europa_agrupada$pais))
causas_europa_es <- setNames(causas_europa_raw, causas_europa_raw)
sexos_europa_es <- setNames(sexos_europa_raw, sexos_europa_raw)
paises_europa_es <- setNames(paises_europa_raw, traducir_pais(paises_europa_raw))

# Catálogos europeos ya agrupados.

# Geometrías europeas
# OPT: caché local en RDS. ne_countries() descarga de Natural Earth si no hay
# caché y es costoso; a partir de la primera ejecución se reutiliza el fichero.
# FIX: el conjunto "Europe" de Natural Earth no trae Turquía (clasificada en
# Asia) y en esta versión tampoco Chipre: se añaden explícitamente y se
# eliminan duplicados para no duplicar polígonos en los joins del mapa.
archivo_mapa_europa <- "mapa_europa.rds"
mapa_europa <- if (file.exists(archivo_mapa_europa)) {
  readRDS(archivo_mapa_europa)
} else {
  m <- dplyr::bind_rows(
    rnaturalearth::ne_countries(
      scale = "medium",
      type = "countries",
      continent = "Europe",
      returnclass = "sf"
    ),
    rnaturalearth::ne_countries(
      scale = "medium",
      type = "countries",
      country = c("Turkey", "Cyprus"),
      returnclass = "sf"
    )
  ) %>%
    st_transform(4326) %>%
    transmute(
      nombre_mapa = name_long,
      iso_a3 = iso_a3,
      geometry = geometry
    ) %>%
    distinct(nombre_mapa, .keep_all = TRUE)
  saveRDS(m, archivo_mapa_europa)
  m
}

mapa_nombre_eurostat <- c(
  "Austria" = "Austria", "Belgium" = "Belgium", "Bulgaria" = "Bulgaria",
  "Croatia" = "Croatia", "Cyprus" = "Cyprus", "Czechia" = "Czech Republic",
  "Czech Republic" = "Czech Republic", "Denmark" = "Denmark", "Estonia" = "Estonia",
  "Finland" = "Finland", "France" = "France", "Germany" = "Germany",
  "Greece" = "Greece", "Hungary" = "Hungary", "Iceland" = "Iceland",
  "Ireland" = "Ireland", "Italy" = "Italy", "Latvia" = "Latvia",
  "Liechtenstein" = "Liechtenstein", "Lithuania" = "Lithuania", "Luxembourg" = "Luxembourg",
  "Malta" = "Malta", "Netherlands" = "Netherlands", "Norway" = "Norway",
  "Poland" = "Poland", "Portugal" = "Portugal", "Romania" = "Romania",
  "Slovakia" = "Slovakia", "Slovenia" = "Slovenia", "Spain" = "Spain",
  "Sweden" = "Sweden", "Switzerland" = "Switzerland", "United Kingdom" = "United Kingdom",
  "Türkiye" = "Turkey", "Turkey" = "Turkey", "Albania" = "Albania",
  "Bosnia and Herzegovina" = "Bosnia and Herzegovina", "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
  "North Macedonia" = "North Macedonia", "Republic of North Macedonia" = "North Macedonia",
  "Montenegro" = "Montenegro", "Serbia" = "Serbia", "Kosovo" = "Kosovo",
  "Moldova" = "Moldova", "Republic of Moldova" = "Moldova", "Moldova (Republic of)" = "Moldova",
  "Ukraine" = "Ukraine", "Belarus" = "Belarus", "Georgia" = "Georgia",
  "Armenia" = "Armenia", "Azerbaijan" = "Azerbaijan", "Russia" = "Russia",
  "Russian Federation" = "Russia", "Andorra" = "Andorra", "Monaco" = "Monaco",
  "San Marino" = "San Marino", "Vatican City" = "Vatican", "Holy See" = "Vatican"
)

pais_eurostat_mapa <- tibble::tibble(
  pais = names(mapa_nombre_eurostat),
  nombre_mapa = unname(mapa_nombre_eurostat)
)

# Carga de Causas de Defunción España
df_causas_raw <- leer_tabla_semicolon("causas_defunciones.csv", col_char = "V5")

copia_causas <- df_causas_raw[-1, ]
colnames(copia_causas) <- c("Provincia", "Sexo", "Defunción", "Año", "Total")

copia_causas$Provincia <- iconv(copia_causas$Provincia, from = "latin1", to = "UTF-8", sub = "")
copia_causas$Defunción <- iconv(copia_causas$Defunción, from = "latin1", to = "UTF-8", sub = "")
copia_causas$Total     <- numero_limpio(copia_causas$Total)
copia_causas$Provincia <- normalizar_provincias(as.character(trimws(sub("^\\d+", "", copia_causas$Provincia))))
copia_causas$Defunción <- limpiar_texto(sub("^.*?\\.", "", copia_causas$Defunción))
copia_causas$Defunción <- trimws(copia_causas$Defunción)

copia_causas <- copia_causas %>%
  filter(!is.na(Defunción), Defunción != "", Defunción != " ") %>%
  filter(!grepl("Embarazo, parto y puerperio", Defunción, ignore.case = TRUE)) %>%
  filter(!grepl("Enfermedades de la piel", Defunción, ignore.case = TRUE)) %>%
  filter(!grepl("periodo perinatal", Defunción, ignore.case = TRUE)) %>%
  filter(!grepl("Enfermedades de la sangre", Defunción, ignore.case = TRUE))

copia_causas$Sexo      <- ifelse(grepl("Ambos", copia_causas$Sexo, ignore.case = TRUE), "Ambos", copia_causas$Sexo)
copia_causas$Provincia <- as.character(copia_causas$Provincia)
copia_causas$Sexo      <- as.character(copia_causas$Sexo)
copia_causas$Defunción <- as.character(copia_causas$Defunción)
copia_causas$Año       <- as.character(copia_causas$Año)

# Carga de Población
  df_pob_raw <- leer_tabla_semicolon("poblacion.csv", col_char = "V6")

if (ncol(df_pob_raw) > 4) {
  copia_p <- df_pob_raw[-1, -c(2, 3)] 
} else {
  copia_p <- df_pob_raw[-1, ]
}
colnames(copia_p) <- c("Provincia", "Sexo", "Año", "Poblacion")

copia_p$Provincia <- iconv(copia_p$Provincia, from = "latin1", to = "UTF-8", sub = "")
copia_p$Poblacion <- numero_limpio(copia_p$Poblacion)
copia_p$Provincia <- normalizar_provincias(as.character(trimws(sub("^\\d+", "", copia_p$Provincia))))

copia_p$Sexo <- case_when(
  grepl("Ambos", copia_p$Sexo, ignore.case = TRUE) ~ "Ambos",
  grepl("Hombres", copia_p$Sexo, ignore.case = TRUE) ~ "Hombres",
  grepl("Mujeres", copia_p$Sexo, ignore.case = TRUE) ~ "Mujeres",
  TRUE ~ copia_p$Sexo
)

copia_p <- copia_p %>%
  filter(!is.na(Poblacion), Poblacion > 0) %>%
  group_by(Provincia, Sexo, Año) %>%
  summarise(Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop")

copia_p$Provincia <- as.character(copia_p$Provincia)
copia_p$Sexo      <- as.character(copia_p$Sexo)
copia_p$Año       <- as.character(copia_p$Año)

copia_causas <- copia_causas %>%
  left_join(copia_p, by = c("Provincia", "Sexo", "Año")) %>%
  mutate(
    Año_Num = suppressWarnings(as.numeric(Año)),
    Tasa = if_else(Poblacion > 0, (Total / Poblacion) * 100000, NA_real_)
  ) %>%
  filter(is.finite(Tasa))

# Carga de Funciones Demográficas
# Procesado pesado de funciones.csv (73 MB) con caché en disco.
# sin_fuente_ok = TRUE: en el despliegue viaja solo la caché (ver .rscignore).
datos_func <- cache_cargar("funciones.csv", file.path("cache", "cache_funciones.rds"),
                           sin_fuente_ok = TRUE)
if (is.null(datos_func)) {
  df_func_raw <- leer_tabla_semicolon("funciones.csv", col_char = "V6")

  copia_func <- df_func_raw[-1, ]
  colnames(copia_func) <- c("Provincia", "Sexo", "Edad", "Funciones", "Año", "Total")

  copia_func$Provincia <- iconv(copia_func$Provincia, from = "latin1", to = "UTF-8", sub = "")
  copia_func$Funciones <- iconv(copia_func$Funciones, from = "latin1", to = "UTF-8", sub = "")
  copia_func$Edad      <- iconv(copia_func$Edad, from = "latin1", to = "UTF-8", sub = "")
  copia_func$Sexo      <- iconv(copia_func$Sexo, from = "latin1", to = "UTF-8", sub = "")

  copia_func$Funciones <- limpiar_texto(trimws(as.character(copia_func$Funciones)))
  funciones_deseadas <- sort(unique(copia_func$Funciones[nzchar(copia_func$Funciones) & copia_func$Funciones != "Defunciones teóricas"]))

  copia_func <- copia_func %>%
    filter(Año %in% c("2018", "2019", "2020", "2021", "2022"))

  copia_func$Edad <- gsub("aÃ±os|años|aÃ\u00b1os", "", copia_func$Edad, ignore.case = TRUE)
  copia_func$Edad <- gsub("mÃ¡s|más", "más", copia_func$Edad, ignore.case = TRUE)
  copia_func$Edad <- trimws(copia_func$Edad)

  copia_func <- copia_func %>%
    filter(!grepl("^90 y más$", Edad, ignore.case = TRUE) & !grepl("^90 años y más$", Edad, ignore.case = TRUE))

  copia_func <- copia_func %>%
    mutate(
      orden_edad = case_when(
        grepl("95", Edad) ~ 95,
        grepl("90", Edad) ~ 90,
        TRUE ~ as.numeric(gsub("[^0-9].*", "", gsub("^De ", "", Edad)))
      )
    ) %>%
    mutate(orden_edad = ifelse(is.na(orden_edad), 0, orden_edad))

  niveles_ordenados <- copia_func %>%
    select(Edad, orden_edad) %>%
    distinct() %>%
    arrange(orden_edad) %>%
    pull(Edad)

  copia_func$Edad <- factor(copia_func$Edad, levels = niveles_ordenados)

  copia_func$Total <- numero_limpio(copia_func$Total)
  copia_func <- copia_func %>% filter(!is.na(Total))

  copia_func <- copia_func %>%
    mutate(
      Total = ifelse(
        grepl("Riesgo de muerte", Funciones, ignore.case = TRUE) & Total > 1,
        Total / 1000,
        Total
      )
    )

  copia_func$Provincia <- normalizar_provincias(as.character(trimws(sub("^\\d+", "", copia_func$Provincia))))

  copia_func$Sexo <- case_when(
    grepl("Ambos", copia_func$Sexo, ignore.case = TRUE) ~ "Ambos",
    grepl("Hombres", copia_func$Sexo, ignore.case = TRUE) ~ "Hombres",
    grepl("Mujeres", copia_func$Sexo, ignore.case = TRUE) ~ "Mujeres",
    TRUE ~ copia_func$Sexo
  )

  copia_func$Provincia <- as.character(copia_func$Provincia)
  copia_func$Sexo      <- as.character(copia_func$Sexo)
  copia_func$Funciones <- as.character(copia_func$Funciones)
  copia_func$Año       <- as.character(copia_func$Año)

  datos_func <- list(
    copia_func = copia_func,
    funciones_deseadas = funciones_deseadas,
    niveles_ordenados = niveles_ordenados
  )
  cache_guardar(datos_func, "funciones.csv", file.path("cache", "cache_funciones.rds"))
}
copia_func <- datos_func$copia_func
funciones_deseadas <- datos_func$funciones_deseadas
niveles_ordenados <- datos_func$niveles_ordenados
rm(datos_func)

# Resúmenes reutilizables (0.52 + 0.83 s): en disco, no se reagrupan.
resumenes <- local({
  .rds_res <- file.path("cache", "cache_resumenes.rds")
  .obj_res <- cache_cargar(c("causas_defunciones.csv", "funciones.csv"), .rds_res)
  if (!is.null(.obj_res)) return(.obj_res)
  causas_provinciales <- copia_causas %>%
    group_by(Año, Año_Num, Sexo, Provincia, Defunción) %>%
    summarise(
      Fallecidos = sum(Total, na.rm = TRUE),
      Poblacion = sum(Poblacion, na.rm = TRUE),
      Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(Total = Fallecidos) %>%
    filter(is.finite(Tasa))

  funciones_provinciales <- copia_func %>%
    group_by(Año, Provincia, Sexo, Funciones, Edad) %>%
    summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop") %>%
    filter(is.finite(Valor))

  .obj_res <- list(causas_provinciales = causas_provinciales,
                   funciones_provinciales = funciones_provinciales)
  cache_guardar(.obj_res, c("causas_defunciones.csv", "funciones.csv"), .rds_res)
  .obj_res
})
causas_provinciales <- resumenes$causas_provinciales
funciones_provinciales <- resumenes$funciones_provinciales
rm(resumenes)

# Relación provincia → comunidad autónoma para el análisis temporal.
# Se usa el diccionario oficial incluido en mapSpain para evitar promediar tasas provinciales.
clave_geo <- function(x) {
  y <- stringr::str_squish(as.character(x))
  y <- iconv(y, from = "", to = "ASCII//TRANSLIT")
  toupper(y)
}

prov_ccaa_lookup <- mapSpain::esp_codelist %>%
  transmute(
    clave_geo = clave_geo(ine.prov.name),
    Comunidad = as.character(ine.ccaa.name)
  ) %>%
  filter(!is.na(clave_geo), nzchar(clave_geo), !is.na(Comunidad), nzchar(Comunidad)) %>%
  distinct(clave_geo, .keep_all = TRUE)

# Fallback explícito provincia → comunidad para evitar comunidades sin nombre
# cuando el nombre de una provincia no coincide exactamente con el diccionario geográfico.
prov_ccaa_fallback <- c(
  "A CORUNA" = "Galicia", "ALAVA" = "País Vasco", "ALBACETE" = "Castilla-La Mancha",
  "ALICANTE" = "Comunidad Valenciana", "ALMERIA" = "Andalucía", "ASTURIAS" = "Asturias",
  "AVILA" = "Castilla y León", "BADAJOZ" = "Extremadura", "BALEARES" = "Islas Baleares",
  "BARCELONA" = "Cataluña", "BIZKAIA" = "País Vasco", "BURGOS" = "Castilla y León",
  "CACERES" = "Extremadura", "CADIZ" = "Andalucía", "CANTABRIA" = "Cantabria",
  "CASTELLON" = "Comunidad Valenciana", "CIUDAD REAL" = "Castilla-La Mancha",
  "CORDOBA" = "Andalucía", "CUENCA" = "Castilla-La Mancha", "GIRONA" = "Cataluña",
  "GRANADA" = "Andalucía", "GUADALAJARA" = "Castilla-La Mancha", "GIPUZKOA" = "País Vasco",
  "GUIPUZCOA" = "País Vasco",
  "HUELVA" = "Andalucía", "HUESCA" = "Aragón", "JAEN" = "Andalucía",
  "LA RIOJA" = "La Rioja", "LAS PALMAS" = "Canarias", "LEON" = "Castilla y León",
  "LLEIDA" = "Cataluña", "LUGO" = "Galicia", "MADRID" = "Madrid",
  "MALAGA" = "Andalucía", "MURCIA" = "Murcia", "NAVARRA" = "Navarra",
  "OURENSE" = "Galicia", "PALENCIA" = "Castilla y León", "PONTEVEDRA" = "Galicia",
  "SALAMANCA" = "Castilla y León", "SANTA CRUZ DE TENERIFE" = "Canarias",
  "SEGOVIA" = "Castilla y León", "SEVILLA" = "Andalucía", "SORIA" = "Castilla y León",
  "TARRAGONA" = "Cataluña", "TERUEL" = "Aragón", "TOLEDO" = "Castilla-La Mancha",
  "VALENCIA" = "Comunidad Valenciana", "VALLADOLID" = "Castilla y León",
  "VIZCAYA" = "País Vasco", "ZAMORA" = "Castilla y León", "ZARAGOZA" = "Aragón",
  "CEUTA" = "Ceuta", "MELILLA" = "Melilla"
)

causas_provinciales <- causas_provinciales %>%
  mutate(clave_geo = clave_geo(Provincia)) %>%
  left_join(prov_ccaa_lookup, by = "clave_geo") %>%
  mutate(
    Comunidad = dplyr::coalesce(Comunidad, unname(prov_ccaa_fallback[clave_geo])),
    Comunidad = dplyr::coalesce(Comunidad, "Sin asignar"),
    Comunidad = case_when(
      stringr::str_detect(clave_geo(Comunidad), "ASTURIAS|PRINCIPADO") ~ "Asturias",
      stringr::str_detect(clave_geo(Comunidad), "NAVARRA") ~ "Navarra",
      stringr::str_detect(clave_geo(Comunidad), "MURCIA") ~ "Murcia",
      stringr::str_detect(clave_geo(Comunidad), "MADRID") ~ "Madrid",
      stringr::str_detect(clave_geo(Comunidad), "VALENCIANA") ~ "Comunidad Valenciana",
      TRUE ~ Comunidad
    )
  ) %>%
  select(-clave_geo)

lista_provincias  <- sort(unique(as.character(copia_func$Provincia)))
lista_defunciones <- sort(unique(as.character(copia_causas$Defunción)))
lista_defunciones <- lista_defunciones[nzchar(trimws(lista_defunciones))]
lista_edades      <- levels(copia_func$Edad)
# Edades disponibles para Esperanza de vida (ordenadas como los niveles).
ev_edades <- lista_edades[lista_edades %in% unique(as.character(
  funciones_provinciales$Edad[funciones_provinciales$Funciones == "Esperanza de vida"]))]

# ==============================================================================
# 2. MAPA DE ESPAÑA
# ==============================================================================
# OPT: en disco (0.44 s; geometrías estáticas: caché permanente hasta bump de
# CACHE_VERSION).
mapa_provincias <- local({
  .rds_mapa <- file.path("cache", "cache_mapa.rds")
  .obj_mapa <- cache_cargar(character(0), .rds_mapa)
  if (!is.null(.obj_mapa)) return(.obj_mapa)
  .obj_mapa <- mapSpain::esp_get_prov() %>%
    st_transform(4326) %>%
    mutate(NAME_2 = normalizar_provincias(ine.prov.name))
  cache_guardar(.obj_mapa, character(0), .rds_mapa)
  .obj_mapa
})

# ------------------------------------------------------------------------------
# OPT: PRECOMPUTACIONES GLOBALES PARA REGRESIONES + MORAN (una sola vez)
# ------------------------------------------------------------------------------
# Vecinos espaciales (k=4 más cercanas, sin Ceuta/Melilla): antes se recalculaban
# st_coordinates + knearneigh + nb2listw en CADA test de Moran.
mapa_vecinos_base <- mapa_provincias %>%
  filter(!NAME_2 %in% c("Ceuta", "Melilla")) %>%
  select(Provincia = NAME_2, geometry)
# OPT: perezoso (0.78 s solo si se abre Regresiones): centroides + k=4 +
# pesos W se calculan una vez por sesión al primer uso.
vecinos_memo <- new.env(parent = emptyenv())
get_vecinos <- function() {
  if (!is.null(vecinos_memo$obj)) return(vecinos_memo$obj)
  coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(mapa_vecinos_base)))
  nb <- spdep::knn2nb(spdep::knearneigh(coords, k = min(4, nrow(mapa_vecinos_base) - 1)))
  vecinos_memo$obj <- list(coords = coords, nb = nb,
                           lw = spdep::nb2listw(nb, style = "W", zero.policy = TRUE))
  vecinos_memo$obj
}


# ==============================================================================
# 2.5. NUEVOS DATASETS: APVP, MENSUAL Y EDAD/SEXO POR COMUNIDAD
# ==============================================================================

# Años potenciales de vida perdidos (APVP)
# Estos CSV proceden del INE y vienen separados por punto y coma y codificados
# en latin1. Los leemos por posición para no depender de nombres de columnas
# que pueden cambiar al interpretar los acentos.
df_apvp_raw <- tryCatch(
  utils::read.csv2("years_perdidos.csv", header = TRUE, check.names = FALSE,
                   stringsAsFactors = FALSE, fileEncoding = "latin1",
                   colClasses = rep("character", 6)),
  error = function(e) NULL
)
if (is.null(df_apvp_raw) || ncol(df_apvp_raw) < 6) {
  stop("No se pudo leer correctamente 'years_perdidos.csv'. Se esperan 6 columnas.")
}

apvp_data <- tibble::tibble(
  Comunidad = as.character(df_apvp_raw[[2]]),
  Causa = as.character(df_apvp_raw[[3]]),
  Indicador = as.character(df_apvp_raw[[4]]),
  Sexo = as.character(df_apvp_raw[[5]]),
  Valor = numero_limpio(df_apvp_raw[[6]])
) %>%
  mutate(
    Comunidad = trimws(sub("^\\d+\\s+", "", Comunidad)),
    Causa = limpiar_texto(trimws(Causa)),
    Causa = trimws(sub("^[IVXLC]+(-[IVXLC]+)?\\.\\s*", "", Causa)),
    Indicador = limpiar_texto(trimws(Indicador)),
    Sexo = trimws(Sexo)
  ) %>%
  filter(
    !is.na(Comunidad), Comunidad != "",
    !is.na(Causa), Causa != "",
    !is.na(Indicador), Indicador != "",
    is.finite(Valor)
  )

apvp_causas <- sort(unique(apvp_data$Causa))
apvp_comunidades <- sort(unique(apvp_data$Comunidad))
apvp_indicadores <- sort(unique(apvp_data$Indicador))

# Defunciones mensuales
df_meses_raw <- tryCatch(
  utils::read.csv2("defunciones_meses.csv", header = TRUE, check.names = FALSE,
                   stringsAsFactors = FALSE, fileEncoding = "latin1",
                   colClasses = rep("character", 5)),
  error = function(e) NULL
)
if (is.null(df_meses_raw) || ncol(df_meses_raw) < 5) {
  stop("No se pudo leer correctamente 'defunciones_meses.csv'. Se esperan 5 columnas.")
}

meses_data <- tibble::tibble(
  Causa = as.character(df_meses_raw[[1]]),
  Mes = as.character(df_meses_raw[[2]]),
  Sexo = as.character(df_meses_raw[[3]]),
  Año = as.character(df_meses_raw[[4]]),
  Defunciones = numero_limpio(df_meses_raw[[5]])
) %>%
  mutate(
    Causa = limpiar_texto(trimws(sub("^[^ ]+\\s+", "", Causa))),
    Causa = trimws(sub("^[IVXLC]+(-[IVXLC]+)?\\.\\s*", "", Causa)),
    Mes = trimws(Mes),
    Sexo = trimws(Sexo),
    Año = as.character(Año)
  ) %>%
  filter(is.finite(Defunciones))

orden_meses <- c(
  "Enero","Febrero","Marzo","Abril","Mayo","Junio",
  "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"
)
meses_data$Mes <- factor(meses_data$Mes, levels = orden_meses, ordered = TRUE)
meses_causas <- sort(unique(as.character(meses_data$Causa)))
meses_anios <- sort(unique(meses_data$Año))
meses_sexos <- c("Ambos", "Hombres", "Mujeres")

# Defunciones por edad, sexo y comunidad autónoma
df_edad_com_raw <- tryCatch(
  utils::read.csv2("defunciones_sex_comunidad.csv", header = TRUE, check.names = FALSE,
                   stringsAsFactors = FALSE, fileEncoding = "latin1",
                   colClasses = rep("character", 6)),
  error = function(e) NULL
)
if (is.null(df_edad_com_raw) || ncol(df_edad_com_raw) < 6) {
  stop("No se pudo leer correctamente 'defunciones_sex_comunidad.csv'. Se esperan 6 columnas.")
}

edad_com_data <- tibble::tibble(
  Comunidad = as.character(df_edad_com_raw[[2]]),
  Causa = as.character(df_edad_com_raw[[3]]),
  Edad = as.character(df_edad_com_raw[[4]]),
  Sexo = as.character(df_edad_com_raw[[5]]),
  Defunciones = numero_limpio(df_edad_com_raw[[6]])
) %>%
  mutate(
    Comunidad = trimws(sub("^\\d+\\s+", "", Comunidad)),
    Causa = limpiar_texto(trimws(sub("^[^ ]+\\s+", "", Causa))),
    Causa = trimws(sub("^[IVXLC]+(-[IVXLC]+)?\\.\\s*", "", Causa)),
    Edad = trimws(limpiar_texto(Edad)),
    Sexo = trimws(Sexo)
  ) %>%
  filter(is.finite(Defunciones))

# Orden de edad para la pirámide
orden_edad_com <- edad_com_data %>%
  distinct(Edad) %>%
  mutate(
    orden = suppressWarnings(as.numeric(gsub("[^0-9].*", "", gsub("^De ", "", Edad))))
  ) %>%
  mutate(orden = ifelse(is.na(orden), 0, orden)) %>%
  arrange(orden) %>%
  pull(Edad)

edad_com_causas <- sort(unique(as.character(edad_com_data$Causa)))
edad_com_comunidades <- sort(unique(as.character(edad_com_data$Comunidad)))

# ==============================================================================
# 2.6. TASAS ESTANDARIZADAS POR EDAD (POBLACIÓN ESTÁNDAR EUROPEA 2013)
# ==============================================================================
# Las tasas brutas mezclan riesgo y estructura de edad: una provincia
# envejecida muestra tasas altas aunque su riesgo real sea normal. La
# estandarización directa repondera las tasas específicas por edad con una
# población de referencia común (ESP 2013, Eurostat) y hace comparables
# los territorios.
#
# Ficheros opcionales (si no existen, la pestaña explica qué se necesita):
#   defunciones_edad_provincia.csv : Provincia;Sexo;Año;Edad;Defunciones[;Causa]
#   poblacion_edad_provincia.csv   : Provincia;Sexo;Año;Edad;Poblacion
# Separador ";", latin1 o UTF-8, con fila de cabecera (se toleran filas de
# metadatos previas: la cabecera se detecta por nombre).

# Pesos ESP 2013 por 100.000 habitantes (Eurostat): 0, 1-4, 5-9, ..., 90-94, 95+.
esp2013_pesos <- c(
  "0" = 1000, "1-4" = 4000, "5-9" = 5500, "10-14" = 5500, "15-19" = 5500,
  "20-24" = 6000, "25-29" = 6000, "30-34" = 6500, "35-39" = 7000,
  "40-44" = 7000, "45-49" = 7000, "50-54" = 7000, "55-59" = 6500,
  "60-64" = 6000, "65-69" = 5500, "70-74" = 5000, "75-79" = 4000,
  "80-84" = 2500, "85-89" = 1500, "90-94" = 800, "95+" = 200
)

# Límite inferior de edad -> grupo ESP 2013.
edad_a_grupo_esp <- function(lim_inf) {
  lim_inf <- suppressWarnings(as.numeric(lim_inf))
  vapply(lim_inf, function(b) {
    if (!is.finite(b) || b < 0) return(NA_character_)
    if (b < 1) return("0")
    if (b < 5) return("1-4")
    if (b >= 95) return("95+")
    ini <- 5 * floor(b / 5)
    paste0(ini, "-", ini + 4)
  }, character(1))
}

# Límite inferior desde etiquetas libres ("De 1 a 4", "95 y más",
# "Menores de 1 año", "80", "80-84"...). NA si no se reconoce.
edad_limite_inferior <- function(x) {
  x0 <- as.character(x)
  xl <- tolower(stringi::stri_trans_general(x0, "Latin-ASCII"))
  n0 <- suppressWarnings(as.numeric(gsub("[^0-9].*", "", gsub("^de ", "", xl))))
  es_menor1 <- grepl("menor|menos|< ?1|de 0", xl)
  ifelse(es_menor1, 0, n0)
}

# Lee un fichero INE de estructura por edad con cabecera flexible.
# col_valor: patrón de la columna de valor ("defuncion|fallecid"|"poblacion").
# Tolera UTF-8 y Latin-1: detecta la codificación (ICU), prueba ambas y se queda
# con la primera cuya decodificación sea UTF-8 válido, sin mojibake ("Ã") y con
# todas las columnas detectadas.
detectar_encoding <- function(path) {
  tryCatch({
    n <- file.info(path)$size
    if (is.na(n) || n <= 0) return("UTF-8")
    raw <- readBin(path, "raw", min(n, 500000))
    det <- stringi::stri_enc_detect(raw)[[1]]
    enc <- det$Encoding[which.max(det$Confidence)]
    if (grepl("1252|8859|latin", enc, ignore.case = TRUE)) "Latin-1" else "UTF-8"
  }, error = function(e) "UTF-8")
}

texto_limpio <- function(df) {
  txt <- unlist(lapply(df, function(col) if (is.character(col)) col else NULL))
  txt <- txt[!is.na(txt)]
  if (!length(txt)) return(TRUE)
  all(validUTF8(txt)) && !any(grepl("Ã", txt, fixed = TRUE))
}

leer_ine_edad <- function(path, col_valor) {
  enc0 <- detectar_encoding(path)
  for (enc in unique(c(enc0, "UTF-8", "Latin-1"))) {
    out <- tryCatch(leer_ine_edad_enc(path, col_valor, enc), error = function(e) NULL)
    if (!is.null(out) && texto_limpio(out)) return(out)
  }
  NULL
}

leer_ine_edad_enc <- function(path, col_valor, enc) {
  raw <- tryCatch(
    # FIX: todo texto (ver leer_tabla_semicolon): "12.460" son miles y como
    # número perdería el cero (12.46). Todo el procesado posterior es char-safe.
    data.table::fread(path, sep = ";", encoding = enc, header = FALSE, showProgress = FALSE,
                      colClasses = "character"),
    error = function(e) NULL
  )
  if (is.null(raw) || !nrow(raw)) return(NULL)
  df <- as.data.frame(raw, stringsAsFactors = FALSE)
  # fread etiqueta pero no siempre transcodifica: se convierte del encoding
  # del intento a UTF-8 real (un intento erróneo deja NA/"" y lo rechaza
  # después la detección de columnas y el validador texto_limpio).
  for (j in seq_len(ncol(df))) {
    if (is.character(df[[j]])) {
      df[[j]] <- tryCatch(
        iconv(df[[j]], from = enc, to = "UTF-8", sub = ""),
        error = function(e) df[[j]]
      )
    }
  }
  norm <- function(v) tolower(stringi::stri_trans_general(as.character(v), "Latin-ASCII"))
  # OPT: la cabecera está siempre al principio (metadatos previos): basta
  # escanear las primeras filas en vez de aplicar una función por fila a
  # los millones de registros (eso costaba cientos de segundos).
  n_scan <- min(100, nrow(df))
  mat <- as.matrix(df[seq_len(n_scan), , drop = FALSE])
  es_cab <- apply(mat, 1, function(fila) {
    n <- norm(fila)
    any(grepl("provincia", n)) && any(grepl("edad", n))
  })
  i_cab <- which(es_cab)[1]
  if (is.na(i_cab) || i_cab >= nrow(df)) return(NULL)
  nombres <- trimws(as.character(unlist(df[i_cab, , drop = TRUE])))
  if (length(nombres) != ncol(df)) {
    length(nombres) <- ncol(df)
    nombres[is.na(nombres)] <- paste0("V", which(is.na(nombres)))
  }
  datos <- df[(i_cab + 1):nrow(df), , drop = FALSE]
  names(datos) <- nombres
  n_norm <- norm(names(datos))
  col_prov  <- which(grepl("provincia", n_norm))[1]
  col_sexo  <- which(grepl("sexo", n_norm))[1]
  # "ano" solo como palabra exacta: como subcadena casa dentro de "Españoles"
  # y robaría esa columna (fue un NULL silencioso en el Padrón).
  col_anio  <- which(grepl("año|periodo|ejercicio", n_norm))[1]
  if (is.na(col_anio)) col_anio <- which(trimws(n_norm) == "ano")[1]
  col_edad  <- which(grepl("edad|tramo", n_norm))[1]
  col_causa <- which(grepl("causa", n_norm))[1]
  col_mes   <- which(grepl("^mes", n_norm))[1]
  asignadas <- stats::na.omit(c(col_prov, col_sexo, col_anio, col_edad, col_causa, col_mes))
  # Valor: patrón pedido entre columnas no asignadas; si no hay (p. ej. la
  # columna se llama "Total"), se acepta una columna llamada exactamente así.
  col_val <- which(grepl(col_valor, n_norm) & !(seq_along(n_norm) %in% asignadas))[1]
  if (is.na(col_val)) {
    col_val <- which(n_norm == "total" & !(seq_along(n_norm) %in% asignadas))[1]
  }
  if (any(is.na(c(col_prov, col_sexo, col_anio, col_edad, col_val)))) return(NULL)
  # OPT: millones de filas con pocos valores distintos: cada normalización
  # costosa se aplica una sola vez por valor único y se propaga por índice.
  mapear_unicos <- function(raw, fun) {
    u <- unique(raw)
    unname(fun(u)[match(raw, u)])
  }
  prov_map <- mapear_unicos(
    as.character(datos[[col_prov]]),
    function(v) normalizar_provincias(trimws(sub("^\\d+\\s*", "", v)))
  )
  sexo_map <- mapear_unicos(
    norm(datos[[col_sexo]]),
    function(v) ifelse(grepl("hombr", v), "Hombres",
                ifelse(grepl("mujer", v), "Mujeres", NA_character_))
  )
  edad_map <- mapear_unicos(
    as.character(datos[[col_edad]]), edad_limite_inferior
  )
  val_map <- mapear_unicos(
    as.character(datos[[col_val]]), numero_limpio
  )
  mes_map <- mapear_unicos(
    if (!is.na(col_mes)) as.character(datos[[col_mes]]) else "Total",
    trimws
  )
  causa_map <- if (!is.na(col_causa)) {
    mapear_unicos(
      as.character(datos[[col_causa]]),
      function(v) limpiar_texto(trimws(sub("^[IVXLC]+(-[IVXLC]+)?\\.\\s*", "",
                    trimws(sub("^\\d+[-–]\\d+\\s+", "", v)))))
    )
  } else {
    "Todas las causas"
  }
  out <- tibble::tibble(
    Provincia = prov_map,
    Sexo = sexo_map,
    Año = trimws(as.character(datos[[col_anio]])),
    Edad_num = edad_map,
    Mes = mes_map,
    Valor = val_map,
    Causa = causa_map
  ) %>%
    mutate(Grupo = edad_a_grupo_esp(Edad_num)) %>%
    filter(!is.na(Sexo), nzchar(Provincia), nzchar(Año),
           is.finite(Valor), Valor >= 0, nzchar(Causa))
  if (!nrow(out)) return(NULL)
  out
}

# Carga y combina defunciones + población por edad. Devuelve NULL si falta
# algún fichero o no hay datos válidos (la UI muestra entonces la guía).
# "Ambos" se calcula sumando sexos (nunca se hereda de filas de totales).
cargar_datos_edad <- function(ruta_def = "defunciones_edad_provincia.csv",
                              ruta_pob = "poblacion_edad_provincia.csv") {
  if (!file.exists(ruta_def) || !file.exists(ruta_pob)) return(NULL)
  def <- leer_ine_edad(ruta_def, "defuncion|fallecid")
  pob <- leer_ine_edad(ruta_pob, "poblacion")
  if (is.null(def) || is.null(pob) || !nrow(def) || !nrow(pob)) return(NULL)
  def <- def %>%
    filter(!Provincia %in% c("Total Nacional", "No residente"))
  if (!nrow(def)) return(NULL)
  # FIX: solo filas de mes total (si el fichero trae desglose mensual, sumar
  # también los 12 meses duplicaría las defunciones).
  base <- def %>%
    filter(Mes == "Total") %>%
    group_by(Provincia, Sexo, Año, Causa, Grupo) %>%
    summarise(Def = sum(Valor, na.rm = TRUE), .groups = "drop")
  base_p <- pob %>%
    group_by(Provincia, Sexo, Año, Grupo) %>%
    summarise(Pob = sum(Valor, na.rm = TRUE), .groups = "drop")
  ambos_d <- base %>%
    group_by(Provincia, Año, Causa, Grupo) %>%
    summarise(Def = sum(Def, na.rm = TRUE), .groups = "drop") %>%
    mutate(Sexo = "Ambos")
  ambos_p <- base_p %>%
    group_by(Provincia, Año, Grupo) %>%
    summarise(Pob = sum(Pob, na.rm = TRUE), .groups = "drop") %>%
    mutate(Sexo = "Ambos")
  tasas <- dplyr::bind_rows(base, ambos_d) %>%
    left_join(dplyr::bind_rows(base_p, ambos_p),
              by = c("Provincia", "Sexo", "Año", "Grupo")) %>%
    filter(is.finite(Def), is.finite(Pob), Pob > 0) %>%
    mutate(Tasa = Def / Pob * 100000,
           Peso = unname(esp2013_pesos[Grupo])) %>%
    filter(is.finite(Tasa), is.finite(Peso))
  if (!nrow(tasas)) return(NULL)
  # Bruta = ponderada real; estandarizada = reponderada ESP con los pesos
  # renormalizados sobre los grupos de edad presentes en los datos.
  resumen <- tasas %>%
    group_by(Provincia, Sexo, Año, Causa) %>%
    summarise(
      Defunciones = sum(Def, na.rm = TRUE),
      Poblacion = sum(Pob, na.rm = TRUE),
      Tasa_bruta = if_else(sum(Pob, na.rm = TRUE) > 0,
                           sum(Def, na.rm = TRUE) / sum(Pob, na.rm = TRUE) * 100000, NA_real_),
      Tasa_std = sum(Tasa * Peso, na.rm = TRUE) / sum(Peso, na.rm = TRUE),
      Grupos = dplyr::n_distinct(Grupo),
      .groups = "drop"
    ) %>%
    filter(is.finite(Tasa_bruta), is.finite(Tasa_std))
  if (!nrow(resumen)) return(NULL)
  list(tasas = tasas,
       resumen = resumen,
       anos = sort(unique(as.character(resumen$Año))),
       causas = sort(unique(as.character(resumen$Causa))))
}

# Con caché en disco: el parseo del CSV de 150 MB es costoso y aquí no se
# puede reutilizar el de datos_edadprov (distinta agregación).
datos_edad_std <- local({
  rds <- file.path("cache", "cache_edadstd.rds")
  ftes <- c("defunciones_edad_provincia.csv", "poblacion_edad_provincia.csv")
  obj <- cache_cargar(ftes, rds)
  if (is.null(obj)) {
    obj <- cargar_datos_edad()
    if (!is.null(obj)) cache_guardar(obj, ftes, rds)
  }
  obj
})

# ------------------------------------------------------------------------------
# PERFIL POR EDAD Y MES (defunciones observadas, sin denominador).
# Usa solo defunciones_edad_provincia.csv: pirámide y mediana por edad simple,
# estacionalidad por provincia y serie anual 2009-2024. Sin filas de totales
# heredadas ("Ambos" se calcula sumando) y sin "Total Nacional"/"No residente"
# (la referencia nacional se calcula sumando provincias).
# ------------------------------------------------------------------------------
cargar_edadprov <- function(ruta_def = "defunciones_edad_provincia.csv") {
  if (!file.exists(ruta_def)) return(NULL)
  def <- leer_ine_edad(ruta_def, "defuncion|fallecid")
  if (is.null(def) || !nrow(def)) return(NULL)
  def <- def %>%
    filter(!Provincia %in% c("Total Nacional", "No residente"))
  if (!nrow(def)) return(NULL)
  base_edad <- def %>%
    filter(Mes == "Total", is.finite(Edad_num)) %>%
    group_by(Provincia, Sexo, Año, Edad_num) %>%
    summarise(Def = sum(Valor, na.rm = TRUE), .groups = "drop")
  base_mes <- def %>%
    filter(is.na(Edad_num), Mes != "Total") %>%
    group_by(Provincia, Sexo, Año, Mes) %>%
    summarise(Def = sum(Valor, na.rm = TRUE), .groups = "drop")
  base_anual <- def %>%
    filter(is.na(Edad_num), Mes == "Total") %>%
    group_by(Provincia, Sexo, Año) %>%
    summarise(Def = sum(Valor, na.rm = TRUE), .groups = "drop")
  if (!nrow(base_edad) || !nrow(base_anual)) return(NULL)
  amb_edad <- base_edad %>%
    group_by(Provincia, Año, Edad_num) %>%
    summarise(Def = sum(Def, na.rm = TRUE), .groups = "drop") %>%
    mutate(Sexo = "Ambos")
  amb_mes <- base_mes %>%
    group_by(Provincia, Año, Mes) %>%
    summarise(Def = sum(Def, na.rm = TRUE), .groups = "drop") %>%
    mutate(Sexo = "Ambos")
  amb_anual <- base_anual %>%
    group_by(Provincia, Año) %>%
    summarise(Def = sum(Def, na.rm = TRUE), .groups = "drop") %>%
    mutate(Sexo = "Ambos")
  edad <- dplyr::bind_rows(base_edad, amb_edad)
  mes <- dplyr::bind_rows(base_mes, amb_mes)
  anual <- dplyr::bind_rows(base_anual, amb_anual)
  if (!nrow(edad) || !nrow(anual)) return(NULL)
  # Los indicadores (mediana, %65+, pico) se calculan en el servidor sobre
  # estas distribuciones para que "Todas" agregue correctamente.
  list(edad = edad,
       mes = mes,
       anual = anual,
       provincias = sort(unique(as.character(edad$Provincia))),
       anos = sort(unique(as.character(edad$Año))),
       meses = intersect(orden_meses, unique(as.character(mes$Mes))))
}

# Con caché en disco (el CSV pesa ~150 MB). Si falta el CSV pero existe la
# caché (despliegue), se usa igualmente avisando por consola.
datos_edadprov <- local({
  rds <- file.path("cache", "cache_edadprov.rds")
  if (file.exists("defunciones_edad_provincia.csv")) {
    obj <- cache_cargar("defunciones_edad_provincia.csv", rds)
    if (is.null(obj)) {
      obj <- cargar_edadprov()
      if (!is.null(obj)) cache_guardar(obj, "defunciones_edad_provincia.csv", rds)
    }
    obj
  } else if (file.exists(rds)) {
    message("defunciones_edad_provincia.csv ausente: usando caché de despliegue (puede estar desactualizada).")
    g <- tryCatch(readRDS(rds), error = function(e) NULL)
    if (is.list(g) && all(c("version", "datos") %in% names(g))) g$datos else g
  } else {
    NULL
  }
})


# ------------------------------------------------------------------------------
# PADRÓN POR EDAD (pirámide real y envejecimiento): en disco (0.26 s).
# -----------------------------------------------------------------------------
padron_edad <- local({
  .rds_pad <- file.path("cache", "cache_padron.rds")
  .obj_pad <- cache_cargar("poblacion_edad_provincia.csv", .rds_pad)
  if (!is.null(.obj_pad)) return(.obj_pad)
  .obj_pad <- leer_ine_edad("poblacion_edad_provincia.csv", "poblacion")
  if (!is.null(.obj_pad)) {
    .obj_pad <- .obj_pad %>%
      filter(Sexo %in% c("Hombres", "Mujeres")) %>%
      transmute(Provincia = Provincia,
                Sexo = Sexo,
                Año = as.character(Año),
                Edad_num = Edad_num,
                Pob = suppressWarnings(as.numeric(Valor))) %>%
      filter(is.finite(Edad_num), is.finite(Pob), Pob >= 0)
    if (!nrow(.obj_pad)) .obj_pad <- NULL
  }
  if (!is.null(.obj_pad)) cache_guardar(.obj_pad, "poblacion_edad_provincia.csv", .rds_pad)
  .obj_pad
})
padron_anos <- if (!is.null(padron_edad)) sort(unique(padron_edad$Año)) else character(0)

# ------------------------------------------------------------------------------
# PRECOMPUTO MULTIVARIANTE (PCA + correlaciones + k-means k=2..6 para cada
# combinación año×sexo; cuesta ~0,3 s). La pestaña lee resultados directos:
# sin cadenas reactivas que puedan quedarse colgadas.
# ------------------------------------------------------------------------------
pm_cache <- local({
  # OPT: en disco (15 PCA + ~1875 k-means deterministas con set.seed).
  .rds_pm <- file.path("cache", "cache_pm.rds")
  .obj_pm <- cache_cargar("causas_defunciones.csv", .rds_pm, sin_fuente_ok = TRUE)
  if (!is.null(.obj_pm)) return(.obj_pm)
  res <- list()
  anos <- sort(unique(as.character(causas_provinciales$Año)))
  for (a in anos) for (s in c("Ambos", "Hombres", "Mujeres")) {
    df <- causas_provinciales %>% filter(Año == a, Defunción != "Total")
    if (s != "Ambos") df <- df %>% filter(Sexo == s)
    wide <- df %>%
      group_by(Provincia, Defunción) %>%
      summarise(Fallecidos = sum(Fallecidos, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      select(Provincia, Defunción, Tasa) %>%
      tidyr::pivot_wider(names_from = Defunción, values_from = Tasa, values_fill = NA_real_)
    mat <- as.data.frame(wide)
    rownames(mat) <- mat$Provincia
    mat <- mat[, setdiff(names(mat), "Provincia"), drop = FALSE]
    n_ini <- ncol(mat)
    ok <- vapply(mat, function(v) {
      is.numeric(v) && sum(is.finite(v)) >= 3 && stats::sd(v, na.rm = TRUE) > 0
    }, logical(1))
    mat <- mat[, ok, drop = FALSE]
    mat <- mat[stats::complete.cases(mat), , drop = FALSE]
    if (ncol(mat) < 2 || nrow(mat) < 4) next
    p <- stats::prcomp(as.matrix(mat), center = TRUE, scale. = TRUE)
    m <- suppressWarnings(stats::cor(as.matrix(mat), use = "pairwise.complete.obs", method = "pearson"))
    m[!is.finite(m)] <- 0
    kms <- lapply(2:6, function(k) {
      kk <- min(k, nrow(p$x) - 1L)
      if (kk < 2L) return(NULL)
      set.seed(123)
      stats::kmeans(p$x[, 1:min(2, ncol(p$x)), drop = FALSE], centers = kk, nstart = 25)
    })
    res[[paste(a, s, sep = "||")]] <- list(mat = as.matrix(mat), n_causas_ini = n_ini,
                                           pca = p, cor = m, kms = kms)
  }
  cache_guardar(res, "causas_defunciones.csv", .rds_pm)
  res
})
pm_key <- function(a, s) paste(as.character(a), as.character(s), sep = "||")

# Provincia -> comunidad con la misma lógica que en causas_provinciales.
prov_a_comunidad <- function(provincias) {
  clave <- clave_geo(provincias)
  com <- prov_ccaa_lookup$Comunidad[match(clave, prov_ccaa_lookup$clave_geo)]
  com <- dplyr::coalesce(com, unname(prov_ccaa_fallback[clave]))
  com[is.na(com) | !nzchar(com)] <- "Sin asignar"
  com <- dplyr::case_when(
    stringr::str_detect(clave_geo(com), "ASTURIAS|PRINCIPADO") ~ "Asturias",
    stringr::str_detect(clave_geo(com), "NAVARRA") ~ "Navarra",
    stringr::str_detect(clave_geo(com), "MURCIA") ~ "Murcia",
    stringr::str_detect(clave_geo(com), "MADRID") ~ "Madrid",
    stringr::str_detect(clave_geo(com), "VALENCIANA") ~ "Comunidad Valenciana",
    TRUE ~ com
  )
  unname(com)
}

# ------------------------------------------------------------------------------
# NORMALIZADOR DE CCAA (nombres INE variados -> nombre canónico de la app).
# Cubre "Castilla - La Mancha" (con espacios), "Comunitat Valenciana" (INE),
# "Balears, Illes", "Rioja, La", "Asturias, Principado de", etc.
# ------------------------------------------------------------------------------
normalizar_ccaa <- function(x) {
  # Sin prefijos numéricos ("01 Andalucía") y con el canónico con espacios
  # ("Castilla - La Mancha", como mapSpain).
  cl <- toupper(stringi::stri_trans_general(as.character(x), "Latin-ASCII"))
  cl <- trimws(gsub("\\s+", " ", sub("^\\d+\\s*", "", cl)))
  dplyr::case_when(
    cl %in% c("ANDALUCIA") ~ "Andalucía",
    cl %in% c("ARAGON") ~ "Aragón",
    cl %in% c("ASTURIAS", "ASTURIAS, PRINCIPADO DE", "PRINCIPADO DE ASTURIAS") ~ "Asturias",
    cl %in% c("BALEARS, ILLES", "ILLES BALEARS", "ISLAS BALEARES") ~ "Islas Baleares",
    cl %in% c("CANARIAS") ~ "Canarias",
    cl %in% c("CANTABRIA") ~ "Cantabria",
    cl %in% c("CASTILLA - LA MANCHA", "CASTILLA-LA MANCHA", "CASTILLA LA MANCHA") ~ "Castilla - La Mancha",
    cl %in% c("CASTILLA Y LEON") ~ "Castilla y León",
    cl %in% c("CATALUNA") ~ "Cataluña",
    cl %in% c("CEUTA") ~ "Ceuta",
    cl %in% c("COMUNITAT VALENCIANA", "COMUNIDAD VALENCIANA", "VALENCIANA") ~ "Comunidad Valenciana",
    cl %in% c("EXTREMADURA") ~ "Extremadura",
    cl %in% c("GALICIA") ~ "Galicia",
    cl %in% c("MADRID", "MADRID, COMUNIDAD DE", "COMUNIDAD DE MADRID") ~ "Madrid",
    cl %in% c("MELILLA") ~ "Melilla",
    cl %in% c("MURCIA", "MURCIA, REGION DE", "REGION DE MURCIA") ~ "Murcia",
    cl %in% c("NAVARRA", "NAVARRA, COMUNIDAD FORAL DE", "COMUNIDAD FORAL DE NAVARRA") ~ "Navarra",
    cl %in% c("PAIS VASCO", "EUSKADI") ~ "País Vasco",
    cl %in% c("RIOJA, LA", "LA RIOJA") ~ "La Rioja",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------------------------------------
# RENTA (provincia × año × indicador) Y MÉDICOS (CCAA × año).
# Ficheros pequeños: sin caché. Año de médicos sale del nombre del fichero
# (2021 trae otra estructura: 3 columnas con fila TOTAL nacional).
# ------------------------------------------------------------------------------
decodificar <- function(x, enc) {
  tryCatch(iconv(as.character(x), from = enc, to = "UTF-8", sub = ""),
           error = function(e) as.character(x))
}

renta_data <- tryCatch({
  enc <- detectar_encoding("renta.csv")
  rr <- data.table::fread("renta.csv", sep = ";", encoding = enc,
                          header = TRUE, showProgress = FALSE,
                          colClasses = c(Total = "character"))
  rr <- as.data.frame(rr, stringsAsFactors = FALSE)
  tibble::tibble(
    Provincia = normalizar_provincias(trimws(sub("^\\d+\\s*", "", decodificar(rr[[3]], enc)))),
    Comunidad = normalizar_ccaa(decodificar(rr[[2]], enc)),
    Indicador = limpiar_texto(trimws(decodificar(rr[[5]], enc))),
    Año = trimws(as.character(rr[[6]])),
    Valor = numero_limpio(rr[[7]])
  ) %>%
    filter(!is.na(Comunidad), nzchar(Provincia), nzchar(Indicador),
           is.finite(Valor), Valor > 0) %>%
    group_by(Provincia, Comunidad, Indicador, Año) %>%
    summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop")
}, error = function(e) NULL)
if (!is.null(renta_data) && !nrow(renta_data)) renta_data <- NULL
renta_indicadores <- if (!is.null(renta_data)) sort(unique(renta_data$Indicador)) else character(0)
renta_anos <- if (!is.null(renta_data)) sort(unique(as.character(renta_data$Año))) else character(0)

leer_medicos <- function(anio) {
  f <- paste0("tasa_medicos_por_100000_", anio, ".csv")
  if (!file.exists(f)) return(NULL)
  enc <- detectar_encoding(f)
  m <- tryCatch(
    data.table::fread(f, sep = ";", encoding = enc, header = TRUE, showProgress = FALSE,
                    colClasses = c(Total = "character")),
    error = function(e) NULL
  )
  if (is.null(m) || !nrow(m)) return(NULL)
  m <- as.data.frame(m, stringsAsFactors = FALSE)
  for (j in seq_len(ncol(m))) {
    if (is.character(m[[j]])) m[[j]] <- decodificar(m[[j]], enc)
  }
  if (ncol(m) == 4) {
    # Total Nacional | CCAA | Situación | Total
    tibble::tibble(CCAA = m[[2]], Sit = m[[3]], Valor = m[[4]], Año = as.character(anio))
  } else if (ncol(m) == 3) {
    # CCAA (con fila TOTAL) | Situación | Total
    tibble::tibble(CCAA = m[[1]], Sit = m[[2]], Valor = m[[3]], Año = as.character(anio))
  } else {
    NULL
  }
}

medicos_data <- tryCatch({
  bind_rows(lapply(2018:2022, leer_medicos)) %>%
    transmute(Comunidad = normalizar_ccaa(as.character(CCAA)),
              Sit = limpiar_texto(trimws(as.character(Sit))),
              Año = trimws(as.character(Año)),
              Valor = numero_limpio(Valor)) %>%
    filter(!is.na(Comunidad),
           grepl("no jubilados", Sit, ignore.case = TRUE),
           is.finite(Valor), Valor > 0) %>%
    group_by(Comunidad, Año) %>%
    summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop")
}, error = function(e) NULL)
if (!is.null(medicos_data) && !nrow(medicos_data)) medicos_data <- NULL

det_ind_medicos <- "Médicos colegiados no jubilados / 100k hab."
det_indicadores <- c(renta_indicadores, if (!is.null(medicos_data)) det_ind_medicos else NULL)
det_anos <- sort(unique(c(renta_anos,
                          if (!is.null(medicos_data)) unique(as.character(medicos_data$Año)) else NULL)))

# Rampas explícitas compartidas mapa (leaflet) + ranking (plotly): la misma
# rampa y el mismo dominio en ambos, bajo = pálido y alto = oscuro, para que
# coincidan (cada librería interpreta los nombres "YlGnBu"/"YlOrRd" a su manera).
PAL_YLGNBU <- c("#FFFFCC", "#C7E9B4", "#7FCDBB", "#41B6C4", "#2C7FB8", "#253494")
PAL_YLORRD <- c("#FFFFCC", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#800026")
# RdBu invertida (coincide con leaflet reverse = TRUE): negativo = azul.
PAL_RDBU_REV <- c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7",
                  "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F")
escala_plotly <- function(hex) {
  n <- length(hex)
  lapply(seq_len(n), function(i) list((i - 1) / (n - 1), hex[i]))
}
dominio_seguro <- function(v, defecto = c(0, 1)) {
  v <- v[is.finite(v)]
  if (!length(v)) return(defecto)
  rg <- range(v)
  if (rg[1] == rg[2]) return(defecto)
  rg
}

# Población por provincia y año (Sexo Ambos) para ponderar la renta a CCAA.
pob_prov <- copia_p %>%
  filter(Sexo == "Ambos") %>%
  group_by(Provincia, Año) %>%
  summarise(Pob = sum(Poblacion, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# 3. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- page_navbar(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2c3e50",
    # OPT: pila de fuentes del sistema en lugar de font_google("Roboto").
    # font_google() descarga la fuente de Google Fonts al construir la UI
    # (petición de red en cada arranque); la pila local es indistinguible
    # en la práctica y funciona sin conexión.
    base_font = font_collection(
      "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"
    )
  ),
  title = "Análisis de defunciones (2009-2024)",
  
  header = tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body { background: #f5f7fa; }
      .card { margin-bottom: 16px !important; border-radius: 12px; }
      .sidebar { border-radius: 12px; }
      .filter-help {
        font-size: 0.86rem;
        line-height: 1.35;
        color: #5f6b76;
        background: #f8fafc;
        border-left: 3px solid #2c3e50;
        padding: 7px 10px;
        margin: -2px 0 10px 0;
        border-radius: 0 6px 6px 0;
      }
      .metric-note { font-size: 0.9rem; color: #5f6b76; margin: 4px 0 12px 0; }
      .value-box { border-radius: 12px; }
      .regression-dashboards .value-box { min-height: 92px; margin-bottom: 4px; }
      .regression-dashboards .value-box .value-box-value { font-size: 1.35rem; line-height: 1.1; }
      .regression-dashboards .value-box .value-box-title { font-size: 0.88rem; }

      /* Responsive layout for phones and small tablets */
      .card-body { min-width: 0; overflow-x: hidden; }
      .card-header { overflow-wrap: anywhere; }
      .selectize-control, .form-group, .shiny-input-container { max-width: 100%; }
      .plotly.html-widget { width: 100% !important; max-width: 100%; }
      .leaflet-container { width: 100% !important; max-width: 100%; }
      .dataTables_wrapper { width: 100%; overflow-x: auto; }
      /* Tablas clásicas (renderTable): scroll horizontal sin romper DT */
      .card-body table.table:not(.dataTable) { display: block; overflow-x: auto; max-width: 100%; -webkit-overflow-scrolling: touch; }
      .card-body table.table:not(.dataTable) th, .card-body table.table:not(.dataTable) td { white-space: nowrap; }
      /* Salida de texto de R sin desbordes */
      pre.shiny-text-output { white-space: pre-wrap; word-break: break-word; }
      /* Value boxes: el texto largo se ajusta en cualquier pestaña */
      .value-box .value-box-title { white-space: normal; overflow-wrap: anywhere; line-height: 1.25; }
      .value-box .value-box-value { white-space: normal; overflow-wrap: anywhere; line-height: 1.2; }
      .value-box .value-box-showcase { flex-shrink: 0; }
      /* Subpestañas: que nunca rompan el ancho */
      .nav-tabs { flex-wrap: wrap; }

      @media (max-width: 767.98px) {
        body { font-size: 0.95rem; }
        .navbar-brand { font-size: 1rem; white-space: normal; line-height: 1.15; }
        /* Navbar clásico sin hamburguesa: pestañas en una fila deslizable */
        .navbar-nav { flex-wrap: nowrap !important; overflow-x: auto; max-width: 100%; -webkit-overflow-scrolling: touch; }
        .navbar-nav > li { flex-shrink: 0; }
        .nav-tabs .nav-link { padding: 0.5rem 0.7rem; font-size: 0.9rem; }
        .leaflet.html-widget { height: 300px !important; }
        .bslib-sidebar-layout { --_sidebar-width: min(88vw, 320px); }
        .bslib-sidebar-layout > .sidebar { width: 100% !important; max-width: 100% !important; }
        .bslib-grid { grid-template-columns: minmax(0, 1fr) !important; }
        .card { margin-bottom: 12px !important; }
        .card-header { font-size: 0.98rem; padding: 0.65rem 0.8rem; }
        .card-body { padding: 0.75rem !important; }
        .value-box { min-height: 105px; }
        .plotly-graph-div { min-width: 0 !important; }
        .plotly .main-svg { max-width: 100%; }
      }

      @media (min-width: 768px) and (max-width: 1100px) {
        .bslib-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      }

      /* Resumen general: texto completo sin scroll interno y cajas sin recortes */
      .resumen-intro { height: auto !important; min-height: 0 !important; }
      .resumen-intro .card-body { overflow: visible !important; max-height: none !important; }
      .resumen-dash .value-box { min-height: 130px; }
      .resumen-dash .value-box .value-box-title { font-size: 0.85rem; white-space: normal; overflow-wrap: anywhere; line-height: 1.25; }
      .resumen-dash .value-box .value-box-value { font-size: 1.2rem; line-height: 1.25; white-space: normal; overflow-wrap: anywhere; }
      .resumen-dash .value-box .value-box-showcase { flex-shrink: 0; }

      /* Exceso de mortalidad: KPIs compactos sin recortes */
      .exceso-dash .value-box { min-height: 118px; }
      .exceso-dash .value-box .value-box-title { font-size: 0.85rem; white-space: normal; overflow-wrap: anywhere; line-height: 1.25; }
      .exceso-dash .value-box .value-box-value { font-size: 1.45rem; line-height: 1.2; white-space: normal; overflow-wrap: anywhere; }
      .exceso-dash .value-box .value-box-showcase { flex-shrink: 0; }

      /* Desplegables: el input de búsqueda de selectize ocupa una línea propia
         bajo valores seleccionados largos (se ve como un hueco en blanco).
         Cerrado se saca del flujo; abierto sigue permitiendo filtrar. */
      .selectize-control.single .selectize-input > input { margin: 0 !important; }
      .selectize-control.single:not(.dropdown-active) .selectize-input > input {
        position: absolute !important;
        opacity: 0 !important;
        pointer-events: none !important;
        width: 0 !important;
        padding: 0 !important;
      }
    "))
  ),
  
  # PESTAÑA 1: RESUMEN GENERAL
  nav_panel(
    title = "Resumen general",
    # Div Bootstrap plano (sin bslib::card): crece con el contenido y muestra
    # todo el texto sin scroll interno. Las gráficas quedan debajo.
    div(
      class = "card resumen-intro",
      div(class = "card-header", "Introducción a la aplicación"),
      div(
        class = "card-body",
        HTML(paste0(
          "<div class='intro-app'>",
          "<h4>¿Qué encontrarás en esta aplicación?</h4>",
          "<p>Esta aplicación ofrece una visión conjunta de la mortalidad en España y permite estudiar las defunciones desde diferentes perspectivas. ",
          "Los datos permiten analizar <b>qué causas de muerte son más frecuentes, dónde se concentran, cómo afectan según el sexo y la edad y cómo cambian a lo largo del tiempo y del año</b>. ",
          "También se incorpora una comparación con otros países europeos y un análisis estadístico de las relaciones entre variables territoriales.</p>",
          "<h5>Causas de defunción</h5>",
          "<p>Esta sección es el punto de partida para analizar las causas de muerte en España. El <b>análisis provincial</b> permite observar la distribución territorial mediante mapas, tasas y evolución temporal. ",
          "El <b>comparador de provincias</b> permite enfrentar dos provincias y estudiar sus diferencias en las causas seleccionadas. ",
          "La sección de <b>evolución temporal</b> resume cómo cambian las tasas de las comunidades autónomas entre 2018 y 2022 y permite estudiar sus variaciones interanuales. ",
          "Las <b>tasas estandarizadas por edad</b> permiten comparar provincias eliminando el efecto de la estructura de edad: reponderan las tasas de cada tramo con la <b>Población Estándar Europea 2013</b> (población ficticia de referencia de Eurostat), de modo que las diferencias restantes reflejan riesgo y no envejecimiento. Son cifras hipotéticas para comparar, no defunciones ocurridas. ",
          "La sección de <b>edad y mes</b> muestra pirámides por edad simple, la <b>mediana de fallecimiento</b> (edad que concentra la mitad de las defunciones), el <b>mes pico</b> y la serie anual por provincia desde 2009.</p>",
          "<h5>Estacionalidad</h5>",
          "<p>Analiza la distribución de las defunciones a lo largo de los meses. Permite comprobar si determinadas causas presentan comportamientos estacionales y comparar estos patrones entre <b>hombres y mujeres</b>.</p>",
          "<h5>Análisis demográfico</h5>",
          "<p>Estudia la mortalidad teniendo en cuenta la <b>edad y el sexo</b> de las personas fallecidas y la estructura de la población. Los gráficos permiten observar cómo se distribuyen las defunciones entre los distintos grupos de edad y comparar la situación de diferentes provincias. ",
          "Se añaden la <b>pirámide de población</b> real del Padrón, el <b>envejecimiento y la dependencia</b> por provincia, la <b>brecha de género</b> de los indicadores, su <b>evolución temporal</b> por comunidades y la <b>esperanza de vida</b> por provincia, sexo y edad.</p>",
          "<h5>Europa</h5>",
          "<p>Amplía el estudio al contexto europeo. Se pueden comparar países, seguir la evolución de sus tasas, analizar las diferencias por sexo y estudiar qué causas presentan los niveles de mortalidad más altos o más bajos. ",
          "Además, la <b>comparación estadística europea</b> permite evaluar si existen diferencias entre países teniendo en cuenta también el año.</p>",
          "<h5>Mortalidad por edad y sexo y mortalidad prematura</h5>",
          "<p>La distribución por <b>edad y sexo</b> se resume en esta misma pestaña. La <b>mortalidad prematura</b> se estudia dentro de <b>Causas de defunción</b> mediante los <b>años potenciales de vida perdidos (APVP)</b>, que cuantifican el impacto de las defunciones ocurridas antes de edades avanzadas.</p>",
          "<h5>Regresiones lineales</h5>",
          "<p>Esta sección busca estudiar si existe relación entre distintos indicadores de mortalidad a nivel provincial. ",
          "Los modelos comprueban sus principales supuestos estadísticos —linealidad, homoscedasticidad, incorrelación de los errores y normalidad de los residuos— y se complementan con el <b>test de Moran</b> para detectar posible dependencia espacial entre provincias. ",
          "La <b>matriz de validez</b> resume de un vistazo qué pares cumplen los supuestos (✓), la validez espacial (●) o ambos (✓ ●). ",
          "La <b>escala logarítmica</b> opcional suele estabilizar la varianza y rescatar supuestos; y cuando hay dependencia espacial, el <b>modelo de rezago espacial</b> (parámetro &rho;) es la alternativa adecuada, pues incorpora la influencia de las provincias vecinas. ",
          "La subpestaña de <b>regresión entre causas</b> aplica el mismo marco a las tasas de causas por 100.000 habitantes.</p>",
          "<h5>Determinantes e índice sintético</h5>",
          "<p>La renta por persona y hogar y los médicos colegiados por comunidad describen el contexto socioeconómico y sanitario por territorios, y el <b>índice sintético</b> los combina con la mortalidad en un solo indicador 0-100 por comunidad.</p>",
          "<h5>Exceso de mortalidad</h5>",
          "<p>Compara las defunciones observadas con las <b>esperadas</b> según la media 2015-2019 (P-score), por año y por mes, para cuantificar el impacto de la pandemia y de otros picos de mortalidad.</p>",
          "<h5>Análisis multivariante</h5>",
          "<p>Más allá de la regresión: el <b>PCA</b> resume el perfil de mortalidad de cada provincia en dos dimensiones y <b>k-means</b> agrupa las provincias parecidas, con mapa de clusters y matriz de correlaciones entre causas.</p>",
          "<h5>Cómo interpretar la aplicación</h5>",
          "<p>Las distintas pestañas se complementan entre sí: las causas ayudan a entender <b>qué ocurre</b>; los análisis provinciales, demográficos y europeos muestran <b>dónde y en quién</b>; la evolución temporal y la estacionalidad permiten estudiar <b>cuándo cambia</b>; y las regresiones permiten explorar <b>qué relaciones estadísticas pueden existir</b> entre los indicadores.</p>",
          "</div>"
        ))
      )
    ),
    div(
      class = "resumen-dash",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(title = "Tasa de mortalidad nacional 2022 (todas las causas)", value = textOutput("res_kpi_tasa"),
                  showcase = bsicons::bs_icon("activity"), theme = "primary"),
        value_box(title = "Causa con más defunciones (2022)", value = textOutput("res_kpi_causa"),
                  showcase = bsicons::bs_icon("exclamation-circle"), theme = "danger"),
        value_box(title = "Provincia con mayor tasa de mortalidad (2022)", value = textOutput("res_kpi_provincia"),
                  showcase = bsicons::bs_icon("geo-alt"), theme = "warning"),
        value_box(title = "Tasa de mortalidad de España en Europa (2022)", value = textOutput("res_kpi_europa"),
                  showcase = bsicons::bs_icon("flag"), theme = "info")
      ),
      div(class = "filter-help", HTML(
        "<b>Contexto:</b> indicadores de 2022; todas las causas y ambos sexos. Tasas de defunción por 100.000 habitantes. Los gráficos agregan todo el periodo disponible, salvo Europa que muestra 2022."
      )),
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(card_header("Top 8 causas por APVP"),
                    card_body(plotlyOutput("res_apvp", height = "380px"))),
        bslib::card(card_header("Top 10 países europeos por tasa de mortalidad"),
                    card_body(plotlyOutput("res_europa", height = "380px")))
      ),
      bslib::card(
        card_header("Pirámide de defunciones por edad y sexo"),
        card_body(
          plotlyOutput("ped_edad_sexo", height = "480px"),
          HTML("<p class='text-muted mb-0'><small>Defunciones agregadas de todo el periodo 2018-2022.</small></p>")
        )
      )
    )
  ),

  # PESTAÑA 2: CAUSAS DE DEFUNCIÓN
  nav_panel(
    title = "Causas de defunción",
    navset_tab(
      nav_panel(
        title = "Análisis provincial",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros",
            width = 280,
            selectInput("p2_ano", "Año:", choices = sort(unique(copia_causas$Año)), selected = "2020"),
            selectInput("p2_defuncion", "Causa de Defunción:", choices = lista_defunciones, selected = lista_defunciones[1]),
            uiOutput("p2_filtro_info"),
            radioButtons("p2_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Tasa nacional de la causa", value = textOutput("p2_kpi_total"), showcase = bsicons::bs_icon("activity"), theme = "primary"),
            value_box(title = "Provincia más afectada", value = textOutput("p2_kpi_max"), showcase = bsicons::bs_icon("arrow-up-circle"), theme = "danger"),
            value_box(title = "Provincia menos afectada", value = textOutput("p2_kpi_min"), showcase = bsicons::bs_icon("arrow-down-circle"), theme = "success")
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Las <b>tasas por 100.000 habitantes</b> permiten comparar provincias con distinta población. El mapa muestra la foto del año elegido; la evolución, a las 5 provincias con mayor y menor tasa junto a la media nacional; y los rankings, las 3 causas con mayor y menor tasa por sexo.</p>"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Mapa de la tasa de la causa seleccionada (por 100k hab.)"), card_body(padding = 0, leafletOutput("p2_mapa", height = "350px"))),
            bslib::card(card_header("Evolución Temporal: Provincias Extremas y Media Nacional"), card_body(plotlyOutput("p2_evolucion_top_bot", height = "350px")))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Causas con mayor tasa por sexo"), card_body(plotlyOutput("p2_causas_mas_sexo", height = "300px"))),
            bslib::card(card_header("Causas con menor tasa por sexo"),
                                              card_body(plotlyOutput("p2_causas_menos_sexo", height = "300px")))
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(
              card_header("Tabla resumen provincial"),
              card_body(DT::DTOutput("p2_tabla_resumen"))
            )
          )
        )
      ),
      nav_panel(
        title = "Estacionalidad",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros mensuales",
            width = 290,
            selectInput("pmes_causa", "Causa:", choices = meses_causas,
                        selected = if(length(meses_causas)) meses_causas[1] else character(0)),
            selectInput("pmes_ano", "Año:", choices = c("Todos", meses_anios),
                        selected = if(length(meses_anios)) "Todos" else character(0)),
            radioButtons("pmes_sexo", "Sexo:", choices = meses_sexos, selected = "Ambos")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Defunciones del periodo", value = textOutput("pmes_kpi_total"),
                      showcase = bsicons::bs_icon("calendar-heart"), theme = "primary"),
            value_box(title = "Mes con mayor mortalidad", value = textOutput("pmes_kpi_max"),
                      showcase = bsicons::bs_icon("arrow-up-circle"), theme = "danger"),
            value_box(title = "Mes con menor mortalidad", value = textOutput("pmes_kpi_min"),
                      showcase = bsicons::bs_icon("arrow-down-circle"), theme = "success")
          ),
          bslib::card(
            card_header("Lectura"),
            card_body(HTML("<p>La estacionalidad permite detectar meses o periodos en los que una causa presenta un número de defunciones sistemáticamente mayor o menor. Con <b>Todos</b> se comparan los años disponibles en paralelo.</p>"))
          ),
          bslib::card(
            card_header("Evolución mensual"),
            card_body(plotlyOutput("pmes_evolucion", height = "430px"))
          )
        )
      ),
      nav_panel(
        title = "Evolución temporal",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros temporales",
            width = 290,
            selectInput("pt_causa", "Causa de defunción:", choices = lista_defunciones,
                        selected = if(length(lista_defunciones)) lista_defunciones[1] else NULL),
            radioButtons("pt_sexo", "Sexo:",
                         choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Defunciones 2018–2022", value = textOutput("pt_kpi_defunciones"),
                      showcase = bsicons::bs_icon("heart-pulse"), theme = "secondary"),
            value_box(title = "Mayor cambio 2018–2022", value = textOutput("pt_kpi_cambio"),
                      showcase = bsicons::bs_icon("graph-up-arrow"), theme = "info"),
            value_box(title = "Año con más defunciones", value = textOutput("pt_kpi_ano_max"),
                      showcase = bsicons::bs_icon("calendar-event"), theme = "primary")
          ),
          bslib::card(
            card_header("Evolución de la tasa de la causa por comunidad"),
            card_body(
              HTML("<p>La tasa de la causa se calcula como <b>defunciones / población × 100.000</b>. La gráfica permite seguir la evolución de cada comunidad entre 2018 y 2022 sin perder el detalle de los valores.</p>"),
              plotlyOutput("pt_evolucion_ccaa", height = "430px")
            )
          ),
          bslib::card(
            card_header("Variación interanual de la tasa de la causa"),
            card_body(
              HTML("<p>En lugar de superponer una línea por comunidad, esta visualización utiliza un <b>mapa de intensidad</b>: cada fila representa una comunidad y cada columna un año. El valor indica cuánto ha cambiado la tasa de la causa respecto al año anterior.</p>"),
              plotlyOutput("pt_variacion_interanual", height = "520px"),
              HTML("<p class='text-muted mb-2'><small>2019 compara con 2018; 2020 con 2019; 2021 con 2020; y 2022 con 2021. Los valores están expresados en puntos por 100.000 habitantes.</small></p>"),
              DT::DTOutput("pt_tabla_ccaa")
            )
          )
        )
      ),
      nav_panel(
        title = "Comparador de Provincias",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            selectInput("p21_prov_a", "Provincia:", choices = lista_provincias,
                        selected = if ("Vizcaya" %in% lista_provincias) "Vizcaya" else lista_provincias[1]),
            selectInput("p21_prov_b", "Provincia:", choices = lista_provincias,
                        selected = if ("Asturias" %in% lista_provincias) "Asturias" else lista_provincias[min(2, length(lista_provincias))]),
            selectInput("p21_defuncion", "Causa de Defunción:", choices = lista_defunciones, selected = lista_defunciones[1]),
            selectInput("p21_ano", "Año (para el radar):", choices = c("Todos los años", "2018", "2019", "2020", "2021", "2022"), selected = "2020"),
            uiOutput("p21_filtro_info")
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Compara <b>dos provincias</b> frente a la <b>media nacional</b>: la evolución muestra si avanzan mejor o peor que el conjunto, y los radares resumen el perfil por grupos de causas en el año elegido (arriba) y en el resto de años (abajo).</p>"))
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(
              card_header("Evolución Temporal Comparada con la Media Nacional (2018-2022)"),
              card_body(plotlyOutput("p21_evol_nacional", height = "350px"))
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header(uiOutput("p21_radar_title_a")), card_body(plotlyOutput("p21_radar_a", height = "320px"))),
            bslib::card(card_header(uiOutput("p21_radar_title_b")), card_body(plotlyOutput("p21_radar_b", height = "320px")))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header(uiOutput("p21_radar_bottom_title_a")), card_body(plotlyOutput("p21_radar_a_bot", height = "320px"))),
            bslib::card(card_header(uiOutput("p21_radar_bottom_title_b")), card_body(plotlyOutput("p21_radar_b_bot", height = "320px")))
          )
        )
      ),
      nav_panel(
        title = "Mortalidad prematura (APVP)",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros APVP",
            width = 290,
            selectInput("papvp_causa", "Causa:", choices = apvp_causas,
                        selected = if(length(apvp_causas)) apvp_causas[1] else character(0)),
            selectInput("papvp_comunidad", "Comunidad:", choices = c("Todas", apvp_comunidades),
                        selected = "Todas"),
            selectInput("papvp_indicador", "Indicador:", choices = apvp_indicadores,
                        selected = if(length(apvp_indicadores)) {
                          if("Nº de APVP" %in% apvp_indicadores) "Nº de APVP" else apvp_indicadores[1]
                        } else character(0)),
            radioButtons("papvp_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "APVP / indicador", value = textOutput("papvp_kpi_total"),
                      showcase = bsicons::bs_icon("hourglass-split"), theme = "primary"),
            value_box(title = "Mayor valor", value = textOutput("papvp_kpi_max"),
                      showcase = bsicons::bs_icon("arrow-up-circle"), theme = "danger"),
            value_box(title = "Media", value = textOutput("papvp_kpi_media"),
                      showcase = bsicons::bs_icon("bar-chart"), theme = "success")
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Los <b>años potenciales de vida perdidos (APVP)</b> cuantifican el impacto de las defunciones ocurridas a edades relativamente tempranas. Además del número de APVP, el dataset permite consultar su tasa estandarizada y el número medio de APVP.</p>"))
          ),
          bslib::card(
            card_header("Distribución por comunidad autónoma"),
            card_body(plotlyOutput("papvp_ranking", height = "430px"))
          )
        )
      ),
      nav_panel(
        title = "Tasas estandarizadas por edad",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 290,
            if (is.null(datos_edad_std)) div(class = "filter-help", HTML(
              "<b>Dataset pendiente.</b> Esta pestaña se activa al añadir a la carpeta los ficheros <b>defunciones_edad_provincia.csv</b> y <b>poblacion_edad_provincia.csv</b> (formato detallado en el panel principal)."
            )) else tagList(
              selectInput("std_ano", "Año:", choices = datos_edad_std$anos,
                          selected = datos_edad_std$anos[1]),
              radioButtons("std_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
              div(class = "filter-help", HTML(
                "Tasa de <b>mortalidad total</b>: el fichero no trae desglose por causa."
              ))
            )
          ),
          if (is.null(datos_edad_std)) bslib::card(
            card_header("Dataset necesario: defunciones y población por edad y provincia"),
            card_body(HTML(paste0(
              "<p>Las <b>tasas estandarizadas por edad</b> eliminan el efecto de la estructura de edad al comparar provincias, usando como referencia la <b>Población Estándar Europea 2013</b>.</p>",
              "<p>Para activar esta pestaña, añade a la carpeta de la aplicación estos dos ficheros (separador <b>;</b>, latin1 o UTF-8, con fila de cabecera; se admiten filas de metadatos previas):</p>",
              "<ul><li><b>defunciones_edad_provincia.csv</b>: columnas <b>Provincia; Sexo; Año; Edad; Defunciones</b> y opcionalmente <b>Causa</b>.</li>",
              "<li><b>poblacion_edad_provincia.csv</b>: columnas <b>Provincia; Sexo; Año; Edad; Poblacion</b>.</li></ul>",
              "<p>La edad puede venir en tramos quinquenales (<i>De 5 a 9</i>), años simples (<i>80</i>) o intervalos (<i>80-84</i>). Las filas de totales por sexo se ignoran: <b>Ambos</b> se calcula sumando.</p>",
              "<p>Fuente sugerida: <b>INE</b> (defunciones por causa, sexo, edad y provincia; Padrón/Cifras de población por edad, provincia y sexo).</p>"
            )))
          ) else tagList(
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(title = "Mayor tasa estandarizada", value = textOutput("std_kpi_max"),
                        showcase = bsicons::bs_icon("arrow-up-circle"), theme = "danger"),
              value_box(title = "Menor tasa estandarizada", value = textOutput("std_kpi_min"),
                        showcase = bsicons::bs_icon("arrow-down-circle"), theme = "success"),
              value_box(title = "Mayor cambio de ranking", value = textOutput("std_kpi_rank"),
                        showcase = bsicons::bs_icon("arrow-left-right"), theme = "primary")
            ),
            bslib::card(
              card_header("Interpretación"),
              card_body(HTML("<p>La <b>tasa bruta</b> mezcla riesgo y estructura de edad; la <b>estandarizada</b> repondera cada tramo con la Población Estándar Europea 2013 y permite comparar provincias en igualdad de condiciones. La diagonal del gráfico marca la igualdad: las provincias alejadas cambian mucho de puesto al eliminar el efecto de la edad.</p>"))
            ),
            layout_columns(
              col_widths = c(6, 6),
              bslib::card(card_header("Mapa de la tasa estandarizada (ESP 2013, por 100k hab.)"),
                          card_body(padding = 0, leafletOutput("std_mapa", height = "430px"))),
              bslib::card(card_header("Bruta frente a estandarizada"),
                          card_body(plotlyOutput("std_scatter", height = "430px")))
            ),
            bslib::card(card_header("Tabla por provincia (bruta, estandarizada y puestos)"),
                        card_body(DT::DTOutput("std_tabla")))
          )
        )
      ),
      nav_panel(
        title = "Edad y mes",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 290,
            if (is.null(datos_edadprov)) div(class = "filter-help", HTML(
              "<b>Dataset pendiente.</b> Esta pestaña se activa al añadir <b>defunciones_edad_provincia.csv</b> (ver formato en Tasas estandarizadas)."
            )) else tagList(
              selectInput("ep_prov", "Provincia:", choices = c("Todas", datos_edadprov$provincias),
                          selected = "Todas"),
              selectInput("ep_ano", "Año:", choices = datos_edadprov$anos,
                          selected = if ("2022" %in% datos_edadprov$anos) "2022" else datos_edadprov$anos[1])
            )
          ),
          if (is.null(datos_edadprov)) bslib::card(
            card_header("Sin datos"),
            card_body(HTML("<p>Añade <b>defunciones_edad_provincia.csv</b> a la carpeta y reinicia para ver pirámides por edad simple, mediana de fallecimiento y estacionalidad por provincia (2009-2024).</p>"))
          ) else tagList(
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(title = "Edad mediana de fallecimiento", value = textOutput("ep_kpi_mediana"),
                        showcase = bsicons::bs_icon("person-standing"), theme = "primary"),
              value_box(title = "% defunciones con 65 años o más", value = textOutput("ep_kpi_65"),
                        showcase = bsicons::bs_icon("people"), theme = "info"),
              value_box(title = "Mes pico", value = textOutput("ep_kpi_mes"),
                        showcase = bsicons::bs_icon("calendar-event"), theme = "success")
            ),
            bslib::card(
              card_header("Interpretación"),
              card_body(HTML("<p>La <b>pirámide</b> usa edades simples agregadas en quinquenios; la <b>mediana</b> resume en qué edad se concentra la mitad de las defunciones. La <b>estacionalidad mensual</b> suele mostrar picos invernales. La serie anual cubre desde 2009 e incluye el exceso de 2020-2021.</p>"))
            ),
            layout_columns(
              col_widths = c(6, 6),
              bslib::card(card_header("Pirámide por edad simple (ambos sexos)"),
                          card_body(plotlyOutput("ep_piramide", height = "520px"))),
              bslib::card(card_header("Defunciones por mes"),
                          card_body(plotlyOutput("ep_meses", height = "520px")))
            ),
            layout_columns(
              col_widths = c(6, 6),
              bslib::card(card_header("Serie anual de defunciones"),
                          card_body(plotlyOutput("ep_evol", height = "350px"))),
              bslib::card(card_header("Totales anuales"),
                          card_body(DT::DTOutput("ep_tabla")))
            )
          )
        )
      )
    )
  ),
  
  # PESTAÑA 3: MÉTRICAS DEMOGRÁFICAS
  nav_panel(
    title = "Métricas demográficas",
    navset_tab(
      nav_panel(
        title = "Análisis demográfico",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros demográficos",
            width = 280,
            selectInput("p3_ano", "Año:", choices = c("2018", "2019", "2020", "2021", "2022"), selected = "2020"),
            selectInput("p3_funcion", "Métrica Demográfica:", choices = funciones_deseadas, selected = funciones_deseadas[1]),
            selectInput("p3_edad", "Tramo de Edad (Mapa y Tabla):", choices = c("Todos los tramos", lista_edades), selected = "Todos los tramos"),
            uiOutput("p3_filtro_info"),
            radioButtons("p3_sexo", "Sexo (Mapa y Tabla):", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Media entre provincias", value = textOutput("p3_kpi_media"), showcase = bsicons::bs_icon("bar-chart-line"), theme = "primary"),
            value_box(title = "Provincia con mayor valor", value = textOutput("p3_kpi_max"), showcase = bsicons::bs_icon("arrow-up-circle"), theme = "danger"),
            value_box(title = "Provincia con menor valor", value = textOutput("p3_kpi_min"), showcase = bsicons::bs_icon("arrow-down-circle"), theme = "success")
          ),
          
          uiOutput("p3_cuadro_informativo_dinamico"),
          
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              class = "equal-height-card",
              card_header("Mapa de la Métrica Seleccionada"), 
              card_body(padding = 0, leafletOutput("p3_mapa", height = "420px"))
            ),
            bslib::card(
              class = "equal-height-card",
              card_header("Indicador por Tramo de Edad y Sexo"),
              card_body(plotlyOutput("p3_piramide", height = "420px"))
            )
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(
              card_header(
                div(
                  class = "d-flex justify-content-between align-items-center",
                   span("Resumen estadístico y posición relativa (ordenado de mayor a menor valor)"),
                  div(
                    actionButton("p3_prev_page", "<", class = "btn-sm btn-outline-secondary me-1"),
                    uiOutput("p3_page_info", inline = TRUE),
                    actionButton("p3_next_page", ">", class = "btn-sm btn-outline-secondary ms-1")
                  )
                )
              ),
              card_body(tableOutput("p3_tabla_ranking"))
            )
          )
        )
      ),
      nav_panel(
        title = "Comparador demográfico",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            selectInput("p31_prov_a", "Provincia:", choices = lista_provincias,
                        selected = if ("Vizcaya" %in% lista_provincias) "Vizcaya" else lista_provincias[1]),
            selectInput("p31_prov_b", "Provincia:", choices = lista_provincias,
                        selected = if ("Asturias" %in% lista_provincias) "Asturias" else lista_provincias[min(2, length(lista_provincias))]),
            selectInput("p31_funcion", "Métrica Demográfica:", choices = funciones_deseadas, selected = funciones_deseadas[1]),
            selectInput("p31_ano_radar", "Año de población:",
                        choices = sort(unique(copia_p$Año), decreasing = TRUE),
                        selected = if ("2022" %in% copia_p$Año) "2022" else sort(unique(copia_p$Año), decreasing = TRUE)[1]),
            uiOutput("p31_filtro_info"),
            radioButtons("p31_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos")
          ),
          layout_columns(
            col_widths = c(6, 6),
            value_box(
              title = textOutput("p31_poblacion_titulo_a"),
              value = textOutput("p31_poblacion_a"),
              showcase = bsicons::bs_icon("people"),
              theme = "primary"
            ),
            value_box(
              title = textOutput("p31_poblacion_titulo_b"),
              value = textOutput("p31_poblacion_b"),
              showcase = bsicons::bs_icon("people"),
              theme = "info"
            )
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Enfrenta <b>dos provincias</b> con la <b>media entre provincias</b>: la evolución muestra si mejoran o empeoran respecto al conjunto, y la pirámide compara su estructura por tramos de edad en el sexo elegido.</p>"))
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(
              card_header("Evolución Temporal Comparada con la Media entre Provincias (2018-2022)"),
              card_body(plotlyOutput("p31_evol_demografica", height = "350px"))
            )
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(
              card_header("Pirámide poblacional comparativa"),
              card_body(plotlyOutput("p31_piramide_comparativa", height = "480px"))
            )
          )
        )
      ),
      nav_panel(
        title = "Pirámide de población",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            if (is.null(padron_edad)) div(class = "filter-help", HTML(
              "<b>Dataset pendiente.</b> Añade <b>poblacion_edad_provincia.csv</b> para ver la pirámide de población real."
            )) else tagList(
              selectInput("demp_prov", "Provincia:", choices = c("Todas", sort(unique(padron_edad$Provincia))),
                          selected = "Todas"),
              selectInput("demp_ano", "Año:", choices = padron_anos,
                          selected = if ("2022" %in% padron_anos) "2022" else padron_anos[1])
            )
          ),
          if (is.null(padron_edad)) bslib::card(
            card_header("Sin datos"),
            card_body(HTML("<p>Añade <b>poblacion_edad_provincia.csv</b> a la carpeta y reinicia.</p>"))
          ) else tagList(
            bslib::card(
              card_header("Interpretación"),
              card_body(HTML("<p>Pirámide con la <b>población censal real</b> (Padrón) por grupos quinquenales de edad. A diferencia de las pirámides de funciones o defunciones, aquí el tamaño de cada barra refleja cuánta gente vive en ese tramo.</p>"))
            ),
            bslib::card(
              card_header("Pirámide de población por edad y sexo"),
              card_body(plotlyOutput("demp_piramide", height = "560px"))
            )
          )
        )
      ),
      nav_panel(
        title = "Envejecimiento y dependencia",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            if (is.null(padron_edad)) div(class = "filter-help", HTML(
              "<b>Dataset pendiente.</b> Añade <b>poblacion_edad_provincia.csv</b>."
            )) else tagList(
              selectInput("deme_ano", "Año:", choices = padron_anos,
                          selected = if ("2022" %in% padron_anos) "2022" else padron_anos[1])
            )
          ),
          if (is.null(padron_edad)) bslib::card(
            card_header("Sin datos"),
            card_body(HTML("<p>Añade <b>poblacion_edad_provincia.csv</b> a la carpeta y reinicia.</p>"))
          ) else tagList(
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(title = "% de 65 años o más (España)", value = textOutput("deme_kpi_65"),
                        showcase = bsicons::bs_icon("people"), theme = "primary"),
              value_box(title = "Índice de envejecimiento (España)", value = textOutput("deme_kpi_env"),
                        showcase = bsicons::bs_icon("bar-chart-line"), theme = "info"),
              value_box(title = "Tasa de dependencia (España)", value = textOutput("deme_kpi_dep"),
                        showcase = bsicons::bs_icon("people"), theme = "success")
            ),
            bslib::card(
              card_header("Interpretación"),
              card_body(HTML("<p>El <b>% de 65+</b> mide el peso de la población mayor; el <b>índice de envejecimiento</b> compara mayores con jóvenes (65+ por cada 100 menores de 16); y la <b>tasa de dependencia</b> relaciona la población fuera de edad laboral (0-15 y 65+) con la de 16-64. Son los indicadores que explican gran parte de las diferencias de mortalidad bruta entre provincias.</p>"))
            ),
            layout_columns(
              col_widths = c(6, 6),
              bslib::card(card_header("Mapa del % de 65 años o más"),
                          card_body(padding = 0, leafletOutput("deme_mapa", height = "430px"))),
              bslib::card(card_header("Ranking por % de 65+"),
                          card_body(plotlyOutput("deme_ranking", height = "430px")))
            ),
            bslib::card(card_header("Indicadores por provincia"),
                        card_body(DT::DTOutput("deme_tabla")))
          )
        )
      ),
      nav_panel(
        title = "Brecha de género",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            selectInput("demb_ano", "Año:", choices = sort(unique(as.character(copia_func$Año))),
                        selected = if ("2022" %in% as.character(copia_func$Año)) "2022" else sort(unique(as.character(copia_func$Año)))[1]),
            selectInput("demb_func", "Indicador:", choices = funciones_deseadas,
                        selected = if ("Esperanza de vida" %in% funciones_deseadas) "Esperanza de vida" else funciones_deseadas[1]),
            div(class = "filter-help", HTML(
              "Diferencia <b>Hombres − Mujeres</b> del indicador a la <b>edad 0</b> (al nacer). Valores negativos en esperanza de vida significan que las mujeres viven más."
            ))
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>La <b>brecha Hombres − Mujeres</b> resume la desigualdad de género del indicador al nacer, por provincia. En esperanza de vida lo habitual es una brecha negativa (ellas viven más años); el ranking ordena dónde esa diferencia es mayor o menor.</p>"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Mapa de la brecha (Hombres − Mujeres)"),
                        card_body(padding = 0, leafletOutput("demb_mapa", height = "430px"))),
            bslib::card(card_header("Ranking de brecha"),
                        card_body(plotlyOutput("demb_ranking", height = "430px")))
          ),
          bslib::card(card_header("Tabla por provincia"),
                      card_body(DT::DTOutput("demb_tabla")))
        )
      ),
      nav_panel(
        title = "Evolución temporal",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            selectInput("demv_func", "Indicador:", choices = funciones_deseadas,
                        selected = if ("Esperanza de vida" %in% funciones_deseadas) "Esperanza de vida" else funciones_deseadas[1]),
            radioButtons("demv_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
            div(class = "filter-help", HTML(
              "Media entre provincias por comunidad y año. Como en el resto de la pestaña, los valores promedian todos los tramos de edad."
            ))
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Evolución del indicador por <b>comunidad autónoma</b> entre 2018 y 2022 frente a la media entre provincias. Permite ver si las diferencias territoriales se amplían, se reducen o se mantienen en el tiempo.</p>"))
          ),
          bslib::card(card_header("Evolución por comunidad autónoma (2018-2022)"),
                      card_body(plotlyOutput("demv_evol", height = "430px"))),
          bslib::card(card_header("Tabla de valores"),
                      card_body(DT::DTOutput("demv_tabla")))
        )
      ),
      nav_panel(
        title = "Esperanza de vida",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            selectInput("ev_ano", "Año:", choices = c("2018", "2019", "2020", "2021", "2022"),
                        selected = "2022"),
            selectInput("ev_edad", "Edad:", choices = ev_edades,
                        selected = if ("0" %in% ev_edades) "0" else ev_edades[1]),
            radioButtons("ev_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
            div(class = "filter-help", HTML(
              "Esperanza de vida <b>restante a la edad elegida</b> (años que viviría en promedio quien ya ha cumplido esa edad). A la edad 0 es la esperanza de vida al nacer."
            ))
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Media nacional", value = textOutput("ev_kpi_media"),
                      showcase = bsicons::bs_icon("heart-pulse"), theme = "success"),
            value_box(title = "Mayor esperanza", value = textOutput("ev_kpi_max"),
                      showcase = bsicons::bs_icon("arrow-up-circle"), theme = "primary"),
            value_box(title = "Menor esperanza", value = textOutput("ev_kpi_min"),
                      showcase = bsicons::bs_icon("arrow-down-circle"), theme = "warning")
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>La <b>esperanza de vida</b> resume la mortalidad de cada territorio en un solo número: cuántos años viviría en promedio una persona si las tasas por edad se mantuvieran. Sube cuando cae la mortalidad y cayó en 2020-2021 por la pandemia. Las mujeres viven más años en todas las provincias; la brecha se aprecia eligiendo sexo.</p>"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Mapa por provincia (años)"),
                        card_body(padding = 0, leafletOutput("ev_mapa", height = "430px"))),
            bslib::card(card_header("Ranking por provincia"),
                        card_body(plotlyOutput("ev_ranking", height = "430px")))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Evolución por comunidad autónoma (2018-2022)"),
                        card_body(plotlyOutput("ev_evol", height = "430px"))),
            bslib::card(card_header("Tabla por provincia"),
                        card_body(DT::DTOutput("ev_tabla")))
          )
        )
      )
    )
  ),
  
  # PESTAÑA 4: EUROPA
  nav_panel(
    title = "Europa",
    navset_tab(
      nav_panel(
        title = "Panel europeo",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros europeos",
            width = 280,
            selectInput("eur_anio", "Año:", choices = as.character(sort(unique(europa_agrupada$anio), decreasing = TRUE)),
                        selected = as.character(max(europa_agrupada$anio, na.rm = TRUE))),
            selectInput("eur_causa", "Causa / grupo de defunción:", choices = causas_europa_es,
                        selected = if (length(causas_europa_raw)) {
                          causas_europa_raw[ifelse(any(causas_europa_raw != "Total"), which(causas_europa_raw != "Total")[1], 1)]
                        } else NULL),
            radioButtons("eur_sexo", "Sexo:",
                         choices = c("Ambos" = "Ambos", "Hombres" = "Hombres", "Mujeres" = "Mujeres"),
                         selected = if ("Ambos" %in% sexos_europa_raw) "Ambos" else sexos_europa_raw[1]),
            uiOutput("eur_filtro_info")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Total de defunciones", value = textOutput("eur_kpi_total"), showcase = bsicons::bs_icon("heartbreak"), theme = "danger"),
            value_box(title = "País con mayor tasa de mortalidad", value = textOutput("eur_kpi_max"), showcase = bsicons::bs_icon("flag"), theme = "primary"),
            value_box(title = "Sexo seleccionado", value = textOutput("eur_kpi_sexo"), showcase = bsicons::bs_icon("people"), theme = "info")
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Compara la <b>mortalidad entre países europeos</b> con tasas por 100.000 habitantes (datos Eurostat): el mapa y la evolución muestran dónde y cómo cambia cada causa, y los rankings separan las 4 causas con mayor y menor tasa en <b>hombres y mujeres</b>.</p>"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              card_header("Mapa europeo — tasa de defunción por 100.000 habitantes"),
              card_body(padding = 0, leafletOutput("eur_mapa", height = "430px"))
            ),
            bslib::card(
              card_header("Evolución temporal — 5 países más afectados, 5 menos afectados y media europea"),
              card_body(plotlyOutput("eur_plot_barras", height = "500px"))
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              card_header("Causas con mayor tasa por sexo"),
              card_body(plotlyOutput("eur_3_causas_top", height = "330px"))
            ),
            bslib::card(
              card_header("Causas con menor tasa por sexo"),
              card_body(plotlyOutput("eur_3_causas_bottom", height = "330px"))
            )
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(card_header("Detalle de los datos europeos — tasas de mortalidad estandarizadas"), card_body(DT::DTOutput("eur_tabla")))
          )
        )
      ),
      nav_panel(
        title = "Comparador de países europeos",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            selectInput("eur_comp_pais_a", "País:", choices = paises_europa_es,
                        selected = if ("Spain" %in% paises_europa_raw) "Spain" else paises_europa_raw[1]),
            selectInput("eur_comp_pais_b", "País:", choices = paises_europa_es,
                        selected = if ("France" %in% paises_europa_raw) "France" else if (length(paises_europa_raw) > 1) paises_europa_raw[2] else paises_europa_raw[1]),
            selectInput("eur_comp_causa", "Causa de Defunción:", choices = causas_europa_es,
                        selected = if (length(causas_europa_raw)) causas_europa_raw[1] else NULL),
            selectInput("eur_comp_anio", "Año de referencia:",
                        choices = as.character(sort(unique(europa_agrupada$anio), decreasing = TRUE)),
                        selected = if ("2022" %in% as.character(europa_agrupada$anio)) "2022" else as.character(max(europa_agrupada$anio, na.rm = TRUE))),
            radioButtons("eur_comp_sexo", "Sexo:",
                         choices = c("Ambos" = "Ambos", "Hombres" = "Hombres", "Mujeres" = "Mujeres"),
                         selected = if ("Ambos" %in% sexos_europa_raw) "Ambos" else sexos_europa_raw[1]),
            uiOutput("eur_comp_filtro_info")
          ),
          layout_columns(
            col_widths = c(6, 6),
            value_box(
              title = textOutput("eur_comp_poblacion_titulo_a"),
              value = textOutput("eur_comp_poblacion_a"),
              showcase = bsicons::bs_icon("people"),
              theme = "primary"
            ),
            value_box(
              title = textOutput("eur_comp_poblacion_titulo_b"),
              value = textOutput("eur_comp_poblacion_b"),
              showcase = bsicons::bs_icon("people"),
              theme = "info"
            )
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Enfrenta <b>dos países</b> con la <b>media europea</b>: la evolución muestra si avanzan mejor o peor que el conjunto, y los radares resumen su perfil por grupos de causas en el año elegido (arriba) y en el resto de años (abajo).</p>"))
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(
              card_header("Evolución Temporal Comparada con la Media Europea (2018-2022)"),
              card_body(plotlyOutput("eur_comp_evol_nacional", height = "350px"))
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              card_header(uiOutput("eur_comp_radar_title_a")),
              card_body(plotlyOutput("eur_comp_radar_a", height = "360px"))
            ),
            bslib::card(
              card_header(uiOutput("eur_comp_radar_title_b")),
              card_body(plotlyOutput("eur_comp_radar_b", height = "360px"))
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              card_header(uiOutput("eur_comp_radar_bottom_title_a")),
              card_body(plotlyOutput("eur_comp_radar_a_bot", height = "360px"))
            ),
            bslib::card(
              card_header(uiOutput("eur_comp_radar_bottom_title_b")),
              card_body(plotlyOutput("eur_comp_radar_b_bot", height = "360px"))
            )
          )
        )
      ),
      nav_panel(
        title = "Comparación estadística europea",
        layout_sidebar(
          sidebar = sidebar(
            title = "Modelo europeo",
            width = 280,
            selectInput("eur_mod_causa", "Causa / grupo de defunción:", choices = causas_europa_es,
                        selected = if (length(causas_europa_raw)) {
                          idx <- if (any(causas_europa_raw != "Total")) which(causas_europa_raw != "Total")[1] else 1
                          causas_europa_raw[idx]
                        } else NULL),
            radioButtons("eur_mod_sexo", "Sexo:",
                         choices = c("Ambos" = "Ambos", "Hombres" = "Hombres", "Mujeres" = "Mujeres"),
                         selected = "Ambos"),
            selectInput("eur_mod_anio", "Años:",
                        choices = c("Todos los años", as.character(sort(unique(europa_agrupada$anio)))),
                        selected = "Todos los años"),
            div(class = "filter-help", HTML(
              "<b>Método:</b> se ajusta un modelo lineal de la tasa de defunciones por país, incluyendo el año para controlar las diferencias temporales. El efecto de <i>país</i> permite comprobar si existen diferencias estadísticamente significativas entre países."
            )),
            uiOutput("eur_mod_info")
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("R² del modelo europeo"), card_body(textOutput("eur_mod_r2"))),
            bslib::card(card_header("¿Existen diferencias entre países?"), card_body(textOutput("eur_mod_validez")))
          ),
          bslib::card(
            card_header("Interpretación del modelo"),
            card_body(HTML(
              paste0(
                "<p>Este análisis compara las <b>tasas de defunción entre países europeos</b> para la causa y sexo seleccionados. Cuando se incluyen varios años, el año se incorpora al modelo para separar, en la medida de lo posible, el efecto temporal del efecto asociado al país.</p>",
                "<p><b>Hipótesis del efecto país:</b> H<sub>0</sub>: las diferencias medias ajustadas por año entre países son nulas; H<sub>1</sub>: al menos un país presenta una diferencia respecto al resto.</p>",
                "<p>Con <b>p &lt; 0,05</b> se considera que existe evidencia estadísticamente significativa de diferencias entre países.</p>"
              )
            ))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Distribución de tasas por país"), card_body(plotlyOutput("eur_mod_boxplot", height = "640px"))),
            bslib::card(card_header("Resultados del modelo"), card_body(tableOutput("eur_mod_tabla")))
          ),
          bslib::card(card_header("Resumen estadístico del modelo"), card_body(verbatimTextOutput("eur_mod_summary")))
        )
      )
    )
  ),

  # PESTAÑA 5BIS: DETERMINANTES SOCIOECONÓMICOS (RENTA Y MÉDICOS)
  nav_panel(
    title = "Determinantes",
    navset_tab(
      nav_panel(
        title = "Renta y médicos",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 290,
            selectInput("det_ind", "Indicador:", choices = det_indicadores,
                        selected = if ("Renta neta media por persona" %in% det_indicadores) "Renta neta media por persona" else det_indicadores[1]),
            selectInput("det_ano", "Año:", choices = det_anos,
                        selected = if ("2022" %in% det_anos) "2022" else det_anos[1]),
            div(class = "filter-help", HTML(
              "<b>Nota:</b> la renta es provincial (se agrega a CCAA ponderando por población); los médicos vienen por CCAA (colegiados no jubilados por 100.000 hab.). Navarra y País Vasco sin renta 2018-2020."
            ))
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Media nacional", value = textOutput("det_kpi_media"),
                      showcase = bsicons::bs_icon("bar-chart-line"), theme = "primary"),
            value_box(title = "Mayor valor", value = textOutput("det_kpi_max"),
                      showcase = bsicons::bs_icon("arrow-up-circle"), theme = "danger"),
            value_box(title = "Menor valor", value = textOutput("det_kpi_min"),
                      showcase = bsicons::bs_icon("arrow-down-circle"), theme = "success")
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>La <b>renta</b> (media por persona y hogar) y los <b>médicos colegiados no jubilados</b> describen el contexto socioeconómico y sanitario por comunidad. La renta provincial se agrega a CCAA <b>ponderando por población</b>; los médicos vienen directos por CCAA. Sin dato donde la fuente no publica (Navarra 2018-2020).</p>"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Mapa por comunidad autónoma"),
                        card_body(padding = 0, leafletOutput("det_mapa", height = "430px"))),
            bslib::card(card_header("Evolución por comunidad (2018-2022)"),
                        card_body(plotlyOutput("det_evol", height = "430px")))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Ranking por comunidad"),
                        card_body(plotlyOutput("det_ranking", height = "430px"))),
            bslib::card(card_header("Tabla por comunidad"),
                        card_body(DT::DTOutput("det_tabla")))
          )
        )
      ),
      nav_panel(
        title = "Índice sintético",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 290,
            selectInput("idx_ano", "Año:", choices = det_anos,
                        selected = if ("2022" %in% det_anos) "2022" else det_anos[1]),
            sliderInput("idx_wr", "Peso renta:", min = 0, max = 100, value = 34),
            sliderInput("idx_wm", "Peso médicos:", min = 0, max = 100, value = 33),
            sliderInput("idx_wmo", "Peso mortalidad:", min = 0, max = 100, value = 33),
            div(class = "filter-help", HTML(
              "<b>Nota:</b> índice 0-100 (min-max 2018-2022; mortalidad invertida: menos mortalidad, más puntos). Sin valor si falta algún componente (Navarra 2018-2020). La mortalidad es tasa bruta: penaliza a las CCAA más envejecidas."
            ))
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Comunidad líder", value = textOutput("idx_kpi_lider"),
                      showcase = bsicons::bs_icon("trophy"), theme = "success"),
            value_box(title = "Índice medio nacional", value = textOutput("idx_kpi_media"),
                      showcase = bsicons::bs_icon("bar-chart-line"), theme = "primary"),
            value_box(title = "Brecha máx-mín", value = textOutput("idx_kpi_brecha"),
                      showcase = bsicons::bs_icon("arrows-expand"), theme = "warning")
          ),
          bslib::card(
            card_header("Metodología"),
            card_body(HTML("<p>Cada componente se normaliza a 0-100 con min-max <b>global 2018-2022</b> (la mortalidad, invertida). El índice es la <b>media ponderada</b> con los pesos del panel. Es descriptivo, no causal: conviene leerlo junto a las tasas estandarizadas por edad.</p>"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Mapa del índice por comunidad"),
                        card_body(padding = 0, leafletOutput("idx_mapa", height = "430px"))),
            bslib::card(card_header("Ranking por comunidad"),
                        card_body(plotlyOutput("idx_ranking", height = "430px")))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Evolución del índice (2018-2022)"),
                        card_body(plotlyOutput("idx_evol", height = "430px"))),
            bslib::card(card_header("Tabla por comunidad"),
                        card_body(DT::DTOutput("idx_tabla")))
          )
        )
      )
    )
  ),
  
  
# PESTAÑA 5: REGRESIONES LINEALES
  nav_panel(
    title = "Regresiones lineales",
    navset_tab(
      nav_panel(
        title = "Panel europeo: efectos fijos (país, año, causa, sexo)",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 290,
            selectInput("eur_reg_causa", "Causa:", choices = causas_europa_es,
                        selected = if (length(causas_europa_raw)) {
                          causas_europa_raw[ifelse(any(causas_europa_raw != "Total"), which(causas_europa_raw != "Total")[1], 1)]
                        } else NULL),
            selectInput("eur_reg_sexo", "Sexo:", choices = sexos_europa_es,
                        selected = if ("Ambos" %in% sexos_europa_raw) "Ambos" else sexos_europa_raw[1]),
            checkboxGroupInput("eur_reg_fe", "Efectos fijos:",
                               choices = c("País" = "pais", "Año" = "anio", "Causa" = "causa", "Sexo" = "sexo"),
                               selected = c("pais", "anio")),
            checkboxInput("eur_reg_log", "Log(tasa)", value = FALSE),
            div(class = "filter-help", HTML(
              "<b>Modelo:</b> tasa_100k ~ efectos fijos seleccionados. <b>Datos:</b> Eurostat 2018-2022, tasas estandarizadas por 100k hab. <b>N obs:</b> ~9k. <b>Cluster:</b> errores robustos por país."
            ))
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "R² ajustado", value = textOutput("eur_reg_r2"),
                      showcase = bsicons::bs_icon("graph-up"), theme = "primary"),
            value_box(title = "N observaciones", value = textOutput("eur_reg_n"),
                      showcase = bsicons::bs_icon("database"), theme = "success"),
            value_box(title = "Países", value = textOutput("eur_reg_paises"),
                      showcase = bsicons::bs_icon("globe"), theme = "info")
          ),
          bslib::card(
            card_header("Resumen del modelo"),
            card_body(verbatimTextOutput("eur_reg_summary"))
          ),
          bslib::card(
            card_header("Coeficientes (efectos fijos)"),
            card_body(DT::DTOutput("eur_reg_coef"))
          ),
          bslib::card(
            card_header("Diagnósticos"),
            card_body(
              div(class = "row",
                div(class = "col-md-6",
                  card_header("Residuos vs ajustados"),
                  card_body(plotlyOutput("eur_reg_diag1", height = "350px"))
                ),
                div(class = "col-md-6",
                  card_header("QQ-plot residuos"),
                  card_body(plotlyOutput("eur_reg_diag2", height = "350px"))
                )
              )
            )
          ),
          bslib::card(
            card_header("Tendencias por país (predichos vs observados)"),
            card_body(plotlyOutput("eur_reg_trends", height = "500px"))
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Panel europeo con efectos fijos bidireccionales (país + año por defecto). Controla heterogeneidad no observada por país y tendencias temporales comunes. <b>Errores robustos clusterizados por país</b> (Arellano, 1987). <b>Log(tasa)</b> opcional para estabilizar varianza. No causalidad: correlaciones condicionadas a los efectos fijos.</p>"))
          )
        )
      ),
      nav_panel(
        title = "Regresión entre causas",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 290,
            selectInput("p32_x", "Causa predictora (X):", choices = setdiff(lista_defunciones, "Total"),
                        selected = { ch <- setdiff(lista_defunciones, "Total"); if (length(ch)) ch[1] else NULL }),
            selectInput("p32_y", "Causa explicativa (Y):", choices = setdiff(lista_defunciones, "Total"),
                        selected = { ch <- setdiff(lista_defunciones, "Total"); if (length(ch) > 1) ch[2] else if (length(ch)) ch[1] else NULL }),
            selectInput("p32_ano", "Año:", choices = sort(unique(as.character(causas_provinciales$Año))),
                        selected = if ("2020" %in% as.character(causas_provinciales$Año)) "2020" else sort(unique(as.character(causas_provinciales$Año)))[1]),
            radioButtons("p32_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
            checkboxInput("p32_log", "Aplicar logaritmo a X e Y", value = FALSE),
            div(class = "filter-help", HTML(
              "<b>Nota:</b> tasas de defunción provinciales por causa (defunciones / población × 100.000). Con <b>Ambos</b> se suman ambos sexos. Ceuta y Melilla se excluyen del análisis. Se aplican los mismos 4 supuestos clásicos y el test de Moran que en la pestaña anterior."
            )),
            uiOutput("p32_filtro_info")
          ),
          div(class = "regression-dashboards",
              layout_columns(
                col_widths = c(4, 4, 4),
                value_box(
                  title = "R²",
                  value = textOutput("p32_r2"),
                  showcase = bsicons::bs_icon("graph-up"),
                  theme = "primary"
                ),
                value_box(
                  title = "4 supuestos",
                  value = textOutput("p32_val"),
                  showcase = bsicons::bs_icon("check-circle"),
                  theme = "success"
                ),
                value_box(
                  title = "Validez espacial",
                  value = textOutput("p32_esp"),
                  showcase = bsicons::bs_icon("geo-alt"),
                  theme = "info"
                )
              )
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Cruza las <b>tasas por causa</b> entre sí para el año y sexo elegidos: si dos causas suben y bajan juntas por provincias, comparten patrón territorial (no causalidad). Se aplican los mismos 4 supuestos y el test de Moran que en el análisis anterior.</p>"))
          ),
          bslib::card(
            card_header("Validez clásica y espacial de los modelos entre causas"),
            card_body(
              div(class = "table-responsive", tableOutput("p32_modelos_info")),
              div(class = "filter-help", HTML(paste0(
                "<div><b>Lectura:</b></div>",
                "<div>cada fila representa la causa explicativa (Y) y cada columna la causa predictora (X), para el año, sexo y escala seleccionados.</div>",
                "<div><b>✓</b> = cumple los 4 supuestos clásicos.</div>",
                "<div><b>●</b> = cumple la validez espacial (Moran, p &gt; 0,05), aunque no cumpla los 4 supuestos.</div>",
                "<div><b>✓ ●</b> = cumple ambos: supuestos clásicos y validez espacial.</div>",
                "<div>La diagonal no se puede utilizar porque X e Y serían la misma causa.</div>",
                "<div><b>Comparaciones múltiples:</b> con 156 contrastes al nivel 5 %, se esperan ~8 falsos positivos por azar; valora cada ✓ ● junto a su R².</div>")))
            )
          ),
          bslib::card(card_header("Dispersión, recta e intervalo de confianza"), card_body(plotlyOutput("p32_scatter", height = "390px"))),
          bslib::card(card_header("Resultados y comprobación de los 4 supuestos"), card_body(tableOutput("p32_supuestos"))),
          bslib::card(
            card_header("Autocorrelación espacial de los residuos (Moran)"),
            card_body(
              tableOutput("p32_moran_tabla"),
              div(class = "metric-note", "p > 0,05 indica que no hay evidencia de autocorrelación espacial significativa de los residuos."),
              textOutput("p32_moran_conclusion")
            )
          )
        )
      )
    )
  ),

  # PESTAÑA 6BIS: EXCESO DE MORTALIDAD (OBSERVADO VS 2015-2019)
  nav_panel(
    title = "Exceso de mortalidad",
    layout_sidebar(
      sidebar = sidebar(
        title = "Configuración",
        width = 290,
        if (is.null(datos_edadprov)) div(class = "filter-help", HTML(
          "<b>Dataset pendiente.</b> Añade <b>defunciones_edad_provincia.csv</b>."
        )) else tagList(
          selectInput("ex_prov", "Provincia:", choices = c("Todas", datos_edadprov$provincias),
                      selected = "Todas"),
          radioButtons("ex_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
          selectInput("ex_ano", "Año (mensual):", choices = datos_edadprov$anos,
                      selected = if ("2020" %in% datos_edadprov$anos) "2020" else datos_edadprov$anos[1])
        )
      ),
      if (is.null(datos_edadprov)) bslib::card(
        card_header("Sin datos"),
        card_body(HTML("<p>Añade <b>defunciones_edad_provincia.csv</b> a la carpeta y reinicia.</p>"))
      ) else div(class = "exceso-dash",
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(title = "Exceso 2020", value = textOutput("ex_kpi_2020"),
                    showcase = bsicons::bs_icon("graph-up"), theme = "danger"),
          value_box(title = "Exceso 2021", value = textOutput("ex_kpi_2021"),
                    showcase = bsicons::bs_icon("bar-chart-line"), theme = "warning"),
          value_box(title = "Peor año", value = textOutput("ex_kpi_peor"),
                    showcase = bsicons::bs_icon("calendar-event"), theme = "primary")
        ),
        bslib::card(
          card_header("Interpretación"),
          card_body(HTML("<p>El <b>exceso</b> compara las defunciones observadas con las <b>esperadas</b> (media 2015-2019 del mismo territorio y sexo). El <b>P-score</b> lo expresa en porcentaje. No distingue causas: incluye COVID directo, indirecto y cambios de registro.</p>"))
        ),
        bslib::card(card_header("Serie anual: observadas frente a esperadas (media 2015-2019)"),
                    card_body(plotlyOutput("ex_evol", height = "620px"))),
        bslib::card(card_header("Defunciones por mes (año seleccionado frente a baseline)"),
                    card_body(plotlyOutput("ex_meses", height = "560px"))),
        bslib::card(card_header("Tabla anual"),
                    card_body(DT::DTOutput("ex_tabla")))
      )
    )
  ),

  # PESTAÑA 7: ANÁLISIS MULTIVARIANTE (PCA + CLUSTERS, NO REGRESIÓN)
  nav_panel(
    title = "Análisis multivariante",
    layout_sidebar(
      sidebar = sidebar(
        title = "Configuración",
        width = 290,
        selectInput("pm_ano", "Año:", choices = sort(unique(as.character(causas_provinciales$Año))),
                    selected = if ("2022" %in% as.character(causas_provinciales$Año)) "2022" else sort(unique(as.character(causas_provinciales$Año)))[1]),
        radioButtons("pm_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
        sliderInput("pm_k", "Nº de clusters (k-means):", min = 2, max = 6, value = 3, step = 1),
        div(class = "filter-help", HTML(
          "<b>Nota:</b> PCA sobre tasas por 100.000 (centradas y escaladas; sin la causa Total ni causas sin variación entre provincias). K-means sobre PC1-PC2 con semilla fija."
        )),
        uiOutput("pm_info")
      ),
      div(class = "regression-dashboards",
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Varianza PC1 + PC2", value = textOutput("pm_kpi_var"),
                      showcase = bsicons::bs_icon("graph-up"), theme = "primary"),
            value_box(title = "Clusters", value = textOutput("pm_kpi_k"),
                      showcase = bsicons::bs_icon("check-circle"), theme = "success"),
            value_box(title = "Provincias × causas", value = textOutput("pm_kpi_n"),
                      showcase = bsicons::bs_icon("grid"), theme = "info")
          )
      ),
      div(
        class = "mb-4",
        bslib::card(
          card_header("Interpretación"),
          card_body(HTML(paste0(
            "<p><b>Biplot:</b> cada punto es una provincia situada según sus dos primeras componentes (las direcciones que más diferencian los perfiles de mortalidad). Provincias cercanas tienen causas de muerte parecidas.</p>",
            "<p><b>Flechas de las causas:</b> cada flecha es una causa de defunción. Apunta hacia donde aumentan sus tasas: las provincias en esa dirección mueren más por esa causa. Su <b>longitud</b> indica cuánto pesa esa causa en la diferencia entre provincias (flechas largas = causas que más discriminan) y el <b>ángulo</b> entre dos flechas su relación: ángulo pequeño = suben juntas, ángulo llano (~180º) = cuando una sube la otra baja, ángulo recto = independientes.</p>",
            "<p><b>Scree:</b> la barra muestra lo que explica cada componente y la línea el acumulado. Si con dos componentes se supera ~2/3 de la varianza, el plano resume bien el conjunto.</p>",
            "<p><b>K-means y mapa:</b> agrupa provincias por perfil de mortalidad (no por geografía): que un cluster salga geográficamente compacto en el mapa es un hallazgo, no un requisito.</p>",
            "<p class='text-muted mb-0'><small>Nivel ecológico (provincias, no personas): las asociaciones no implican causalidad individual. Tasas centradas y escaladas antes del PCA.</small></p>"
          )))
        ),
        div(class = "card mb-4",
            div(class = "card-header", "Biplot PC1-PC2 (provincias por cluster)"),
            div(class = "card-body p-2", plotlyOutput("pm_biplot", height = "850px"))
        ),
        div(class = "card mb-4",
            div(class = "card-header", "Mapa de clusters"),
            div(class = "card-body p-0", leafletOutput("pm_mapa", height = "750px"))
        ),
        div(class = "card mb-4",
            div(class = "card-header", "Varianza explicada (scree)"),
            div(class = "card-body p-2", plotlyOutput("pm_scree", height = "600px"))
        ),
        div(class = "card mb-4",
            div(class = "card-header", "Correlaciones entre causas (Pearson)"),
            div(class = "card-body p-2", plotlyOutput("pm_cor", height = "800px"))
        ),
        div(class = "card mb-4",
            div(class = "card-header", "Perfil medio por cluster (tasas por 100k hab.)"),
            div(class = "card-body p-2", div(class = "table-responsive", tableOutput("pm_perfil")))
        ),
)
    )
  )
)

# ==============================================================================
# 4. SERVIDOR (SERVER)
# ==============================================================================
server <- function(input, output, session) {
  
  obtener_unidad <- function(funcion) {
    if (grepl("Esperanza|Promedio|Tiempo", funcion, ignore.case = TRUE)) return("años")
    if (grepl("Tasa", funcion, ignore.case = TRUE)) return("por mil")
    if (grepl("Riesgo", funcion, ignore.case = TRUE)) return("probabilidad (0 a 1)")
    if (grepl("Superviviente|Defunciones|Población", funcion, ignore.case = TRUE)) return("personas")
    return("unidades")
  }
  
  evaluar_supuestos <- function(modelo, df_data) {
    residuos <- residuals(modelo)
    ajustados <- fitted(modelo)
    n <- length(residuos)
    
    # 1. Linealidad: comparación del modelo lineal con un modelo cuadrático.
    p_quad <- tryCatch({
      modelo_quad <- lm(Y_val ~ X_val + I(X_val^2), data = df_data)
      anova(modelo, modelo_quad)$`Pr(>F)`[2]
    }, error = function(e) NA_real_)
    
    concl_linealidad <- if (is.na(p_quad)) {
      "No evaluable"
    } else if (p_quad > 0.05) {
      "Cumplido: no hay evidencia de curvatura"
    } else {
      "No cumplido: posible no linealidad"
    }
    
    codigo_linealidad <- "anova(lm(Y_val ~ X_val), lm(Y_val ~ X_val + I(X_val^2)))"
    resultado_linealidad <- if (is.na(p_quad)) "p = NA" else paste0("p = ", format.pval(p_quad, digits = 4, eps = 0.001))
    
    # 2. Homocedasticidad: prueba de Breusch-Pagan.
    p_bp <- tryCatch({
      lmtest::bptest(modelo)$p.value
    }, error = function(e) NA_real_)
    
    concl_homocedasticidad <- if (is.na(p_bp)) {
      "No evaluable"
    } else if (p_bp > 0.05) {
      "Cumplido: no hay evidencia de heterocedasticidad"
    } else {
      "No cumplido: evidencia de heterocedasticidad"
    }
    
    codigo_bp <- "lmtest::bptest(modelo)"
    resultado_bp <- if (is.na(p_bp)) "p = NA" else paste0("p = ", format.pval(p_bp, digits = 4, eps = 0.001))
    
    # 3. Incorrelación de errores: contraste Durbin-Watson con p-valor.
    dw_obj <- tryCatch(lmtest::dwtest(modelo), error = function(e) NULL)
    dw_stat <- if (is.null(dw_obj)) NA_real_ else unname(dw_obj$statistic)
    p_dw <- if (is.null(dw_obj)) NA_real_ else dw_obj$p.value
    
    concl_incorrelacion <- if (is.na(p_dw)) {
      "No evaluable"
    } else if (p_dw > 0.05) {
      "Cumplido: no hay evidencia de autocorrelación"
    } else {
      "No cumplido: evidencia de autocorrelación"
    }
    
    codigo_dw <- "lmtest::dwtest(modelo)"
    resultado_dw <- if (is.na(p_dw)) {
      "DW = NA; p = NA"
    } else {
      paste0("DW = ", round(dw_stat, 4), "; p = ", format.pval(p_dw, digits = 4, eps = 0.001))
    }
    
    # 4. Normalidad de los residuos: Shapiro-Wilk.
    p_norm <- tryCatch({
      if (n >= 3 && n <= 5000) shapiro.test(residuos)$p.value else NA_real_
    }, error = function(e) NA_real_)
    
    concl_normalidad <- if (is.na(p_norm)) {
      "No evaluable"
    } else if (p_norm > 0.05) {
      "Cumplido: no hay evidencia contra la normalidad"
    } else {
      "No cumplido: evidencia contra la normalidad"
    }
    
    codigo_norm <- "shapiro.test(residuos)"
    resultado_norm <- if (is.na(p_norm)) "p = NA" else paste0("p = ", format.pval(p_norm, digits = 4, eps = 0.001))
    
    data.frame(
      Supuesto = c(
        "1. Linealidad",
        "2. Homocedasticidad",
        "3. Incorrelación de errores",
        "4. Normalidad de los residuos"
      ),
      Codigo = c(
        codigo_linealidad,
        codigo_bp,
        codigo_dw,
        codigo_norm
      ),
      Resultado_codigo = c(
        resultado_linealidad,
        resultado_bp,
        resultado_dw,
        resultado_norm
      ),
      Conclusion = c(
        concl_linealidad,
        concl_homocedasticidad,
        concl_incorrelacion,
        concl_normalidad
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  
  # Cuadro informativo dinámico
  output$p3_cuadro_informativo_dinamico <- renderUI({
    req(input$p3_funcion)
    f_sel <- input$p3_funcion
    
    info <- switch(
      f_sel,
      "Esperanza de vida" = list(
        nombre = "Esperanza de vida / Años a vivir a la edad x",
        def = "Número medio de años adicionales que le quedan por vivir a una persona que alcanza exactamente el tramo de edad x.",
        unidad = "Años",
        recom = "**Menores de 1 año (De 0 a 0)** para la esperanza de vida al nacer, y **De 65 a 69 años** para analizar la expectativa de vida residual al alcanzar la jubilación."
      ),
      "Población estacionaria" = list(
        nombre = "Población estacionaria",
        def = "Número total de personas que estarían con vida permanentemente en un tramo de edad si nacieran continuamente 100.000 personas al año y se mantuviera constante la mortalidad.",
        unidad = "Personas (Años-Persona)",
        recom = "**De 15 a 64 años** para estimar el peso relativo del volumen poblacional en edad de trabajar en una estructura demográfica estable."
      ),
      "Promedio de años vividos el último año de vida" = list(
        nombre = "Promedio de años vividos el último año de vida",
        def = "Número medio de años vividos dentro del intervalo de edad por las personas que fallecen en dicho tramo.",
        unidad = "Años",
        recom = "**Menores de 1 año (De 0 a 0)** debido a la concentración del riesgo en los primeros días/meses de vida."
      ),
      "Riesgo de muerte" = list(
        nombre = "Riesgo de muerte",
        def = "Probabilidad de que una persona que alcanza el inicio del tramo de edad x fallezca antes de cumplir o superar la siguiente edad.",
        unidad = "Probabilidad (escala decimal entre 0 y 1)",
        recom = "**De 80 a 84 años y superiores**, tramos donde la probabilidad de fallecer se acelera exponencialmente."
      ),
      "Supervivientes" = list(
        nombre = "Supervivientes / Personas que alcanzan la edad x",
        def = "Número de personas (de los 100.000 nacidos iniciales) que sobreviven al llegar exactamente a la edad x.",
        unidad = "Personas (sobre la cohorte teórica de 100.000)",
        recom = "**De 65 a 69 años** para observar la proporción de la cohorte inicial que alcanza la edad madura, y **De 85 a 89 años** para evaluar la longevidad extrema."
      ),
      "Tasa de mortalidad" = list(
        nombre = "Tasa de mortalidad / Probabilidad de muerte",
        def = "Proporción de personas de la edad x que fallecen antes de cumplir la siguiente edad.",
        unidad = "Promil (‰)",
        recom = "**De 50 a 54 años y De 70 a 74 años** para medir el impacto de enfermedades crónicas antes y después del retiro profesional."
      ),
      "Tiempo por vivir" = list(
        nombre = "Tiempo por vivir",
        def = "Número total acumulado de años que le restan por vivir a todo el grupo de población que ha alcanzado la edad x.",
        unidad = "Años-Persona acumulados",
        recom = "**Menores de 1 año (De 0 a 0)** para evaluar la bolsa total del capital de vida potencial de la cohorte."
      ),
      list(
        nombre = f_sel,
        def = "Información estadística correspondiente a las tablas demográficas de mortalidad del INE.",
        unidad = obtener_unidad(f_sel),
        recom = "**De 0 a 0 años y De 65 a 69 años** como tramos de referencia demográfica estándar."
      )
    )
    
    bslib::card(
      card_header(class = "bg-info text-white", paste("Guía Explicativa:", info$nombre)),
      card_body(
        markdown(sprintf("
        * **Definición:** %s
        * **Unidad de Medida:** **%s**
        * **Grupos de Edad Recomendados:** %s
        ", info$def, info$unidad, info$recom))
      )
    )
  })
  
  # Ayudas contextuales
  output$p2_filtro_info <- renderUI({
    req(input$p2_defuncion)
    div(class = "filter-help",
        HTML(paste0("<b>Causa:</b> ", htmltools::htmlEscape(input$p2_defuncion),
                    ". La tasa de la causa es defunciones / población × 100.000.")))
  })
  
  output$p21_filtro_info <- renderUI({
    req(input$p21_prov_a, input$p21_prov_b, input$p21_defuncion, input$p21_ano)
    div(class = "filter-help",
        HTML(paste0("<b>Comparación:</b> ", htmltools::htmlEscape(input$p21_prov_a),
                    " vs. ", htmltools::htmlEscape(input$p21_prov_b),
                    ". El radar usa ", htmltools::htmlEscape(input$p21_ano), ".")))
  })
  
  output$p3_filtro_info <- renderUI({
    req(input$p3_funcion, input$p3_edad)
    texto <- if (input$p3_edad == "Todos los tramos")
      "El mapa resume la media provincial para todos los tramos."
    else
      paste0("Se está mostrando específicamente el tramo ", htmltools::htmlEscape(input$p3_edad), ".")
    div(class = "filter-help",
        HTML(paste0("<b>Métrica:</b> ", htmltools::htmlEscape(input$p3_funcion), ". ", texto)))
  })
  
  output$p31_filtro_info <- renderUI({
    req(input$p31_funcion, input$p31_prov_a, input$p31_prov_b)
    div(class = "filter-help",
        HTML(paste0("<b>Lectura:</b> ", htmltools::htmlEscape(input$p31_funcion),
                    " para ", htmltools::htmlEscape(input$p31_prov_a),
                    " y ", htmltools::htmlEscape(input$p31_prov_b),
                    " · población mostrada para ", htmltools::htmlEscape(input$p31_ano_radar), ".")))
  })
  
  # --- PESTAÑA 2 SERVER ---
  datos_p2 <- reactive({
    req(input$p2_defuncion, input$p2_ano, input$p2_sexo)
    df <- causas_provinciales %>%
      filter(Año == input$p2_ano, Defunción == input$p2_defuncion)
    if (input$p2_sexo != "Ambos") df <- df %>% filter(Sexo == input$p2_sexo)
    
    df %>%
      group_by(Provincia) %>%
      summarise(
        Total_Fallecidos = sum(Fallecidos, na.rm = TRUE),
        Poblacion_Total = sum(Poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Tasa = if_else(Poblacion_Total > 0,
                            Total_Fallecidos / Poblacion_Total * 100000,
                            NA_real_)) %>%
      filter(is.finite(Tasa))
  }) %>% bindCache(input$p2_defuncion, input$p2_ano, input$p2_sexo)
  
  output$p2_kpi_total <- renderText({
    df <- datos_p2()
    if (nrow(df) == 0) return("-")
    tasa_nac <- (sum(df$Total_Fallecidos, na.rm = TRUE) / sum(df$Poblacion_Total, na.rm = TRUE)) * 100000
    paste0(format(round(tasa_nac, 2), big.mark = ".", decimal.mark = ","), " / 100k hab.")
  })
  
  output$p2_kpi_max <- renderText({
    df <- datos_p2()
    if (nrow(df) == 0) return("-")
    max_row <- df %>% arrange(desc(Tasa)) %>% slice(1)
    paste0(max_row$Provincia, " (", format(round(max_row$Tasa, 2), big.mark = ".", decimal.mark = ","), ")")
  })
  
  output$p2_kpi_min <- renderText({
    df <- datos_p2()
    if (nrow(df) == 0) return("-")
    min_row <- df %>% arrange(Tasa) %>% slice(1)
    paste0(min_row$Provincia, " (", format(round(min_row$Tasa, 2), big.mark = ".", decimal.mark = ","), ")")
  })
  
  output$p2_mapa <- renderLeaflet({
    df <- datos_p2()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    
    val_max <- max(mapa_datos$Tasa, na.rm = TRUE)
    dominio_colores <- if (is.infinite(val_max) || is.na(val_max) || val_max == 0) c(0, 1) else c(0, val_max)
    pal <- colorNumeric(palette = PAL_YLORRD, domain = dominio_colores, na.color = "#E0E0E0")
    
    etiquetas <- sprintf("<strong>%s</strong><br/>Tasa de la causa: %s por 100k hab.", mapa_datos$NAME_2, format(round(mapa_datos$Tasa, 2), decimal.mark = ",")) %>% lapply(htmltools::HTML)
    
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Tasa), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio_colores, opacity = 0.8, title = "Tasa de la causa / 100k hab.", position = "bottomright")
  })
  
  output$p2_evolucion_top_bot <- renderPlotly({
    req(input$p2_defuncion)
    
    df_evo <- causas_provinciales %>% filter(Defunción == input$p2_defuncion)
    if (input$p2_sexo != "Ambos") df_evo <- df_evo %>% filter(Sexo == input$p2_sexo)
    
    df_evo <- df_evo %>% mutate(Ano_Num = Año_Num)
    
    df_nacional <- df_evo %>%
      group_by(Ano_Num) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
             Provincia = "Tasa nacional") %>%
      filter(is.finite(Tasa))
    
    ultimo_ano <- max(df_evo$Ano_Num, na.rm = TRUE)
    ranking <- df_evo %>%
      filter(Ano_Num == ultimo_ano) %>%
      group_by(Provincia) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>%
      arrange(desc(Tasa))
    
    top5 <- head(ranking$Provincia, 5)
    bot5 <- tail(ranking$Provincia, 5)
    
    # FIX: tasa ponderada por población (defunciones/población × 100k),
    # coherente con la "Tasa nacional" del mismo gráfico. Antes se usaba la
    # media simple de las tasas por sexo.
    df_top_bot <- df_evo %>%
      filter(Provincia %in% c(top5, bot5)) %>%
      group_by(Ano_Num, Provincia) %>%
      summarise(
        Fallecidos = sum(Total, na.rm = TRUE),
        Poblacion = sum(Poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa))
    
    colores_top <- c("#8B0000", "#B22222", "#CD5C5C", "#E53935", "#FF5252")
    colores_bot <- c("#FBC02D", "#FDD835", "#FFEE58", "#FFF176", "#FFF59D")
    
    vector_colores <- setNames(
      c(colores_top, colores_bot, "#000000"), 
      c(top5, bot5, "Tasa nacional")
    )
    
    p <- plot_ly()
    
    for (prov in top5) {
      sub_df <- df_top_bot %>% filter(Provincia == prov)
      p <- p %>% add_trace(data = sub_df, x = ~Ano_Num, y = ~Tasa, name = prov, type = "scatter", mode = "lines+markers", line = list(color = vector_colores[prov], width = 2), marker = list(color = vector_colores[prov], size = 6, line = list(color = "white", width = 1)), hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
    }
    
    for (prov in bot5) {
      sub_df <- df_top_bot %>% filter(Provincia == prov)
      p <- p %>% add_trace(data = sub_df, x = ~Ano_Num, y = ~Tasa, name = prov, type = "scatter", mode = "lines+markers", line = list(color = vector_colores[prov], width = 2), marker = list(color = vector_colores[prov], size = 6, line = list(color = "white", width = 1)), hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
    }
    
    p <- p %>% add_trace(
      data = df_nacional, x = ~Ano_Num, y = ~Tasa, name = "Tasa nacional",
      type = "scatter", mode = "lines+markers",
      line = list(color = "#000000", width = 3, dash = "dash"),
      marker = list(color = "#000000", size = 7, line = list(color = "white", width = 1.5)),
      hovertemplate = "<b>Tasa nacional</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>"
    )
    
    p %>% layout(
      xaxis = list(title = "Año", tickmode = "linear", dtick = 1),
      yaxis = list(title = "Tasa de la causa (por 100k hab.)"),
      margin = list(l = 50, r = 20, t = 10, b = 40),
      hoverlabel = list(bgcolor = "white"),
      legend = list(orientation = "v", x = 1.02, y = 1)
    )
  })
  
  output$p2_causas_mas_sexo <- renderPlotly({
    req(input$p2_ano)
    
    top_df <- causas_provinciales %>%
      filter(Año == input$p2_ano, Sexo %in% c("Hombres", "Mujeres")) %>%
      group_by(Sexo, Defunción) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>%
      group_by(Sexo) %>%
      slice_max(order_by = Tasa, n = 3, with_ties = FALSE) %>%
      ungroup()
    
    causas_sel <- unique(top_df$Defunción)
    
    df_plot <- causas_provinciales %>%
      filter(Año == input$p2_ano, Sexo %in% c("Hombres", "Mujeres"), Defunción %in% causas_sel) %>%
      group_by(Defunción, Sexo) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
             Defunción_Short = stringr::str_wrap(Defunción, width = 25)) %>%
      filter(is.finite(Tasa))
    
    plot_ly(
      data = df_plot,
      x = ~Tasa,
      y = ~Defunción_Short,
      color = ~Sexo,
      colors = c("Hombres" = "#00B2A9", "Mujeres" = "#FF6F61"),
      type = "bar",
      orientation = "h",
      marker = list(line = list(color = "white", width = 1)),
      hovertemplate = "<b>%{y}</b><br>%{fullData.name}: %{x:.1f} /100k<extra></extra>"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Tasa de la causa (por 100k hab.)"),
        yaxis = list(title = "", automargin = TRUE, categoryorder = "total ascending"),
        margin = list(l = 180, r = 20, t = 10, b = 40),
        hoverlabel = list(bgcolor = "white"),
        legend = list(orientation = "h", x = 0.2, y = 1.15)
      )
  })
  
  output$p2_causas_menos_sexo <- renderPlotly({
    req(input$p2_ano)
    
    bot_df <- causas_provinciales %>%
      filter(Año == input$p2_ano, Sexo %in% c("Hombres", "Mujeres")) %>%
      group_by(Sexo, Defunción) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa), Tasa > 0) %>%
      group_by(Sexo) %>%
      slice_min(order_by = Tasa, n = 3, with_ties = FALSE) %>%
      ungroup()
    
    causas_sel <- unique(bot_df$Defunción)
    
    df_plot <- causas_provinciales %>%
      filter(Año == input$p2_ano, Sexo %in% c("Hombres", "Mujeres"), Defunción %in% causas_sel) %>%
      group_by(Defunción, Sexo) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
             Defunción_Short = stringr::str_wrap(Defunción, width = 25)) %>%
      filter(is.finite(Tasa))
    
    plot_ly(
      data = df_plot,
      x = ~Tasa,
      y = ~Defunción_Short,
      color = ~Sexo,
      colors = c("Hombres" = "#00B2A9", "Mujeres" = "#FF6F61"),
      type = "bar",
      orientation = "h",
      marker = list(line = list(color = "white", width = 1)),
      hovertemplate = "<b>%{y}</b><br>%{fullData.name}: %{x:.1f} /100k<extra></extra>"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Tasa de la causa (por 100k hab.)"),
        yaxis = list(title = "", automargin = TRUE, categoryorder = "total descending"),
        margin = list(l = 180, r = 20, t = 10, b = 40),
        hoverlabel = list(bgcolor = "white"),
        legend = list(orientation = "h", x = 0.2, y = 1.15)
      )
  })
  
  observeEvent(input$p21_prov_a, {
    req(input$p21_prov_a)
    updateSelectInput(session, "p21_prov_a", label = paste0("Provincia: ", input$p21_prov_a))
  }, ignoreInit = TRUE)
  
  observeEvent(input$p21_prov_b, {
    req(input$p21_prov_b)
    updateSelectInput(session, "p21_prov_b", label = paste0("Provincia: ", input$p21_prov_b))
  }, ignoreInit = TRUE)
  
  output$p2_tabla_resumen <- DT::renderDT({
    df <- datos_p2() %>%
      transmute(
        Provincia = Provincia,
        Defunciones = Total_Fallecidos,
        Población = Poblacion_Total,
        `Tasa de la causa por 100.000 habitantes` = Tasa
      ) %>%
      arrange(desc(`Tasa de la causa por 100.000 habitantes`))
    
    DT::datatable(
      df,
      options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      DT::formatRound("Tasa de la causa por 100.000 habitantes", 2, dec.mark = ",", mark = ".")
  })
  
  # --- PESTAÑA 2.1 SERVER ---
  output$p21_evol_nacional <- renderPlotly({
    req(input$p21_prov_a, input$p21_prov_b, input$p21_defuncion)
    
    df_provs <- causas_provinciales %>% 
      filter(Defunción == input$p21_defuncion, Provincia %in% c(input$p21_prov_a, input$p21_prov_b)) %>%
      group_by(Año, Provincia) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
             Ano_Num = as.numeric(Año)) %>%
      filter(is.finite(Tasa))
    
    df_nac <- causas_provinciales %>% 
      filter(Defunción == input$p21_defuncion) %>%
      group_by(Año) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
             Provincia = "Tasa nacional",
             Ano_Num = as.numeric(Año)) %>%
      filter(is.finite(Tasa))
    
    p <- plot_ly()
    
    df_a <- df_provs %>% filter(Provincia == input$p21_prov_a)
    if(nrow(df_a) > 0) {
      p <- p %>% add_trace(data = df_a, x = ~Ano_Num, y = ~Tasa, name = input$p21_prov_a,
                           type = "scatter", mode = "lines+markers",
                           line = list(color = "#1f77b4", width = 3),
                           marker = list(color = "#1f77b4", size = 8, line = list(color = "white", width = 1.5)),
                           hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
    }
    
    df_b <- df_provs %>% filter(Provincia == input$p21_prov_b)
    if(nrow(df_b) > 0) {
      p <- p %>% add_trace(data = df_b, x = ~Ano_Num, y = ~Tasa, name = input$p21_prov_b,
                           type = "scatter", mode = "lines+markers",
                           line = list(color = "#ff7f0e", width = 3),
                           marker = list(color = "#ff7f0e", size = 8, line = list(color = "white", width = 1.5)),
                           hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
    }
    
    p <- p %>% add_trace(data = df_nac, x = ~Ano_Num, y = ~Tasa, name = "Tasa nacional",
                         type = "scatter", mode = "lines+markers",
                         line = list(color = "#2c3e50", width = 2.5, dash = "dash"),
                         marker = list(color = "#2c3e50", size = 6, line = list(color = "white", width = 1)),
                         hovertemplate = "<b>Tasa nacional</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
    
    p %>% layout(
      xaxis = list(title = "Año", tickmode = "linear", dtick = 1, showgrid = TRUE, gridcolor = "#E5E5E5"),
      yaxis = list(title = "Tasa de la causa (por 100k hab.)", showgrid = TRUE, gridcolor = "#E5E5E5"),
      hovermode = "x unified",
      hoverlabel = list(bgcolor = "white"),
      legend = list(orientation = "h", x = 0.2, y = 1.12),
      margin = list(l = 50, r = 20, t = 10, b = 40)
    )
  })
  
  color_prov_a <- "#1f77b4"
  color_prov_b <- "#ff7f0e"
  
  construir_radar_provincia <- function(prov_sel, ano_sel, tipo = c("top", "bottom"), color = "#1f77b4") {
    tipo <- match.arg(tipo)
    req(prov_sel, ano_sel)
    
    df <- causas_provinciales %>%
      filter(Provincia == prov_sel)
    
    if (ano_sel != "Todos los años") {
      df <- df %>% filter(Año == ano_sel)
    }
    
    # FIX: tasa ponderada por población (defunciones/población × 100k).
    # Antes se usaba la media simple de las tasas por sexo y año, que no
    # equivale a la tasa del periodo (los años/sexos pesan distinto).
    df <- df %>%
      group_by(Defunción) %>%
      summarise(
        Fallecidos = sum(Total, na.rm = TRUE),
        Poblacion = sum(Poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa), Tasa >= 0)
    
    df <- if (tipo == "top") {
      df %>% arrange(desc(Tasa)) %>% slice_head(n = 6)
    } else {
      df %>% filter(Tasa > 0) %>% arrange(Tasa) %>% slice_head(n = 6)
    }
    
    validate(need(nrow(df) >= 3, "No hay suficientes causas para construir el radar."))
    df$Defuncion_Short <- stringr::str_wrap(df$Defunción, width = 18)
    max_tasa <- max(df$Tasa, na.rm = TRUE)
    if (!is.finite(max_tasa) || max_tasa <= 0) max_tasa <- 1
    
    alpha_fill <- if (tipo == "top") "0.35" else "0.20"
    fill_rgba <- function(hex, alpha) {
      rgbv <- grDevices::col2rgb(hex)[,1]
      paste0("rgba(", paste(rgbv, collapse = ","), ", ", alpha, ")")
    }
    
    vals <- c(df$Tasa, df$Tasa[1])
    cats <- c(df$Defuncion_Short, df$Defuncion_Short[1])
    hover <- c(paste0(df$Defunción, "<br>Tasa: ", format(round(df$Tasa, 1), decimal.mark = ",")),
               paste0(df$Defunción[1], "<br>Tasa: ", format(round(df$Tasa[1], 1), decimal.mark = ",")))
    
    plot_ly(
      r = vals, theta = cats, text = hover,
      type = "scatterpolar", mode = "lines+markers",
      fill = "toself",
      fillcolor = fill_rgba(color, alpha_fill),
      line = list(color = color, width = 2),
      marker = list(color = color, size = 6),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, max_tasa * 1.1)),
          angularaxis = list(tickfont = list(size = 10))
        ),
        margin = list(l = 40, r = 40, t = 20, b = 20),
        showlegend = FALSE
      )
  }
  
  output$p21_radar_title_a <- renderUI({
    req(input$p21_prov_a)
    paste0("6 causas principales — ", input$p21_prov_a)
  })
  output$p21_radar_title_b <- renderUI({
    req(input$p21_prov_b)
    paste0("6 causas principales — ", input$p21_prov_b)
  })
  output$p21_radar_bottom_title_a <- renderUI({
    req(input$p21_prov_a)
    paste0("6 causas menos frecuentes — ", input$p21_prov_a)
  })
  output$p21_radar_bottom_title_b <- renderUI({
    req(input$p21_prov_b)
    paste0("6 causas menos frecuentes — ", input$p21_prov_b)
  })
  
  output$p21_radar_a <- renderPlotly({
    construir_radar_provincia(input$p21_prov_a, input$p21_ano, "top", color_prov_a)
  })
  output$p21_radar_b <- renderPlotly({
    construir_radar_provincia(input$p21_prov_b, input$p21_ano, "top", color_prov_b)
  })
  output$p21_radar_a_bot <- renderPlotly({
    construir_radar_provincia(input$p21_prov_a, input$p21_ano, "bottom", color_prov_a)
  })
  output$p21_radar_b_bot <- renderPlotly({
    construir_radar_provincia(input$p21_prov_b, input$p21_ano, "bottom", color_prov_b)
  })
  
  # --- PESTAÑA 3: SERVER ---
  p3_pagina_actual <- reactiveVal(1)
  
  observeEvent(c(input$p3_funcion, input$p3_ano, input$p3_sexo, input$p3_edad), {
    p3_pagina_actual(1)
  })
  
  datos_p3 <- reactive({
    req(input$p3_funcion, input$p3_ano, input$p3_edad)
    df <- copia_func %>% filter(Año == input$p3_ano, Funciones == input$p3_funcion)

    # FIX: filtro de sexo exacto siempre. copia_func trae filas pre-agregadas
    # "Ambos"; no filtrar mezclaba Ambos+Hombres+Mujeres en la media.
    df <- df %>% filter(Sexo == input$p3_sexo)
    if (input$p3_edad != "Todos los tramos") df <- df %>% filter(Edad == input$p3_edad)
    
    df %>%
      group_by(Provincia) %>%
      summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Valor))
  })
  
  output$p3_kpi_media <- renderText({
    df <- datos_p3()
    if (nrow(df) == 0) return("-")
    num_dec <- if (grepl("Riesgo", input$p3_funcion, ignore.case = TRUE)) 4 else 2
    paste0(format(round(mean(df$Valor, na.rm = TRUE), num_dec), big.mark = ".", decimal.mark = ","), " ", obtener_unidad(input$p3_funcion))
  })
  
  output$p3_kpi_max <- renderText({
    df <- datos_p3()
    if (nrow(df) == 0) return("-")
    num_dec <- if (grepl("Riesgo", input$p3_funcion, ignore.case = TRUE)) 4 else 2
    max_row <- df %>% arrange(desc(Valor)) %>% slice(1)
    paste0(max_row$Provincia, " (", format(round(max_row$Valor, num_dec), decimal.mark = ","), ")")
  })
  
  output$p3_kpi_min <- renderText({
    df <- datos_p3()
    if (nrow(df) == 0) return("-")
    num_dec <- if (grepl("Riesgo", input$p3_funcion, ignore.case = TRUE)) 4 else 2
    min_row <- df %>% arrange(Valor) %>% slice(1)
    paste0(min_row$Provincia, " (", format(round(min_row$Valor, num_dec), decimal.mark = ","), ")")
  })
  
  output$p3_mapa <- renderLeaflet({
    df <- datos_p3()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    
    val_max <- max(mapa_datos$Valor, na.rm = TRUE)
    val_min <- min(mapa_datos$Valor, na.rm = TRUE)
    
    dominio_colores <- if (is.na(val_max) || val_max == val_min) c(0, 1) else c(val_min, val_max)
    pal <- colorNumeric(palette = PAL_YLORRD, domain = dominio_colores, na.color = "#E0E0E0")
    
    num_dec <- if (grepl("Riesgo", input$p3_funcion, ignore.case = TRUE)) 4 else 2
    
    etiquetas <- sprintf("<strong>%s</strong><br/>Valor: %s %s", 
                         mapa_datos$NAME_2, 
                         format(round(mapa_datos$Valor, num_dec), decimal.mark = ","),
                         obtener_unidad(input$p3_funcion)) %>% 
      lapply(htmltools::HTML)
    
    leaflet(mapa_datos) %>% 
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>% 
      addPolygons(fillColor = ~pal(Valor), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio_colores, opacity = 0.8, 
                title = paste0(input$p3_funcion, "<br/>(", obtener_unidad(input$p3_funcion), ")"), 
                position = "bottomright")
  })
  
  output$p3_piramide <- renderPlotly({
    req(input$p3_ano, input$p3_funcion)
    
    df_pira <- copia_func %>% 
      filter(Año == input$p3_ano, 
             Funciones == input$p3_funcion,
             Sexo %in% c("Hombres", "Mujeres")) %>%
      filter(!grepl("95", Edad, ignore.case = TRUE) & !grepl("90 y más", Edad, ignore.case = TRUE)) %>%
      group_by(Edad, Sexo) %>% 
      summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop")
    
    df_hombres <- df_pira %>% filter(Sexo == "Hombres")
    df_mujeres <- df_pira %>% filter(Sexo == "Mujeres")
    
    lista_edades_filtradas <- setdiff(lista_edades, c("95 y más", "95 y mas", "90 y más", "90 y mas"))
    max_val <- max(df_pira$Valor, na.rm = TRUE) * 1.1
    
    plot_ly() %>%
      add_trace(
        data = df_hombres, 
        x = ~-Valor, 
        y = ~Edad, 
        type = "bar", 
        orientation = "h",
        name = "Hombres",
        marker = list(color = "#00B2A9", line = list(color = "white", width = 1)),
        customdata = ~Valor,
        hovertemplate = "<b>Edad: %{y}</b><br>Hombres: %{customdata:,.2f}<extra></extra>"
      ) %>%
      add_trace(
        data = df_mujeres, 
        x = ~Valor, 
        y = ~Edad, 
        type = "bar", 
        orientation = "h",
        name = "Mujeres",
        marker = list(color = "#FF6F61", line = list(color = "white", width = 1)),
        hovertemplate = "<b>Edad: %{y}</b><br>Mujeres: %{x:,.2f}<extra></extra>"
      ) %>%
      layout(
        barmode = "overlay",
        bargap = 0.1,
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(
          title = paste(input$p3_funcion, "(", obtener_unidad(input$p3_funcion), ")"),
          range = c(-max_val, max_val),
          tickmode = "array",
          tickvals = seq(-round(max_val), round(max_val), length.out = 7),
          ticktext = abs(round(seq(-round(max_val), round(max_val), length.out = 7)))
        ),
        yaxis = list(
          title = "Tramo de Edad", 
          categoryorder = "array", 
          categoryarray = lista_edades_filtradas
        ),
        legend = list(orientation = "h", x = 0.35, y = 1.1),
        margin = list(l = 60, r = 20, t = 20, b = 40)
      )
  })
  
  ranking_p3_data <- reactive({
    datos_p3() %>% 
      arrange(desc(Valor)) %>% 
      mutate(Posicion = row_number())
  })
  
  p3_total_paginas <- reactive({
    ceiling(nrow(ranking_p3_data()) / 13)
  })
  
  observeEvent(input$p3_prev_page, {
    if (p3_pagina_actual() > 1) {
      p3_pagina_actual(p3_pagina_actual() - 1)
    }
  })
  
  observeEvent(input$p3_next_page, {
    if (p3_pagina_actual() < p3_total_paginas()) {
      p3_pagina_actual(p3_pagina_actual() + 1)
    }
  })
  
  output$p3_page_info <- renderUI({
    span(paste("Página", p3_pagina_actual(), "de", max(1, p3_total_paginas())), class = "fw-bold px-2")
  })
  
  output$p3_tabla_ranking <- renderTable({
    df <- ranking_p3_data()
    if (nrow(df) == 0) return(NULL)
    
    inicio <- (p3_pagina_actual() - 1) * 13 + 1
    fin <- min(p3_pagina_actual() * 13, nrow(df))
    
    df %>% 
      slice(inicio:fin) %>% 
      select(Posicion, Provincia, Valor)
  }, striped = TRUE, hover = TRUE)
  
  # --- PESTAÑA 3.1 SERVER ---
  output$p31_evol_demografica <- renderPlotly({
    req(input$p31_prov_a, input$p31_prov_b, input$p31_funcion)
    
    df_func_sub <- copia_func %>% filter(Funciones == input$p31_funcion)

    # FIX: filtro de sexo exacto siempre (ver datos_p3: existen filas "Ambos"
    # pre-agregadas y no filtrar las mezclaba con Hombres/Mujeres).
    df_func_sub <- df_func_sub %>% filter(Sexo == input$p31_sexo)
    
    df_provs <- df_func_sub %>% 
      filter(Provincia %in% c(input$p31_prov_a, input$p31_prov_b)) %>%
      group_by(Año, Provincia) %>% 
      summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop") %>% 
      mutate(Ano_Num = as.numeric(as.character(Año)))
    
    df_nac <- df_func_sub %>% 
      group_by(Año) %>%
      summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop") %>%
      mutate(Provincia = "Media entre provincias", Ano_Num = as.numeric(as.character(Año)))
    
    p <- plot_ly()
    
    df_a <- df_provs %>% filter(Provincia == input$p31_prov_a)
    if(nrow(df_a) > 0) {
      p <- p %>% add_trace(data = df_a, x = ~Ano_Num, y = ~Valor, name = input$p31_prov_a,
                           type = "scatter", mode = "lines+markers",
                           line = list(color = "#1f77b4", width = 3),
                           marker = list(color = "#1f77b4", size = 8, line = list(color = "white", width = 1.5)),
                           hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Valor: %{y:,.2f}<extra></extra>")
    }
    
    df_b <- df_provs %>% filter(Provincia == input$p31_prov_b)
    if(nrow(df_b) > 0) {
      p <- p %>% add_trace(data = df_b, x = ~Ano_Num, y = ~Valor, name = input$p31_prov_b,
                           type = "scatter", mode = "lines+markers",
                           line = list(color = "#ff7f0e", width = 3),
                           marker = list(color = "#ff7f0e", size = 8, line = list(color = "white", width = 1.5)),
                           hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Valor: %{y:,.2f}<extra></extra>")
    }
    
    p <- p %>% add_trace(data = df_nac, x = ~Ano_Num, y = ~Valor, name = "Media entre provincias",
                         type = "scatter", mode = "lines+markers",
                         line = list(color = "#2c3e50", width = 2.5, dash = "dash"),
                         marker = list(color = "#2c3e50", size = 6, line = list(color = "white", width = 1)),
                         hovertemplate = "<b>Media entre provincias</b><br>Año: %{x}<br>Valor: %{y:,.2f}<extra></extra>")
    
    p %>% layout(
      xaxis = list(title = "Año", tickmode = "linear", dtick = 1, showgrid = TRUE, gridcolor = "#E5E5E5"),
      yaxis = list(title = paste(input$p31_funcion, "(", obtener_unidad(input$p31_funcion), ")"), showgrid = TRUE, gridcolor = "#E5E5E5"),
      hovermode = "x unified",
      hoverlabel = list(bgcolor = "white"),
      legend = list(orientation = "h", x = 0.2, y = 1.12),
      margin = list(l = 50, r = 20, t = 10, b = 40)
    )
  })
  
  output$p31_piramide_comparativa <- renderPlotly({
    req(input$p31_prov_a, input$p31_prov_b)
    
    # La pirámide compara las dos provincias por tramo de edad.
    # Se utiliza el sexo seleccionado; con "Ambos" se muestran los totales.
    sexo_pira <- if (is.null(input$p31_sexo) || is.na(input$p31_sexo)) "Ambos" else input$p31_sexo
    
    df_pira <- copia_func %>%
      filter(Provincia %in% c(input$p31_prov_a, input$p31_prov_b)) %>%
      filter(!grepl("^(90|95) y (más|mas)$|^(90|95) años y (más|mas)$", Edad, ignore.case = TRUE)) %>%
      { if (sexo_pira %in% c("Hombres", "Mujeres", "Ambos")) filter(., Sexo == sexo_pira) else . } %>%
      group_by(Provincia, Edad) %>%
      summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Valor))
    
    validate(need(nrow(df_pira) > 0, "No hay datos suficientes para construir la pirámide."))
    
    edades <- intersect(lista_edades, unique(df_pira$Edad))
    df_pira$Edad <- factor(df_pira$Edad, levels = edades)
    
    df_a <- df_pira %>% filter(Provincia == input$p31_prov_a)
    df_b <- df_pira %>% filter(Provincia == input$p31_prov_b)
    max_val <- max(c(df_a$Valor, df_b$Valor), na.rm = TRUE)
    if (!is.finite(max_val) || max_val <= 0) max_val <- 1
    
    p <- plot_ly()
    
    if (nrow(df_a) > 0) {
      p <- p %>% add_trace(
        data = df_a, x = ~-Valor, y = ~Edad, type = "bar", orientation = "h",
        name = input$p31_prov_a,
        marker = list(color = "#1f77b4", line = list(color = "white", width = 1)),
        customdata = ~Valor,
        hovertemplate = paste0("<b>", htmltools::htmlEscape(input$p31_prov_a), "</b><br>Edad: %{y}<br>Valor: %{customdata:,.2f}<extra></extra>")
      )
    }
    if (nrow(df_b) > 0) {
      p <- p %>% add_trace(
        data = df_b, x = ~Valor, y = ~Edad, type = "bar", orientation = "h",
        name = input$p31_prov_b,
        marker = list(color = "#ff7f0e", line = list(color = "white", width = 1)),
        customdata = ~Valor,
        hovertemplate = paste0("<b>", htmltools::htmlEscape(input$p31_prov_b), "</b><br>Edad: %{y}<br>Valor: %{customdata:,.2f}<extra></extra>")
      )
    }
    
    tick_span <- pretty(c(-max_val, max_val), n = 7)
    tick_span <- tick_span[tick_span >= -max_val & tick_span <= max_val]
    p %>% layout(
      barmode = "overlay",
      bargap = 0.08,
      xaxis = list(
        title = obtener_unidad(input$p31_funcion),
        range = c(-max_val * 1.1, max_val * 1.1),
        tickmode = "array",
        tickvals = sort(unique(c(-rev(abs(tick_span)), abs(tick_span)))),
        ticktext = format(abs(sort(unique(c(-rev(abs(tick_span)), abs(tick_span))))), big.mark = ".", decimal.mark = ",", trim = TRUE)
      ),
      yaxis = list(title = "Tramo de Edad", categoryorder = "array", categoryarray = edades),
      legend = list(orientation = "h", x = 0.28, y = 1.08),
      margin = list(l = 70, r = 30, t = 35, b = 55)
    )
  })
  
  output$p31_poblacion_titulo_a <- renderText({
    req(input$p31_prov_a, input$p31_ano_radar)
    paste0("Población total — ", input$p31_prov_a)
  })
  
  output$p31_poblacion_titulo_b <- renderText({
    req(input$p31_prov_b, input$p31_ano_radar)
    paste0("Población total — ", input$p31_prov_b)
  })
  
  poblacion_provincia_total <- function(provincia, anio) {
    val <- copia_p %>%
      filter(Provincia == provincia, Año == as.character(anio), Sexo == "Ambos") %>%
      summarise(Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      pull(Poblacion)
    if (!length(val) || !is.finite(val)) return(NA_real_)
    val
  }
  
  output$p31_poblacion_a <- renderText({
    req(input$p31_prov_a, input$p31_ano_radar)
    val <- poblacion_provincia_total(input$p31_prov_a, input$p31_ano_radar)
    if (!is.finite(val)) return("Sin datos")
    format(round(val), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  
  output$p31_poblacion_b <- renderText({
    req(input$p31_prov_b, input$p31_ano_radar)
    val <- poblacion_provincia_total(input$p31_prov_b, input$p31_ano_radar)
    if (!is.finite(val)) return("Sin datos")
    format(round(val), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })

  # --- PIRÁMIDE DE POBLACIÓN SERVER ---
  datos_demp <- reactive({
    req(!is.null(padron_edad), input$demp_prov, input$demp_ano)
    df <- padron_edad %>% filter(Año == as.character(input$demp_ano))
    if (input$demp_prov != "Todas") df <- df %>% filter(Provincia == input$demp_prov)
    df %>%
      mutate(Q5 = ifelse(Edad_num >= 95, "95 y más",
                         paste0("De ", 5 * (Edad_num %/% 5), " a ", 5 * (Edad_num %/% 5) + 4))) %>%
      group_by(Q5, Sexo) %>%
      summarise(Pob = sum(Pob, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Pob))
  })

  output$demp_piramide <- renderPlotly({
    df <- datos_demp()
    req(nrow(df) > 0)
    orden_q5 <- df %>% distinct(Q5) %>%
      mutate(ini = suppressWarnings(as.numeric(gsub("[^0-9].*", "", Q5))),
             ini = ifelse(grepl("^95", Q5), 95, ini)) %>%
      arrange(ini) %>% pull(Q5)
    df$Q5 <- factor(df$Q5, levels = orden_q5)
    df_h <- df %>% filter(Sexo == "Hombres")
    df_m <- df %>% filter(Sexo == "Mujeres")
    max_v <- max(df$Pob, na.rm = TRUE) * 1.08
    if (!is.finite(max_v) || max_v <= 0) return(plotly_empty())
    ticks <- seq(-round(max_v), round(max_v), length.out = 7)
    plot_ly() %>%
      add_trace(data = df_h, x = ~-Pob, y = ~Q5, type = "bar", orientation = "h",
                name = "Hombres", marker = list(color = "#00B2A9", line = list(color = "white", width = 1)),
                customdata = ~Pob,
                hovertemplate = "<b>%{y}</b><br>Hombres: %{customdata:,.0f}<extra></extra>") %>%
      add_trace(data = df_m, x = ~Pob, y = ~Q5, type = "bar", orientation = "h",
                name = "Mujeres", marker = list(color = "#FF6F61", line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{y}</b><br>Mujeres: %{x:,.0f}<extra></extra>") %>%
      layout(barmode = "overlay", bargap = 0.08,
             hoverlabel = list(bgcolor = "white"),
             xaxis = list(title = "Población",
                          range = c(-max_v, max_v), tickmode = "array",
                          tickvals = ticks,
                          ticktext = format(abs(round(ticks)), big.mark = ".", decimal.mark = ",", trim = TRUE)),
             yaxis = list(title = "Tramo de edad", categoryorder = "array", categoryarray = orden_q5),
             legend = list(orientation = "h", x = 0.35, y = 1.05),
             margin = list(l = 90, r = 30, t = 30, b = 60))
  })

  # --- ENVEJECIMIENTO Y DEPENDENCIA SERVER ---
  datos_deme <- reactive({
    req(!is.null(padron_edad), input$deme_ano)
    # FIX: no reutilizar el nombre Pob dentro del mismo summarise: desde dplyr
    # 1.0 las expresiones se evalúan en secuencia y Pob[...] usaría el escalar
    # ya agregado (daba P65 = 0 en todas las provincias).
    padron_edad %>%
      filter(Año == as.character(input$deme_ano)) %>%
      group_by(Provincia) %>%
      summarise(Pob_total = sum(Pob, na.rm = TRUE),
                P65 = sum(Pob[Edad_num >= 65], na.rm = TRUE),
                P015 = sum(Pob[Edad_num <= 15], na.rm = TRUE),
                P1664 = sum(Pob[Edad_num >= 16 & Edad_num <= 64], na.rm = TRUE),
                .groups = "drop") %>%
      transmute(Provincia = Provincia,
                Pob = Pob_total, P65 = P65, P015 = P015, P1664 = P1664,
                Pct65 = P65 / Pob_total * 100,
                IndEnv = if_else(P015 > 0, P65 / P015 * 100, NA_real_),
                Dep = if_else(P1664 > 0, (P015 + P65) / P1664 * 100, NA_real_)) %>%
      filter(is.finite(Pct65))
  })

  output$deme_kpi_65 <- renderText({
    df <- datos_deme()
    req(nrow(df) > 0)
    p <- sum(df$P65, na.rm = TRUE) / sum(df$Pob, na.rm = TRUE) * 100
    paste0(format(round(p, 1), decimal.mark = ","), " %")
  })

  output$deme_kpi_env <- renderText({
    df <- datos_deme()
    req(nrow(df) > 0)
    v <- sum(df$P65, na.rm = TRUE) / sum(df$P015, na.rm = TRUE) * 100
    paste0(format(round(v, 0), decimal.mark = ","), " mayores por cada 100 jóvenes")
  })

  output$deme_kpi_dep <- renderText({
    df <- datos_deme()
    req(nrow(df) > 0)
    v <- sum(df$P015 + df$P65, na.rm = TRUE) / sum(df$P1664, na.rm = TRUE) * 100
    paste0(format(round(v, 1), decimal.mark = ","), " %")
  })

  output$deme_mapa <- renderLeaflet({
    df <- datos_deme()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    pal <- colorNumeric(palette = PAL_YLORRD, domain = c(0, max(mapa_datos$Pct65, na.rm = TRUE)), na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>%% de 65+: %s %%", mapa_datos$NAME_2,
                         format(round(mapa_datos$Pct65, 1), decimal.mark = ",")) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Pct65), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = ~Pct65, opacity = 0.8, title = "% de 65+", position = "bottomright")
  })

  output$deme_ranking <- renderPlotly({
    df <- datos_deme()
    req(nrow(df) > 0)
    df <- df %>% arrange(Pct65) %>% mutate(Provincia = factor(Provincia, levels = Provincia))
    dom <- dominio_seguro(df$Pct65)
    plot_ly(df, x = ~Pct65, y = ~Provincia, type = "bar", orientation = "h",
            marker = list(color = ~Pct65, cmin = dom[1], cmax = dom[2],
                          colorscale = escala_plotly(PAL_YLORRD), showscale = FALSE,
                          line = list(color = "white", width = 1)),
            text = ~format(round(Pct65, 1), decimal.mark = ","),
            textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>% de 65+: %{x:.1f} %<extra></extra>") %>%
      layout(xaxis = list(title = "% de población con 65 años o más"),
             yaxis = list(title = "", automargin = TRUE),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 110, r = 60, t = 20, b = 50))
  })

  output$deme_tabla <- DT::renderDT({
    df <- datos_deme() %>%
      transmute(Provincia = Provincia,
                `% de 65+` = Pct65,
                `Índice de envejecimiento` = IndEnv,
                `Tasa de dependencia` = Dep) %>%
      arrange(desc(`% de 65+`))
    DT::datatable(df, options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound(c("% de 65+", "Índice de envejecimiento", "Tasa de dependencia"), 1, dec.mark = ",", mark = ".")
  })

  # --- BRECHA DE GÉNERO SERVER (indicador a la edad 0) ---
  datos_demb <- reactive({
    req(input$demb_ano, input$demb_func)
    df <- copia_func %>%
      filter(Año == as.character(input$demb_ano), Funciones == input$demb_func,
             Edad == "0", Sexo %in% c("Hombres", "Mujeres")) %>%
      group_by(Provincia, Sexo) %>%
      summarise(V = mean(Total, na.rm = TRUE), .groups = "drop")
    wide <- df %>% tidyr::pivot_wider(names_from = Sexo, values_from = V)
    req(all(c("Hombres", "Mujeres") %in% names(wide)))
    wide %>%
      transmute(Provincia = as.character(Provincia),
                Hombres = Hombres, Mujeres = Mujeres,
                Brecha = Hombres - Mujeres) %>%
      filter(is.finite(Brecha))
  })

  output$demb_mapa <- renderLeaflet({
    df <- datos_demb()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    maxabs <- max(abs(mapa_datos$Brecha), na.rm = TRUE)
    if (!is.finite(maxabs) || maxabs <= 0) maxabs <- 1
    pal <- colorNumeric(palette = "RdBu", domain = c(-maxabs, maxabs), reverse = TRUE)
    unidad <- obtener_unidad(input$demb_func)
    etiquetas <- sprintf("<strong>%s</strong><br/>Brecha H-M: %s %s", mapa_datos$NAME_2,
                         format(round(mapa_datos$Brecha, 2), decimal.mark = ","),
                         unidad) %>% lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Brecha), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = ~Brecha, opacity = 0.8, title = paste0("Brecha H-M (", unidad, ")"), position = "bottomright")
  })

  output$demb_ranking <- renderPlotly({
    df <- datos_demb()
    req(nrow(df) > 0)
    df <- df %>% arrange(Brecha) %>% mutate(Provincia = factor(Provincia, levels = Provincia))
    maxabs <- max(abs(df$Brecha), na.rm = TRUE)
    if (!is.finite(maxabs) || maxabs <= 0) maxabs <- 1
    plot_ly(df, x = ~Brecha, y = ~Provincia, type = "bar", orientation = "h",
            marker = list(color = ~Brecha, cmin = -maxabs, cmax = maxabs,
                          colorscale = escala_plotly(PAL_RDBU_REV), showscale = FALSE,
                          line = list(color = "white", width = 1)),
            hovertemplate = "<b>%{y}</b><br>Brecha H-M: %{x:+.2f}<extra></extra>") %>%
      layout(xaxis = list(title = paste0("Brecha Hombres − Mujeres (", obtener_unidad(input$demb_func), ")"),
                          zeroline = FALSE),
             yaxis = list(title = "", automargin = TRUE),
             shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1,
                                yref = "paper", line = list(color = "#2c3e50", width = 1.5))),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 110, r = 60, t = 20, b = 50))
  })

  output$demb_tabla <- DT::renderDT({
    df <- datos_demb() %>%
      transmute(Provincia = Provincia, Hombres = Hombres, Mujeres = Mujeres, `Brecha H-M` = Brecha) %>%
      arrange(`Brecha H-M`)
    DT::datatable(df, options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound(c("Hombres", "Mujeres", "Brecha H-M"), 2, dec.mark = ",", mark = ".")
  })

  # --- EVOLUCIÓN DEMOGRÁFICA POR COMUNIDAD SERVER ---
  datos_demv <- reactive({
    req(input$demv_func, input$demv_sexo)
    copia_func %>%
      filter(Funciones == input$demv_func, Sexo == input$demv_sexo) %>%
      mutate(Comunidad = prov_a_comunidad(Provincia)) %>%
      filter(Comunidad != "Sin asignar") %>%
      group_by(Comunidad, Año) %>%
      summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop") %>%
      mutate(Ano_Num = suppressWarnings(as.numeric(Año))) %>%
      filter(is.finite(Valor), is.finite(Ano_Num))
  })

  output$demv_evol <- renderPlotly({
    df <- datos_demv() %>% arrange(Ano_Num, Comunidad)
    req(nrow(df) > 0)
    media <- df %>%
      group_by(Ano_Num) %>%
      summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(Comunidad = "Media entre provincias")
    nccaa <- dplyr::n_distinct(df$Comunidad)
    pal_demv <- grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                              "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(max(nccaa, 1))
    plot_ly() %>%
      add_trace(data = df, x = ~Ano_Num, y = ~Valor, color = ~Comunidad, colors = pal_demv,
                type = "scatter", mode = "lines+markers",
                marker = list(line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Valor: %{y:.2f}<extra></extra>") %>%
      add_trace(data = media, x = ~Ano_Num, y = ~Valor, name = "Media entre provincias",
                type = "scatter", mode = "lines+markers",
                line = list(color = "#2c3e50", width = 3, dash = "dash"),
                marker = list(color = "#2c3e50", size = 6, line = list(color = "white", width = 1)),
                hovertemplate = "<b>Media entre provincias</b><br>Año: %{x}<br>Valor: %{y:.2f}<extra></extra>") %>%
      layout(xaxis = list(title = "Año", dtick = 1),
             yaxis = list(title = paste0(input$demv_func, " (", obtener_unidad(input$demv_func), ")")),
             legend = list(orientation = "h", x = 0, y = -0.25),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 10, b = 100))
  })

  output$demv_tabla <- DT::renderDT({
    df <- datos_demv() %>%
      transmute(Comunidad = Comunidad, Año = Ano_Num, Valor = Valor) %>%
      arrange(Comunidad, Año)
    DT::datatable(df, options = list(pageLength = 19, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound("Valor", 2, dec.mark = ",", mark = ".")
  })
  
  # --- ESPERANZA DE VIDA SERVER ---
  datos_ev <- reactive({
    req(input$ev_ano, input$ev_edad, input$ev_sexo)
    funciones_provinciales %>%
      filter(Funciones == "Esperanza de vida", Año == as.character(input$ev_ano),
             Edad == input$ev_edad, Sexo == input$ev_sexo) %>%
      filter(is.finite(Valor)) %>%
      transmute(Provincia = as.character(Provincia), Valor = Valor)
  }) %>% bindCache(input$ev_ano, input$ev_edad, input$ev_sexo)

  output$ev_kpi_media <- renderText({
    df <- datos_ev()
    req(nrow(df) > 0)
    a <- as.character(input$ev_ano)
    m <- df %>%
      left_join(pob_prov %>% filter(Año == a), by = "Provincia") %>%
      summarise(m = sum(Valor * Pob, na.rm = TRUE) / sum(Pob, na.rm = TRUE)) %>%
      pull(m)
    if (!is.finite(m)) m <- mean(df$Valor, na.rm = TRUE)
    paste0(format(round(m, 1), decimal.mark = ","), " años")
  })

  output$ev_kpi_max <- renderText({
    df <- datos_ev()
    req(nrow(df) > 0)
    x <- df %>% arrange(desc(Valor)) %>% slice(1)
    paste0(x$Provincia, " (", format(round(x$Valor, 1), decimal.mark = ","), ")")
  })

  output$ev_kpi_min <- renderText({
    df <- datos_ev()
    req(nrow(df) > 0)
    x <- df %>% arrange(Valor) %>% slice(1)
    paste0(x$Provincia, " (", format(round(x$Valor, 1), decimal.mark = ","), ")")
  })

  output$ev_mapa <- renderLeaflet({
    df <- datos_ev()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    dominio <- dominio_seguro(mapa_datos$Valor)
    pal <- colorNumeric(palette = PAL_YLGNBU, domain = dominio, na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>%s",
                         mapa_datos$NAME_2,
                         ifelse(is.finite(mapa_datos$Valor),
                                paste0("Esperanza de vida: ",
                                       format(round(mapa_datos$Valor, 1), big.mark = ".", decimal.mark = ","),
                                       " años"),
                                "sin dato")) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Valor), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio, opacity = 0.8, title = "Años de vida", position = "bottomright")
  })

  output$ev_ranking <- renderPlotly({
    df <- datos_ev()
    req(nrow(df) > 0)
    df <- df %>% arrange(Valor) %>% mutate(Provincia = factor(Provincia, levels = Provincia))
    dom <- dominio_seguro(df$Valor)
    plot_ly(df, x = ~Valor, y = ~Provincia, type = "bar", orientation = "h",
            marker = list(color = ~Valor, cmin = dom[1], cmax = dom[2],
                          colorscale = escala_plotly(PAL_YLGNBU), showscale = FALSE,
                          line = list(color = "white", width = 1)),
            text = ~format(round(Valor, 1), decimal.mark = ","),
            textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Esperanza de vida: %{x:.1f} años<extra></extra>") %>%
      layout(xaxis = list(title = "Años de vida"),
             yaxis = list(title = "", automargin = TRUE),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 140, r = 60, t = 20, b = 50))
  })

  datos_ev_evol <- reactive({
    req(input$ev_edad, input$ev_sexo)
    funciones_provinciales %>%
      filter(Funciones == "Esperanza de vida", Edad == input$ev_edad, Sexo == input$ev_sexo) %>%
      mutate(Comunidad = prov_a_comunidad(Provincia)) %>%
      filter(Comunidad != "Sin asignar") %>%
      group_by(Comunidad, Año) %>%
      summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(Ano_Num = suppressWarnings(as.numeric(Año))) %>%
      filter(is.finite(Valor), is.finite(Ano_Num))
  }) %>% bindCache(input$ev_edad, input$ev_sexo)

  output$ev_evol <- renderPlotly({
    df <- datos_ev_evol() %>% arrange(Ano_Num, Comunidad)
    req(nrow(df) > 0)
    media <- df %>%
      group_by(Ano_Num) %>%
      summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(Comunidad = "Media entre provincias")
    ncolores <- dplyr::n_distinct(df$Comunidad)
    plot_ly() %>%
      add_trace(data = df, x = ~Ano_Num, y = ~Valor, color = ~Comunidad,
                colors = grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                                      "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(max(ncolores, 1)),
                type = "scatter", mode = "lines+markers",
                marker = list(line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Valor: %{y:.1f}<extra></extra>") %>%
      add_trace(data = media, x = ~Ano_Num, y = ~Valor, name = "Media entre provincias",
                type = "scatter", mode = "lines+markers",
                line = list(color = "#2c3e50", width = 3, dash = "dash"),
                marker = list(color = "#2c3e50", size = 6, line = list(color = "white", width = 1)),
                hovertemplate = "<b>Media entre provincias</b><br>Año: %{x}<br>Valor: %{y:.1f}<extra></extra>") %>%
      layout(xaxis = list(title = "Año", dtick = 1),
             yaxis = list(title = "Años de vida"),
             legend = list(orientation = "h", x = 0, y = -0.25),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 10, b = 100))
  })

  output$ev_tabla <- DT::renderDT({
    df <- datos_ev() %>%
      transmute(Provincia = Provincia, `Esperanza de vida (años)` = round(Valor, 1)) %>%
      arrange(desc(`Esperanza de vida (años)`))
    DT::datatable(df, options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound("Esperanza de vida (años)", 1, dec.mark = ",", mark = ".")
  })

  # --- PESTAÑA 4: EUROPA SERVER ---
  observe({
    req(nrow(europa_agrupada) > 0)
    
    anios_eur <- sort(unique(europa_agrupada$anio), decreasing = TRUE)
    causas_eur <- causas_europa_raw
    sexos_eur <- sexos_europa_raw
    paises_eur <- paises_europa_raw
    
    updateSelectInput(
      session, "eur_anio",
      choices = as.character(anios_eur),
      selected = if (length(anios_eur)) as.character(anios_eur[1]) else NULL
    )
    updateSelectInput(
      session, "eur_causa",
      choices = causas_europa_es,
      selected = if (length(causas_eur)) causas_eur[1] else character(0)
    )
    updateRadioButtons(
      session, "eur_sexo",
      choices = sexos_europa_es,
      selected = if ("Ambos" %in% sexos_eur) "Ambos" else sexos_eur[1]
    )
    
    updateSelectInput(
      session, "eur_comp_anio",
      choices = as.character(anios_eur),
      selected = if (length(anios_eur)) as.character(anios_eur[1]) else NULL
    )
    updateSelectInput(
      session, "eur_comp_causa",
      choices = causas_europa_es,
      selected = if (length(causas_eur)) causas_eur[1] else character(0)
    )
    updateRadioButtons(
      session, "eur_comp_sexo",
      choices = sexos_europa_es,
      selected = if ("Ambos" %in% sexos_eur) "Ambos" else sexos_eur[1]
    )
    pais_a_inicial <- if ("Spain" %in% paises_eur) "Spain" else paises_eur[1]
    pais_b_inicial <- if ("France" %in% paises_eur) "France" else if (length(paises_eur) > 1) paises_eur[2] else paises_eur[1]
    
    updateSelectInput(
      session, "eur_comp_pais_a",
      label = paste0("País: ", traducir_pais(pais_a_inicial)),
      choices = paises_europa_es,
      selected = pais_a_inicial
    )
    updateSelectInput(
      session, "eur_comp_pais_b",
      label = paste0("País: ", traducir_pais(pais_b_inicial)),
      choices = paises_europa_es,
      selected = pais_b_inicial
    )
  })
  
  observeEvent(input$eur_comp_pais_a, {
    req(input$eur_comp_pais_a)
    updateSelectInput(session, "eur_comp_pais_a",
                      label = paste0("País: ", traducir_pais(input$eur_comp_pais_a)))
  }, ignoreInit = TRUE)
  
  observeEvent(input$eur_comp_pais_b, {
    req(input$eur_comp_pais_b)
    updateSelectInput(session, "eur_comp_pais_b",
                      label = paste0("País: ", traducir_pais(input$eur_comp_pais_b)))
  }, ignoreInit = TRUE)
  
  datos_europa_filtrados <- reactive({
    req(input$eur_causa, input$eur_sexo, input$eur_anio)
    
    europa_agrupada %>%
      filter(
        causa_grupo == input$eur_causa,
        sexo == input$eur_sexo,
        anio == as.numeric(input$eur_anio)
      )
  }) %>% bindCache(input$eur_causa, input$eur_sexo, input$eur_anio)
  
  output$eur_filtro_info <- renderUI({
    req(input$eur_anio, input$eur_causa, input$eur_sexo)
    anio_visible <- input$eur_anio
    div(
      class = "filter-help",
      HTML(paste0(
        "<b>Selección:</b> ", htmltools::htmlEscape(input$eur_sexo),
        " · ", htmltools::htmlEscape(input$eur_causa),
        " · ", htmltools::htmlEscape(anio_visible),
        ". Las categorías agregadas priorizan la categoría madre de Eurostat cuando está disponible para evitar doble conteo."
      ))
    )
  })
  
  output$eur_kpi_total <- renderText({
    total <- sum(datos_europa_filtrados()$defunciones, na.rm = TRUE)
    if (!is.finite(total)) return("-")
    format(round(total), big.mark = ".", decimal.mark = ",")
  })
  
  output$eur_kpi_max <- renderText({
    top_pais <- datos_europa_filtrados() %>%
      group_by(pais) %>%
      summarise(tasa = sum(defunciones, na.rm = TRUE) / sum(poblacion, na.rm = TRUE) * 100000,
                .groups = "drop") %>%
      filter(is.finite(tasa)) %>%
      slice_max(tasa, n = 1, with_ties = FALSE)
    if (nrow(top_pais)) paste0(traducir_pais(top_pais$pais), " (", format(round(top_pais$tasa, 1), decimal.mark = ","), ")") else "Sin datos"
  })
  
  output$eur_kpi_sexo <- renderText({ input$eur_sexo })
  
  output$eur_plot_barras <- renderPlotly({
    req(input$eur_causa, input$eur_sexo, input$eur_anio)
    
    df_base <- europa_agrupada %>%
      filter(causa_grupo == input$eur_causa, sexo == input$eur_sexo) %>%
      group_by(anio, pais) %>%
      summarise(
        defunciones = sum(defunciones, na.rm = TRUE),
        poblacion = max(poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(is.finite(defunciones), is.finite(poblacion), poblacion > 0) %>%
      mutate(tasa_100k = defunciones / poblacion * 100000)
    
    validate(need(nrow(df_base) > 0, "No hay datos de población para la selección actual."))
    
    anio_ref <- as.numeric(input$eur_anio)
    
    ranking_ref <- df_base %>%
      filter(anio == anio_ref) %>%
      arrange(desc(tasa_100k))
    
    validate(need(nrow(ranking_ref) > 0, "No hay países con población y defunciones para el año seleccionado."))
    
    n_grupo <- min(5, floor(nrow(ranking_ref) / 2))
    validate(need(n_grupo >= 1, "No hay suficientes países para construir la comparación."))
    
    top5 <- ranking_ref %>% slice_head(n = n_grupo) %>% mutate(grupo = "5 países más afectados")
    bottom5 <- ranking_ref %>% slice_tail(n = n_grupo) %>% mutate(grupo = "5 países menos afectados")
    seleccion <- bind_rows(top5, bottom5)
    
    df_evol <- df_base %>%
      filter(pais %in% seleccion$pais) %>%
      mutate(
        grupo = if_else(pais %in% top5$pais, "5 países más afectados", "5 países menos afectados"),
        pais_es = traducir_pais(pais)
      ) %>%
      left_join(seleccion %>% select(pais, tasa_ref = tasa_100k), by = "pais") %>%
      mutate(
        tooltip = paste0(
          "<b>", pais_es, "</b>",
          "<br>Año: ", anio,
          "<br>Tasa: ", format(round(tasa_100k, 1), decimal.mark = ","), " por 100.000 hab.",
          "<br>Defunciones: ", format(round(defunciones), big.mark = ".", decimal.mark = ","),
          "<br>Población: ", format(round(poblacion), big.mark = ".", decimal.mark = ",")
        )
      )
    
    # Media europea ponderada por población: defunciones totales / población total.
    media_europea <- df_base %>%
      group_by(anio) %>%
      summarise(
        defunciones = sum(defunciones, na.rm = TRUE),
        poblacion = sum(poblacion, na.rm = TRUE),
        tasa_100k = if_else(poblacion > 0, defunciones / poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      filter(is.finite(tasa_100k)) %>%
      mutate(
        pais_es = "Media europea",
        grupo = "Media europea",
        tooltip = paste0(
          "<b>Media europea</b>",
          "<br>Año: ", anio,
          "<br>Tasa: ", format(round(tasa_100k, 1), decimal.mark = ","), " por 100.000 hab."
        )
      )
    
    colores_base <- grDevices::colorRampPalette(c("#440154", "#31688E", "#35B779", "#FDE725"))(256)
    val_min <- min(seleccion$tasa_100k, na.rm = TRUE)
    val_max <- max(seleccion$tasa_100k, na.rm = TRUE)
    if (!is.finite(val_min) || !is.finite(val_max) || val_min == val_max) {
      val_min <- 0
      val_max <- max(1, val_max)
    }
    idx <- floor((seleccion$tasa_100k - val_min) / (val_max - val_min) * 255) + 1
    idx <- pmax(1, pmin(256, idx))
    colores_paises <- setNames(colores_base[idx], traducir_pais(seleccion$pais))
    
    # La media se dibuja en gris oscuro para distinguirla de los países.
    colores <- c(colores_paises, "Media europea" = "#2c3e50")
    
    df_evol$pais_es <- factor(df_evol$pais_es, levels = c(rev(traducir_pais(top5$pais)), rev(traducir_pais(bottom5$pais))))
    
    plot_ly() %>%
      add_trace(
        data = df_evol,
        x = ~anio, y = ~tasa_100k,
        color = ~pais_es, colors = colores,
        type = "scatter", mode = "lines+markers",
        line = list(width = 2.2), marker = list(size = 6),
        text = ~tooltip, hovertemplate = "%{text}<extra></extra>"
      ) %>%
      add_trace(
        data = media_europea,
        x = ~anio, y = ~tasa_100k,
        name = "Media europea",
        type = "scatter", mode = "lines+markers",
        line = list(color = "#2c3e50", width = 3, dash = "dash"),
        marker = list(color = "#2c3e50", size = 7),
        text = ~tooltip, hovertemplate = "%{text}<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "Año", tickmode = "linear", dtick = 1),
        yaxis = list(title = "Tasa de defunción por 100.000 habitantes"),
        hovermode = "x unified",
        legend = list(title = list(text = "País"), orientation = "v", x = 1.02, y = 1),
        margin = list(l = 70, r = 175, t = 35, b = 45),
        annotations = list(list(
          x = 0, y = 1.08, xref = "paper", yref = "paper",
          text = paste0("5 más afectadas + 5 menos afectadas · Media europea · referencia: ", anio_ref),
          showarrow = FALSE, xanchor = "left"
        ))
      )
  })
  
  output$eur_mapa <- renderLeaflet({
    datos_valores <- datos_europa_filtrados() %>%
      group_by(pais) %>%
      summarise(
        defunciones = sum(defunciones, na.rm = TRUE),
        poblacion = max(poblacion, na.rm = TRUE),
        tasa_100k = if_else(poblacion > 0, defunciones / poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      left_join(pais_eurostat_mapa, by = "pais") %>%
      filter(!is.na(nombre_mapa), is.finite(tasa_100k))
    
    datos_mapa <- mapa_europa %>%
      left_join(datos_valores %>% select(nombre_mapa, tasa_100k), by = "nombre_mapa") %>%
      mutate(pais_es = traducir_pais(coalesce(nombre_mapa, "Europa")))
    
    val_max <- max(datos_mapa$tasa_100k, na.rm = TRUE)
    if (!is.finite(val_max) || val_max <= 0) val_max <- 1
    
    pal <- colorNumeric(palette = "viridis", domain = c(0, val_max), na.color = "#D9D9D9")
    
    etiquetas <- sprintf(
      "<strong>%s</strong><br/>%s",
      datos_mapa$pais_es,
      ifelse(is.na(datos_mapa$tasa_100k),
             "Sin datos de población",
              paste0("Tasa de defunción: ", format(round(datos_mapa$tasa_100k, 1), decimal.mark = ","), " por 100.000 hab."))
    ) %>% lapply(htmltools::HTML)
    
    leaflet(datos_mapa) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(
        fillColor = ~pal(tasa_100k), weight = 1, color = "white", fillOpacity = 0.82,
        label = etiquetas,
        highlightOptions = highlightOptions(weight = 2, color = "#2c3e50", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~tasa_100k, opacity = 0.85,
                title = "Tasa de defunción / 100.000 hab.", position = "bottomright") %>%
      fitBounds(lng1 = -12, lat1 = 34, lng2 = 45, lat2 = 72)
  })
  
  # ---------------------------------------------------------------------------
  # RANKING EUROPEO POR GÉNERO
  # Siempre muestra Hombres y Mujeres para el año seleccionado en el panel.
  # ---------------------------------------------------------------------------
  datos_europa_ranking_causas <- reactive({
    req(input$eur_anio)
    
    anio_sel <- as.character(input$eur_anio)
    
    # OPT: usa sexo_norm precomputado en europa_agrupada.
    base <- europa_agrupada %>%
      mutate(Sexo = sexo_norm) %>%
      filter(Sexo %in% c("Hombres", "Mujeres"), causa_grupo != "Total")
    
    if (anio_sel != "Todos los años") {
      base <- base %>% filter(anio == as.numeric(anio_sel))
    }
    
    base %>%
      group_by(Sexo, causa_grupo) %>%
      summarise(
        Defunciones = sum(defunciones, na.rm = TRUE),
        Poblacion = sum(poblacion, na.rm = TRUE),
        Tasa = if_else(Poblacion > 0, Defunciones / Poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      filter(is.finite(Tasa), is.finite(Defunciones), is.finite(Poblacion), Tasa >= 0)
  })
  
  output$eur_3_causas_top <- renderPlotly({
    df_base <- datos_europa_ranking_causas()
    
    df <- df_base %>%
      group_by(Sexo) %>%
      arrange(desc(Tasa), .by_group = TRUE) %>%
      slice_head(n = 4) %>%
      ungroup() %>%
      mutate(
        Causa = as.character(causa_grupo),
        Genero = factor(Sexo, levels = c("Hombres", "Mujeres"))
      )
    
    validate(need(nrow(df) >= 2, "No hay datos suficientes para el ranking europeo por género."))
    
    plot_ly(
      df,
      x = ~Tasa,
      y = ~Causa,
      color = ~Genero,
      colors = c("Hombres" = "#00B2A9", "Mujeres" = "#FF6F61"),
      type = "bar",
      orientation = "h",
      marker = list(line = list(color = "white", width = 1)),
      hovertemplate = "Tasa: %{x:.1f} por 100.000 habitantes<extra>%{fullData.name}</extra>"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Tasa de defunción del grupo (por 100.000 hab.)"),
        yaxis = list(title = "", automargin = TRUE),
        legend = list(orientation = "h", x = 0.25, y = 1.15),
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 230, r = 20, t = 45, b = 45)
      )
  })
  
  output$eur_3_causas_bottom <- renderPlotly({
    df_base <- datos_europa_ranking_causas()
    
    df <- df_base %>%
      filter(Tasa > 0) %>%
      group_by(Sexo) %>%
      arrange(Tasa, .by_group = TRUE) %>%
      slice_head(n = 4) %>%
      ungroup() %>%
      mutate(
        Causa = as.character(causa_grupo),
        Genero = factor(Sexo, levels = c("Hombres", "Mujeres"))
      )
    
    validate(need(nrow(df) >= 2, "No hay suficientes datos positivos para el ranking europeo por género."))
    
    plot_ly(
      df,
      x = ~Tasa,
      y = ~Causa,
      color = ~Genero,
      colors = c("Hombres" = "#00B2A9", "Mujeres" = "#FF6F61"),
      type = "bar",
      orientation = "h",
      marker = list(line = list(color = "white", width = 1)),
      hovertemplate = "Tasa: %{x:.1f} por 100.000 habitantes<extra>%{fullData.name}</extra>"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Tasa de defunción del grupo (por 100.000 hab.)"),
        yaxis = list(title = "", automargin = TRUE),
        legend = list(orientation = "h", x = 0.25, y = 1.15),
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 230, r = 20, t = 45, b = 45)
      )
  })
  
  output$eur_tabla <- DT::renderDT({
    df <- datos_europa_filtrados() %>%
      transmute(
        Sexo = sexo,
        `Causa / grupo` = causa_grupo,
        País = traducir_pais(pais),
        Año = anio,
        Población = poblacion,
        Defunciones = defunciones,
        `Tasa de defunción por 100.000 hab.` = tasa_100k
      ) %>%
      arrange(desc(Año), País)
    
    DT::datatable(
      df,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      DT::formatRound("Tasa de defunción por 100.000 hab.", 1, dec.mark = ",", mark = ".")
  })
  
  datos_europa_comparador <- reactive({
    req(input$eur_comp_anio, input$eur_comp_causa, input$eur_comp_sexo,
        input$eur_comp_pais_a, input$eur_comp_pais_b)
    europa_agrupada %>%
      filter(
        causa_grupo == input$eur_comp_causa,
        sexo == input$eur_comp_sexo,
        pais %in% c(input$eur_comp_pais_a, input$eur_comp_pais_b)
      ) %>%
      group_by(anio, pais) %>%
      summarise(
        Defunciones = sum(defunciones, na.rm = TRUE),
        Poblacion = max(poblacion, na.rm = TRUE),
        Tasa = if_else(Poblacion > 0, Defunciones / Poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      filter(is.finite(Tasa))
  })
  
  output$eur_comp_filtro_info <- renderUI({
    req(input$eur_comp_anio, input$eur_comp_causa, input$eur_comp_sexo,
        input$eur_comp_pais_a, input$eur_comp_pais_b)
    div(
      class = "filter-help",
      HTML(paste0(
        "<b>Comparación:</b> ", htmltools::htmlEscape(traducir_pais(input$eur_comp_pais_a)),
        " vs. ", htmltools::htmlEscape(traducir_pais(input$eur_comp_pais_b)),
        " · ", htmltools::htmlEscape(input$eur_comp_causa),
        " · ", htmltools::htmlEscape(input$eur_comp_sexo),
        " · población: ", htmltools::htmlEscape(input$eur_comp_anio), "."
      ))
    )
  })
  
  output$eur_comp_evol_nacional <- renderPlotly({
    req(input$eur_comp_pais_a, input$eur_comp_pais_b, input$eur_comp_causa, input$eur_comp_sexo)
    
    df_paises <- europa_agrupada %>%
      filter(
        causa_grupo == input$eur_comp_causa,
        sexo == input$eur_comp_sexo,
        pais %in% c(input$eur_comp_pais_a, input$eur_comp_pais_b)
      ) %>%
      group_by(anio, pais) %>%
      summarise(
        Defunciones = sum(defunciones, na.rm = TRUE),
        Poblacion = max(poblacion, na.rm = TRUE),
        Tasa = if_else(Poblacion > 0, Defunciones / Poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      filter(is.finite(Tasa))
    
    df_media <- europa_agrupada %>%
      filter(causa_grupo == input$eur_comp_causa, sexo == input$eur_comp_sexo) %>%
      group_by(anio) %>%
      summarise(
        Defunciones = sum(defunciones, na.rm = TRUE),
        Poblacion = sum(poblacion, na.rm = TRUE),
        Tasa = if_else(Poblacion > 0, Defunciones / Poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      filter(is.finite(Tasa))
    
    validate(need(nrow(df_paises) > 0, "No hay datos disponibles para los países seleccionados."))
    
    p <- plot_ly()
    
    df_a <- df_paises %>% filter(pais == input$eur_comp_pais_a) %>% mutate(País = traducir_pais(pais))
    if (nrow(df_a) > 0) {
      p <- p %>% add_trace(
        data = df_a, x = ~anio, y = ~Tasa, name = unique(df_a$País),
        type = "scatter", mode = "lines+markers",
        line = list(color = "#1f77b4", width = 3),
        marker = list(color = "#1f77b4", size = 8, line = list(color = "white", width = 1.5)),
        text = ~paste0("<b>", País, "</b><br>Año: ", anio,
                       "<br>Tasa: ", format(round(Tasa, 1), decimal.mark = ",")),
        hovertemplate = "%{text}<extra></extra>"
      )
    }
    
    df_b <- df_paises %>% filter(pais == input$eur_comp_pais_b) %>% mutate(País = traducir_pais(pais))
    if (nrow(df_b) > 0) {
      p <- p %>% add_trace(
        data = df_b, x = ~anio, y = ~Tasa, name = unique(df_b$País),
        type = "scatter", mode = "lines+markers",
        line = list(color = "#ff7f0e", width = 3),
        marker = list(color = "#ff7f0e", size = 8, line = list(color = "white", width = 1.5)),
        text = ~paste0("<b>", País, "</b><br>Año: ", anio,
                       "<br>Tasa: ", format(round(Tasa, 1), decimal.mark = ",")),
        hovertemplate = "%{text}<extra></extra>"
      )
    }
    
    p <- p %>% add_trace(
      data = df_media, x = ~anio, y = ~Tasa, name = "Tasa media europea",
      type = "scatter", mode = "lines+markers",
      line = list(color = "#2c3e50", width = 2.5, dash = "dash"),
      marker = list(color = "#2c3e50", size = 6, line = list(color = "white", width = 1)),
      text = ~paste0("<b>Tasa media europea</b><br>Año: ", anio,
                     "<br>Tasa: ", format(round(Tasa, 1), decimal.mark = ",")),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Año", tickmode = "linear", dtick = 1, showgrid = TRUE, gridcolor = "#E5E5E5"),
        yaxis = list(title = "Tasa de defunción del grupo (por 100k hab.)", showgrid = TRUE, gridcolor = "#E5E5E5"),
        hovermode = "x unified",
        hoverlabel = list(bgcolor = "white"),
        legend = list(orientation = "h", x = 0.2, y = 1.12),
        margin = list(l = 60, r = 20, t = 15, b = 40)
      )
  })
  
  # ---------------------------------------------------------------------------
  # Radares comparativos de países europeos.
  # Se mantienen junto a los dashboards de población y la evolución temporal.
  # ---------------------------------------------------------------------------
  preparar_radar_europa_comparador <- function(pais_sel, anio_sel, sexo_sel, extremos = "top") {
    req(pais_sel, sexo_sel)
    
    df <- europa_agrupada %>%
      filter(
        pais == pais_sel,
        sexo == sexo_sel,
        causa_grupo != "Total"
      )
    
    if (!is.null(anio_sel) && !is.na(anio_sel) && anio_sel != "Todos los años") {
      df <- df %>% filter(anio == as.numeric(anio_sel))
    }
    
    df <- df %>%
      group_by(causa_grupo) %>%
      summarise(
        Defunciones = sum(defunciones, na.rm = TRUE),
        Poblacion = sum(poblacion, na.rm = TRUE),
        Tasa = if_else(Poblacion > 0, Defunciones / Poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      filter(is.finite(Tasa), Tasa >= 0)
    
    if (!nrow(df)) return(df)
    
    df <- df %>%
      arrange(if (extremos == "top") desc(Tasa) else Tasa) %>%
      slice_head(n = min(4, nrow(df)))
    
    # En los radares inferiores excluimos, cuando sea posible, causas con tasa 0
    # para evitar un gráfico vacío o poco informativo.
    if (extremos == "bottom") {
      positivos <- df[df$Tasa > 0, , drop = FALSE]
      if (nrow(positivos) > 0) df <- positivos
    }
    
    df
  }
  
  construir_radar_europa <- function(pais_sel, anio_sel, sexo_sel, extremos = "top", color = "#1f77b4") {
    df <- preparar_radar_europa_comparador(pais_sel, anio_sel, sexo_sel, extremos)
    validate(need(nrow(df) > 0, "No hay datos suficientes para construir el radar."))
    
    df$Causa <- as.character(df$causa_grupo)
    df <- df %>% arrange(if (extremos == "top") desc(Tasa) else Tasa)
    
    # Cerramos el polígono repitiendo exactamente la primera causa.
    df_radar <- bind_rows(df, df[1, , drop = FALSE])
    df_radar$theta <- df_radar$Causa
    df_radar$r <- df_radar$Tasa
    df_radar$hover <- paste0(
      "<b>", htmltools::htmlEscape(df_radar$Causa), "</b>",
      "<br>Tasa: ", format(round(df_radar$Tasa, 1), decimal.mark = ","),
      " por 100.000 habitantes"
    )
    
    fill_rgba <- function(hex, alpha = 0.30) {
      rgbv <- grDevices::col2rgb(hex)[, 1]
      paste0("rgba(", paste(rgbv, collapse = ","), ", ", alpha, ")")
    }
    
    plot_ly(
      df_radar,
      type = "scatterpolar",
      r = ~r,
      theta = ~theta,
      mode = "lines+markers",
      fill = "toself",
      fillcolor = fill_rgba(color, 0.30),
      line = list(color = color, width = 2.5),
      marker = list(color = color, size = 6),
      text = ~hover,
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        polar = list(
          radialaxis = list(showline = TRUE, gridcolor = "#D9D9D9", rangemode = "tozero"),
          angularaxis = list(direction = "clockwise")
        ),
        showlegend = FALSE,
        margin = list(l = 55, r = 55, t = 20, b = 35)
      )
  }
  
  output$eur_comp_radar_title_a <- renderUI({
    req(input$eur_comp_pais_a)
    HTML(paste0("4 causas más afectadas — ", htmltools::htmlEscape(traducir_pais(input$eur_comp_pais_a))))
  })
  
  output$eur_comp_radar_title_b <- renderUI({
    req(input$eur_comp_pais_b)
    HTML(paste0("4 causas más afectadas — ", htmltools::htmlEscape(traducir_pais(input$eur_comp_pais_b))))
  })
  
  output$eur_comp_radar_bottom_title_a <- renderUI({
    req(input$eur_comp_pais_a)
    HTML(paste0("4 causas menos afectadas — ", htmltools::htmlEscape(traducir_pais(input$eur_comp_pais_a))))
  })
  
  output$eur_comp_radar_bottom_title_b <- renderUI({
    req(input$eur_comp_pais_b)
    HTML(paste0("4 causas menos afectadas — ", htmltools::htmlEscape(traducir_pais(input$eur_comp_pais_b))))
  })
  
  output$eur_comp_radar_a <- renderPlotly({
    construir_radar_europa(input$eur_comp_pais_a, input$eur_comp_anio, input$eur_comp_sexo, "top", "#1f77b4")
  })
  
  output$eur_comp_radar_b <- renderPlotly({
    # El segundo país usa el mismo eje radial para facilitar la comparación visual.
    construir_radar_europa(input$eur_comp_pais_b, input$eur_comp_anio, input$eur_comp_sexo, "top", "#ff7f0e")
  })
  
  output$eur_comp_radar_a_bot <- renderPlotly({
    construir_radar_europa(input$eur_comp_pais_a, input$eur_comp_anio, input$eur_comp_sexo, "bottom", "#1f77b4")
  })
  
  output$eur_comp_radar_b_bot <- renderPlotly({
    construir_radar_europa(input$eur_comp_pais_b, input$eur_comp_anio, input$eur_comp_sexo, "bottom", "#ff7f0e")
  })
  
  output$eur_comp_poblacion_titulo_a <- renderText({
    req(input$eur_comp_pais_a, input$eur_comp_anio)
    paste0("Población total — ", traducir_pais(input$eur_comp_pais_a))
  })
  
  output$eur_comp_poblacion_titulo_b <- renderText({
    req(input$eur_comp_pais_b, input$eur_comp_anio)
    paste0("Población total — ", traducir_pais(input$eur_comp_pais_b))
  })
  
  poblacion_pais_total <- function(pais_sel, anio_sel) {
    val <- poblacion_europa %>%
      filter(pais_key == normalizar_clave_pais_europa(pais_sel), anio == as.numeric(anio_sel)) %>%
      summarise(Poblacion = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
      pull(Poblacion)
    if (!length(val) || !is.finite(val)) return(NA_real_)
    val
  }
  
  output$eur_comp_poblacion_a <- renderText({
    req(input$eur_comp_pais_a, input$eur_comp_anio)
    val <- poblacion_pais_total(input$eur_comp_pais_a, input$eur_comp_anio)
    if (!is.finite(val)) return("Sin datos")
    format(round(val), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  
  output$eur_comp_poblacion_b <- renderText({
    req(input$eur_comp_pais_b, input$eur_comp_anio)
    val <- poblacion_pais_total(input$eur_comp_pais_b, input$eur_comp_anio)
    if (!is.finite(val)) return("Sin datos")
    format(round(val), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  
  # ---------------------------------------------------------------------------
  # MODELO ESTADÍSTICO EUROPEO
  # Modelo lineal: tasa ~ país + año. Permite comprobar si existen diferencias
  # entre países controlando por la evolución temporal.
  # ---------------------------------------------------------------------------
  datos_modelo_europa <- reactive({
    req(input$eur_mod_causa, input$eur_mod_sexo, input$eur_mod_anio)
    
    # OPT: usa sexo_norm precomputado en europa_agrupada.
    df <- europa_agrupada %>%
      mutate(Sexo = sexo_norm) %>%
      filter(
        causa_grupo == input$eur_mod_causa,
        Sexo == input$eur_mod_sexo,
        is.finite(tasa_100k),
        is.finite(anio)
      )
    
    if (input$eur_mod_anio != "Todos los años") {
      df <- df %>% filter(anio == as.numeric(input$eur_mod_anio))
    }
    
    df %>%
      group_by(pais, anio) %>%
      summarise(
        Tasa = if (all(!is.finite(tasa_100k))) NA_real_ else mean(tasa_100k, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(is.finite(Tasa)) %>%
      mutate(
        pais = factor(pais),
        anio = as.numeric(anio)
      )
  })
  
  modelo_europa <- reactive({
    df <- datos_modelo_europa()
    validate(need(nrow(df) >= 5, "No hay suficientes datos para el modelo europeo: se necesitan al menos 5 observaciones."))
    validate(need(dplyr::n_distinct(df$pais) >= 3, "No hay suficientes países para comparar: se necesitan al menos 3 países."))
    
    if (dplyr::n_distinct(df$anio) >= 2) {
      lm(Tasa ~ pais + anio, data = df)
    } else {
      lm(Tasa ~ pais, data = df)
    }
  })
  
  output$eur_mod_info <- renderUI({
    req(input$eur_mod_causa, input$eur_mod_sexo, input$eur_mod_anio)
    df <- datos_modelo_europa()
    HTML(paste0(
      "<b>Observaciones:</b> ", nrow(df),
      " · <b>Países:</b> ", dplyr::n_distinct(df$pais),
      " · <b>Periodo:</b> ", htmltools::htmlEscape(input$eur_mod_anio),
      " · <b>Modelo:</b> ", if (dplyr::n_distinct(df$anio) >= 2) "tasa ~ país + año" else "tasa ~ país"
    ))
  })
  
  output$eur_mod_r2 <- renderText({
    fit <- modelo_europa()
    r2 <- summary(fit)$r.squared
    paste0(round(r2, 4), " (", round(r2 * 100, 2), " % de variabilidad explicada)")
  })
  
  output$eur_mod_validez <- renderText({
    fit <- modelo_europa()
    a <- anova(fit)
    p_pais <- if ("pais" %in% rownames(a) && "Pr(>F)" %in% names(a)) a["pais", "Pr(>F)"] else NA_real_
    if (!is.finite(p_pais)) {
      "No evaluable con estos datos (ajuste perfecto o sin grados de libertad residuales)"
    } else if (p_pais < 0.05) {
      "Diferencias significativas entre países"
    } else {
      "Sin diferencias significativas entre países"
    }
  })
  
  output$eur_mod_boxplot <- renderPlotly({
    df <- datos_modelo_europa()
    validate(need(nrow(df) > 5, "No hay suficientes datos para el modelo europeo."))
    # Horizontal y ordenado por mediana: con ~30 países el eje X vertical
    # solapaba los nombres y el gráfico quedaba ilegible.
    df_plot <- df %>%
      mutate(País = traducir_pais(as.character(pais))) %>%
      mutate(País = reorder(País, Tasa, FUN = function(v) median(v, na.rm = TRUE)))
    plot_ly(df_plot, y = ~País, x = ~Tasa, type = "box", orientation = "h",
            boxpoints = "outliers",
            marker = list(color = "#2c3e50"),
            line = list(color = "#2c3e50"),
            hovertemplate = "<b>%{y}</b><br>Tasa: %{x:.1f} por 100.000 habitantes<extra></extra>") %>%
      layout(
        xaxis = list(title = "Tasa de defunción del grupo (por 100.000 hab.)"),
        yaxis = list(title = "", automargin = TRUE),
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 185, r = 30, t = 15, b = 55)
      )
  })
  
  output$eur_mod_tabla <- renderTable({
    df <- datos_modelo_europa()
    fit <- modelo_europa()
    a <- anova(fit)
    tiene_anio <- dplyr::n_distinct(df$anio) >= 2
    formula_txt <- if (tiene_anio) "lm(Tasa ~ pais + anio)" else "lm(Tasa ~ pais)"
    
    p_pais <- if ("pais" %in% rownames(a) && "Pr(>F)" %in% names(a)) a["pais", "Pr(>F)"] else NA_real_
    f_pais <- if ("pais" %in% rownames(a) && "F value" %in% names(a)) a["pais", "F value"] else NA_real_
    
    f_txt <- if (is.finite(f_pais)) format(round(unname(f_pais), 4), nsmall = 4) else "No evaluable"
    p_txt <- if (is.finite(p_pais)) format.pval(p_pais, digits = 4, eps = 0.001) else "No evaluable"
    conclusion <- if (!is.finite(p_pais)) {
      "No evaluable: el ajuste es esencialmente perfecto o no quedan grados de libertad residuales para contrastar el efecto país."
    } else if (p_pais < 0.05) {
      if (tiene_anio) "Se rechaza H0: existen diferencias entre países, controlando por año." else "Se rechaza H0: existen diferencias entre países."
    } else {
      if (tiene_anio) "No se rechaza H0: no hay evidencia suficiente de diferencias entre países, controlando por año." else "No se rechaza H0: no hay evidencia suficiente de diferencias entre países."
    }
    
    data.frame(
      Elemento = c("Modelo", "F del efecto país", "p-valor país", "Conclusión"),
      Resultado = c(formula_txt, f_txt, p_txt, conclusion),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$eur_mod_summary <- renderPrint({
    summary(modelo_europa())
  })

  # ---------------------------------------------------------------------------
  # DETERMINANTES SERVER (renta/médicos por CCAA + cruce con mortalidad)
  # ---------------------------------------------------------------------------
  # Valor del determinante por CCAA y año (renta agregada ponderada por
  # población provincial; médicos directos por CCAA).
  # OPT: los datos son estáticos por sesión: cada (indicador, año) y cada
  # (mortalidad, año) se calcula una sola vez aunque lo pidan varios reactives.
  det_memo <- new.env(parent = emptyenv())
  det_ccaa_ano <- function(ind, ano) {
    key <- paste0(ind, "||", ano)
    hit <- mget(key, envir = det_memo, ifnotfound = list(NULL), inherits = FALSE)[[1]]
    if (!is.null(hit)) return(hit)
    req(ind, ano)
    a <- as.character(ano)
    val <- if (identical(ind, det_ind_medicos)) {
      req(!is.null(medicos_data))
      medicos_data %>%
        filter(Año == a) %>%
        transmute(Comunidad = as.character(Comunidad), Valor = Valor)
    } else {
      req(!is.null(renta_data))
      renta_data %>%
        filter(Indicador == ind, Año == a) %>%
        left_join(pob_prov %>% filter(Año == a) %>% select(Provincia, Pob), by = "Provincia") %>%
        group_by(Comunidad) %>%
        summarise(Valor = sum(Valor * Pob, na.rm = TRUE) / sum(Pob, na.rm = TRUE), .groups = "drop") %>%
        mutate(Comunidad = as.character(Comunidad))
    }
    assign(key, val, envir = det_memo)
    val
  }

  # Población por CCAA y año (para ponderar medias nacionales).
  pob_ccaa_ano <- function(a) {
    pob_prov %>%
      filter(Año == as.character(a)) %>%
      mutate(Comunidad = prov_a_comunidad(Provincia)) %>%
      filter(Comunidad != "Sin asignar") %>%
      group_by(Comunidad) %>%
      summarise(Pob = sum(Pob, na.rm = TRUE), .groups = "drop")
  }

  det_fmt <- function(v, ind) {
    dec <- if (grepl("Médicos", ind)) 1 else 0
    format(round(v, dec), big.mark = ".", decimal.mark = ",", nsmall = dec, scientific = FALSE, trim = TRUE)
  }

  datos_det <- reactive({
    req(input$det_ind, input$det_ano)
    det_ccaa_ano(input$det_ind, input$det_ano) %>% filter(is.finite(Valor))
  }) %>% bindCache(input$det_ind, input$det_ano)

  datos_det_evol <- reactive({
    req(input$det_ind)
    lapply(det_anos, function(a) {
      det_ccaa_ano(input$det_ind, a) %>% mutate(Año = a)
    }) %>% bind_rows()
  }) %>% bindCache(input$det_ind)

  output$det_kpi_media <- renderText({
    df <- datos_det()
    req(nrow(df) > 0)
    pob <- pob_ccaa_ano(input$det_ano)
    j <- df %>% inner_join(pob, by = "Comunidad")
    req(nrow(j) > 0)
    v <- sum(j$Valor * j$Pob, na.rm = TRUE) / sum(j$Pob, na.rm = TRUE)
    det_fmt(v, input$det_ind)
  })

  output$det_kpi_max <- renderText({
    df <- datos_det()
    req(nrow(df) > 0)
    x <- df %>% arrange(desc(Valor)) %>% slice(1)
    paste0(x$Comunidad, " (", det_fmt(x$Valor, input$det_ind), ")")
  })

  output$det_kpi_min <- renderText({
    df <- datos_det()
    req(nrow(df) > 0)
    x <- df %>% arrange(Valor) %>% slice(1)
    paste0(x$Comunidad, " (", det_fmt(x$Valor, input$det_ind), ")")
  })

  output$det_mapa <- renderLeaflet({
    df <- datos_det()
    mapa_datos <- mapa_provincias %>%
      mutate(Comunidad = prov_a_comunidad(NAME_2)) %>%
      left_join(df, by = "Comunidad")
    val_max <- max(mapa_datos$Valor, na.rm = TRUE)
    val_min <- min(mapa_datos$Valor, na.rm = TRUE)
    dominio <- if (!all(is.finite(c(val_min, val_max))) || val_min == val_max) c(0, 1) else c(val_min, val_max)
    pal <- colorNumeric(palette = PAL_YLORRD, domain = dominio, na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>%s: %s",
                         mapa_datos$NAME_2,
                         htmltools::htmlEscape(input$det_ind),
                         det_fmt(mapa_datos$Valor, input$det_ind)) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Valor), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio, opacity = 0.8, title = input$det_ind, position = "bottomright")
  })

  output$det_evol <- renderPlotly({
    df <- datos_det_evol()
    req(nrow(df) > 0)
    df <- df %>% mutate(Ano_Num = suppressWarnings(as.numeric(Año)))
    nccaa <- dplyr::n_distinct(df$Comunidad)
    plot_ly(data = df, x = ~Ano_Num, y = ~Valor, color = ~Comunidad,
            colors = grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                                  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(max(nccaa, 1)),
            type = "scatter", mode = "lines+markers",
            marker = list(line = list(color = "white", width = 1)),
            hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Valor: %{y:,.2f}<extra></extra>") %>%
      layout(xaxis = list(title = "Año", dtick = 1),
             yaxis = list(title = input$det_ind),
             legend = list(orientation = "h", x = 0, y = -0.2),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 10, b = 100))
  })

  output$det_ranking <- renderPlotly({
    df <- datos_det()
    req(nrow(df) > 0)
    df <- df %>% arrange(Valor) %>% mutate(Comunidad = factor(Comunidad, levels = Comunidad))
    dom <- dominio_seguro(df$Valor)
    plot_ly(df, x = ~Valor, y = ~Comunidad, type = "bar", orientation = "h",
            marker = list(color = ~Valor, cmin = dom[1], cmax = dom[2],
                          colorscale = escala_plotly(PAL_YLORRD), showscale = FALSE,
                          line = list(color = "white", width = 1)),
            text = ~det_fmt(Valor, input$det_ind), textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Valor: %{x:,.2f}<extra></extra>") %>%
      layout(xaxis = list(title = input$det_ind),
             yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 140, r = 60, t = 20, b = 50))
  })

  output$det_tabla <- DT::renderDT({
    df <- datos_det_evol() %>%
      transmute(Comunidad = Comunidad, Año = Año, Valor = Valor) %>%
      arrange(Comunidad, Año)
    DT::datatable(df, options = list(pageLength = 19, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound("Valor", 1, dec.mark = ",", mark = ".")
  })

  # --- Índice sintético por CCAA (renta + médicos + mortalidad) ---
  idx_ind_renta <- if ("Renta neta media por persona" %in% det_indicadores) "Renta neta media por persona" else det_indicadores[1]

  # Tasa bruta de mortalidad total (ambos sexos) por CCAA y año (memoizada).
  mort_ccaa_ano <- function(a) {
    key <- paste0("mort||", a)
    hit <- mget(key, envir = det_memo, ifnotfound = list(NULL), inherits = FALSE)[[1]]
    if (!is.null(hit)) return(hit)
    val <- causas_provinciales %>%
      filter(Año == as.character(a), Comunidad != "Sin asignar") %>%
      group_by(Comunidad) %>%
      summarise(F = sum(Fallecidos, na.rm = TRUE),
                P = sum(dplyr::distinct(dplyr::pick(Provincia, Sexo, Año, Poblacion))$Poblacion, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(Tasa = if_else(P > 0, F / P * 100000, NA_real_)) %>%
      transmute(Comunidad = as.character(Comunidad), Mortalidad = Tasa)
    assign(key, val, envir = det_memo)
    val
  }

  # Rangos min-max globales 2018-2022 por componente (la evolución es comparable).
  # OPT: estáticos por sesión (no dependen de inputs) -> fuera de reactive.
  idx_rangos <- local({
    rg <- function(getter) {
      v <- unlist(lapply(det_anos, function(x) getter(as.character(x))[[2]]))
      v <- v[is.finite(v)]
      req(length(v) > 0)
      c(min(v), max(v))
    }
    list(
      renta = rg(function(x) det_ccaa_ano(idx_ind_renta, x)),
      med = rg(function(x) det_ccaa_ano(det_ind_medicos, x)),
      mort = rg(function(x) mort_ccaa_ano(x))
    )
  })

  idx_norm <- function(v, rg, invert = FALSE) {
    if (!all(is.finite(rg)) || rg[2] <= rg[1]) return(rep(NA_real_, length(v)))
    n <- (v - rg[1]) / (rg[2] - rg[1]) * 100
    if (invert) n <- 100 - n
    n
  }

  idx_build <- function(a, wr, wm, wmo, rg) {
    req(wr + wm + wmo > 0)
    det_ccaa_ano(idx_ind_renta, a) %>% rename(Renta = Valor) %>%
      inner_join(det_ccaa_ano(det_ind_medicos, a) %>% rename(Medicos = Valor), by = "Comunidad") %>%
      inner_join(mort_ccaa_ano(a), by = "Comunidad") %>%
      mutate(Indice = (wr * idx_norm(Renta, rg$renta) +
                       wm * idx_norm(Medicos, rg$med) +
                       wmo * idx_norm(Mortalidad, rg$mort, invert = TRUE)) / (wr + wm + wmo)) %>%
      filter(is.finite(Indice)) %>%
      arrange(desc(Indice))
  }

  datos_idx <- reactive({
    req(input$idx_ano, input$idx_wr, input$idx_wm, input$idx_wmo)
    idx_build(as.character(input$idx_ano), input$idx_wr, input$idx_wm, input$idx_wmo, idx_rangos)
  }) %>% bindCache(input$idx_ano, input$idx_wr, input$idx_wm, input$idx_wmo)

  datos_idx_evol <- reactive({
    req(input$idx_wr, input$idx_wm, input$idx_wmo)
    rg <- idx_rangos
    lapply(det_anos, function(x) {
      idx_build(as.character(x), input$idx_wr, input$idx_wm, input$idx_wmo, rg) %>%
        mutate(Año = as.character(x))
    }) %>% bind_rows()
  }) %>% bindCache(input$idx_wr, input$idx_wm, input$idx_wmo)

  output$idx_kpi_lider <- renderText({
    df <- datos_idx()
    req(nrow(df) > 0)
    paste0(df$Comunidad[1], " (",
           format(round(df$Indice[1], 1), decimal.mark = ",", nsmall = 1), ")")
  })

  output$idx_kpi_media <- renderText({
    df <- datos_idx()
    req(nrow(df) > 0)
    format(round(mean(df$Indice, na.rm = TRUE), 1), decimal.mark = ",", nsmall = 1)
  })

  output$idx_kpi_brecha <- renderText({
    df <- datos_idx()
    req(nrow(df) > 0)
    format(round(max(df$Indice, na.rm = TRUE) - min(df$Indice, na.rm = TRUE), 1),
           decimal.mark = ",", nsmall = 1)
  })

  output$idx_mapa <- renderLeaflet({
    df <- datos_idx()
    req(nrow(df) > 0)
    mapa_datos <- mapa_provincias %>%
      mutate(Comunidad = prov_a_comunidad(NAME_2)) %>%
      left_join(df %>% select(Comunidad, Indice), by = "Comunidad")
    val_max <- max(mapa_datos$Indice, na.rm = TRUE)
    val_min <- min(mapa_datos$Indice, na.rm = TRUE)
    dominio <- dominio_seguro(mapa_datos$Indice)
    pal <- colorNumeric(palette = PAL_YLGNBU, domain = dominio, na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>Índice sintético: %s",
                         mapa_datos$NAME_2,
                         format(round(mapa_datos$Indice, 1), big.mark = ".", decimal.mark = ",", nsmall = 1)) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Indice), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio, opacity = 0.8, title = "Índice (0-100)", position = "bottomright")
  })

  output$idx_ranking <- renderPlotly({
    df <- datos_idx()
    req(nrow(df) > 0)
    df <- df %>% arrange(Indice) %>% mutate(Comunidad = factor(Comunidad, levels = Comunidad))
    dom <- dominio_seguro(df$Indice)
    plot_ly(df, x = ~Indice, y = ~Comunidad, type = "bar", orientation = "h",
            marker = list(color = ~Indice, cmin = dom[1], cmax = dom[2],
                          colorscale = escala_plotly(PAL_YLGNBU), showscale = FALSE,
                          line = list(color = "white", width = 1)),
            text = ~format(round(Indice, 1), decimal.mark = ",", nsmall = 1), textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Índice: %{x:.1f}<extra></extra>") %>%
      layout(xaxis = list(title = "Índice sintético (0-100)", range = c(0, 105)),
             yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 140, r = 60, t = 20, b = 50))
  })

  output$idx_evol <- renderPlotly({
    df <- datos_idx_evol()
    req(nrow(df) > 0)
    nccaa <- dplyr::n_distinct(df$Comunidad)
    plot_ly(df, x = ~Año, y = ~Indice, color = ~Comunidad,
            colors = grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                                  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(max(nccaa, 1)),
            type = "scatter", mode = "lines+markers",
            hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Índice: %{y:.1f}<extra></extra>") %>%
      layout(xaxis = list(title = "Año", dtick = 1),
             yaxis = list(title = "Índice sintético (0-100)"),
             legend = list(orientation = "h", x = 0, y = -0.2),
             margin = list(l = 60, r = 20, t = 10, b = 100))
  })

  output$idx_tabla <- DT::renderDT({
    df <- datos_idx() %>%
      transmute(Comunidad = Comunidad,
                Renta = round(Renta),
                Médicos = round(Medicos, 1),
                `Tasa bruta` = round(Mortalidad, 1),
                Índice = round(Indice, 1)) %>%
      arrange(desc(Índice))
    DT::datatable(df, options = list(pageLength = 19, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound("Renta", 0, dec.mark = ",", mark = ".") %>%
      DT::formatRound(c("Médicos", "Tasa bruta", "Índice"), 1, dec.mark = ",", mark = ".")
  })

  # --- EXCESO DE MORTALIDAD SERVER (baseline = media 2015-2019) ---
  ex_base_anos <- function(anos_disp) {
    intersect(as.character(2015:2019), as.character(anos_disp))
  }

  datos_ex_anual <- reactive({
    req(!is.null(datos_edadprov), input$ex_prov, input$ex_sexo)
    df <- datos_edadprov$anual %>% filter(Sexo == input$ex_sexo)
    if (input$ex_prov != "Todas") df <- df %>% filter(Provincia == input$ex_prov)
    df <- df %>%
      group_by(Año) %>%
      summarise(Obs = sum(Def, na.rm = TRUE), .groups = "drop") %>%
      mutate(Ano_Num = suppressWarnings(as.numeric(Año))) %>%
      filter(is.finite(Ano_Num)) %>%
      arrange(Ano_Num)
    req(nrow(df) > 0)
    base <- ex_base_anos(df$Año)
    req(length(base) >= 3)
    esp <- mean(df$Obs[df$Año %in% base], na.rm = TRUE)
    df %>%
      mutate(Esp = esp,
             Exc = Obs - Esp,
             Pscore = if_else(Esp > 0, Exc / Esp * 100, NA_real_))
  })

  ex_fmt <- function(v, dec = 0) {
    format(round(v, dec), big.mark = ".", decimal.mark = ",", nsmall = dec, scientific = FALSE, trim = TRUE)
  }

  ex_kpi_ano <- function(df, ano) {
    x <- df %>% filter(Ano_Num == ano)
    if (!nrow(x)) return("-")
    signo <- ifelse(x$Exc >= 0, "+", "")
    paste0(signo, ex_fmt(x$Exc), " (", signo, ex_fmt(x$Pscore, 1), " %)")
  }

  output$ex_kpi_2020 <- renderText({
    ex_kpi_ano(datos_ex_anual(), 2020)
  })

  output$ex_kpi_2021 <- renderText({
    ex_kpi_ano(datos_ex_anual(), 2021)
  })

  output$ex_kpi_peor <- renderText({
    df <- datos_ex_anual()
    req(nrow(df) > 0)
    x <- df %>% arrange(desc(Pscore)) %>% slice(1)
    paste0(as.integer(x$Ano_Num), " (", ex_fmt(x$Pscore, 1), " %)")
  })

  output$ex_evol <- renderPlotly({
    df <- datos_ex_anual()
    req(nrow(df) > 0)
    df <- df %>% mutate(
      Techo = Esp + pmax(Exc, 0),
      signo = ifelse(Exc >= 0, "+", ""),
      Etiq = paste0(signo, ex_fmt(Exc), " (", signo, ex_fmt(Pscore, 1), " %)")
    )
    pico <- df %>% arrange(desc(Pscore)) %>% slice(1)
    plot_ly() %>%
      add_trace(data = df, x = ~Ano_Num, y = ~Esp, name = "Esperadas (2015-2019)",
                type = "scatter", mode = "lines",
                line = list(color = "#95a5a6", width = 2, dash = "dash"),
                hovertemplate = "<b>%{x}</b><br>Esperadas: %{y:,.0f}<extra></extra>") %>%
      add_trace(data = df, x = ~Ano_Num, y = ~Techo, name = "Exceso",
                type = "scatter", mode = "none", fill = "tonexty",
                fillcolor = "rgba(231,76,60,0.22)", hoverinfo = "skip") %>%
      add_trace(data = df, x = ~Ano_Num, y = ~Obs, name = "Observadas",
                type = "scatter", mode = "lines+markers", text = ~Etiq,
                line = list(color = "#1f3a5f", width = 3.5, shape = "spline", smoothing = 0.4),
                marker = list(color = "#1f3a5f", size = 7, line = list(color = "white", width = 1.5)),
                hovertemplate = "<b>%{x}</b><br>Observadas: %{y:,.0f}<br>Exceso: %{text}<extra></extra>") %>%
      add_annotations(x = pico$Ano_Num, y = pico$Obs,
                      text = paste0("<b>", as.integer(pico$Ano_Num), "</b><br>", pico$Etiq),
                      showarrow = TRUE, arrowhead = 2, arrowcolor = "#c0392b",
                      ax = 55, ay = -55, bgcolor = "white", bordercolor = "#c0392b",
                      borderwidth = 1.5, font = list(size = 12)) %>%
      layout(xaxis = list(title = "Año", tickmode = "linear", dtick = 2),
             yaxis = list(title = "Defunciones"),
             legend = list(orientation = "h", x = 0.1, y = -0.25),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 70, r = 20, t = 30, b = 95))
  })

  datos_ex_mes <- reactive({
    req(!is.null(datos_edadprov), input$ex_prov, input$ex_sexo, input$ex_ano)
    df <- datos_edadprov$mes %>% filter(Sexo == input$ex_sexo)
    if (input$ex_prov != "Todas") df <- df %>% filter(Provincia == input$ex_prov)
    obs <- df %>%
      filter(Año == as.character(input$ex_ano)) %>%
      group_by(Mes) %>%
      summarise(Obs = sum(Def, na.rm = TRUE), .groups = "drop")
    base <- df %>%
      filter(Año %in% ex_base_anos(unique(df$Año))) %>%
      group_by(Año, Mes) %>%
      summarise(Tot = sum(Def, na.rm = TRUE), .groups = "drop") %>%
      group_by(Mes) %>%
      summarise(Esp = mean(Tot, na.rm = TRUE), .groups = "drop")
    lv <- intersect(orden_meses, unique(c(as.character(obs$Mes), as.character(base$Mes))))
    req(length(lv) > 0)
    data.frame(Mes = factor(lv, levels = lv), stringsAsFactors = FALSE) %>%
      left_join(obs, by = "Mes") %>%
      left_join(base, by = "Mes") %>%
      mutate(Obs = if_else(is.finite(Obs), Obs, 0),
             Esp = if_else(is.finite(Esp), Esp, NA_real_),
             P = if_else(is.finite(Esp) & Esp > 0, (Obs - Esp) / Esp * 100, NA_real_))
  })

  output$ex_meses <- renderPlotly({
    df <- datos_ex_mes()
    req(nrow(df) > 0)
    mes_abr <- c(Enero = "Ene", Febrero = "Feb", Marzo = "Mar", Abril = "Abr", Mayo = "May",
                 Junio = "Jun", Julio = "Jul", Agosto = "Ago", Septiembre = "Sep",
                 Octubre = "Oct", Noviembre = "Nov", Diciembre = "Dic")
    df <- df %>% mutate(
      MesLab = factor(unname(mes_abr[as.character(Mes)]), levels = unname(mes_abr)),
      signo = ifelse(is.finite(P) & P >= 0, "+", ""),
      Etiq = ifelse(is.finite(P),
                    paste0(signo, ex_fmt(Obs - Esp), " (", signo, ex_fmt(P, 1), " %)"),
                    "sin baseline")
    )
    pr <- dominio_seguro(df$P)
    df <- df %>% mutate(Pplot = ifelse(is.finite(P), P, pr[1]))
    pico <- df %>% filter(is.finite(P)) %>% arrange(desc(P)) %>% slice(1)
    fig <- plot_ly() %>%
      add_trace(data = df, x = ~MesLab, y = ~Obs, text = ~Mes, customdata = ~Etiq,
                name = paste0("Observadas ", input$ex_ano),
                type = "bar",
                marker = list(color = ~Pplot, cmin = pr[1], cmax = pr[2],
                              colorscale = "Reds", showscale = TRUE,
                              colorbar = list(title = "Exceso %"),
                              line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{text}</b><br>Observadas: %{y:,.0f}<br>Exceso: %{customdata}<extra></extra>") %>%
      add_trace(data = df, x = ~MesLab, y = ~Esp, text = ~Mes,
                name = "Esperadas (media 2015-2019)",
                type = "scatter", mode = "lines+markers",
                line = list(color = "#2c3e50", width = 3, dash = "dash"),
                marker = list(color = "white", size = 8, line = list(color = "#2c3e50", width = 2)),
                hovertemplate = "<b>%{text}</b><br>Esperadas: %{y:,.0f}<extra></extra>") %>%
      layout(barmode = "group",
             xaxis = list(title = "Mes", categoryorder = "array", categoryarray = levels(df$MesLab)),
             yaxis = list(title = "Defunciones"),
             legend = list(orientation = "h", y = -0.3),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 70, r = 20, t = 20, b = 100))
    if (nrow(pico) > 0) {
      fig <- fig %>% add_annotations(x = as.character(pico$MesLab), y = pico$Obs,
                                     text = paste0("<b>", as.character(pico$Mes), "</b><br>", pico$Etiq),
                                     showarrow = TRUE, arrowhead = 2, arrowcolor = "#c0392b",
                                     ax = 0, ay = -60, bgcolor = "white", bordercolor = "#c0392b",
                                     borderwidth = 1.5, font = list(size = 12))
    }
    fig
  })

  output$ex_tabla <- DT::renderDT({
    df <- datos_ex_anual() %>%
      transmute(Año = as.integer(Ano_Num),
                Observadas = Obs,
                Esperadas = round(Esp),
                Exceso = round(Exc),
                `P-score (%)` = round(Pscore, 1)) %>%
      arrange(desc(Año))
    DT::datatable(df, options = list(pageLength = 16, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound(c("Observadas", "Esperadas", "Exceso"), 0, dec.mark = ",", mark = ".") %>%
      DT::formatRound("P-score (%)", 1, dec.mark = ",", mark = ".")
  })


  # --- RESUMEN GENERAL SERVER ---
  # FIX: causas_provinciales solo trae Hombres/Mujeres (sin filas "Ambos"),
  # así que el total se obtiene sumando ambos sexos en lugar de filtrar
  # Sexo == "Ambos" (el filtro devolvía 0 filas y los KPI mostraban "-").
  output$res_kpi_tasa <- renderText({
    # FIX: la población se repite en cada causa; se suma una sola vez
    # (antes el denominador contaba 13 veces y la tasa salía ~13× menor).
    df <- causas_provinciales %>% filter(Año_Num == 2022)
    if (nrow(df) == 0) return("-")
    pob <- df %>% distinct(Provincia, Sexo, Año, Poblacion) %>%
      summarise(P = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      pull(P)
    if (!length(pob) || !is.finite(pob) || pob <= 0) return("-")
    paste0(format(round(sum(df$Fallecidos, na.rm = TRUE) / pob * 100000, 2), decimal.mark = ","), " / 100k")
  })

  output$res_kpi_causa <- renderText({
    df <- causas_provinciales %>% filter(Año_Num == 2022, Defunción != "Total") %>%
      group_by(Defunción) %>% summarise(Fallecidos = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Fallecidos)) %>% slice(1)
    if (nrow(df) == 0) return("-")
    as.character(df$Defunción)
  })

  output$res_kpi_provincia <- renderText({
    # FIX: igual que res_kpi_tasa, la población se suma una sola vez.
    df <- causas_provinciales %>% filter(Año_Num == 2022)
    if (nrow(df) == 0) return("-")
    pob <- df %>% distinct(Provincia, Sexo, Año, Poblacion) %>%
      group_by(Provincia) %>%
      summarise(Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop")
    df <- df %>%
      group_by(Provincia) %>%
      summarise(Fallecidos = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob, by = "Provincia") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>%
      arrange(desc(Tasa)) %>%
      slice(1)
    if (nrow(df) == 0) return("-")
    paste0(df$Provincia, " (", format(round(df$Tasa, 2), decimal.mark = ","), " / 100k)")
  })

  output$res_kpi_europa <- renderText({
    tryCatch({
      df <- europa_agrupada %>% filter(anio == max(anio, na.rm = TRUE), sexo == "Ambos", causa_grupo == "Total") %>%
        group_by(pais) %>% summarise(Tasa = sum(defunciones, na.rm = TRUE) / sum(poblacion, na.rm = TRUE) * 100000, .groups = "drop") %>%
        filter(is.finite(Tasa))
      # FIX: la columna pais está en inglés ("Spain", no "España").
      esp <- df %>% filter(pais == "Spain") %>% pull(Tasa)
      if (length(esp) == 0) return("Disponible en Comparación europea")
      paste0("España: ", format(round(esp[1], 2), decimal.mark = ","), " / 100k")
    }, error = function(e) "Disponible en Comparación europea")
  })

  output$res_apvp <- renderPlotly({
    tryCatch({
      # FIX: apvp_data solo trae Hombres/Mujeres; se suman ambos sexos.
      df <- apvp_data %>% filter(Indicador == "Nº de APVP") %>%
        group_by(Causa) %>%
        summarise(APVP = sum(Valor, na.rm = TRUE), .groups = "drop") %>% arrange(desc(APVP)) %>% slice_head(n = 8) %>%
        mutate(Causa = stringr::str_wrap(Causa, width = 32))
      plot_ly(df, x = ~APVP, y = ~reorder(Causa, APVP), type = "bar", orientation = "h",
              marker = list(color = ~APVP, colorscale = "Greens", showscale = FALSE,
                            line = list(color = "rgba(0,0,0,0.15)", width = 1)),
              text = ~format(round(APVP), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE),
              textposition = "outside",
              hovertemplate = "<b>%{y}</b><br>APVP: %{x:,.0f}<extra></extra>") %>%
        layout(xaxis = list(title = "APVP"), yaxis = list(title = "", automargin = TRUE),
               hoverlabel = list(bgcolor = "white"),
               margin = list(l = 220, r = 60, t = 20, b = 50))
    }, error = function(e) plotly_empty())
  })
  
  output$res_europa <- renderPlotly({
    tryCatch({
      df <- europa_agrupada %>% filter(anio == max(anio, na.rm = TRUE), sexo == "Ambos", causa_grupo == "Total") %>%
        group_by(pais) %>% summarise(Tasa = sum(defunciones, na.rm = TRUE) / sum(poblacion, na.rm = TRUE) * 100000, .groups = "drop") %>%
        filter(is.finite(Tasa)) %>% arrange(desc(Tasa)) %>% slice_head(n = 10)
      plot_ly(df, x = ~Tasa, y = ~reorder(pais, Tasa), type = "bar", orientation = "h",
              marker = list(color = ~Tasa, colorscale = "Blues", showscale = FALSE,
                            line = list(color = "rgba(0,0,0,0.15)", width = 1)),
              hovertemplate = "<b>%{y}</b><br>Tasa de mortalidad: %{x:.1f} por 100.000 hab.<extra></extra>") %>%
        layout(xaxis = list(title = "Tasa de mortalidad por 100.000"), yaxis = list(title = "", automargin = TRUE),
               margin = list(l = 150, r = 30, t = 20, b = 50))
    }, error = function(e) plotly_empty())
  })
  

  
  # --- EVOLUCIÓN TEMPORAL POR COMUNIDADES SERVER ---
  datos_pt_ccaa <- reactive({
    req(input$pt_causa, input$pt_sexo)
    df <- causas_provinciales %>%
      filter(Defunción == input$pt_causa)
    if (input$pt_sexo != "Ambos") df <- df %>% filter(Sexo == input$pt_sexo)
    
    df %>%
      group_by(Comunidad, Año) %>%
      summarise(
        Fallecidos = sum(Fallecidos, na.rm = TRUE),
        Poblacion = sum(Poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
        Año_Num = suppressWarnings(as.numeric(Año))
      ) %>%
      filter(is.finite(Tasa), !is.na(Comunidad), Comunidad != "Sin asignar")
  }) %>% bindCache(input$pt_causa, input$pt_sexo)
  
  output$pt_kpi_cambio <- renderText({
    df <- datos_pt_ccaa() %>%
      select(Comunidad, Año_Num, Tasa) %>%
      filter(Año_Num %in% c(2018, 2022)) %>%
      group_by(Comunidad, Año_Num) %>%
      summarise(Tasa = mean(Tasa, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Año_Num, values_from = Tasa)
    if (!all(c("2018", "2022") %in% names(df))) return("-")
    cambios <- df %>% mutate(Cambio = .data[["2022"]] - .data[["2018"]]) %>%
      filter(is.finite(Cambio)) %>% arrange(desc(Cambio)) %>% slice(1)
    if (nrow(cambios) == 0) return("-")
    paste0(cambios$Comunidad, " (", format(round(cambios$Cambio, 2), decimal.mark = ","), ")")
  })
  
  output$pt_kpi_defunciones <- renderText({
    df <- datos_pt_ccaa()
    if (nrow(df) == 0) return("-")
    format(sum(df$Fallecidos, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })
  
  output$pt_kpi_ano_max <- renderText({
    df <- datos_pt_ccaa() %>%
      group_by(Año_Num) %>%
      summarise(Defunciones = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Año_Num), is.finite(Defunciones)) %>%
      arrange(desc(Defunciones))
    if (nrow(df) == 0) return("-")
    paste0(as.integer(df$Año_Num[1]), " (",
           format(round(df$Defunciones[1]), big.mark = ".", decimal.mark = ","),
           " defunciones)")
  })
  
  output$pt_evolucion_ccaa <- renderPlotly({
    df <- datos_pt_ccaa() %>% arrange(Año_Num, Comunidad)
    req(nrow(df) > 0)
    nccaa <- dplyr::n_distinct(df$Comunidad)
    plot_ly(
      data = df, x = ~Año_Num, y = ~Tasa, color = ~Comunidad,
      colors = grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                             "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(max(nccaa, 1)),
      type = "scatter", mode = "lines+markers",
      marker = list(line = list(color = "white", width = 1)),
      text = ~paste0(
        Comunidad, "<br>Año: ", Año_Num,
        "<br>Tasa: ", format(round(Tasa, 1), decimal.mark = ","), " / 100k",
        "<br>Defunciones: ", format(Fallecidos, big.mark = ".", decimal.mark = ","),
        "<br>Población: ", format(round(Poblacion), big.mark = ".", decimal.mark = ",")
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Año", dtick = 1),
        yaxis = list(title = "Tasa de la causa seleccionada (por 100.000 hab.)"),
        legend = list(orientation = "h", x = 0, y = -0.2),
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 60, r = 20, t = 10, b = 100)
      )
  })
  
  datos_pt_variacion <- reactive({
    datos_pt_ccaa() %>%
      arrange(Comunidad, Año_Num) %>%
      group_by(Comunidad) %>%
      mutate(
        Variacion_interanual = Tasa - lag(Tasa),
        Variacion_porcentual = if_else(lag(Tasa) != 0, (Tasa - lag(Tasa)) / lag(Tasa) * 100, NA_real_)
      ) %>%
      ungroup() %>%
      filter(!is.na(Variacion_interanual), is.finite(Variacion_interanual))
  })
  
  output$pt_variacion_interanual <- renderPlotly({
    df <- datos_pt_variacion() %>%
      mutate(Año = as.integer(Año_Num)) %>%
      arrange(Comunidad, Año)
    
    req(nrow(df) > 0)
    
    # Mapa de intensidad: evita el solapamiento de decenas de líneas y
    # permite localizar rápidamente aumentos y descensos interanuales.
    z <- df %>%
      select(Comunidad, Año, Variacion_interanual) %>%
      tidyr::pivot_wider(names_from = Año, values_from = Variacion_interanual) %>%
      arrange(Comunidad)
    
    anos <- sort(unique(df$Año))
    mat <- as.matrix(z[, intersect(as.character(anos), names(z)), drop = FALSE])
    rownames(mat) <- z$Comunidad
    
    # OPT: vectorizado en lugar de double loop
    finitos <- is.finite(mat)
    texto <- matrix("", nrow = nrow(mat), ncol = ncol(mat))
    if (any(finitos)) {
      idx <- which(finitos, arr.ind = TRUE)
      coms <- z$Comunidad[idx[, 1]]
      ans  <- colnames(mat)[idx[, 2]]
      vals <- mat[idx]
      texto[idx] <- paste0(
        coms, "<br>Año: ", ans,
        "<br>Variación: ",
        ifelse(vals >= 0, "+", ""),
        round(vals, 2), " puntos / 100k"
      )
    }
    
    plot_ly(
      x = colnames(mat),
      y = rownames(mat),
      z = mat,
      type = "heatmap",
      text = texto,
      hoverinfo = "text",
      colorbar = list(title = "Cambio<br>/100k")
    ) %>%
      layout(
        xaxis = list(title = "Año", dtick = 1),
        yaxis = list(title = "", autorange = "reversed", automargin = TRUE),
        margin = list(l = 135, r = 70, t = 10, b = 55)
      )
  })
  
  output$pt_tabla_ccaa <- DT::renderDT({
    df <- datos_pt_variacion() %>%
      mutate(Año = as.integer(Año_Num)) %>%
      select(Comunidad, Año, Variacion_interanual) %>%
      arrange(Comunidad, Año)
    
    req(nrow(df) > 0)
    
    tabla <- df %>%
      mutate(
        Variacion = round(Variacion_interanual, 2),
        Periodo = paste0(Año - 1, "–", Año)
      ) %>%
      select(Comunidad, Periodo, Variacion) %>%
      tidyr::pivot_wider(
        names_from = Periodo,
        values_from = Variacion,
        names_prefix = "Variación "
      ) %>%
      arrange(Comunidad)
    
    DT::datatable(
      tabla,
      rownames = FALSE,
      options = list(
        pageLength = 19,
        autoWidth = TRUE,
        scrollX = TRUE,
        order = list(0, "asc")
      ),
      caption = "Variación interanual de la tasa de la causa (puntos por 100.000 habitantes)"
    ) %>%
      DT::formatRound(
        columns = grep("^Variación ", names(tabla), value = TRUE),
        digits = 2, dec.mark = ",", mark = "."
      )
  })
  
  # --- DISTRIBUCIÓN POR EDAD Y SEXO (RESUMEN GENERAL) SERVER ---
  output$ped_edad_sexo <- renderPlotly({
    # Pirámide fija con ambos sexos: hombres a la izquierda, mujeres a la derecha.
    # La fuente agrega todo el periodo 2018-2022 (sin desglose anual).
    df <- edad_com_data %>%
      group_by(Edad, Sexo) %>%
      summarise(Valor = sum(Defunciones, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Valor), Sexo %in% c("Hombres", "Mujeres"))
    req(nrow(df) > 0)
    df$Edad <- factor(df$Edad, levels = orden_edad_com)
    df_h <- df %>% filter(Sexo == "Hombres")
    df_m <- df %>% filter(Sexo == "Mujeres")
    max_v <- max(df$Valor, na.rm = TRUE) * 1.1
    ticks <- seq(-round(max_v), round(max_v), length.out = 7)
    plot_ly() %>%
      add_trace(
        data = df_h, x = ~-Valor, y = ~Edad,
        type = "bar", orientation = "h",
        name = "Hombres", marker = list(color = "#00B2A9", line = list(color = "white", width = 1)),
        customdata = ~Valor,
        hovertemplate = "<b>Edad: %{y}</b><br>Hombres: %{customdata:,.0f}<extra></extra>"
      ) %>%
      add_trace(
        data = df_m, x = ~Valor, y = ~Edad,
        type = "bar", orientation = "h",
        name = "Mujeres", marker = list(color = "#FF6F61", line = list(color = "white", width = 1)),
        hovertemplate = "<b>Edad: %{y}</b><br>Mujeres: %{x:,.0f}<extra></extra>"
      ) %>%
      layout(
        barmode = "overlay",
        bargap = 0.1,
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(
          title = "Defunciones",
          range = c(-max_v, max_v),
          tickmode = "array",
          tickvals = ticks,
          ticktext = format(abs(round(ticks)), big.mark = ".", decimal.mark = ",", trim = TRUE)
        ),
        yaxis = list(title = "Tramo de edad", categoryorder = "array", categoryarray = orden_edad_com),
        legend = list(orientation = "h", x = 0.35, y = 1.05),
        margin = list(l = 100, r = 30, t = 30, b = 60)
      )
  })
  
  # --- MORTALIDAD PREMATURA (APVP, EN CAUSAS DE DEFUNCIÓN) SERVER ---
  datos_apvp_filtrados <- reactive({
    req(input$papvp_causa, input$papvp_indicador, input$papvp_sexo)
    df <- apvp_data %>%
      filter(Causa == input$papvp_causa, Indicador == input$papvp_indicador)
    if (input$papvp_comunidad != "Todas") df <- df %>% filter(Comunidad == input$papvp_comunidad)
    if (input$papvp_sexo != "Ambos") df <- df %>% filter(Sexo == input$papvp_sexo)
    df %>% filter(is.finite(Valor))
  })
  
  output$papvp_kpi_total <- renderText({
    df <- datos_apvp_filtrados()
    if (!nrow(df)) return("-")
    format(round(sum(df$Valor, na.rm = TRUE), 2), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  
  output$papvp_kpi_max <- renderText({
    df <- datos_apvp_filtrados()
    if (!nrow(df)) return("-")
    x <- df %>% group_by(Comunidad) %>% summarise(Valor = sum(Valor, na.rm = TRUE), .groups = "drop") %>% arrange(desc(Valor)) %>% slice(1)
    paste0(x$Comunidad, " (", format(round(x$Valor, 2), big.mark = ".", decimal.mark = ","), ")")
  })
  
  output$papvp_kpi_media <- renderText({
    df <- datos_apvp_filtrados()
    if (!nrow(df)) return("-")
    format(round(mean(df$Valor, na.rm = TRUE), 2), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  
  output$papvp_ranking <- renderPlotly({
    df <- datos_apvp_filtrados()
    req(nrow(df) > 0)
    df <- df %>%
      group_by(Comunidad) %>%
      summarise(Valor = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
      arrange(Valor) %>%
      mutate(Comunidad = factor(Comunidad, levels = Comunidad))
    dom <- dominio_seguro(df$Valor)
    plot_ly(
      df, x = ~Valor, y = ~Comunidad, type = "bar", orientation = "h",
      marker = list(color = ~Valor, cmin = dom[1], cmax = dom[2],
                    colorscale = escala_plotly(PAL_YLORRD), showscale = FALSE,
                    line = list(color = "white", width = 1)),
      text = ~format(round(Valor, 1), big.mark = ".", decimal.mark = ","),
      textposition = "outside",
      hovertemplate = "<b>%{y}</b><br>Valor: %{x:,.2f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = input$papvp_indicador),
        yaxis = list(title = "", automargin = TRUE),
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 140, r = 60, b = 60, t = 20)
      )
  })

  # --- TASAS ESTANDARIZADAS POR EDAD SERVER (solo si existen los ficheros) ---
  datos_std_sel <- reactive({
    req(!is.null(datos_edad_std), input$std_ano, input$std_sexo)
    datos_edad_std$resumen %>%
      filter(Año == as.character(input$std_ano),
             Sexo == input$std_sexo) %>%
      filter(is.finite(Tasa_bruta), is.finite(Tasa_std)) %>%
      mutate(Puesto_bruta = rank(-Tasa_bruta, ties.method = "min"),
             Puesto_std = rank(-Tasa_std, ties.method = "min"),
             Cambio = Puesto_std - Puesto_bruta)
  })

  output$std_kpi_max <- renderText({
    df <- datos_std_sel()
    if (!nrow(df)) return("-")
    x <- df %>% arrange(desc(Tasa_std)) %>% slice(1)
    paste0(x$Provincia, " (", format(round(x$Tasa_std, 1), decimal.mark = ","), " / 100k)")
  })

  output$std_kpi_min <- renderText({
    df <- datos_std_sel()
    if (!nrow(df)) return("-")
    x <- df %>% arrange(Tasa_std) %>% slice(1)
    paste0(x$Provincia, " (", format(round(x$Tasa_std, 1), decimal.mark = ","), " / 100k)")
  })

  output$std_kpi_rank <- renderText({
    df <- datos_std_sel()
    if (!nrow(df)) return("-")
    x <- df %>% arrange(desc(abs(Cambio))) %>% slice(1)
    paste0(x$Provincia, " (bruta: ", x$Puesto_bruta, " → std: ", x$Puesto_std, ")")
  })

  output$std_mapa <- renderLeaflet({
    df <- datos_std_sel()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    val_max <- max(mapa_datos$Tasa_std, na.rm = TRUE)
    dominio <- if (!is.finite(val_max) || val_max <= 0) c(0, 1) else c(0, val_max)
    pal <- colorNumeric(palette = PAL_YLORRD, domain = dominio, na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>Bruta: %s<br/>Estandarizada: %s por 100k hab.",
                         mapa_datos$NAME_2,
                         format(round(mapa_datos$Tasa_bruta, 1), decimal.mark = ","),
                         format(round(mapa_datos$Tasa_std, 1), decimal.mark = ",")) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Tasa_std), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio, opacity = 0.8, title = "Tasa std / 100k", position = "bottomright")
  })

  output$std_scatter <- renderPlotly({
    df <- datos_std_sel()
    req(nrow(df) > 0)
    df <- df %>% mutate(
      etiqueta = paste0("<b>", Provincia, "</b><br>Bruta: ",
                        format(round(Tasa_bruta, 1), decimal.mark = ","),
                        "<br>Estandarizada: ",
                        format(round(Tasa_std, 1), decimal.mark = ","),
                        "<br>Puesto ", Puesto_bruta, " → ", Puesto_std)
    )
    lim <- max(c(df$Tasa_bruta, df$Tasa_std), na.rm = TRUE) * 1.05
    diag <- data.frame(x = c(0, lim), y = c(0, lim))
    plot_ly() %>%
      add_trace(data = df, x = ~Tasa_bruta, y = ~Tasa_std, text = ~etiqueta,
                type = "scatter", mode = "markers",
                marker = list(size = 9, color = "#1f77b4",
                              line = list(color = "white", width = 1.5)),
                hovertemplate = "%{text}<extra></extra>") %>%
      add_trace(data = diag, x = ~x, y = ~y, type = "scatter", mode = "lines",
                line = list(color = "#7f8c8d", width = 1.5, dash = "dash"),
                hoverinfo = "none", showlegend = FALSE) %>%
      layout(xaxis = list(title = "Tasa bruta (por 100k hab.)"),
             yaxis = list(title = "Tasa estandarizada ESP 2013 (por 100k hab.)"),
             showlegend = FALSE,
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 15, b = 50))
  })

  output$std_tabla <- DT::renderDT({
    df <- datos_std_sel() %>%
      transmute(Provincia = Provincia,
                `Tasa bruta / 100k` = Tasa_bruta,
                `Tasa estandarizada / 100k` = Tasa_std,
                `Puesto (bruta)` = Puesto_bruta,
                `Puesto (std)` = Puesto_std,
                `Cambio de puesto` = Cambio) %>%
      arrange(`Puesto (std)`)
    DT::datatable(df, options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound(c("Tasa bruta / 100k", "Tasa estandarizada / 100k"), 1, dec.mark = ",", mark = ".")
  })

  # --- EDAD Y MES POR PROVINCIA SERVER (solo si existe el fichero) ---
  q5_etiqueta <- function(e) {
    ifelse(e >= 95, "95 y más", paste0("De ", 5 * (e %/% 5), " a ", 5 * (e %/% 5) + 4))
  }

  datos_ep_edad <- reactive({
    req(!is.null(datos_edadprov), input$ep_prov, input$ep_ano)
    df <- datos_edadprov$edad %>% filter(Año == as.character(input$ep_ano))
    if (input$ep_prov != "Todas") df <- df %>% filter(Provincia == input$ep_prov)
    df %>%
      filter(Sexo %in% c("Hombres", "Mujeres")) %>%
      group_by(Edad_num, Sexo) %>%
      summarise(Def = sum(Def, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Def)) %>%
      mutate(Q5 = q5_etiqueta(Edad_num),
             Q5 = factor(Q5, levels = unique(Q5[order(Edad_num)])))
  })

  datos_ep_mes <- reactive({
    req(!is.null(datos_edadprov), input$ep_prov, input$ep_ano)
    df <- datos_edadprov$mes %>% filter(Año == as.character(input$ep_ano))
    if (input$ep_prov != "Todas") df <- df %>% filter(Provincia == input$ep_prov)
    df %>%
      filter(Sexo %in% c("Hombres", "Mujeres")) %>%
      group_by(Mes, Sexo) %>%
      summarise(Def = sum(Def, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Def)) %>%
      mutate(Mes = factor(Mes, levels = intersect(orden_meses, unique(as.character(Mes)))))
  })

  datos_ep_anual <- reactive({
    req(!is.null(datos_edadprov), input$ep_prov)
    df <- datos_edadprov$anual %>% filter(Sexo == "Ambos")
    nac <- df %>%
      group_by(Año) %>%
      summarise(Nacional = sum(Def, na.rm = TRUE), .groups = "drop")
    if (input$ep_prov != "Todas") {
      df <- df %>%
        filter(Provincia == input$ep_prov) %>%
        group_by(Año) %>%
        summarise(Provincia = sum(Def, na.rm = TRUE), .groups = "drop") %>%
        left_join(nac, by = "Año")
    } else {
      df <- nac %>% transmute(Año = Año, Provincia = Nacional, Nacional = Nacional)
    }
    df %>% filter(is.finite(Provincia)) %>% arrange(suppressWarnings(as.numeric(Año)))
  })

  output$ep_kpi_mediana <- renderText({
    df <- datos_ep_edad() %>% arrange(Edad_num)
    req(nrow(df) > 0)
    tot <- sum(df$Def, na.rm = TRUE)
    med <- df$Edad_num[which(cumsum(df$Def) >= tot / 2)[1]]
    if (!is.finite(med)) return("-")
    paste0(med, " años (", format(round(tot), big.mark = ".", decimal.mark = ","), " defunciones)")
  })

  output$ep_kpi_65 <- renderText({
    df <- datos_ep_edad()
    req(nrow(df) > 0)
    p <- sum(df$Def[df$Edad_num >= 65], na.rm = TRUE) / sum(df$Def, na.rm = TRUE) * 100
    if (!is.finite(p)) return("-")
    paste0(format(round(p, 1), decimal.mark = ","), " %")
  })

  output$ep_kpi_mes <- renderText({
    df <- datos_ep_mes()
    req(nrow(df) > 0)
    x <- df %>% group_by(Mes) %>% summarise(Def = sum(Def, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Def)) %>% slice(1)
    paste0(as.character(x$Mes), " (", format(round(x$Def), big.mark = ".", decimal.mark = ","), ")")
  })

  output$ep_piramide <- renderPlotly({
    df <- datos_ep_edad()
    req(nrow(df) > 0)
    df_h <- df %>% filter(Sexo == "Hombres")
    df_m <- df %>% filter(Sexo == "Mujeres")
    max_v <- max(df$Def, na.rm = TRUE) * 1.1
    if (!is.finite(max_v) || max_v <= 0) return(plotly_empty())
    ticks <- seq(-round(max_v), round(max_v), length.out = 7)
    plot_ly() %>%
      add_trace(data = df_h, x = ~-Def, y = ~Q5, type = "bar", orientation = "h",
                name = "Hombres", marker = list(color = "#00B2A9", line = list(color = "white", width = 1)),
                customdata = ~Def,
                hovertemplate = "<b>%{y}</b><br>Hombres: %{customdata:,.0f}<extra></extra>") %>%
      add_trace(data = df_m, x = ~Def, y = ~Q5, type = "bar", orientation = "h",
                name = "Mujeres", marker = list(color = "#FF6F61", line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{y}</b><br>Mujeres: %{x:,.0f}<extra></extra>") %>%
      layout(barmode = "overlay", bargap = 0.1,
             hoverlabel = list(bgcolor = "white"),
             xaxis = list(title = "Defunciones", range = c(-max_v, max_v), tickmode = "array",
                          tickvals = ticks,
                          ticktext = format(abs(round(ticks)), big.mark = ".", decimal.mark = ",", trim = TRUE)),
             yaxis = list(title = "Tramo de edad", categoryorder = "array",
                          categoryarray = levels(df$Q5)),
             legend = list(orientation = "h", x = 0.35, y = 1.05),
             margin = list(l = 90, r = 30, t = 30, b = 60))
  })

  output$ep_meses <- renderPlotly({
    df <- datos_ep_mes()
    req(nrow(df) > 0)
    plot_ly(df, x = ~Mes, y = ~Def, color = ~Sexo, type = "bar",
            colors = c("Hombres" = "#00B2A9", "Mujeres" = "#FF6F61"),
            marker = list(line = list(color = "white", width = 1)),
            hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:,.0f}<extra></extra>") %>%
      layout(barmode = "group",
             xaxis = list(title = "Mes", categoryorder = "array",
                          categoryarray = levels(df$Mes)),
             yaxis = list(title = "Defunciones"),
             legend = list(orientation = "h", y = -0.2),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 70, r = 20, t = 20, b = 80))
  })

  output$ep_evol <- renderPlotly({
    df <- datos_ep_anual()
    req(nrow(df) > 0)
    df <- df %>% mutate(Ano_Num = suppressWarnings(as.numeric(Año)))
    p <- plot_ly(data = df, x = ~Ano_Num, y = ~Provincia, name = "Selección",
                 type = "scatter", mode = "lines+markers",
                 line = list(color = "#1f77b4", width = 3),
                 marker = list(color = "#1f77b4", size = 7, line = list(color = "white", width = 1.5)),
                 hovertemplate = "<b>%{x}</b><br>Defunciones: %{y:,.0f}<extra></extra>")
    if (input$ep_prov != "Todas" && all(is.finite(df$Nacional))) {
      p <- p %>% add_trace(data = df, x = ~Ano_Num, y = ~Nacional, name = "Total nacional",
                           type = "scatter", mode = "lines",
                           line = list(color = "#7f8c8d", width = 2, dash = "dash"),
                           hovertemplate = "<b>%{x}</b><br>Nacional: %{y:,.0f}<extra></extra>")
    }
    p %>% layout(xaxis = list(title = "Año", tickmode = "linear", dtick = 2),
                 yaxis = list(title = "Defunciones"),
                 legend = list(orientation = "h", x = 0.25, y = 1.1),
                 hoverlabel = list(bgcolor = "white"),
                 margin = list(l = 70, r = 20, t = 30, b = 50))
  })

  output$ep_tabla <- DT::renderDT({
    df <- datos_ep_anual() %>%
      transmute(Año = Año,
                Defunciones = Provincia,
                `Total nacional` = Nacional) %>%
      arrange(desc(suppressWarnings(as.numeric(Año))))
    DT::datatable(df, options = list(pageLength = 16, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound(c("Defunciones", "Total nacional"), 0, dec.mark = ",", mark = ".")
  })

  # --- PESTAÑA 8: ESTACIONALIDAD SERVER ---
  datos_meses_filtrados <- reactive({
    req(input$pmes_causa, input$pmes_ano, input$pmes_sexo)
    df <- meses_data %>% filter(Causa == input$pmes_causa)
    if (input$pmes_ano != "Todos") df <- df %>% filter(Año == input$pmes_ano)
    if (input$pmes_sexo != "Ambos") df <- df %>% filter(Sexo == input$pmes_sexo)
    df
  })
  
  output$pmes_kpi_total <- renderText({
    df <- datos_meses_filtrados()
    if (!nrow(df)) return("-")
    format(round(sum(df$Defunciones, na.rm = TRUE), 0), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  
  output$pmes_kpi_max <- renderText({
    df <- datos_meses_filtrados() %>% group_by(Mes) %>% summarise(Defunciones = sum(Defunciones, na.rm = TRUE), .groups = "drop")
    if (!nrow(df)) return("-")
    x <- df %>% arrange(desc(Defunciones)) %>% slice(1)
    paste0(as.character(x$Mes), " (", format(round(x$Defunciones, 0), big.mark = ".", decimal.mark = ","), ")")
  })
  
  output$pmes_kpi_min <- renderText({
    df <- datos_meses_filtrados() %>% group_by(Mes) %>% summarise(Defunciones = sum(Defunciones, na.rm = TRUE), .groups = "drop")
    if (!nrow(df)) return("-")
    x <- df %>% arrange(Defunciones) %>% slice(1)
    paste0(as.character(x$Mes), " (", format(round(x$Defunciones, 0), big.mark = ".", decimal.mark = ","), ")")
  })
  
  output$pmes_evolucion <- renderPlotly({
    df <- datos_meses_filtrados()
    req(nrow(df) > 0)
    df <- df %>%
      group_by(Año, Mes) %>%
      summarise(Defunciones = sum(Defunciones, na.rm = TRUE), .groups = "drop") %>%
      mutate(Año = as.character(Año), Mes = factor(Mes, levels = orden_meses))
    p <- plot_ly(
      df, x = ~Mes, y = ~Defunciones, color = ~Año,
      type = "scatter", mode = "lines+markers",
      marker = list(line = list(color = "white", width = 1)),
      hovertemplate = "<b>%{x} %{fullData.name}</b><br>Defunciones: %{y:,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Mes", categoryorder = "array", categoryarray = orden_meses),
        yaxis = list(title = "Defunciones"),
        legend = list(orientation = "h", y = -0.15),
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 70, r = 20, b = 80, t = 20)
      )
    p
  })
  
# ---------------------------------------------------------------------------
  # PANEL EUROPEO: EFECTOS FIJOS (pais, anio, causa_grupo, sexo)
  # tasas_100k ~ efectos fijos seleccionados. Errores robustos cluster país.
  # ---------------------------------------------------------------------------
  datos_eur_reg <- reactive({
    req(input$eur_reg_causa, input$eur_reg_sexo, input$eur_reg_fe)
    df <- europa_agrupada %>%
      filter(causa_grupo == input$eur_reg_causa, sexo == input$eur_reg_sexo) %>%
      filter(is.finite(tasa_100k), tasa_100k > 0)
    if (isTRUE(input$eur_reg_log)) {
      df <- df %>% mutate(tasa_100k = log(tasa_100k))
    }
    req(nrow(df) > 10)
    df
  })
  
  modelo_eur_reg <- reactive({
    df <- datos_eur_reg()
    req(nrow(df) > 10)
    fe_terms <- input$eur_reg_fe
    # Mapear términos UI a nombres de columnas reales
    term_map <- c(pais = "pais", anio = "anio", causa = "causa_grupo", sexo = "sexo")
    fe_cols <- term_map[fe_terms]
    
    # Solo mantener efectos fijos con 2+ niveles en los datos filtrados
    fe_cols <- fe_cols[vapply(fe_cols, function(col) {
      nlevels(factor(df[[col]])) >= 2
    }, logical(1))]
    
    if (length(fe_cols) == 0) {
      formula_str <- "tasa_100k ~ 1"
    } else {
      formula_str <- paste("tasa_100k ~", paste(fe_cols, collapse = " + "))
    }
    lm(as.formula(formula_str), data = df)
  })
  
  # Errores robustos cluster país (Arellano, 1987) - implementación manual
  coef_cluster_pais <- function(fit, df) {
    if (!"pais" %in% names(df)) return(NULL)
    X <- model.matrix(fit)
    u <- residuals(fit)
    paises <- df$pais
    n <- nrow(X)
    k <- ncol(X)
    G <- length(unique(paises))
    
    # Calcular meat matrix: sum_g (X_g' u_g) (X_g' u_g)'
    # Para cada cluster (país), sumar X_i * u_i sobre observaciones del cluster
    uX <- u * X  # n x k matrix
    # Sumar por país usando split
    uX_by_country <- split(as.data.frame(uX), paises)
    uX_sums <- lapply(uX_by_country, colSums)
    uX_mat <- do.call(rbind, uX_sums)
    
    meat <- crossprod(uX_mat)
    bread <- solve(crossprod(X))
    # Factor de corrección de grados de libertad
    dfc <- (G / (G - 1)) * (n / (n - k))
    vcov_cl <- dfc * bread %*% meat %*% bread
    # pmax: redondeos numéricos pueden dar varianzas ligeramente negativas
    se <- sqrt(pmax(diag(vcov_cl), 0))
    coefs <- coef(fit)
    tval <- coefs / se
    pval <- 2 * pt(abs(tval), df = G - 1, lower.tail = FALSE)
    data.frame(
      term = names(coefs),
      estimate = unname(coefs),
      std.error = unname(se),
      statistic = unname(tval),
      p.value = unname(pval),
      stringsAsFactors = FALSE
    )
  }
  
  coef_table <- reactive({
    fit <- modelo_eur_reg()
    df <- datos_eur_reg()
    coef_cluster_pais(fit, df)
  })
  
  # Diagnostics
  diag_plots <- reactive({
    fit <- modelo_eur_reg()
    df <- datos_eur_reg()
    req(nrow(df) > 10)
    df$.fitted <- fitted(fit)
    df$.resid <- residuals(fit)
    df$.stdresid <- rstandard(fit)
    list(fit = fit, df = df)
  })
  
  # Predictions by country for trends plot
  pred_by_country <- reactive({
    fit <- modelo_eur_reg()
    df <- datos_eur_reg()
    req(nrow(df) > 10)
    newdata <- expand.grid(
      pais = unique(df$pais),
      anio = unique(df$anio),
      causa_grupo = unique(df$causa_grupo),
      sexo = unique(df$sexo)
    )
    newdata <- newdata[newdata$causa_grupo == input$eur_reg_causa & newdata$sexo == input$eur_reg_sexo, ]
    newdata$.fitted <- predict(fit, newdata = newdata)
    newdata$pais_es <- traducir_pais(newdata$pais)
    newdata
  })
  
  output$eur_reg_r2 <- renderText({
    fit <- modelo_eur_reg()
    r2 <- summary(fit)$adj.r.squared
    formatC(r2, format = "f", digits = 3, decimal.mark = ",")
  })
  
  output$eur_reg_n <- renderText({
    df <- datos_eur_reg()
    format(nrow(df), big.mark = ".", decimal.mark = ",")
  })
  
  output$eur_reg_paises <- renderText({
    df <- datos_eur_reg()
    length(unique(df$pais))
  })
  
  output$eur_reg_summary <- renderPrint({
    summary(modelo_eur_reg())
  })
  
  output$eur_reg_coef <- DT::renderDT({
    ct <- coef_table()
    req(!is.null(ct), nrow(ct) > 0)
    DT::datatable(ct,
      options = list(pageLength = 20, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      DT::formatRound(c("estimate", "std.error", "statistic"), 4, dec.mark = ",", mark = ".") %>%
      DT::formatRound("p.value", 4, dec.mark = ",", mark = ".")
  })
  
  output$eur_reg_diag1 <- renderPlotly({
    d <- diag_plots()
    req(!is.null(d))
    plot_ly(d$df, x = ~.fitted, y = ~.resid,
            type = "scatter", mode = "markers",
            marker = list(color = "#1f77b4", size = 5, opacity = 0.6),
            hovertemplate = "<b>%{fullData.name}</b><br>Ajustado: %{x:.2f}<br>Residuo: %{y:.2f}<extra></extra>") %>%
      add_trace(x = range(d$df$.fitted), y = c(0, 0), type = "scatter", mode = "lines",
                line = list(color = "#e74c3c", width = 2, dash = "dash"),
                showlegend = FALSE, hoverinfo = "skip") %>%
      layout(xaxis = list(title = "Valores ajustados"),
             yaxis = list(title = "Residuos"),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 20, b = 50))
  })
  
  output$eur_reg_diag2 <- renderPlotly({
    d <- diag_plots()
    req(!is.null(d))
    qq <- qqnorm(d$df$.stdresid, plot.it = FALSE)
    ok <- is.finite(qq$x) & is.finite(qq$y)
    qx <- qq$x[ok]
    qy <- qq$y[ok]
    req(length(qx) > 2)
    plot_ly(x = qx, y = qy,
            type = "scatter", mode = "markers",
            marker = list(color = "#1f77b4", size = 5, opacity = 0.6),
            hovertemplate = "Teórico: %{x:.2f}<br>Muestral: %{y:.2f}<extra></extra>") %>%
      add_trace(x = range(qx), y = range(qx), type = "scatter", mode = "lines",
                line = list(color = "#e74c3c", width = 2, dash = "dash"),
                showlegend = FALSE, hoverinfo = "skip") %>%
      layout(xaxis = list(title = "Cuantiles teóricos (Normal)"),
             yaxis = list(title = "Cuantiles muestrales"),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 20, b = 50))
  })
  
output$eur_reg_trends <- renderPlotly({
    d <- pred_by_country()
    req(nrow(d) > 0)
    npaises <- dplyr::n_distinct(d$pais_es)
    plot_ly(d, x = ~anio, y = ~.fitted, color = ~pais_es,
            colors = grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                                  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(max(npaises, 1)),
            type = "scatter", mode = "lines+markers",
            hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Predicho: %{y:.1f}<extra></extra>") %>%
      layout(xaxis = list(title = "Año", dtick = 1),
             yaxis = list(title = "Tasa predicha (por 100k hab.)"),
             legend = list(orientation = "h", x = 0, y = -0.2),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 20, b = 100))
  })
  
  # ---------------------------------------------------------------------------
  # REGRESIÓN ENTRE CAUSAS (tasas provinciales por 100.000 hab.)
  # ---------------------------------------------------------------------------
  construir_datos_modelo_p32 <- function(causa_x, causa_y, anio, sexo, usar_log = FALSE) {
    if (identical(causa_x, causa_y)) return(data.frame())
    df <- causas_provinciales %>%
      filter(Año == as.character(anio), Defunción %in% c(causa_x, causa_y)) %>%
      filter(!Provincia %in% c("Ceuta", "Melilla"))
    # causas_provinciales solo trae Hombres/Mujeres: con "Ambos" se suman.
    if (!identical(sexo, "Ambos")) df <- df %>% filter(Sexo == sexo)
    if (!nrow(df)) return(data.frame())
    # Tasas ponderadas por población (coherente con el resto de la app).
    wide <- df %>%
      group_by(Provincia, Defunción) %>%
      summarise(
        Fallecidos = sum(Fallecidos, na.rm = TRUE),
        Poblacion = sum(Poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      select(Provincia, Defunción, Tasa) %>%
      tidyr::pivot_wider(names_from = Defunción, values_from = Tasa, values_fill = NA_real_)
    if (!all(c(causa_x, causa_y) %in% names(wide))) return(data.frame())
    out <- wide %>%
      transmute(
        Provincia = as.character(Provincia),
        X_val = suppressWarnings(as.numeric(.data[[causa_x]])),
        Y_val = suppressWarnings(as.numeric(.data[[causa_y]]))
      ) %>%
      filter(is.finite(X_val), is.finite(Y_val))
    if (isTRUE(usar_log)) {
      out <- out %>%
        transmute(
          Provincia = Provincia,
          X_val = suppressWarnings(log(X_val)),
          Y_val = suppressWarnings(log(Y_val))
        ) %>%
        filter(is.finite(X_val), is.finite(Y_val))
    }
    out
  }

  datos_32 <- reactive({
    req(input$p32_x, input$p32_y, input$p32_ano, input$p32_sexo)
    df <- construir_datos_modelo_p32(
      input$p32_x, input$p32_y, input$p32_ano, input$p32_sexo,
      usar_log = isTRUE(input$p32_log)
    )
    req(nrow(df) > 2)
    df
  })

  modelo_32 <- reactive({
    df <- datos_32()
    req(nrow(df) > 2)
    lm(Y_val ~ X_val, data = df)
  })

  supuestos_32 <- reactive({
    df <- datos_32()
    evaluar_supuestos(modelo_32(), df)
  })

  # Submatriz de vecinas limitada a las provincias con datos.
  listw_provincias <- function(provincias) {
    idx <- match(as.character(provincias), mapa_vecinos_base$Provincia)
    idx <- idx[is.finite(idx)]
    if (length(idx) <= 4) return(NULL)
    coords_sub <- get_vecinos()$coords[idx, , drop = FALSE]
    k_sub <- min(4, nrow(coords_sub) - 1)
    nb_sub <- spdep::knn2nb(spdep::knearneigh(coords_sub, k = k_sub))
    spdep::nb2listw(nb_sub, style = "W", zero.policy = TRUE)
  }
  
  moran_32 <- reactive({
    df <- datos_32()
    req(nrow(df) > 4)
    fit <- lm(Y_val ~ X_val, data = df)
    df$residuo <- residuals(fit)
    
    # Submatriz de vecinas limitada a las provincias con datos.
    lw <- listw_provincias(df$Provincia)
    req(!is.null(lw))
    moran <- spdep::moran.test(df$residuo, lw, zero.policy = TRUE)
    list(moran = moran, n = nrow(df))
  })
  
  output$p32_filtro_info <- renderUI({
    req(input$p32_x, input$p32_y, input$p32_ano, input$p32_sexo)
    div(class = "filter-help",
        HTML(paste0("<b>Modelo:</b> X = ", htmltools::htmlEscape(input$p32_x),
                    " · Y = ", htmltools::htmlEscape(input$p32_y),
                    " · ", htmltools::htmlEscape(as.character(input$p32_ano)),
                    " · ", htmltools::htmlEscape(input$p32_sexo),
                    if (isTRUE(input$p32_log)) " · <b>escala logarítmica</b>." else ".")))
  })

  output$p32_r2 <- renderText({
    fit <- modelo_32()
    r2 <- summary(fit)$r.squared
    formatC(r2, format = "f", digits = 2, decimal.mark = ".")
  })

  output$p32_val <- renderText({
    tab <- supuestos_32()
    if (all(grepl("^Cumplido:", tab$Conclusion))) "VÁLIDO" else "NO VÁLIDO"
  })

  output$p32_esp <- renderText({
    obj <- moran_32()
    p <- obj$moran$p.value
    if (is.finite(p) && p > 0.05) "VÁLIDO" else "NO VÁLIDO"
  })

  output$p32_scatter <- renderPlotly({
    df <- datos_32()
    req(nrow(df) > 2)
    suf_log <- if (isTRUE(input$p32_log)) " (log)" else ""
    fit <- lm(Y_val ~ X_val, data = df)
    x_seq <- seq(min(df$X_val, na.rm = TRUE), max(df$X_val, na.rm = TRUE), length.out = 100)
    df_line <- data.frame(X_val = x_seq)
    pred_ic <- suppressWarnings(predict(fit, newdata = df_line, interval = "confidence", level = 0.95))
    df_line$Y_val <- pred_ic[, "fit"]
    df_line$lwr <- pred_ic[, "lwr"]
    df_line$upr <- pred_ic[, "upr"]
    p <- plot_ly() %>%
      add_trace(
        data = df,
        x = ~X_val,
        y = ~Y_val,
        text = ~Provincia,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, color = "#1f77b4", line = list(color = "white", width = 1.5)),
        hovertemplate = "<b>%{text}</b><br>X: %{x:.2f}<br>Y: %{y:.2f}<extra></extra>"
      ) %>%
      add_trace(
        data = df_line,
        x = ~X_val,
        y = ~Y_val,
        type = "scatter",
        mode = "lines",
        line = list(color = "#e74c3c", width = 2.5),
        hoverinfo = "none"
      ) %>%
      add_trace(
        data = df_line,
        x = ~X_val,
        y = ~upr,
        type = "scatter",
        mode = "lines",
        line = list(width = 0),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
      add_trace(
        data = df_line,
        x = ~X_val,
        y = ~lwr,
        type = "scatter",
        mode = "lines",
        fill = "tonexty",
        fillcolor = "rgba(231,76,60,0.15)",
        line = list(width = 0),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(title = paste0(input$p32_x, suf_log, " (tasa de defunción por 100k hab.)"), showgrid = TRUE, gridcolor = "#E5E5E5"),
        yaxis = list(title = paste0(input$p32_y, suf_log, " (tasa de defunción por 100k hab.)"), showgrid = TRUE, gridcolor = "#E5E5E5"),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 55, r = 20, t = 10, b = 45)
      )
  })

  output$p32_supuestos <- renderTable({
    supuestos_32()
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$p32_moran_tabla <- renderTable({
    obj <- moran_32()
    p <- obj$moran$p.value
    i <- unname(obj$moran$estimate[[1]])
    esperado <- unname(obj$moran$estimate[[2]])
    z <- unname(obj$moran$statistic)
    data.frame(
      Elemento = c("Código", "Resultado", "Conclusión"),
      Valor = c(
        "spdep::moran.test(residuo, submatriz de vecinas style = 'W')",
        paste0("Moran's I = ", round(i, 4), "; Esperanza = ", round(esperado, 4),
               "; z = ", round(z, 4), "; p = ", format.pval(p, digits = 4, eps = 0.001)),
        if (is.finite(p) && p > 0.05) "p > 0,05: sin evidencia de autocorrelación espacial" else "p ≤ 0,05: evidencia de autocorrelación espacial"
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$p32_moran_conclusion <- renderText({
    obj <- moran_32()
    p <- obj$moran$p.value
    if (is.finite(p) && p > 0.05) {
      "VÁLIDO: no hay evidencia de autocorrelación espacial en los residuos."
    } else {
      "NO VÁLIDO para inferencia espacial: hay autocorrelación espacial en los residuos."
    }
  })



  # Matriz de validez (clásica + espacial) para todos los pares de causas,
  # con el año, sexo y escala seleccionados. Misma lectura que la demográfica.
  evaluar_candidatos_p32 <- function(anio, sexo, usar_log = FALSE) {
    causas <- setdiff(lista_defunciones, "Total")
    if (length(causas) < 2) return(data.frame())
    df <- causas_provinciales %>%
      filter(Año == as.character(anio), Defunción %in% causas) %>%
      filter(!Provincia %in% c("Ceuta", "Melilla"))
    if (!identical(sexo, "Ambos")) df <- df %>% filter(Sexo == sexo)
    wide <- df %>%
      group_by(Provincia, Defunción) %>%
      summarise(
        Fallecidos = sum(Fallecidos, na.rm = TRUE),
        Poblacion = sum(Poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      select(Provincia, Defunción, Tasa) %>%
      tidyr::pivot_wider(names_from = Defunción, values_from = Tasa, values_fill = NA_real_)
    cols <- intersect(causas, names(wide))
    if (length(cols) < 2) return(data.frame())
    pares <- expand.grid(x = cols, y = cols, stringsAsFactors = FALSE) %>% filter(x != y)
    resultados <- lapply(seq_len(nrow(pares)), function(i) {
      vx <- pares$x[i]
      vy <- pares$y[i]
      xv <- suppressWarnings(as.numeric(wide[[vx]]))
      yv <- suppressWarnings(as.numeric(wide[[vy]]))
      if (isTRUE(usar_log)) {
        xv <- suppressWarnings(log(xv))
        yv <- suppressWarnings(log(yv))
      }
      ok <- is.finite(xv) & is.finite(yv)
      ddf <- data.frame(
        Provincia = as.character(wide$Provincia[ok]),
        X_val = xv[ok], Y_val = yv[ok],
        stringsAsFactors = FALSE
      )
      if (nrow(ddf) <= 2) {
        return(data.frame(Variable_predictora = vx, Variable_explicativa = vy, N = nrow(ddf), Valido = FALSE, EspacialValido = NA, stringsAsFactors = FALSE))
      }
      fit <- tryCatch(lm(Y_val ~ X_val, data = ddf), error = function(e) NULL)
      if (is.null(fit)) {
        return(data.frame(Variable_predictora = vx, Variable_explicativa = vy, N = nrow(ddf), Valido = FALSE, EspacialValido = NA, stringsAsFactors = FALSE))
      }
      tab <- tryCatch(evaluar_supuestos(fit, ddf), error = function(e) NULL)
      valido <- !is.null(tab) && nrow(tab) == 4 && all(grepl("^Cumplido:", tab$Conclusion))
      # Moran con submatriz limitada a las provincias con datos.
      espacial <- tryCatch({
        ddf$residuo <- residuals(fit)
        lw <- listw_provincias(ddf$Provincia)
        if (is.null(lw)) NA
        else {
          m <- spdep::moran.test(ddf$residuo, lw, zero.policy = TRUE)
          is.finite(m$p.value) && m$p.value > 0.05
        }
      }, error = function(e) NA)
      data.frame(
        Variable_predictora = vx,
        Variable_explicativa = vy,
        N = nrow(ddf),
        Valido = valido,
        EspacialValido = espacial,
        stringsAsFactors = FALSE
      )
    })
    bind_rows(resultados)
  }

  modelos_candidatos_p32 <- reactive({
    evaluar_candidatos_p32(input$p32_ano, input$p32_sexo, usar_log = isTRUE(input$p32_log))
  }) %>% bindCache(input$p32_ano, input$p32_sexo, input$p32_log)

  output$p32_modelos_info <- renderTable({
    res <- modelos_candidatos_p32()
    causas <- setdiff(lista_defunciones, "Total")
    if (!length(causas)) return(data.frame())

    # Matriz: filas = causa explicativa (Y), columnas = causa predictora (X).
    mat <- matrix("—", nrow = length(causas), ncol = length(causas),
                  dimnames = list(causas, causas))

    if (nrow(res)) {
      # OPT: vectorizado con matching por índices
      m <- match(res$Variable_explicativa, causas)
      n <- match(res$Variable_predictora, causas)
      ok <- !is.na(m) & !is.na(n) & m != n
      if (any(ok)) {
        idx <- cbind(m[ok], n[ok])
        v <- res$Valido[ok]
        e <- res$EspacialValido[ok]
        mat[idx] <- ifelse(v & e, "✓ ●", ifelse(v, "✓", ifelse(e, "●", "")))
      }
    }

    out <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
    out <- cbind(`Causa explicativa (Y)` = rownames(out), out)
    rownames(out) <- NULL
    out
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs", width = "100%")
  
  # ---------------------------------------------------------------------------
  # ANÁLISIS MULTIVARIANTE: PCA + K-MEANS SOBRE TASAS POR CAUSA (PROVINCIAS)
  # ---------------------------------------------------------------------------
  pal_cluster <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")

  # Lecturas directas del precomputo global (sin cadenas de cálculo en sesión).
  datos_pm_sel <- reactive({
    req(input$pm_ano, input$pm_sexo)
    obj <- pm_cache[[pm_key(input$pm_ano, input$pm_sexo)]]
    req(!is.null(obj))
    obj
  })

  datos_pm_wide <- reactive({
    obj <- datos_pm_sel()
    list(mat = obj$mat, n_causas_ini = obj$n_causas_ini)
  })

  pca_pm <- reactive({
    datos_pm_sel()$pca
  })

  km_pm <- reactive({
    obj <- datos_pm_sel()
    k <- min(max(2L, as.integer(input$pm_k)), 6L)
    req(k >= 2)
    km <- obj$kms[[k - 1L]]
    req(!is.null(km))
    km
  })

  output$pm_info <- renderUI({
    w <- datos_pm_wide()
    div(class = "filter-help", HTML(paste0(
      "<b>Datos:</b> ", nrow(w$mat), " provincias × ", ncol(w$mat),
      " causas (", w$n_causas_ini - ncol(w$mat), " sin variación). ",
      "PCA centrado y escalado · k-means sobre PC1-PC2."
    )))
  })

  output$pm_kpi_var <- renderText({
    p <- pca_pm()
    v <- summary(p)$importance
    paste0(format(round(100 * sum(v["Proportion of Variance", 1:min(2, ncol(v))]), 1), decimal.mark = ","), " %")
  })

  output$pm_kpi_k <- renderText({
    as.character(nrow(km_pm()$centers))
  })

  output$pm_kpi_n <- renderText({
    w <- datos_pm_wide()
    paste0(nrow(w$mat), " × ", ncol(w$mat))
  })

  output$pm_scree <- renderPlotly({
    p <- pca_pm()
    v <- summary(p)$importance
    df <- data.frame(
      PC_num = seq_len(ncol(v)),
      PC = paste0("PC", seq_len(ncol(v))),
      Var = 100 * v["Proportion of Variance", ]
    )
    df$Cum <- cumsum(df$Var)
    # Eje X numérico con etiquetas (los ejes categóricos fallaban en algunos navegadores).
    plot_ly() %>%
      add_trace(data = df, x = ~PC_num, y = ~Var, type = "bar",
                marker = list(color = ~Var, colorscale = "Blues", showscale = FALSE,
                              line = list(color = "rgba(0,0,0,0.15)", width = 1)),
                text = ~PC,
                hovertemplate = "<b>%{text}</b><br>Varianza: %{y:.1f} %<extra></extra>") %>%
      add_trace(data = df, x = ~PC_num, y = ~Cum, type = "scatter", mode = "lines+markers",
                yaxis = "y2", line = list(color = "#e74c3c", width = 2.5),
                marker = list(color = "#e74c3c", size = 7),
                text = ~PC,
                hovertemplate = "<b>%{text}</b><br>Acumulado: %{y:.1f} %<extra></extra>") %>%
      layout(xaxis = list(title = "Componente", tickmode = "array",
                          tickvals = df$PC_num, ticktext = df$PC),
             yaxis = list(title = "% de varianza explicada"),
             yaxis2 = list(title = "% acumulado", overlaying = "y", side = "right",
                           range = c(0, 100), showgrid = FALSE),
             margin = list(l = 55, r = 60, t = 20, b = 50))
  })

  output$pm_biplot <- renderPlotly({
    p <- pca_pm()
    km <- km_pm()
    k <- nrow(km$centers)
    cols <- pal_cluster[seq_len(k)]
    sc <- as.data.frame(p$x[, 1:2])
    sc$Provincia <- rownames(p$x)
    sc$Cluster <- factor(km$cluster, levels = seq_len(k),
                         labels = paste0("Cluster ", seq_len(k)))
    rot <- p$rotation[, 1:2, drop = FALSE]
    mult <- min(diff(range(sc$PC1)), diff(range(sc$PC2))) /
      max(abs(rot), na.rm = TRUE) * 0.7
    if (!is.finite(mult) || mult <= 0) mult <- 1
    fl <- as.data.frame(rot * mult)
    fl$Causa <- rownames(rot)
    # Flechas como segmentos NaN-separados (una sola traza de líneas estándar).
    seg <- data.frame(x = as.vector(rbind(0, fl$PC1, NA_real_)),
                      y = as.vector(rbind(0, fl$PC2, NA_real_)))
    plot_ly() %>%
      add_trace(data = sc, x = ~PC1, y = ~PC2, color = ~Cluster, colors = cols,
                text = ~Provincia, type = "scatter", mode = "markers",
                marker = list(size = 9),
                hovertemplate = "<b>%{text}</b><br>%{fullData.name}<br>PC1: %{x:.2f}<br>PC2: %{y:.2f}<extra></extra>") %>%
      add_trace(data = seg, x = ~x, y = ~y, type = "scatter", mode = "lines",
                line = list(color = "#7f8c8d", width = 1.5, dash = "dot"),
                hoverinfo = "none", showlegend = FALSE) %>%
      add_text(data = fl, x = ~PC1, y = ~PC2, text = ~Causa,
               textfont = list(size = 10, color = "#2c3e50"),
               hoverinfo = "none", showlegend = FALSE) %>%
      layout(xaxis = list(title = "PC1", zeroline = TRUE, showgrid = TRUE, gridcolor = "#E5E5E5"),
             yaxis = list(title = "PC2", zeroline = TRUE, showgrid = TRUE, gridcolor = "#E5E5E5"),
             legend = list(orientation = "h", x = 0.2, y = -0.18),
             margin = list(l = 55, r = 20, t = 20, b = 90))
  })

  output$pm_mapa <- renderLeaflet({
    p <- pca_pm()
    km <- km_pm()
    k <- nrow(km$centers)
    cols <- pal_cluster[seq_len(k)]
    df <- data.frame(Provincia = rownames(p$x),
                     Cluster = factor(km$cluster, levels = seq_len(k),
                                      labels = paste0("Cluster ", seq_len(k))),
                     stringsAsFactors = FALSE)
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    pal <- leaflet::colorFactor(palette = cols, domain = levels(df$Cluster), na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>%s",
                         mapa_datos$NAME_2,
                         ifelse(is.na(mapa_datos$Cluster), "Sin datos", as.character(mapa_datos$Cluster))) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Cluster), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = ~Cluster, opacity = 0.8, title = "Cluster", position = "bottomright")
  })

  output$pm_cor <- renderPlotly({
    m <- datos_pm_sel()$cor
    # Ejes numéricos con etiquetas (los categóricos fallaban en algunos navegadores).
    labs <- stringr::str_wrap(colnames(m), width = 14)
    n <- nrow(m)
    ord <- rev(seq_len(n))
    m <- m[ord, , drop = FALSE]
    labs_y <- labs[ord]
    # array(): paste0 aplana matrices; se restaura la dimensión para que
    # cada celda lleve su etiqueta (orden column-major, igual que z).
    hov <- array(
      paste0(outer(labs_y, labs, FUN = function(a, b) paste0(a, " × ", b)),
             "<br>r = ", format(round(m, 2), nsmall = 2, decimal.mark = ",")),
      dim = dim(m)
    )
    plot_ly(x = seq_len(ncol(m)), y = rev(seq_len(n)), z = m, type = "heatmap",
            colorscale = "RdBu", zmin = -1, zmax = 1, showscale = TRUE,
            colorbar = list(title = "r"),
            text = hov,
            hovertemplate = "%{text}<extra></extra>") %>%
      layout(xaxis = list(title = "", tickmode = "array", tickvals = seq_len(ncol(m)),
                          ticktext = labs, tickfont = list(size = 9)),
             yaxis = list(title = "", tickmode = "array", tickvals = rev(seq_len(n)),
                          ticktext = labs[ord], tickfont = list(size = 9)),
             margin = list(l = 150, r = 40, t = 20, b = 100))
  })

  output$pm_perfil <- renderTable({
    p <- pca_pm()
    km <- km_pm()
    w <- datos_pm_wide()
    df <- as.data.frame(w$mat)
    df$Cluster <- factor(km$cluster, levels = seq_len(nrow(km$centers)),
                         labels = paste0("Cluster ", seq_len(nrow(km$centers))))
    df %>%
      group_by(Cluster) %>%
      summarise(N = dplyr::n(), across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
      arrange(Cluster) %>%
      mutate(across(where(is.numeric), ~ round(.x, 1)))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs")

  # Forzar renderizado aunque la pestaña no esté activa
  for (nm in c("pm_info", "pm_kpi_var", "pm_kpi_k", "pm_kpi_n",
               "pm_scree", "pm_biplot", "pm_mapa", "pm_cor", "pm_perfil")) {
    outputOptions(output, nm, suspendWhenHidden = FALSE)
  }
}

# ==============================================================================
# 5. EJECUCIÓN DE LA APLICACIÓN
# ==============================================================================
# Validaciones mínimas para fallar con un mensaje claro si faltan datos.
if (!nrow(copia_causas)) warning("No se han cargado datos de causas de defunción de España.")
if (!nrow(copia_func)) warning("No se han cargado datos de funciones demográficas.")
if (!nrow(europa_agrupada)) warning("No se han cargado datos europeos o no se han podido agrupar las causas.")

shinyApp(ui = ui, server = server)





