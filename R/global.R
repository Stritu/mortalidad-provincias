suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(dplyr)
  library(sf)
  library(bslib)
  library(plotly)
})

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
  col <- as.character(trimws(col))
  # OPT: lookup nominal en vez de case_match() (deprecado en dplyr 1.2.0).
  corr <- c("Coruña, A" = "A Coruña",
            "Coruña (A)" = "A Coruña",
            "La Coruña" = "A Coruña",
            "A Coruna" = "A Coruña",
            "Araba/Álava" = "Álava",
            "Araba / Álava" = "Álava",
            "Alicante/Alacant" = "Alicante",
            "Balears, Illes" = "Baleares",
            "Illes Balears" = "Baleares",
            "Castellón/Castelló" = "Castellón",
            "Ciudad Real" = "Ciudad Real",
            "Gipuzkoa" = "Guipúzcoa",
            "Rioja, La" = "La Rioja",
            "Rioja" = "La Rioja",
            "Palmas, Las" = "Las Palmas",
            "Santa Cruz de Tenerife" = "Santa Cruz de Tenerife",
            "Valencia/València" = "Valencia",
            "Valencia/ValÃ¨ncia" = "Valencia",
            "Bizkaia" = "Vizcaya")
  out <- unname(corr[col])
  out[is.na(out)] <- col[is.na(out)]
  out
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
  .obj_res <- cache_cargar(c("causas_defunciones.csv", "funciones.csv"), .rds_res,
                           sin_fuente_ok = TRUE)
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

# Nombre corto de CCAA para etiquetas (clave = canónico de normalizar_ccaa).
ccaa_corto <- c(
  "Andalucía" = "Andalucía", "Aragón" = "Aragón", "Asturias" = "Asturias",
  "Canarias" = "Canarias", "Cantabria" = "Cantabria",
  "Castilla - La Mancha" = "Castilla-La Mancha", "Castilla y León" = "Castilla y León",
  "Cataluña" = "Cataluña", "Ceuta" = "Ceuta", "Comunidad Valenciana" = "Valencia",
  "Extremadura" = "Extremadura", "Galicia" = "Galicia", "Islas Baleares" = "Baleares",
  "La Rioja" = "La Rioja", "Madrid" = "Madrid", "Melilla" = "Melilla",
  "Murcia" = "Murcia", "Navarra" = "Navarra", "País Vasco" = "País Vasco"
)

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

# Mapa por comunidad autónoma (19 CCAA, para los indicadores que ya vienen
# agregados por CCAA: renta, médicos e índice). Misma caché permanente.
mapa_ccaa <- local({
  .rds_ccaa <- file.path("cache", "cache_mapa_ccaa.rds")
  .obj_ccaa <- cache_cargar(character(0), .rds_ccaa)
  if (!is.null(.obj_ccaa)) return(.obj_ccaa)
  .obj_ccaa <- mapSpain::esp_get_ccaa() %>%
    st_transform(4326) %>%
    mutate(Comunidad = normalizar_ccaa(ine.ccaa.name))
  cache_guardar(.obj_ccaa, character(0), .rds_ccaa)
  .obj_ccaa
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
  obj <- cache_cargar(ftes, rds, sin_fuente_ok = TRUE)
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
# RENTA (provincia × año × indicador) Y MÉDICOS (CCAA × año).
# (normalizar_ccaa vive arriba, junto a los mapas, porque mapa_ccaa lo necesita)
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Animación temporal: avanza el selector de año mientras el checkbox play
# está marcado. Uso: animar_anos(input, session, "det_animar", "det_ano", det_anos)
# ------------------------------------------------------------------------------
animar_anos <- function(input, session, id_play, id_ano, anos, intervalo = 1200) {
  anos <- as.character(anos)
  if (!length(anos)) return(invisible(NULL))
  observe({
    req(isTRUE(input[[id_play]]))
    invalidateLater(intervalo)
    isolate({
      i <- match(input[[id_ano]], anos)
      if (is.na(i)) i <- 0
      updateSelectInput(session, id_ano, selected = anos[(i %% length(anos)) + 1])
    })
  })
}

# Insignia con el año visible dentro del mapa (útil al animar años).
control_ano <- function(mapa, ano) {
  leaflet::addControl(
    mapa,
    html = sprintf("<div class=\"map-year-badge\">%s</div>",
                   htmltools::htmlEscape(as.character(ano)[1])),
    position = "topright"
  )
}
