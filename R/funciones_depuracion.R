# =========================================================
# FUNCIONES DE DEPURACIÓN DE DATOS eBIRD - AVES MARINAS
# =========================================================
# Lógica de limpieza separada de la app Shiny, para poder usarla
# desde la app (app.R), desde un script o desde un notebook:
#
#   source("R/funciones_depuracion.R")
#   ebd  <- leer_ebd("data/ebd_EC-E_....txt")
#   res1 <- depurar_ebd(ebd, provincia = "Esmeraldas")
#   res2 <- validar_revision(leer_csv_flexible("base_revisada.csv"))
#
# Autor: Kevin Tapia-Rodriguez
# =========================================================

library(tidyverse)

# ---------------------------------------------------------
# 0. CONFIGURACIÓN
# ---------------------------------------------------------
# Todos los parámetros ajustables están aquí. Para agregar una
# provincia o una especie focal, basta con modificar esta lista.
config_depuracion <- function() {
  list(
    # Límites aproximados por provincia: c(lat_min, lat_max, lon_min, lon_max)
    limites_prov = list(
      "Esmeraldas"  = c( 0.20,  1.50, -80.20, -78.80),
      "Manabi"      = c(-1.70,  0.45, -81.15, -79.90),
      "Santa Elena" = c(-2.50, -1.70, -81.20, -80.55),
      "Guayas"      = c(-3.00, -2.00, -80.55, -79.70),
      "El Oro"      = c(-3.50, -3.00, -80.50, -79.75),
      "Galapagos"   = c(-1.50,  2.00, -92.00, -88.00)
    ),
    especies_focales = c("Sula variegata", "Sula nebouxii",
                         "Leucocarbo bougainvilliorum", "Larosterna inca",
                         "Ardenna grisea"),
    protocolos_prohibidos = c("Incidental", "Historical", "Banding"),
    # Umbrales de esfuerzo para listas no estructuradas
    max_duracion_min = 360,
    max_distancia_km = 10,
    max_observadores = 10,
    max_velocidad_kmh = 100,
    # Expresión para detectar dudas en SPECIES COMMENTS
    patron_dudas = "(?i)posible|\\?|no muy seguro|escuchado|solo canto|visto volando",
    columnas_esperadas = c("GLOBAL UNIQUE IDENTIFIER", "DURATION MINUTES",
                           "EFFORT DISTANCE KM", "NUMBER OBSERVERS", "LATITUDE",
                           "LONGITUDE", "PROTOCOL CODE", "OBSERVATION TYPE",
                           "APPROVED", "ALL SPECIES REPORTED", "OBSERVATION COUNT",
                           "COMMON NAME", "EXOTIC CODE", "SPECIES COMMENTS",
                           "SAMPLING EVENT IDENTIFIER", "CATEGORY",
                           "SCIENTIFIC NAME", "STATE")
  )
}

# ---------------------------------------------------------
# 1. FUNCIONES AUXILIARES
# ---------------------------------------------------------

# Agrega una nota a la columna de revisión sin borrar las anteriores
add_revision <- function(col_actual, nueva_nota) {
  if_else(is.na(col_actual), nueva_nota, paste(col_actual, nueva_nota, sep = " | "))
}

# Agrega la nota solo donde la condición es verdadera (NA se trata como FALSE).
# Permite acumular varias alertas en un mismo registro.
agregar_si <- function(col_actual, condicion, nota) {
  if_else(!is.na(condicion) & condicion, add_revision(col_actual, nota), col_actual)
}

# Une conteos de filas duplicadas: si alguno es "X", el resultado es "X"
unificar_conteos <- function(conteos) {
  if (any(conteos == "X", na.rm = TRUE)) {
    "X"
  } else {
    as.character(suppressWarnings(sum(as.numeric(conteos), na.rm = TRUE)))
  }
}

# Normaliza texto para comparar (minúsculas, sin tildes)
normalizar <- function(x) iconv(tolower(x), to = "ASCII//TRANSLIT")

# ---------------------------------------------------------
# 2. LECTURA DE ARCHIVOS
# ---------------------------------------------------------

# Lee uno o varios archivos EBD (.txt separados por tabulador)
leer_ebd <- function(rutas) {
  ebd <- purrr::map_df(rutas, ~ read_delim(.x, delim = "\t",
                                           locale = locale(encoding = "UTF-8"),
                                           col_types = cols(.default = "c"),
                                           show_col_types = FALSE,
                                           name_repair = "unique_quiet"))
  # El EBD trae un tabulador final que genera columnas vacías (...53)
  ebd[, !str_detect(names(ebd), "^\\.\\.\\.\\d+$"), drop = FALSE]
}

# Lee un CSV detectando si el separador es coma o punto y coma
leer_csv_flexible <- function(ruta) {
  primera_linea <- readLines(ruta, n = 1, warn = FALSE)
  if (str_detect(primera_linea, ";")) {
    read_delim(ruta, delim = ";", locale = locale(encoding = "UTF-8"),
               col_types = cols(.default = "c"), show_col_types = FALSE)
  } else {
    read_csv(ruta, locale = locale(encoding = "UTF-8"),
             col_types = cols(.default = "c"), show_col_types = FALSE)
  }
}

# Lee el catálogo de protocolos (opcional)
leer_catalogo_protocolos <- function(ruta) {
  read_delim(ruta, delim = "\t", show_col_types = FALSE) %>%
    select(`PROTOCOL CODE`, `PROTOCOL DESCRIPTION`)
}

# ---------------------------------------------------------
# 3. FASE 1: DEPURACIÓN INICIAL
# ---------------------------------------------------------
# Devuelve una lista con:
#   producto1 -> base completa para revisión de voluntarios
#   producto2 -> solo especies focales
#   bitacora  -> resumen de registros
depurar_ebd <- function(ebd_raw, provincia,
                        catalogo_protocolos = NULL,
                        x_revisada = FALSE,
                        n_archivos = 1,
                        config = config_depuracion()) {

  registros_originales <- nrow(ebd_raw)

  # --- Validaciones estructurales ---
  faltantes <- setdiff(config$columnas_esperadas, colnames(ebd_raw))
  if (length(faltantes) > 0) {
    stop(paste("Faltan columnas estructurales:", paste(faltantes, collapse = ", ")))
  }
  if (!provincia %in% names(config$limites_prov)) {
    stop(paste("Provincia sin límites definidos en la configuración:", provincia))
  }
  estados <- unique(na.omit(ebd_raw$STATE))
  if (!any(str_detect(normalizar(estados), normalizar(provincia)))) {
    stop(paste("¡ALTO! Seleccionaste", provincia, "pero el archivo es de:",
               paste(estados, collapse = ", ")))
  }

  # --- Tipos de datos y duplicados ---
  ebd <- ebd_raw %>%
    mutate(PROTOCOLO_CENSO = NA_character_,
           REVISION_MANUAL = NA_character_,
           across(c(`DURATION MINUTES`, `EFFORT DISTANCE KM`, `NUMBER OBSERVERS`,
                    LATITUDE, LONGITUDE),
                  ~ suppressWarnings(as.numeric(.x)))) %>%
    distinct(`GLOBAL UNIQUE IDENTIFIER`, .keep_all = TRUE)

  # --- Protocolo / proyecto de censo ---
  if (!is.null(catalogo_protocolos)) {
    ebd <- left_join(ebd, catalogo_protocolos, by = "PROTOCOL CODE")
  }
  # NOTA: en el EBD actual la columna PROTOCOL NAME viene llena en todos los
  # registros, por lo que esta regla marca TODO como estructurado (ver README).
  ebd <- ebd %>%
    mutate(PROTOCOLO_CENSO = case_when(
      !is.na(`PROJECT NAMES`) & `PROJECT NAMES` != "" ~ paste("Proyecto:", `PROJECT NAMES`),
      !is.na(`PROTOCOL NAME`) ~ paste("Catálogo:", `PROTOCOL NAME`),
      TRUE ~ NA_character_))

  # --- Filtros de esfuerzo (solo listas no estructuradas) ---
  tipos_validos <- ebd %>%
    filter(!`OBSERVATION TYPE` %in% config$protocolos_prohibidos) %>%
    pull(`OBSERVATION TYPE`) %>% unique()

  ebd_filtrado <- ebd %>%
    mutate(es_estructurado = !is.na(PROTOCOLO_CENSO),
           velocidad_kmh = if_else(!is.na(`EFFORT DISTANCE KM`) & !is.na(`DURATION MINUTES`) &
                                     `DURATION MINUTES` > 0,
                                   `EFFORT DISTANCE KM` / (`DURATION MINUTES` / 60),
                                   NA_real_)) %>%
    filter(es_estructurado |
             (`APPROVED` != "0" &
                `OBSERVATION TYPE` %in% tipos_validos &
                (!is.na(`DURATION MINUTES`) & `DURATION MINUTES` <= config$max_duracion_min) &
                (!is.na(`EFFORT DISTANCE KM`) & `EFFORT DISTANCE KM` <= config$max_distancia_km) &
                (!is.na(`NUMBER OBSERVERS`) & `NUMBER OBSERVERS` <= config$max_observadores) &
                (is.na(velocidad_kmh) | velocidad_kmh <= config$max_velocidad_kmh))) %>%
    mutate(REVISION_MANUAL = agregar_si(REVISION_MANUAL,
                                        !es_estructurado & `ALL SPECIES REPORTED` == "0",
                                        "ALL SPECIES REPORTED = 0"),
           REVISION_MANUAL = agregar_si(REVISION_MANUAL,
                                        !es_estructurado &
                                          `OBSERVATION TYPE` %in% c("Traveling", "eBird Pelagic Protocol") &
                                          `EFFORT DISTANCE KM` == 0,
                                        "Distancia 0 en ruta/pelágico"))

  if (nrow(ebd_filtrado) == 0) stop("Aviso: Ningún registro sobrevivió a los filtros de esfuerzo.")

  # --- Alertas geográficas (jerárquicas: se anota solo la más grave) ---
  caja <- config$limites_prov[[provincia]]
  galap <- config$limites_prov[["Galapagos"]]

  ebd_filtrado <- ebd_filtrado %>%
    mutate(REVISION_MANUAL = case_when(
      LATITUDE == 0 & LONGITUDE == 0 ~
        add_revision(REVISION_MANUAL, "Coordenadas 0,0"),
      provincia != "Galapagos" &
        LATITUDE >= galap[1] & LATITUDE <= galap[2] &
        LONGITUDE >= galap[3] & LONGITUDE <= galap[4] ~
        add_revision(REVISION_MANUAL, "Cae en Galápagos"),
      LATITUDE < caja[1] | LATITUDE > caja[2] | LONGITUDE < caja[3] | LONGITUDE > caja[4] ~
        add_revision(REVISION_MANUAL, paste("Fuera de", provincia)),
      TRUE ~ REVISION_MANUAL))

  # --- Alertas de contenido (independientes: se acumulan todas) ---
  # Antes estaban en un solo case_when y solo se anotaba la primera.
  ebd_filtrado <- ebd_filtrado %>%
    mutate(REVISION_MANUAL = agregar_si(REVISION_MANUAL,
                                        !x_revisada & `OBSERVATION COUNT` == "X",
                                        paste("Conteo X en", `COMMON NAME`)),
           REVISION_MANUAL = agregar_si(REVISION_MANUAL,
                                        !is.na(`EXOTIC CODE`) & `EXOTIC CODE` != "",
                                        paste("Exotic Code en", `COMMON NAME`)),
           REVISION_MANUAL = agregar_si(REVISION_MANUAL,
                                        str_detect(tolower(`SPECIES COMMENTS`), config$patron_dudas),
                                        "Duda en comentarios"))

  # --- Propagar alertas a toda la lista (checklist) ---
  ebd_filtrado <- ebd_filtrado %>%
    group_by(`SAMPLING EVENT IDENTIFIER`) %>%
    mutate(listado_necesita_revision = any(!is.na(REVISION_MANUAL)),
           REVISION_MANUAL = if_else(listado_necesita_revision,
                                     paste(unique(na.omit(REVISION_MANUAL)), collapse = " || "),
                                     NA_character_)) %>%
    ungroup() %>%
    select(-es_estructurado, -velocidad_kmh, -listado_necesita_revision)

  # --- Unificar subespecies / filas repetidas por especie y lista ---
  base_limpia <- ebd_filtrado %>%
    group_by(`SAMPLING EVENT IDENTIFIER`, `COMMON NAME`) %>%
    arrange(desc(CATEGORY == "species")) %>%
    mutate(conteo_unificado = unificar_conteos(`OBSERVATION COUNT`),
           filas_fusionadas = n()) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(`OBSERVATION COUNT` = conteo_unificado,
           CATEGORY = "species",
           REVISION_MANUAL = if_else(filas_fusionadas > 1,
                                     add_revision(REVISION_MANUAL, "Unificación aplicada"),
                                     REVISION_MANUAL)) %>%
    select(-conteo_unificado, -filas_fusionadas)

  # --- Columnas vacías para los voluntarios ---
  producto1 <- base_limpia %>%
    mutate(across(everything(), as.character),
           REVISION = NA_character_,
           COMENTARIO_E = NA_character_)

  producto2 <- producto1 %>% filter(`SCIENTIFIC NAME` %in% config$especies_focales)

  bitacora <- tibble(
    "Métrica" = c("Archivos crudos analizados", "Registros originales totales",
                "Sobrevivientes (Producto 1 Completo)", "Sobrevivientes (Producto 2 Focales)"),
    Cantidad = as.character(c(n_archivos, registros_originales,
                              nrow(producto1), nrow(producto2)))
  )

  list(producto1 = producto1, producto2 = producto2, bitacora = bitacora)
}

# ---------------------------------------------------------
# 4. FASE 2: VALIDACIÓN POST-REVISIÓN
# ---------------------------------------------------------
# base_in: base revisada por voluntarios (REVISION = R, E o V)
# eliminados_previo: (opcional) registro histórico de eliminados
validar_revision <- function(base_in, eliminados_previo = NULL) {

  if (!all(c("REVISION", "COMENTARIO_E") %in% colnames(base_in))) {
    stop("El archivo no tiene las columnas 'REVISION' o 'COMENTARIO_E'.")
  }

  base_in <- base_in %>% mutate(REVISION = str_trim(toupper(REVISION)))

  nuevos_eliminados <- base_in %>% filter(REVISION == "E")

  if (is.null(eliminados_previo)) eliminados_previo <- tibble()

  base_eliminada <- bind_rows(nuevos_eliminados, eliminados_previo) %>%
    distinct(`GLOBAL UNIQUE IDENTIFIER`, .keep_all = TRUE)

  base_verificada <- base_in %>%
    filter(is.na(REVISION) | REVISION != "E") %>%
    mutate(REVISION = if_else(!is.na(REVISION) & REVISION == "R", "V", REVISION))

  bitacora <- tibble(
    "Métrica" = c("Filas totales en el archivo subido HOY",
                "Nuevos Datos Eliminados hoy ('E')",
                "Nuevos Datos Verificados hoy ('R' -> 'V')",
                "--------",
                "TOTAL HISTÓRICO DE ELIMINADOS ACUMULADOS",
                "TOTAL HISTÓRICO DE VERIFICADOS ('V' totales)",
                "TOTAL BASE VERIFICADA FINAL (Lista para usar)"),
    Cantidad = as.character(c(nrow(base_in),
                              nrow(nuevos_eliminados),
                              sum(base_in$REVISION == "R", na.rm = TRUE),
                              "--------",
                              nrow(base_eliminada),
                              sum(base_verificada$REVISION == "V", na.rm = TRUE),
                              nrow(base_verificada)))
  )

  list(base_verificada = base_verificada, base_eliminada = base_eliminada, bitacora = bitacora)
}
