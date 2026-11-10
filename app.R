# =========================================================
# DEPURADOR INTERACTIVO DE DATOS eBIRD - AVES MARINAS
# =========================================================
# Interfaz Shiny. La lógica de limpieza vive en R/funciones_depuracion.R
# Ejecutar con: shiny::runApp()  (desde la carpeta del repositorio)
# Autor: Kevin Tapia-Rodriguez
# =========================================================
library(shiny)
library(tidyverse)
source("R/funciones_depuracion.R")

# Ampliamos el límite de subida a 3 Gigabytes
options(shiny.maxRequestSize = 3000 * 1024^2)

config <- config_depuracion()

# ==========================================
# 1. INTERFAZ DE USUARIO (UI)
# ==========================================
ui <- fluidPage(
  titlePanel("Depurador Interactivo de Datos eBird - Aves Marinas"),

  tabsetPanel(
    # -------------------------------------------------------------------
    # PESTAÑA 1: DEPURACIÓN INICIAL
    # -------------------------------------------------------------------
    tabPanel("Fase 1: Depuración Inicial",
             sidebarLayout(
               sidebarPanel(
                 h4("Configuración Inicial"),
                 fileInput("archivo_protocolos", "1. (Opcional) Sube el catálogo de protocolos (Protocols.txt):",
                           accept = c(".txt"), buttonLabel = "Buscar archivo..."),

                 selectInput("provincia", "2. Selecciona la Provincia a trabajar:",
                             choices = names(config$limites_prov)),

                 hr(),
                 h4("Archivos de Datos"),
                 fileInput("archivos_nuevos", "3. Sube los archivos EBD NUEVOS (.txt):",
                           multiple = TRUE, accept = c(".txt"),
                           buttonLabel = "Archivos nuevos..."),

                 h5("Validación de Datos (Paso 5)"),
                 checkboxInput("x_revisada",
                               "Marcar: Los registros con conteo 'X' ya fueron revisados manualmente y no requieren alerta.",
                               value = FALSE),

                 hr(),
                 actionButton("procesar_f1", "4. Ejecutar Depuración Inicial",
                              class = "btn-primary", style = "width: 100%;"),

                 hr(),
                 uiOutput("botones_descarga_f1")
               ),

               mainPanel(
                 h4("Estado del Proceso (Fase 1)"),
                 verbatimTextOutput("estado_proceso_f1"),
                 br(),
                 h4("Bitácora de Depuración Generada"),
                 tableOutput("tabla_bitacora_f1")
               )
             )
    ),

    # -------------------------------------------------------------------
    # PESTAÑA 2: VALIDACIÓN POST-REVISIÓN (CON MEMORIA HISTÓRICA)
    # -------------------------------------------------------------------
    tabPanel("Fase 2: Validación Post-Revisión",
             sidebarLayout(
               sidebarPanel(
                 h4("Ingreso de Datos Revisados"),
                 p("Asegúrate de que los voluntarios hayan llenado la columna REVISION con R (Revisar), E (Eliminar) o V (Verificado)."),

                 fileInput("archivo_revisado", "1. Sube la base de datos ya revisada (.csv):",
                           multiple = FALSE, accept = c(".csv"),
                           buttonLabel = "Base Revisada..."),

                 fileInput("eliminados_previo", "2. (Opcional) Sube tu Registro de Eliminados anterior (.csv) para acumularlos:",
                           multiple = FALSE, accept = c(".csv"),
                           buttonLabel = "Eliminados Históricos..."),

                 hr(),
                 actionButton("procesar_f2", "3. Ejecutar Filtro y Verificación",
                              class = "btn-success", style = "width: 100%;"),

                 hr(),
                 uiOutput("botones_descarga_f2")
               ),

               mainPanel(
                 h4("Estado de la Validación (Fase 2)"),
                 verbatimTextOutput("estado_proceso_f2"),
                 br(),
                 h4("Resumen de Cambios y Acumulados"),
                 tableOutput("tabla_bitacora_f2")
               )
             )
    )
  )
)

# ==========================================
# 2. LÓGICA DEL SERVIDOR (Server)
# ==========================================
server <- function(input, output, session) {

  datos_f1 <- reactiveValues(producto1 = tibble(), producto2 = tibble(), bitacora = tibble(),
                             log_texto = "Esperando archivos en Fase 1...")
  datos_f2 <- reactiveValues(base_verificada = tibble(), base_eliminada = tibble(), bitacora = tibble(),
                             log_texto = "Esperando archivos en Fase 2...")

  # ----------------------- FASE 1 -----------------------
  observeEvent(input$procesar_f1, {
    if (is.null(input$archivos_nuevos)) {
      datos_f1$log_texto <- "¡Error! Sube al menos un archivo .txt"
      return()
    }

    tryCatch({
      datos_f1$log_texto <- "Iniciando motor de depuración interno...\n- Aplicando protocolo a archivos crudos...\n"

      ebd_raw <- leer_ebd(input$archivos_nuevos$datapath)

      catalogo <- NULL
      if (!is.null(input$archivo_protocolos)) {
        catalogo <- leer_catalogo_protocolos(input$archivo_protocolos$datapath)
      }

      res <- depurar_ebd(ebd_raw,
                         provincia = input$provincia,
                         catalogo_protocolos = catalogo,
                         x_revisada = input$x_revisada,
                         n_archivos = length(input$archivos_nuevos$name),
                         config = config)

      datos_f1$producto1 <- res$producto1
      datos_f1$producto2 <- res$producto2
      datos_f1$bitacora  <- res$bitacora
      datos_f1$log_texto <- paste(datos_f1$log_texto,
                                  "\n¡DEPURACIÓN FINALIZADA CON ÉXITO! Las casillas REVISION y COMENTARIO_E fueron creadas vacías.")

    }, error = function(e) {
      datos_f1$log_texto <- paste(datos_f1$log_texto, "\nERROR CRÍTICO DETECTADO:\n", e$message)
    })
  })

  # ----------------------- FASE 2 -----------------------
  observeEvent(input$procesar_f2, {
    req(input$archivo_revisado)

    tryCatch({
      datos_f2$log_texto <- "Iniciando análisis del archivo revisado por voluntarios...\n"

      base_in <- leer_csv_flexible(input$archivo_revisado$datapath)

      eliminados_previo <- NULL
      if (!is.null(input$eliminados_previo)) {
        datos_f2$log_texto <- paste(datos_f2$log_texto, "- Recuperando el historial de eliminados anterior...\n")
        eliminados_previo <- leer_csv_flexible(input$eliminados_previo$datapath)
      }

      res <- validar_revision(base_in, eliminados_previo)

      datos_f2$base_verificada <- res$base_verificada
      datos_f2$base_eliminada  <- res$base_eliminada
      datos_f2$bitacora        <- res$bitacora
      datos_f2$log_texto <- paste(datos_f2$log_texto,
                                  "\n¡VERIFICACIÓN Y ACUMULACIÓN COMPLETADA!\nLa base final y el registro de eliminados están listos para descargar.")

    }, error = function(e) {
      datos_f2$log_texto <- paste(datos_f2$log_texto, "\nERROR CRÍTICO DETECTADO:\n", e$message)
    })
  })

  # ----------------------- SALIDAS Y DESCARGAS -----------------------
  descarga_csv <- function(prefijo, datos) {
    downloadHandler(
      filename = function() paste0(prefijo, format(Sys.Date(), "%Y%m%d"), ".csv"),
      content  = function(file) write_excel_csv(datos(), file, na = "")
    )
  }

  # Fase 1
  output$estado_proceso_f1 <- renderText({ datos_f1$log_texto })
  output$tabla_bitacora_f1 <- renderTable({ req(datos_f1$bitacora); datos_f1$bitacora })

  output$botones_descarga_f1 <- renderUI({
    req(nrow(datos_f1$producto1) > 0)
    tagList(
      downloadButton("descargar_p1", "Descargar Producto 1 (Completo para Revisar)"), br(), br(),
      downloadButton("descargar_p2", "Descargar Producto 2 (Focales para Revisar)"), br(), br(),
      downloadButton("descargar_bitacora_f1", "Descargar Resumen")
    )
  })

  output$descargar_p1          <- descarga_csv("ebd_producto1_para_revision_",         function() datos_f1$producto1)
  output$descargar_p2          <- descarga_csv("ebd_producto2_focales_para_revision_", function() datos_f1$producto2)
  output$descargar_bitacora_f1 <- descarga_csv("Bitacora_Fase1_",                      function() datos_f1$bitacora)

  # Fase 2
  output$estado_proceso_f2 <- renderText({ datos_f2$log_texto })
  output$tabla_bitacora_f2 <- renderTable({ req(datos_f2$bitacora); datos_f2$bitacora })

  output$botones_descarga_f2 <- renderUI({
    req(nrow(datos_f2$base_verificada) > 0)
    tagList(
      downloadButton("descargar_verificada", "Descargar Base Verificada Final"), br(), br(),
      downloadButton("descargar_eliminados", "Descargar Registro de Eliminados Histórico"), br(), br(),
      downloadButton("descargar_bitacora_f2", "Descargar Resumen Fase 2")
    )
  })

  output$descargar_verificada  <- descarga_csv("ebd_Base_Verificada_",               function() datos_f2$base_verificada)
  output$descargar_eliminados  <- descarga_csv("ebd_Registro_Eliminados_Historico_", function() datos_f2$base_eliminada)
  output$descargar_bitacora_f2 <- descarga_csv("Bitacora_Fase2_",                    function() datos_f2$bitacora)
}

shinyApp(ui = ui, server = server)
