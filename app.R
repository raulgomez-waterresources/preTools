library(shiny)
source("preTools.R")

# ============================================================
# UI — lo que ve el usuario
# ============================================================
ui <- fluidPage(
  
  # Tipografía manuscrita para la firma personal (carga desde Google Fonts)
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Dancing+Script:wght@700&display=swap"
    )
  ),
  
  fluidRow(
    column(12, align = "center",
           tags$img(src = "logo.png", height = "100px"),
           titlePanel("preTools"),
           p("Herramienta para el completado de series de precipitación mediante regresión lineal entre estaciones meteorológicas.")
    )
  ),
  
  hr(),
  
  fluidRow(
    column(12,
           h4("Instrucciones"),
           p("El Excel debe tener la siguiente estructura, con encabezados en la primera fila:"),
           tags$ul(
             tags$li("Columna 1: Fecha"),
             tags$li("Columna 2: Estación a completar"),
             tags$li("Columnas 3 en adelante: Estaciones de apoyo")
           ),
           p("Los datos de precipitación deben ser numéricos. Los datos ausentes pueden dejarse como celdas vacías."),
           tableOutput("tabla_ejemplo")
    )
  ),
  
  # Aviso de privacidad y responsabilidad, en tono discreto
  fluidRow(
    column(12,
           tags$div(
             style = "font-size:13px; color:#6c757d; font-style:italic; border-left:3px solid #dee2e6; padding:8px 12px; margin-bottom:15px; background-color:#f8f9fa;",
             "Los archivos que subas se procesan únicamente para generar el resultado y no se almacenan de forma permanente. ",
             "preTools es una herramienta de apoyo técnico y no sustituye la validación profesional de los datos ni de los resultados obtenidos."
           )
    )
  ),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Cargar archivo"),
      fileInput("archivo", "Selecciona tu Excel (.xlsx)", accept = ".xlsx"),
      
      h4("2. Parámetros"),
      numericInput("min_n", "Mínimo de datos coincidentes", value = 100, min = 1),
      numericInput("min_r2", "R² mínimo", value = 0.5, min = 0, max = 1, step = 0.05),
      checkboxInput("limitar_negativos", "Limitar estimaciones negativas a 0", value = TRUE),
      
      actionButton("procesar", "Procesar archivo", class = "btn-primary")
    ),
    
    mainPanel(
      h4("3. Resultado"),
      textOutput("estado_texto"),
      uiOutput("avisos_ui"),
      uiOutput("resumen_ui"),
      uiOutput("descarga_ui")
    )
  ),
  
  hr(),
  
  # Firma personal / sello de marca
  fluidRow(
    column(12, align = "center",
           tags$div(
             style = "font-family:'Dancing Script', cursive; font-size:34px; color:#1f6f8b; margin-top:10px;",
             "Raúl Gómez C."
           ),
           tags$div(
             style = "font-family: Arial, sans-serif; font-size:14px; letter-spacing:1px; color:#5a8a99; margin-top:-6px; margin-bottom:20px;",
             "HIDROLOGÍA Y GESTIÓN DE RECURSOS HÍDRICOS"
           )
    )
  )
)

# ============================================================
# SERVER — la lógica
# ============================================================
server <- function(input, output, session) {
  
  resultado       <- reactiveVal(NULL)          # lo que devuelve completar_primera_estacion()
  ruta_salida_tmp <- reactiveVal(NULL)          # ruta temporal del Excel ya generado
  estado          <- reactiveVal(NULL)          # NULL / "ok" / texto de error
  avisos          <- reactiveVal(character(0))  # warnings capturados del v9
  
  output$tabla_ejemplo <- renderTable({
    data.frame(
      Fecha = c("01/01/2020", "02/01/2020", "03/01/2020"),
      ESTACION_OBJETIVO = c(12.4, NA, 4.2),
      ESTACION_APOYO_1 = c(13.1, 8.4, 3.9),
      ESTACION_APOYO_2 = c(11.8, 9.1, 4.8)
    )
  })
  
  observeEvent(input$procesar, {
    
    req(input$archivo)
    
    ext <- tolower(tools::file_ext(input$archivo$name))
    if (ext != "xlsx") {
      estado(paste0("El archivo debe ser .xlsx (se ha subido un archivo ." , ext, ")."))
      resultado(NULL)
      avisos(character(0))
      return()
    }
    
    salida_tmp <- tempfile(fileext = ".xlsx")
    avisos_capturados <- character(0)
    
    withProgress(message = "Procesando archivo...", value = 0.6, {
      
      tryCatch({
        
        res <- withCallingHandlers({
          completar_primera_estacion(
            ruta_excel        = input$archivo$datapath,
            hoja               = 1,
            min_n              = input$min_n,
            min_r2             = input$min_r2,
            limitar_negativos  = input$limitar_negativos,
            ruta_salida        = salida_tmp
          )
        }, warning = function(w) {
          avisos_capturados <<- c(avisos_capturados, conditionMessage(w))
          invokeRestart("muffleWarning")
        })
        
        resultado(res)
        ruta_salida_tmp(salida_tmp)
        estado("ok")
        avisos(avisos_capturados)
        
      }, error = function(e) {
        estado(paste0("No se ha podido procesar el archivo.\n\nMotivo: ", conditionMessage(e)))
        resultado(NULL)
        avisos(character(0))
      })
    })
  })
  
  output$estado_texto <- renderText({
    if (is.null(estado())) {
      ""
    } else if (identical(estado(), "ok")) {
      "Procesamiento completado correctamente."
    } else {
      estado()
    }
  })
  
  output$avisos_ui <- renderUI({
    if (length(avisos()) == 0) return(NULL)
    tags$div(
      style = "color:#8a6d3b; background:#fcf8e3; padding:8px; border-radius:4px; margin-top:8px;",
      tags$b("Avisos del proceso:"),
      tags$ul(lapply(avisos(), tags$li))
    )
  })
  
  output$resumen_ui <- renderUI({
    req(resultado())
    r <- resultado()
    
    datos_completados <- r$datos_completados
    col_target <- names(datos_completados)[2]
    col_estado <- names(datos_completados)[3]
    
    modelo_sel <- r$tabla_modelos[r$tabla_modelos$seleccionado, ]
    
    n_completados   <- nrow(r$registro_completados)
    n_sin_completar <- sum(datos_completados[[col_estado]] == "SIN COMPLETAR")
    
    if (nrow(modelo_sel) == 0) {
      return(tags$div(
        tags$p(tags$b("Estación objetivo: "), col_target),
        tags$p("Ningún modelo cumplió los criterios mínimos: no se ha completado ningún valor.")
      ))
    }
    
    tags$div(
      tags$p(tags$b("Archivo procesado: "), input$archivo$name),
      tags$p(tags$b("Estación objetivo: "), col_target),
      tags$p(tags$b("Estación auxiliar seleccionada: "), modelo_sel$estacion_auxiliar),
      tags$p(tags$b("R²: "), round(modelo_sel$r2, 3)),
      tags$p(tags$b("Datos utilizados: "), modelo_sel$n),
      tags$p(tags$b("Valores completados: "), n_completados),
      tags$p(tags$b("Valores sin completar: "), n_sin_completar)
    )
  })
  
  output$descarga_ui <- renderUI({
    req(resultado())
    downloadButton("descargar", "Descargar Excel generado", class = "btn-success")
  })
  
  output$descargar <- downloadHandler(
    filename = function() {
      paste0("preTools_completado_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(ruta_salida_tmp())
      file.copy(ruta_salida_tmp(), file)
    }
  )
}

shinyApp(ui = ui, server = server)