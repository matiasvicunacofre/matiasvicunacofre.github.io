########################################################################################
# Shiny - Visualización de Datos Financieros
# Laboratorio de Ciencia de Datos
# Universidad Alberto Hurtado
# Autor: Matías Vicuña Cofré
########################################################################################

# Paquetes
library(shiny)
library(DT)
library(tidyverse)
library(haven)

# Paquetes
library(shiny)
library(DT)
library(tidyverse)
library(haven)

# Predeterminar la ubicación dónde se encuentran los datos en formato .dta
setwd("C:/Users/matei/Dropbox/2- Trabajo/Trabajos_Tiago/Shiny_Tiago_R/datasets")

# Define el servidor
server <- function(input, output, session) {
  loaded_data <- reactiveVal(NULL)
  
  observe({
    if (is.null(input$base)) {
      return(NULL)
    }
    
    data <- load_data(input$base)
    
    loaded_data(data)
    
    # Actualiza las opciones de los elementos de entrada
    updateSelectizeInput(session, "nombre", choices = unique(data$nombre), server = TRUE)
    updateTextInput(session, "rut", value = "")
    updateSelectizeInput(session, "per", choices = unique(data$per))
    updateSelectizeInput(session, "carresfun", choices = unique(data$carresfun), server = TRUE)
    updateSelectizeInput(session, "date", choices = unique(data$date))
    updateSelectizeInput(session, "year", choices = unique(data$year))
    updateSelectizeInput(session, "quarter", choices = unique(data$quarter))
    updateSelectizeInput(session, "cuenta", choices = unique(data$cuenta), server = TRUE)  # Cambiado a selectizeInput
  })
  
  filtered_data <- eventReactive(input$actualizar, {
    data <- loaded_data()
    
    if (is.null(data)) {
      return(NULL)
    }
    
    filtered <- data
    
    if (!is.null(input$nombre) && length(input$nombre) > 0) {
      filtered <- filtered %>% filter(nombre %in% input$nombre)
    }
    
    if (input$rut != "") {
      filtered <- filtered %>% filter(substr(rut, 1, 8) == substr(input$rut, 1, 8))
    }
    
    if (!is.null(input$cuenta) && length(input$cuenta) > 0) {
      filtered <- filtered %>% filter(cuenta %in% input$cuenta)
    }
    
    if (!is.null(input$per) && length(input$per) > 0) {
      filtered <- filtered %>% filter(per %in% input$per)
    }
    
    if (!is.null(input$carresfun) && length(input$carresfun) > 0) {
      filtered <- filtered %>% filter(carresfun %in% input$carresfun)
    }
    
    if (!is.null(input$date) && length(input$date) > 0) {
      filtered <- filtered %>% filter(date %in% input$date)
    }
    
    if (!is.null(input$year) && length(input$year) > 0) {
      filtered <- filtered %>% filter(year %in% input$year)
    }
    
    if (!is.null(input$quarter) && length(input$quarter) > 0) {
      filtered <- filtered %>% filter(quarter %in% input$quarter)
    }
    
    return(filtered)
  })
  
  output$table <- renderDataTable({
    filtered <- filtered_data()
    
    if (!is.null(filtered)) {
      datatable(
        filtered,
        options = list(
          scrollX = TRUE,  # Barras de desplazamiento horizontal
          scrollY = "633px"  # Altura máxima de la tabla
        )
      )
    }
  })
  
  # Permite descargar la tabla filtrada en formato CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("data_cmf_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Permite descargar la tabla filtrada en formato .dta
  output$download_dta <- downloadHandler(
    filename = function() {
      paste("data_cmf_", Sys.Date(), ".dta", sep = "")
    },
    content = function(file) {
      write_dta(filtered_data(), file)
    }
  )
  
  # Permite descargar la tabla filtrada en formato .txt
  output$download_txt <- downloadHandler(
    filename = function() {
      paste("data_cmf_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(filtered_data(), file, sep = "\t", row.names = FALSE)
    }
  )
  
  load_data <- function(base) {
    data <- read_dta(base)
    return(data)
  }
}

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Bases de Datos, LabFENUAH"),
  sidebarLayout(
    sidebarPanel(
      selectInput("base", "Seleccione una base:",
                  choices = c("dataset_cmf.dta", "dataset_cmf_consolidado.dta", "dataset_cmf_indicadores_consolidado.dta", "dataset_cmf_discapacidad.dta"),
                  multiple = FALSE),
      selectizeInput("nombre", "Nombre:", choices = c(""), multiple = TRUE),
      textInput("rut", "RUT:", ""),
      selectizeInput("cuenta", "Cuenta", choices = c(""), multiple = TRUE),
      selectizeInput("per", "Periodo:", choices = c(""), multiple = TRUE),
      selectizeInput("carresfun", "Cargo o Función", choices = c(""), multiple = TRUE),
      selectizeInput("date", "Fecha Completa", choices = c(""), multiple = TRUE),
      selectizeInput("year", "Año", choices = c(""), multiple = TRUE),
      selectizeInput("quarter", "Trimestre", choices = c(""), multiple = TRUE),
      actionButton("actualizar", "Actualizar Datos")
    ),
    mainPanel(
      DT::dataTableOutput("table"),
      downloadButton("download_csv", "Descargar (.csv)"),
      downloadButton("download_dta", "Descargar (.dta)"),
      downloadButton("download_txt", "Descargar (.txt)"),
    )
  )
)

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)