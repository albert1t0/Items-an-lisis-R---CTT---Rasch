# ######################
# Convierte archivos en matrices
#
# Alberto Torreblanca - Septiembre 2021
########################

########################
# Libraries
########################

library(shiny)
library(shinyjs)
library(shinythemes)
library(openxlsx)
library(tidyverse)

########################

#######################
# Function definition
#######################

# Change numbers 1-4 to alfabhetic letters A-D

alternativas <- function(x) {
    alt <- ifelse(is.na(x), " ",
                  ifelse( x == "1", "A",
                          ifelse( x == "2", "B",
                                  ifelse( x == "3", "C",
                                         ifelse( x == "4", "D", " ")))))
    return(alt)
}

# Code to close de app
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Set locale parameter to systems
Sys.setlocale(category = "LC_ALL", locale = "es_PE.UTF-8")

########################
# USER Interface
########################

ui <- fluidPage(

    # Application title
    
    titlePanel("Generar matrices DTI"),

    sidebarLayout(
        sidebarPanel(
           fileInput("identificador", label = "Identificadores:", buttonLabel = "Selecciona", 
                     placeholder = "No hay archivo.",
                     accept = c("application/excel", ".xls", ".xlsx")),
           fileInput("ordenitem", label = "Orden ítems:", buttonLabel = "Selecciona",
                     placeholder = "No hay archivo.",
                     accept = c("application/excel", ".xls", ".xlsx")),
           fileInput("datarespuestas", label = "Archivo respuestas:", buttonLabel = "Selecciona",
                     placeholder = "No hay archivo.",
                     accept = c("application/excel", ".xls", ".xlsx")),
           actionButton("procesar", "Procesar", icon = icon("gear"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("numids"),br(),
           textOutput("numitems"),br(),
           textOutput("nummatriz"),br()
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
     tabla.identificador <- eventReactive( input$procesar, {
         req(input$identificador)
         archivo <- input$identificador
         ext <- tools::file_ext(archivo$datapath)
         validate(need(ext == "xlsx", "El archivo Identificadores debe ser de tipo XLSX."))
         read.xlsx( archivo$datapath, sheet = 2, startRow = 4)
     })
     
     tabla.ordenitem <- eventReactive( input$procesar, {
         req(input$ordenitem)
         archivo <- input$ordenitem
         ext <- tools::file_ext(archivo$datapath)
         validate(need(ext == "xlsx", "El archivo de items debe ser de tipo XLSX."))
         read.xlsx( archivo$datapath, sheet = 1, startRow = 4)
     })
     
     tabla.datarespuestas <- eventReactive( input$procesar, {
         req(input$datarespuestas)
         archivo <- input$datarespuestas
         ext <- tools::file_ext(archivo$datapath)
         validate(need(ext == "xlsx", "El archivo de respuestas debe ser de tipo XLSX."))
         read.xlsx( archivo$datapath, sheet = 1, startRow = 3)
     })

    output$numids <- renderText({
         input$procesar
         registros.id <- tabla.identificador()
         registros <- dim(registros.id)[1]
         paste0("Número de registros - Identificadores: ", registros)
     })
     
     output$numitems <- renderText({
         input$procesar
         registros.id <- tabla.ordenitem()
         registros <- dim(registros.id)[1]
         paste0("Número de registros - Orden Items: ", registros)
     })
     
     output$nummatriz <- renderText({
         input$procesar
         registros.id <- tabla.datarespuestas()
         registros <- dim(registros.id)[1]
         paste0("Número de registros - Matriz respuestas: ", registros)
     })
     
     
     
}

# Run the application 
shinyApp(ui = ui, server = server)
