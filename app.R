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
library(gdata)

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
           radioButtons("version", "Seleccione version:", c("Ciencias", "Letras"), selected = "Ciencias", inline = TRUE),
           actionButton("procesar", "Procesar", icon = icon("gear"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3("Registros en archivos"), hr(),
           textOutput("numids"),br(),
           textOutput("numitems"),br(),
           textOutput("nummatriz"),br(),
           strong(textOutput("numsalida")),hr(),
           h3("Descargar archivo respuestas"),
           downloadButton("descargaTabla", "Descargar", icon = icon("download")),
           
           
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
         tabla <- read.xlsx( archivo$datapath, sheet = 2, startRow = 4)
         tabla[!duplicated(tabla[,2]),]
     })
     
     tabla.ordenitem <- eventReactive( input$procesar, {
         req(input$ordenitem)
         archivo <- input$ordenitem
         ext <- tools::file_ext(archivo$datapath)
         validate(need(ext == "xlsx", "El archivo de items debe ser de tipo XLSX."))
         read.xlsx( archivo$datapath, sheet = 1)
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


# Para validación
   
     lista.iditems <- eventReactive( input$procesar, {
         id.items <- tabla.ordenitem()
         id.items$version <- substr(id.items$Question.Text,5,5)
         id.items$orden <- as.numeric(substr(id.items$Question.Text,7,8))
         id.items$orden <- ifelse((id.items$version == "C") &
                             (substr(id.items$Question.Text,1,3) == "LEC"), id.items$orden + 72,
                         ifelse((id.items$version == "C") &
                             (substr(id.items$Question.Text,1,3) == "RED"), id.items$orden + 48,
                             id.items$orden))
          id.items$orden <- ifelse((id.items$version == "L") &
                             (substr(id.items$Question.Text,1,3) == "MAT"), id.items$orden + 56,
                          ifelse((id.items$version == "L") &
                             (substr(id.items$Question.Text,1,3) == "RED"), id.items$orden + 28,
                             id.items$orden))
          id.items <- id.items %>% arrange(version, orden)
          orden.cie <- as.character(id.items$Question.ID[id.items$version == "C"])
          orden.let <- as.character(id.items$Question.ID[id.items$version == "L"])

        list( ciencias = orden.cie, letras = orden.let)
     })
     
     matriz.ordenada <- eventReactive( input$procesar, {
         ordenitem <- lista.iditems()
         matriz <- tabla.datarespuestas()
         seleccion <- input$version
         if (seleccion == "Ciencias") 
             orden <-  ordenitem$ciencias
         else 
             orden <- ordenitem$letras
         matriz <- matriz %>% select("Test-Taker.Email", all_of(orden))
         matriz <- bind_cols(matriz$`Test-Taker.Email`, as.data.frame(lapply(matriz[,2:97], alternativas)))
     })

     matriz.descarga <- eventReactive(input$procesar, {
         identificador <- tabla.identificador() %>% select(2,18)
         names(identificador) <- c("EMAIL", "EXAMEN")
         matriz <- matriz.ordenada()
         names(matriz) <- c("EMAIL", paste0("ITEM", 1:96))
         identificador %>% inner_join(matriz, by = "EMAIL") %>% select(-1) %>% arrange(EXAMEN)
     })

     # Descarga de tabla ####
     
     output$numsalida <- renderText({
         input$procesar
         tabla <- matriz.descarga()
         numero <- dim(tabla)[1]
         paste0("Número de registros de salida: ", numero)
     })
     
     output$descargaTabla <- downloadHandler(
         filename = function(){
             file <- input$identificador
             paste0(file$name,"-", Sys.time(),".txt")},
         content = function(file) {
             tabla <- matriz.descarga()
             write.fwf(matriz.descarga(), file, rownames = FALSE, colnames = FALSE, sep = "", 
                       eol = "\r\n")
         }, contentType = "text/csv")

}

# Run the application 
shinyApp(ui = ui, server = server)
