library(shiny)
library(rvest)
library(stringr)
library(DT)
library(plotly)
library(tidyverse)
library(pxR)
library(readr)

setwd("D:/00_PseudoEsciptori/Illesndades/IllesNDades/IllesNDades_V2/Shiny")
source("functions_grafic.R")
source("functions_cleaning_date.R")

# des filtrat de BBDD:
# filtrar per data (fecha) no va!

setwd("D:/00_PseudoEsciptori/Illesndades/IllesNDades/IllesNDades_V2/Shiny")

metadata <- read_csv("imp_metadata_links.csv", locale = locale(encoding = "UTF-8")) #No se on s'empra'


#metadata_cat <- read_csv("BBDD_web_05_01_2023.csv", locale = locale(encoding = "UTF-8"))
metadata_cat<- metadata[, c("SUBJECT.AREA.ca.1", "SUBJECT.AREA.ca.2", "TITLE.ca.", "REFPERIOD.start.", "REFPERIOD.end.", "Nom_BBDD")]
names(metadata_cat) <- c("Area", "Subarea", "Titol", "Inici periode", "Fi periode", "NomBBDD")
           
vec_Area <- c(unique(metadata_cat[, "Area"]))[[1]]
list_Area <- as.list(unique(metadata_cat[, "Area"]))[[1]]
names(list_Area) <- vec_Area
selec_Area <- list_Area


shinyServer(function(input, output) {
  ### inici pestanya 1
  
  # Filtrat BBDD:
  sub_metadata <- reactive({
    metadata_cat
  })
  
  sub_metadata_Area <- reactive({
    data <- metadata_cat
    data  %>%
      filter(
        Area %in% input$selec_Area
      )
  })
  
  sub_metadata_Subarea <- reactive({
    data <- sub_metadata_Area()
    data %>%
      filter(
        Subarea %in% input$selec_Subarea
      )
  })
  
  sub_metadata_Titol <- reactive({
    data <- sub_metadata_Subarea()
    data %>%
      filter(
        Titol %in% input$selec_Titol
        )
  })
  
  
  ### start recomanacio IA:
  #sub_metadata_filtered <- eventReactive(c(input$selec_Area, input$selec_Subarea, input$selec_Titol), {
  #  data <- metadata_cat
  #  data <- data %>% filter(Area %in% input$selec_Area)
  #  data <- data %>% filter(Subarea %in% input$selec_Subarea)
  #  data <- data %>% filter(Titol %in% input$selec_Titol)
  #  return(data)
  #})
  #
  #dades_reactives <- reactive({
  #  data <- ifelse(nrow(sub_metadata_filtered()) > 0, sub_metadata_filtered(), metadata_cat)
  #  return(data)
  #})
  ### end recomanacio IA
  
  
  dades_reactives <- reactive({
    
    if(nrow(sub_metadata_Titol())){ #nrow == !is.null
      data <- sub_metadata_Titol()
    }else{
      if(nrow(sub_metadata_Subarea())){
        data <- sub_metadata_Subarea()
      }else{
        if(nrow(sub_metadata_Area())){
          data <- sub_metadata_Area()
        } else{
          data <- metadata_cat
        }
      }
    }
    
  })
  
    ### funciona pero es super lent!
  sub_metadata_Data <- reactive({
    
    if(input$mostrar_Data == "No"){
      data <- data.frame()
    }else{
      data <- dades_reactives()
      data_2 <- data[1,] # això fa que si no hi ha cap dada surti la primera pero no ho se arreglar!
      j=1
      
      for (i in 1:nrow(data)) {
        if (
          (between(min(input$selec_Data_slider), data$"Inici periode"[i], data$"Fi periode"[i]) == T ) |
          (between(max(input$selec_Data_slider), data$"Inici periode"[i], data$"Fi periode"[i]) == T ) |
          (between(data$"Inici periode"[i], min(input$selec_Data_slider), max(input$selec_Data_slider)) == T ) |
          (between(data$"Fi periode"[i], min(input$selec_Data_slider), max(input$selec_Data_slider)) == T ) |
          (is.na(data$"Inici periode"[i]) == T)
        ){data_2[j, ] <-  data[i, ]; j = j+1}
      }
      
      return(data_2)
      
    }
  })
    #### nyeeee 
  
  sub_metadata_NomBBDD <- reactive({
    if(input$mostrar_NomBBDD == "No"){
      data <- data.frame()
    } else{
      data <- dades_reactives()
      
      data %>%
        filter(
          NomBBDD %in% input$selec_NomBBDD
        ) 
    }
  })
  
  dades_reactives_2 <-reactive({
    
    if(nrow(sub_metadata_Data()) == 0 & nrow(sub_metadata_NomBBDD()) == 0){
      data <- dades_reactives()
    }else{
      if(nrow(sub_metadata_Data()) != 0 & nrow(sub_metadata_NomBBDD()) != 0){
        data<- sub_metadata_Data()
        data <- data %>% filter(
          NomBBDD %in% input$selec_NomBBDD
        )
      }else{
        if(nrow(sub_metadata_NomBBDD()) != 0){ data <-sub_metadata_NomBBDD() }
        if(nrow(sub_metadata_Data()) != 0){ data <-sub_metadata_Data() } 
      }
    }
  })
  
  # Llistes reactives:
  
  list_Subarea_reactive <- reactive({
    # inici prova IA
    #data <- sub_metadata_Area()
    # fi prova IA
    
    data <- sub_metadata_Area()
    
    vec_Subarea <- c(unique(data[, "Subarea"]))[[1]]
    list_Subarea <- as.list(unique(data[, "Subarea"]))[[1]]
    names(list_Subarea) <- vec_Subarea
    return(list_Subarea)
  })
  
  list_Titol_reactive <- reactive({
    # inici prova IA
    #data <- sub_metadata_Subarea()
    # fi prova IA
    
    data <- sub_metadata_Subarea()
    
    vec_Titol <- c(unique(data[, "Titol"]))[[1]]
    list_Titol <- as.list(unique(data[, "Titol"]))[[1]]
    names(list_Titol) <- vec_Titol
    return(list_Titol)
  })
  
  # aixo es molt lento! 
  list_NomBBDD_reactive <- reactive({
    data <-  dades_reactives()
    
    vec_NomBBDD <- c(unique(data[, "NomBBDD"]))[[1]]
    list_NomBBDD <- as.list(unique(data[, "NomBBDD"]))[[1]]
    names(list_NomBBDD) <- vec_NomBBDD
    return(list_NomBBDD)
  })
  
  # Revisar: No s'usa
  #list_Data_reactive <- reactive({
  #  data <- dades_reactives()
  #  
  #  vec_Data <- c(unique(data[, "Data"]))[[1]]
  #  list_Data <- as.list(unique(data[, "Data"]))[[1]]
  #  names(list_Data) <- vec_Data
  #  return(list_Data)
  #})
  #
  
  # UI Reactiva:
  
  output$UI_selec_Subarea = renderUI({
    checkboxGroupInput("selec_Subarea", "Seleccionau la subarea que desitgueu:",
                       choices = list_Subarea_reactive())
  })
  
  output$UI_selec_Titol = renderUI({
    checkboxGroupInput("selec_Titol", "Seleccionau el titol que desitgueu:",
                       choices = list_Titol_reactive())
  })
  
  output$UI_selec_Data_slider = renderUI({
    sliderInput(
      "selec_Data_slider",
      "Slider:",
      min = 1857,
      max = 2023,
      value = c(1857, 2023)
    )
  })
  
  output$UI_selec_NomBBDD = renderUI({
    selectInput("selec_NomBBDD", "Seleccionau la BBDD que desitgueu:",
                choices = as.list(list_NomBBDD_reactive()), multiple = TRUE)
    
  })
  
  # Taula seleccio bbdd:
  
  output$Taula_metadata = DT::renderDataTable({
    data <- dades_reactives_2()
    
    DT::datatable(
      data,
      
      rownames = FALSE,
      
      extensions = 'Buttons',
      
      options = list(
        pageLength = 10,
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = FALSE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      
      selection = list(
        mode = "single", # canviar a multiple !
        selected = NULL,
        target = "row",
        selectable = NULL
      ),
      
      class = "display"
      
    )
  }, server = FALSE)
  
  output$download_table_metadata <- downloadHandler(
    filename = function() {
      paste("metadata", ".csv", sep="")
    },
    content = function(file) {
      write.csv(dades_reactives_2(), fileEncoding = "UTF-8", file)
    }
  )
  

  bbdd_seleccionada <- reactive({
    ####
    # Aixo peta
    # dona error pero dissimulat
    ###
    if(!is.null(input$Taula_metadata_rows_selected)){
      rows_selected <-  input$Taula_metadata_rows_selected
      # hauria de funcionar amb mode = "multiple" pero no ho fa...
      return(dades_reactives_2()[[rows_selected, "NomBBDD"]])
    }
  })
  
  output$bbdd_seleccionada_txt <- renderPrint({
    if (length(bbdd_seleccionada()) != 0) {
      cat('Base de dades seleccionada: ')
      cat(bbdd_seleccionada() , sep = ', ')
    }
  })
  
  ### fi pestanya 1
  
  ### inici pestanya 2
  
  # lectura bbdd:
  
  doc_px <- reactive({
    
    #on line:
    data <- filter(metadata, MATRIX == input$bbdd)
    read.px(data[, "Enllac"][[1]])
    
    
    # off line:
    #setwd(directori)
    #read.px(paste0(input$bbdd, ".px"))
    
  })
  
  doc_df <- reactive({
    
    #as_tibble(doc_px()) no va =(
    as.data.frame(doc_px()) 
    
  })
  
  output$nombre_columnes <- reactive({
    
    return(ncol(doc_df()))
    
  })
  outputOptions(output, "nombre_columnes", suspendWhenHidden = FALSE) # magia!
  
  doc_df_reactive <- reactive({
    
    data <- doc_df()
    
    if(!is.null(input$selec_1)){ data <- filter(data, data[,1] %in% input$selec_1)}
    if(!is.null(input$selec_2)){ data <- filter(data, data[,2] %in% input$selec_2)}
    if(!is.null(input$selec_3)){ data <- filter(data, data[,3] %in% input$selec_3)}
    if(!is.null(input$selec_4)){ data <- filter(data, data[,4] %in% input$selec_4)}
    if(!is.null(input$selec_5)){ data <- filter(data, data[,5] %in% input$selec_5)}
    if(!is.null(input$selec_6)){ data <- filter(data, data[,6] %in% input$selec_6)}
    if(!is.null(input$selec_7)){ data <- filter(data, data[,7] %in% input$selec_7)}
    
    return(data)
    
  })
  
  # UI de la pestanya 2:
  
  output$UI_provisional_bbdd  = renderUI({
    selectInput(
      "bbdd", "Seleccioneu la BBDD",
      bbdd_seleccionada(),
      multiple = F
    )
  })
  
  output$UI_selec_1 = renderUI({
    selectInput("selec_1", "Seleccionau de la primera columna:",
                choices = (unique(doc_df()[,1])), multiple = TRUE)
    
  })
  
  output$UI_selec_2 = renderUI({
    selectInput("selec_2", "Seleccionau de la segona columna:",
                choices = unique(doc_df()[,2]), multiple = TRUE)
    
  })
  
  output$UI_selec_3 = renderUI({
    selectInput("selec_3", "Seleccionau de la tercera columna:",
                choices = unique(doc_df()[,3]), multiple = TRUE)
    
  })
  
  output$UI_selec_4 = renderUI({
    selectInput("selec_4", "Seleccionau de la quarta columna:",
                choices = unique(doc_df()[,4]), multiple = TRUE)
    
  })
  
  output$UI_selec_5 = renderUI({
    selectInput("selec_5", "Seleccionau de la cinquena columna:",
                choices = unique(doc_df()[,5]), multiple = TRUE)
  })
  
  output$UI_selec_6 = renderUI({
    selectInput("selec_6", "Seleccionau de la sisena columna:",
                choices = unique(doc_df()[,6]), multiple = TRUE)
    
  })
  
  output$UI_selec_7 = renderUI({
    selectInput("selec_7", "Seleccionau de la setena columna:",
                choices = unique(doc_df()[,7]), multiple = TRUE)
    
  })
  
  #output$Taula_variables =DT::renderDataTable({
  #  
  #  DT::datatable(doc_df_reactive())
  #})
  
  output$Taula_variables = DT::renderDataTable({
    data <- doc_df_reactive()
    
    DT::datatable(
      data,
      
      rownames = FALSE,
      
      extensions = 'Buttons',
      
      options = list(
        pageLength = 10,
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = FALSE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      
      class = "display"
      
    )
  }, server = FALSE)
  
  output$download_table_var <- downloadHandler(
    filename = function() {
      paste(bbdd_seleccionada(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(doc_df_reactive(), fileEncoding = "UTF-8", file)
    }
  )
  
  ### fi Pestanya 2
  
  ### inici Pestanya 3
  
  # UI:
  
  nom_columnes <- reactive({
    colnames(doc_df_reactive())
  })
  
  output$UI_selec_eix_x <- renderUI({
    selectInput(
      "selec_eix_x", "Selecciona les dades de l'eix X",
      nom_columnes(),
      multiple = F,
      selected = nom_columnes()[1]
    )
  })
  
  output$UI_selec_eix_y <- renderUI({
    selectInput(
      "selec_eix_y", "Selecciona les dades de l'eix y",
      nom_columnes(),
      multiple = F, 
      selected = "value"
    )
  })
  
  output$UI_selec_eix_color <- renderUI({
    selectInput(
      "selec_eix_color", "Seleccioneu les dades color",
      nom_columnes(),
      multiple = F,
      selected = nom_columnes()[sample.int(n = length(nom_columnes())-2, size = 1)+1]
    )
  })
  
  output$UI_selec_eix_shape <- renderUI({
    selectInput(
      "selec_eix_shape", "Selecciona les dades de forma",
      nom_columnes(),
      multiple = F,
      nom_columnes()[sample.int(n = length(nom_columnes())-2, size = 1)+1]
    )
  })
  
  output$UI_selec_eix_linetype <- renderUI({
    selectInput(
      "selec_eix_linetype", "Seleccioneu les dades linetype",
      nom_columnes(),
      multiple = F,
      nom_columnes()[sample.int(n = length(nom_columnes())-2, size = 1)+1]
    )
  })
  
  output$UI_selec_eix_size <- renderUI({
    selectInput(
      "selec_eix_", "Seleccioneu les dades de mida",
      nom_columnes(),
      multiple = F,
      nom_columnes()[sample.int(n = length(nom_columnes())-2, size = 1)+1]
    )
  })
  
  # grafics:
  
  grafic_reactive<-reactive({
    titol <- str_remove_all(doc_px()[["DESCRIPTION"]][["value"]], '\"\n\"')
    
    
    grafic <- funcio.grafics(
      doc_df = doc_df_reactive(),
      eix_x = input$selec_eix_x,
      eix_y = input$selec_eix_y,
      eix_color = input$selec_eix_color,
      eix_shape = input$selec_eix_shape,
      eix_linetype = input$selec_eix_linetype,
      eix_size = input$selec_eix_size,
      titol=titol
    )
    
    grafic
    
  })
  
  output$grafic_estatic<- renderPlot({
    
    grafic_reactive()
    
  })
  
  output$download_grafic <- downloadHandler(
    filename = function() {
      paste(bbdd_seleccionada(), "_grafic.jpeg", sep="")
    },
    content = function(file) {
      device <- function(..., width, height)
      grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = grafic_reactive(), device = device)
    }
  )
  
  
  
  output$grafic_interactiu <- renderPlotly({
    
    ggplotly(grafic_reactive())
    
  })
  
  ### fi 3
  
})







