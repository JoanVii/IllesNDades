library(shiny)

shinyUI(
  navbarPage(
    theme = shinythemes::shinytheme("flatly"),

    "Illes'n'dades",
    
    tabPanel(
      "Presentació",
      
      fluidRow(
        align="center",
        h1("Illes'n'Dades:"),
        h3("dades per conèixer les Illes Balears")
      ),
     
      p("L'objectiu d'Illes'n'dades és facilitar l'accés a les dades recollides per l'",
        a(href="https://ibestat.caib.es/ibestat/page?&p=inicio&lang=ca", "IBESTAT"),
        "."),
      p("Al mateix temps, assolir l'objectiu de l'agenda 2030."),
      p("Aquest web és un projecte personal impulsat per una única persona, perdó per si tot és tant cutre no me dona sa vida per més"),
      
      hr(),
      
      
      h4("Com usar el web:"),
      p("1. Anar a la secció de 'Selecció base de dades', escollir una base de dades i clicar-hi a sobre (S'ha de posar d'un color més fosc)."),
      p("2. A 'Selecció de variables' escollir les variables que desitgeu, com menys millor si ho voleu visualitzar en un gràfic!"),
      p("3. Triar com voleu que es vegin les dades recoman no tocar l'eix y i que l'eix x sigui l'eix temporal (si són dades temporals). A més, si teniu moltes dades millor reduir-ho que sinó es un cacao"),
      
      hr(),
      h4("Millores en ment:"),
      p("Dur a terme 3 publicacions setmanals tant al web com a Twitter on es mostrin un o diversos gràfics i generar debat"),
      p("Donar l'opció de descarregar els gràfics i les BBDD (modificades i originals)."),
      p("Permetre la combinació de diferents tipus de BBDD, o sigui si una BBDD és anual i fa anys que es publica poder la fer contínua."),
      p("Nous tipus de gràfics com per exemple: gràfics de barres o boxplots!"),
      p("Mapes de Balears per les dades de les Illes!!"),
      p("Invertir odre de l'eix x"),
      p("Millorar eficiència i experiència d'usuari del web"),
      p("Millorar els motors de cerca de les bases de dades (BBDD), principalment per data i nom de la BBDD (aquest darrer ara no funciona)."),
      p("Evitar que es canvii la selecció de la visualització de variables quan es modifiquen les variables i no es canvia la BBDD"),     
      p("Garantir que el web estigui 100% en català (varibles i gràfics inclosos)"),
      p("Traduir el web a l'anglès i el castellà!")
    ),
    
    tabPanel(
      "Selecció base de dades",
      titlePanel("Selecció de la base de dades que es vol estudiar:"),
      sidebarLayout(
        sidebarPanel(
          
          checkboxGroupInput(
            "selec_Area",
            "Seleccionau l'area que desitgueu:",
            choices = list_Area #input$list_Area
          ),
          
          ###
          
          conditionalPanel(
            "input.selec_Area != input.NULL",
            uiOutput("UI_selec_Subarea"),
          ),
          
          ###
          
          conditionalPanel(
            "input.selec_Subarea != input.NULL",
            uiOutput("UI_selec_Titol")
          ),
          
          ###
          
          # Arreglar:
          
          radioButtons(
            "mostrar_Data",
            "Filtar per dates? (No funciona)",
            choices = c("Si", "No"),
            selected = "No"
          ),
          conditionalPanel(
            "input.mostrar_Data != 'No'",
            uiOutput("UI_selec_Data_slider"),
            
            #conditionalPanel(
            #  "input.selec_Data_buttons == 'D1'",
            #  uiOutput("UI_selec_Data_slider")
            #  #UI_selec_Data_buttons
            #)
          ),
          
          ###
          
          radioButtons(
            "mostrar_NomBBDD",
            "Filtar per nom de la BBDD?",
            choices = c("Si", "No"),
            selected = "No"
          ),
          conditionalPanel(
            "input.mostrar_NomBBDD == 'Si'",
            uiOutput("UI_selec_NomBBDD")
          )
          
          ###
        ),
        
        mainPanel(
          p("Pija sobre a *UNA* fila per seleccionar la base de dades corresponent."),
          verbatimTextOutput("bbdd_seleccionada_txt"),
          
          dataTableOutput(outputId = "Taula_metadata")
        )
      )
    ),
    
    
    tabPanel(
      "Selecció variables",
      titlePanel("Selecció de les variables que vol estudiar:"),
      sidebarLayout(
        sidebarPanel(
          
          uiOutput("UI_provisional_bbdd"), #
          
          conditionalPanel( "output.nombre_columnes >= 1", uiOutput("UI_selec_1")),
          conditionalPanel( "output.nombre_columnes >= 2", uiOutput("UI_selec_2")),
          conditionalPanel( "output.nombre_columnes >= 3", uiOutput("UI_selec_3")),
          conditionalPanel( "output.nombre_columnes >= 4", uiOutput("UI_selec_4")),
          conditionalPanel( "output.nombre_columnes >= 5", uiOutput("UI_selec_5")),
          conditionalPanel( "output.nombre_columnes >= 6", uiOutput("UI_selec_6")),
          conditionalPanel( "output.nombre_columnes >= 7", uiOutput("UI_selec_7")),
          
        ),
        
        mainPanel(
          dataTableOutput(outputId = "Taula_variables")
        )
        
      )
      
      
    ),
    
    tabPanel(
      "Gràfic!",
      titlePanel("Seleccioneu com desitja visualitzar les variables:"),
      sidebarLayout(
        sidebarPanel(
          
          uiOutput("UI_selec_eix_x"),
          uiOutput("UI_selec_eix_y"),
          uiOutput("UI_selec_eix_color"),
          
          
          
          uiOutput("UI_selec_eix_shape"),
          uiOutput("UI_selec_eix_linetype"),
          uiOutput("UI_selec_eix_size")
          
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Gràfic estàtic",
                     plotOutput("grafic_estatic")
            ),
            tabPanel("Gràfic interactiu",
                     p("Keep calm"),
                     plotlyOutput("grafic_interactiu")
            )
          )
        )
        
      )
    )
  
  )
)