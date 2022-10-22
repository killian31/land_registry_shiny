library(shiny)
library(shinycssloaders)
library(shinyalert)
library(shinyWidgets)
library(bs4Dash)
library(magrittr)
library(purrr)
library(stringr)
library(rstudioapi)
library(DT)

wd <- getwd()

ui = dashboardPage(
  title = "Land Registry UX",
  dark = NULL,
  scrollToTop = TRUE,
  header = dashboardHeader(title = "Land Registry",
                           div(style = "position:relative; left:calc(80%);",
                               actionBttn("close", "Close",
                                          color = "danger",
                                          icon = icon("door-open")))
                           ),
                                        
  
  sidebar = dashboardSidebar(
    minified = FALSE,
    sidebarMenu(id = "tabs",
                menuItem("Overview",
                         tabName = "overview",
                         icon = icon("globe")),
              menuItem('New Contract',
                       tabName = 'import',
                       icon = icon('upload')),
              menuItem("Payment",
                       tabName = 'payment',
                       icon = icon('money')),
              menuItem('Sign Contract',
                       tabName = 'sign_contract',
                       icon = icon('pen')),
              menuItem('Checking',
                       tabName = 'checking',
                       icon = icon('check'))
    
      )
    ),
  controlbar = bs4DashControlbar(
    skin = "light",
    width = 350,
     div(style = "position:relative; left:calc(10%);",
         skinSelector())
  ),
  footer = dashboardFooter(
    fluidRow(
      column(11,
             actionBttn(inputId = "Previous", label = icon("arrow-left"))),
      column(1,
             actionBttn(inputId = "Next", label = icon("arrow-right")))
    )
  ),
  body = dashboardBody(
    
    tabItems(
      tabItem(
        tabName = "overview",
        tags$h4("Aide à la création des tableaux associés aux publications."),
        tags$h5("Veuillez lire attentivement ces instructions avant de débuter :"),
        div(class = "card-deck",
            div(class = "card text-white bg-primary mb-3", style = "max-width: 18rem;",
            div(class = "card-header", tags$h4("Import")),
            div(class = "card-body",
                div(class = "card-title", "Fichiers à importer :"),
                div(class = "card-text",
                    tags$ul(
                      tags$li("Fichier de plan si nécessaire"),
                      tags$li("Tableaux de données à formater"),
                      tags$li("Sélection du dossier où sont stockées les tableaux")
                    ))),
              div(class = "card-footer", icon("exclamation-triangle", verify_fa = FALSE),
                  " Si vous importez un fichier de plan, il est nécessaire de l'importer avant les tableaux.")),
            div(class = "card text-white bg-secondary mb-3", style = "max-width: 18rem;",
                div(class = "card-header", tags$h4("Paramètres d'import")),
                div(class = "card-body",
                    div(class = "card-text",
                        tags$ul(
                          tags$li("Séparateur de colonnes"),
                          tags$li("Séparateur décimal"),
                          tags$li("Type de publication")
                        ))
                    )
                ),
            div(class = "card text-white bg-info mb-3", style = "max-width: 18rem;",
                div(class = "card-header", tags$h4("Ordre des tableaux")),
                div(class = "card-body",
                    div(class = "card-text",
                        "Choix de l'ordre dans lequel vous souhaitez que vos tableaux apparaissent dans le fichier Excel formaté.")
                    )
                ),
            div(class = "card text-white bg-dark mb-3", style = "max-width: 18rem;",
                div(class = "card-header", tags$h4("Formatage")),
                div(class = "card-body",
                    div(class = "card-text",
                        "Parcourez vos différents tableaux et définissez les différents paramètres d'apparence de chacun (par exemple taille des colonnes, unités, source, cellules à fusionner...)")
                    ),
                div(class = "card-footer", icon("exclamation-triangle", verify_fa = FALSE),
                    " Si vous n'avez pas importé de plan, vous devez valider pour chaque tableau les colonnes et les unités")
                ),
            div(class = "card text-white bg-success mb-3", style = "max-width: 18rem;",
                div(class = "card-header", tags$h4("Enregistrement")),
                div(class = "card-body",
                    div(class = "card-text",
                        tags$ul(
                          tags$li("Exporter le plan créé (pour réutilisation)"),
                          tags$li("Export du fichier Excel formaté")
                        )
                    )
                )))
      ),
      tabItem(
        tabName = "import",
        div(style="display: inline-block;vertical-align:top; width: 500px;",
            tags$p(
              "Vous devez impérativement importer vos fichiers de données, et sélectionner le dossier qui les contient pour que le formatage fonctionne. Si vous ne choisissez pas de dossier, votre répertoire de travail actuel sera utilisé."
            ),
            tags$p(
              "Si vous avez déjà créé un plan auparavant, vous pouvez l'importer afin de le modifier."
            )),
        div(style="display: inline-block;vertical-align:top; width: 500px;",
            fileInput(inputId = "upload_plan",
              label = "Importer un plan",
              multiple = FALSE,
              accept = ".xlsx"),
          fileInput(
            inputId = "upload",
            label = "Choisir les fichiers",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv",
                       ".xlsx",
                       ".rds",
                       ".Rds",
                       ".parquet",
                       ".xls",
                       ".ods")),
          actionBttn(inputId = "datadir", label = "Dossier de données", icon = icon("folder"), color = "royal"),
          verbatimTextOutput(outputId = "dirpath"))
      ),
      tabItem(tabName = "payment",
              div(style="display: inline-block;vertical-align:top; width: 500px;",
                  radioButtons("sep", "Separateur",
                     choices = c(Virgule = ",",
                                 "Point Virgule" = ";",
                                 Tab = "\t"),
                     selected = ","),
              tags$hr(),
              radioButtons("virg", "Séparateur décimal",
                           choices = c(Virgule = ",",
                                       Point = "."),
                           selected = ",")),
              div(style="display: inline-block;vertical-align:top; width: 500px;",
                  prettyRadioButtons(inputId = "format",
                                     label = "Type de publication",
                                     choices = c("Chiffres et données" = "chiffres_et_donnees",
                                                 Primeur = "primeur"),
                                     selected = "chiffres_et_donnees"),
                  checkboxInput(
                    inputId = "largeur_max",
                    label = "Utiliser la règle de diffusion de largeur maximale des tableaux (actuellement 83)",
                    value = TRUE
                  )
                )),
      tabItem(tabName = "sign_contract",
              p("Sélectionnez tous les fichiers dans l'ordre que vous souhaitez pour le modifier :"),
              DTOutput("files_order"),
              verbatimTextOutput("liste_ordre"),
              actionBttn("validate_order", "Valider", color = "success",
                         icon = icon("check"))),
      tabItem(tabName = "checking",
              downloadBttn("exporter", "Exporter le plan"),
              downloadBttn("enreg", "Enregistrer les tableaux formatés"))
    )
  )
)

server = function(input, output, session) {
  
  options(shiny.maxRequestSize=15*1024^2) # set maximum file size to 15MB
  
  # next and previous buttons
  global <- reactiveValues(tab_id = "")
  tab_id <- c('overview', 'import','payment','sign_contract','checking')
  
  Current <- reactiveValues(
    Tab = "overview"
  )
  
  observeEvent(
    input[["tabs"]],
    {
      Current$Tab <- input[["tabs"]]
    }
  )
  
  observeEvent(
    input[["Previous"]],
    {
      tab_id_position <- match(Current$Tab, tab_id) - 1
      if (isTRUE(tab_id_position == 0)) tab_id_position <- length(tab_id)
      Current$Tab <- tab_id[tab_id_position]
      updateTabItems(session, "tabs", tab_id[tab_id_position]) 
    }
  )
  
  observeEvent(
    input[["Next"]],
    {
      tab_id_position <- match(Current$Tab, tab_id) + 1
      if (isTRUE(tab_id_position > length(tab_id))) tab_id_position <- 1
      Current$Tab <- tab_id[tab_id_position]
      updateTabItems(session, "tabs", tab_id[tab_id_position]) 
    }
  )
  
  observeEvent(input$close, {
    stopApp()
  })
  
  
}

shinyApp(ui = ui,
        server = server,
        options = list(launch.browser = TRUE))
