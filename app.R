#' Ouvrir l'application Shiny d'édition du tableur
#' 
#' Cette fonction permet d'ouvrir l'interface graphique s'appuyant 
#' sur les fonctions de formatage du package pour éditer des tableaux plus rapidement
#' 
#' @return Rien n'est renvoyé, l'appli s'ouvre.
#' 
#' @importFrom shiny runApp
#' @importFrom shiny shinyAppDir
#' @importFrom shiny fileInput
#' @importFrom shiny shinyApp
#' @importFrom shiny tags
#' @importFrom shiny checkboxInput
#' @importFrom shiny radioButtons
#' @importFrom shiny textInput
#' @importFrom shiny tableOutput
#' @importFrom shiny uiOutput
#' @importFrom shiny verbatimTextOutput
#' @importFrom shiny actionButton
#' @importFrom shiny req
#' @importFrom shiny renderUI
#' @importFrom shiny renderPrint
#' @importFrom shiny selectInput
#' @importFrom shiny observeEvent
#' @importFrom shiny showModal
#' @importFrom shiny removeModal
#' @importFrom shiny p
#' @importFrom shiny div
#' @importFrom shiny fluidRow
#' @importFrom shiny column
#' @importFrom shiny tabPanel
#' @importFrom shiny downloadButton
#' @importFrom shiny downloadHandler
#' @importFrom shiny icon
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyalert shinyalert
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinyWidgets actionBttn
#' @importFrom shinyWidgets downloadBttn
#' @importFrom bs4Dash dashboardPage
#' @importFrom bs4Dash dashboardHeader
#' @importFrom bs4Dash dashboardSidebar
#' @importFrom bs4Dash dashboardFooter
#' @importFrom bs4Dash dashboardBody
#' @importFrom bs4Dash bs4DashControlbar
#' @importFrom bs4Dash tabBox
#' @importFrom bs4Dash sidebarMenu
#' @importFrom bs4Dash menuItem
#' @importFrom bs4Dash tabItems
#' @importFrom bs4Dash tabItem
#' @importFrom bs4Dash updateTabItems
#' @importFrom bs4Dash skinSelector
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom purrr %>%
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#' @importFrom rstudioapi selectFile
#' @importFrom rstudioapi selectDirectory
#' @importFrom DT datatable
#' @importFrom DT DTOutput
#' @importFrom DT renderDataTable
#' @importFrom DT selectRows
#' @importFrom DT selectCells
#' @importFrom DT dataTableProxy
#' 
#' @export
#' 
#' @examples
#' library(agreste)
#' \dontrun{
#' app_formatage()
#' }

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
  title = "Mise en forme de tableaux",
  dark = NULL,
  scrollToTop = TRUE,
  header = dashboardHeader(title = "Mise en forme de tableaux",
                           div(style = "position:relative; left:calc(80%);",
                               actionBttn("close", "Fermer",
                                          color = "danger",
                                          icon = icon("door-open")))
                           ),
                                        
  
  sidebar = dashboardSidebar(
    minified = FALSE,
    sidebarMenu(id = "tabs",
                menuItem("Vue d'ensemble",
                         tabName = "overview",
                         icon = icon("globe")),
              menuItem('Import de fichiers',
                       tabName = 'import',
                       icon = icon('upload')),
              menuItem("Paramètres d'import",
                       tabName = 'import_param',
                       icon = icon('bars')),
              menuItem('Ordre des tableaux',
                       tabName = 'table_order',
                       icon = icon('sort')),
              menuItem('Format des tableaux',
                       tabName = 'table_format',
                       icon = icon('table')),
              menuItem('Enregistrement',
                       tabName = 'sauvegarde',
                       icon = icon('download'))
    
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
      tabItem(tabName = "import_param",
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
      tabItem(tabName = "table_order",
              p("Sélectionnez tous les fichiers dans l'ordre que vous souhaitez pour le modifier :"),
              DTOutput("files_order"),
              verbatimTextOutput("liste_ordre"),
              actionBttn("validate_order", "Valider", color = "success",
                         icon = icon("check"))),
      tabItem(tabName = "table_format",
              DTOutput("files"),
              DTOutput("contents") %>% withSpinner(),
              tabBox(
                width = 12,
                tabPanel(
                  title = "Paramètres",
                  tabBox(
                tabPanel(title = "Titres",
                         id = "tab_titles",
                         textInput("feuille", "Nom de la feuille :"),
                         textInput("titre_primeur", "Titre du document (si primeur)"),
                         textInput("title", "Titre du tableau :")),
                tabPanel(title = "Colonnes",
                         id = "tab_cols",
                         uiOutput("deroul"),
                         uiOutput("larg"),
                         actionBttn("validate_colonnes", "Valider", color = "success",
                                    icon = icon("check"))),
                tabPanel(title = "Lignes",
                         id = "tab_lines",
                         p("Séléctionnez les lignes souhaitées puis cliquez sur le bouton pour les formater :"),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             actionButton("lignes_titre", "Lignes de surtitre")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             actionButton("lignes_section", "Lignes de section")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             actionButton("lignes_precision_1", "Lignes de précision 1")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             actionButton("lignes_precision_2", "Lignes de précision 2")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             actionButton("lignes_precision_3", "Lignes de précision 3")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             actionButton("lignes_precision_4", "Lignes de précision 4")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             actionButton("lignes_sous_total", "Lignes de sous-total")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             actionButton("lignes_total", "Lignes de total")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             actionButton("lignes_italique", "Lignes en italique")),
                         tags$hr(),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             verbatimTextOutput("disp_lignes_titre")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             verbatimTextOutput("disp_lignes_section")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             verbatimTextOutput("disp_lignes_precision_1")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             verbatimTextOutput("disp_lignes_precision_2")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             verbatimTextOutput("disp_lignes_precision_3")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             verbatimTextOutput("disp_lignes_precision_4")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             verbatimTextOutput("disp_lignes_sous_total")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             verbatimTextOutput("disp_lignes_total")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             verbatimTextOutput("disp_lignes_italique"))),
                tabPanel(title = "Unités",
                         id = "tab_units",
                         radioButtons(inputId = "is_unite_unif",
                                      label = "L'unité est commune à toutes les colonnes :",
                                      choices = c("Oui" = "oui",
                                                  "Non" = "non"),
                                      selected = "oui"),
                         p("(remplir uniquement la première si oui)"),
                         uiOutput("unites"),
                         actionBttn("validate_unites", "Valider", color = "success",
                                    icon = icon("check"))),
                tabPanel(title = "Textes",
                         id = "tab_text",
                         tags$h5("Lecture :"),
                         div(style="display: inline-block;vertical-align:top; width: 300px;",
                             textInput("note", "Note de lecture :")),
                         div(style="display: inline-block;vertical-align:top; width: 300px;",
                             textInput("source", "Source")),
                         div(style="display: inline-block;vertical-align:top; width: 300px;",
                             textInput("champ", "Champ")),
                         tags$br(),
                         tags$h5("Sommaire :"),
                         p("Laissez les champs vides si identiques au tableau précédent"),
                         div(style="display: inline-block;vertical-align:top; width: 300px;",
                             textInput("chapitre", "Nom du chapitre :")),
                         div(style="display: inline-block;vertical-align:top; width: 300px;",
                             textInput("sous_chapitre", "Nom du sous-chapitre :"))),
                width = 12, 
                height = 500,
                collapsed = FALSE,
                id = "setting_box"
              )),
                tabPanel(
                  title = "Cellules à fusionner",
                  DTOutput("contents_fusion"),
                  verbatimTextOutput("liste_fusion"),
                  actionBttn("add_fusion", "Fusionner",
                             color = "royal", icon = icon("plus")),
                  actionBttn("delete_last_merge", "Annuler la dernière fusion",
                             color = "warning", icon = icon("trash"))
                )
              )
              ),
      tabItem(tabName = "sauvegarde",
              downloadBttn("exporter", "Exporter le plan"),
              downloadBttn("enreg", "Enregistrer les tableaux formatés"))
    )
  )
)

server = function(input, output, session) {
  
  options(shiny.maxRequestSize=15*1024^2) # set maximum file size to 15MB
  
  # next and previous buttons
  global <- reactiveValues(tab_id = "")
  tab_id <- c('overview', 'import','import_param','table_order','table_format','sauvegarde')
  
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
