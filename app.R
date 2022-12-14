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
library(shinyjs)

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
                menuItem("Payment",
                         tabName = 'payment',
                         icon = icon('money')),
              menuItem('Upload Documents',
                       tabName = 'import',
                       icon = icon('upload')),
              menuItem('Sign Contract',
                       tabName = 'sign_contract',
                       icon = icon('pen')),
              menuItem('Download Deed',
                       tabName = 'receipt',
                       icon = icon('check'))
    
      )
    ),
  # controlbar = bs4DashControlbar(
  #   skin = "light",
  #   width = 350,
  #    div(style = "position:relative; left:calc(10%);",
  #        skinSelector())
  # ),
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
        tags$h4("Welcome to the Land Registry. Logged in as user cb027ebd54ea46301301f7703fb72f48770b5d345b563e331e5b8197fe483553"),
        tags$h5("Please have a look at these instructions before starting."),
        div(class = "card-deck",
            div(class = "card text-white bg-secondary mb-3", style = "max-width: 20rem;",
                div(class = "card-header", tags$h4("Payment")),
                div(class = "card-body",
                    div(class = "card-title", "Pay the fee to initiate the process."),
                    div(class = "card-text", "The fee is required for the transfer of ownership to occur.")
                )
            ),
            div(class = "card text-white bg-primary mb-3", style = "max-width: 20rem;",
            div(class = "card-header", tags$h4("Upload Documents")),
            div(class = "card-body",
                div(class = "card-title", "Import your documents:"),
                div(class = "card-text",
                    tags$ul(
                      tags$li("Identity proof: Aadhar card, Pan Card"),
                      tags$li("The latest property registers card copy"),
                      tags$li("A copy of the municipal tax bill"),
                      tags$li("Construction completion certificate"),
                      tags$li("NOC (No Objection Certificate) "),
                      tags$li("Verified Sale deed copy")
                    )))
              ),
            div(class = "card text-white bg-info mb-3", style = "max-width: 20rem;",
                div(class = "card-header", tags$h4("Fill up the contract")),
                div(class = "card-body",
                    div(class = "card-title",
                        "Fill up each fields with relevant information about you and verify the land's information")
                    )
                ),
            div(class = "card text-white bg-success mb-3", style = "max-width: 20rem;",
                div(class = "card-header", tags$h4("Download deed")),
                div(class = "card-body",
                    div(class = "card-text",
                        "You can download your title deed on this tab."
                    )
                )))
      ),
      tabItem(tabName = "payment",
              p("The button below will redirect you to the payment page."),
              actionBttn(inputId = "pay_btn",
                         label = "Procced to payment",
                         icon = icon("money"),
                         color = "success")
      ),
      tabItem(
        tabName = "import",
        div(style="display: inline-block;vertical-align:top; width: 500px;",
            
            fileInput(
              inputId = "upload_id",
              label = "Identity proof",
              multiple = FALSE,
              accept = c(".pdf",
                         ".png",
                         ".jpg")),
            
            fileInput(inputId = "upload_property_reg",
                      label = "Latest property registers card copy",
                      multiple = TRUE,
                      accept = c(".pdf",
                                 ".png",
                                 ".jpg")),
            
            fileInput(inputId = "upload_construction",
                      label = "Construction completion certificate",
                      multiple = TRUE,
                      accept = ".pdf")
        ),
        div(style="display: inline-block;vertical-align:top; width: 500px;",
            fileInput(inputId = "upload_municipal_tax",
                      label = "Municipal tax bill",
                      multiple = TRUE,
                      accept = ".pdf"),
            
            fileInput(inputId = "upload_no_objection",
                      label = "No Objection Certificate",
                      multiple = TRUE,
                      accept = ".pdf"),
            
            fileInput(inputId = "upload_deed",
                      label = "Verified Sale deed copy",
                      multiple = TRUE,
                      accept = ".pdf")
          ),
        div(style = "position:relative; left:calc(30%);",
            actionBttn(inputId = "validate_docs",
                     label = "Send documents",
                     icon = icon("check"),
                     color = "success")
        )
      ),
      tabItem(tabName = "sign_contract",
              div(style="display: inline-block;vertical-align:top; width: 1000px;",
                  tags$h1("Your contract"),
                  p("This contract was initiated by seller bef3cde7136ba829665ebe1b2a36484375d7c8515558655b9343fd8ee1842f10"),
                  p(""),
                  useShinyjs(),
                  disabled(textInput(inputId = "land_id",
                            label = "Land ID",
                            value = "83c89702f4d913cd904ec99a4531a5a81d7ba2a12ce68e205a6fef2526511aef",
                            width = "1000px")),
                  disabled(textInput(inputId = "land_location",
                                     label = "Land location",
                                     value = "2, Air India Rd, Kismat Nagar, Kurla West, Mumbai, Maharashtra 400070, India",
                                     width = "1000px")),
                  disabled(textInput(inputId = "land_value",
                                     label = "Transaction value",
                                     value = "Rs. 40000000",
                                     width = "1000px")),
                  disabled(textInput(inputId = "seller_name",
                                     label = "Seller's name",
                                     value = "Droupadi Murmu",
                                     width = "1000px")),
                  tags$hr(),
                  textInput(inputId = "name_input",
                            label = "Full name",
                            width = "1000px"),
                  textInput(inputId = "aadhaar_input",
                            label = "Aadhaar number",
                            width = "1000px"),
                  dateInput(inputId = "birth_input",
                            label = "Birth date",
                            format = "dd-mm-yyyy",
                            width = "1000px"), 
                  textInput(inputId = "address_input",
                            label = "Address",
                            width = "1000px"),
                  textInput(inputId = "bankId_input",
                            label = "Bank ID",
                            width = "1000px"),
                  tags$hr(),
                  disabled(selectInput(inputId = "transaction_type",
                              label = "Nature of transaction",
                              choices = c('Whole land',
                                          'Split land'))),
                  disabled(selectInput(inputId = "land_type",
                              label = "Type of land",
                              choices = c('Residential',
                                          'Agricultural',
                                          'Industrial',
                                          'Work area'))),
                  disabled(dateInput(inputId = "deadline_date",
                            label = "Deadline date",
                            format = "dd-mm-yyyy",
                            width = "1000px",
                            value = "05-11-2022")),
                  actionBttn(inputId = "sign_contract_btn",
                             label = "Sign contract digitally",
                             icon = icon("signature"),
                             color = "success")
                 )
              ),
      tabItem(tabName = "receipt",
              tags$h4("Click here to download your title deed."),
              downloadBttn(outputId = "download_receipt", label = "Download title deed", style = "jelly", color = "royal"))
    )
  )
)

server = function(input, output, session) {
  
  options(shiny.maxRequestSize=15*1024^2) # set maximum file size to 15MB
  
  # next and previous buttons
  global <- reactiveValues(tab_id = "")
  tab_id <- c('overview','payment', 'import','sign_contract','receipt')
  
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
  
  observeEvent(input$validate_docs, {
    is_error <- FALSE
    if (is.null(input$upload_id) & !is_error){
      is_error <- TRUE
      shinyalert(title = "A document is missing",
                 text = "The ID has not been uploaded.",
                 type = "error")
    }
    if (is.null(input$upload_property_reg) & !is_error){
      is_error <- TRUE
      shinyalert(title = "A document is missing",
                 text = "The latest property registers card copy has not been uploaded.",
                 type = "error")
    }
    if (is.null(input$upload_municipal_tax) & !is_error){
      is_error <- TRUE
      shinyalert(title = "A document is missing",
                 text = "The municipal tax bill has not been uploaded.",
                 type = "error")
    }
    if (is.null(input$upload_construction) & !is_error){
      is_error <- TRUE
      shinyalert(title = "A document is missing",
                 text = "The construction completion certificate has not been uploaded.",
                 type = "error")
    }
    if (is.null(input$upload_no_objection) & !is_error){
      is_error <- TRUE
      shinyalert(title = "A document is missing",
                 text = "The No Objection Certificate has not been uploaded.",
                 type = "error")
    }
    if (is.null(input$upload_deed) & !is_error){
      is_error <- TRUE
      shinyalert(title = "A document is missing",
                 text = "The verified sale deed has not been uploaded.",
                 type = "error")
    }
    if (!is_error) {
      shinyalert(title = "All documents have been sent", type = "success")
    }
  })
  
  paymentModal <- function(failed = FALSE) {
    modalDialog(
      disabled(textInput(inputId = "payment_amount", label = "Amount", value = "RS. 1500")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "Ok"),
        tags$script(src = "https://www.paypalobjects.com/api/checkout.js"),
        tags$script("paypal.Button.render({
                            // Configure environment
                            env: 'sandbox',
                            client: {
                            sandbox: 'demo_sandbox_client_id',
                            production: 'demo_production_client_id'
                            },
                            // Customize button (optional)
                            locale: 'en_US',
                            style: {
                            size: 'small',
                            color: 'gold',
                            shape: 'pill',
                            },
                            // Set up a payment
                            payment: function (data, actions) {
                            return actions.payment.create({
                            transactions: [{
                            amount: {
                            total: '0.01',
                            currency: 'USD'
                            }
                            }]
                            });
                            },
                            // Execute the payment
                            onAuthorize: function (data, actions) {
                            return actions.payment.execute()
                            .then(function () {
                            // Show a confirmation message to the buyer
                            window.alert('Thank you for your purchase!');
                            });
                            }
                            }, '#paypal_button');"),
        tags$div(id = "paypal_button"))
    )
  }
  
  observeEvent(input$pay_btn, {
    showModal(paymentModal())
  })
  
  observeEvent(input$ok, {
    removeModal()
    shinyalert("Payment successful", text = "Your payment has been received.", type = "success")
  })
  
  observeEvent(input$sign_contract_btn, {
    filled_up <- TRUE
    if (input$name_input == "" & filled_up) {
      filled_up <- FALSE
      shinyalert(title = "Missing field", text = "Your name is missing", type = "error", closeOnClickOutside = TRUE)
    }
    if (input$aadhaar_input == "" & filled_up) {
      filled_up <- FALSE
      shinyalert(title = "Missing field", text = "Your Aadhaar number is missing", type = "error", closeOnClickOutside = TRUE)
    }
    if (input$address_input == "" & filled_up) {
      filled_up <- FALSE
      shinyalert(title = "Missing field", text = "Your address is missing", type = "error", closeOnClickOutside = TRUE)
    }
    if (input$bankId_input == "" & filled_up) {
      filled_up <- FALSE
      shinyalert(title = "Missing field", text = "Your bank ID is missing", type = "error", closeOnClickOutside = TRUE)
    }
    
    if (filled_up) {
      showModal(modalDialog(
      tagList(
        p("You will not be able to make any changes after you confirm")
      ),
      title = "Are you sure you want to sign?",
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirmSignature", "Confirm")
                       )))
    }
    
  })
  
  observeEvent(input$confirmSignature, {
    removeModal()
    shinyalert(title = "Signature successful!",
               text = "Your contract have been digitally signed with the following associated hash: e3c897afcdd6471a00c29ae8a511eea4f65f3ed4dddba3d836a94e648d33ed04",
               type = "success", closeOnClickOutside = TRUE)
    toggleState(id = "name_input")
    toggleState(id = "aadhaar_input")
    toggleState(id = "birth_input")
    toggleState(id = "address_input")
    toggleState(id = "bankId_input")
  })
  
  output$download_receipt <- downloadHandler(
    filename = function() {
      paste("Deed-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      writeLines("This is a fake title deed.", file)
    }
  )
}

shinyApp(ui = ui,
        server = server,
        options = list(launch.browser = TRUE))

