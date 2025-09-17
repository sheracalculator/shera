library(shiny)
library(shiny.i18n)

addResourcePath("static", "www")

ui <- shinyUI(fluidPage(list(tags$head(HTML('<link rel = "icon", href = "static/ico.png",
                                            type = "image/png" />'))),
                        div(style = "padding: 1px 0px; width: '100%'",
                            titlePanel(windowTitle = "SHERA Scoring System",
                                       title = div(img(src = "static/logo.png",
                                                       height = 72, width = 72),
                                                   "SHERA Scoring System"))),
                        uiOutput('page_content')
))

translator <- Translator$new(
  translation_csvs_path = "data/",
  translation_csv_config = "data/translation_csv_config.yaml"
)

server <- shinyServer(function(input, output, session) {

  i18n <- reactive({selected <- input$selected_language
  if (length(selected) > 0 && selected %in% translator$get_languages()) {
    translator$set_translation_language(selected)
  }
  translator
})

  x <- reactive({
    as.numeric(input$age26) + as.numeric(input$sexo) + as.numeric(input$rop) +
      as.numeric(input$edema) + as.numeric(input$has)
  })

  y <- reactive({
    req(input$age26, input$sexo, input$rop, input$has, input$edema)
    if (x()<5)
      i18n()$t("Low Risk of Chronic Arthralgia")
    else
      i18n()$t("High Risk of Chronic Arthralgia")
    }
  )

  output$brutescore <- renderText({ x() })
  output$prediction <- renderText({ y() })  

  output$page_content <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          radioButtons("selected_language",
                      "Choose language | Escolha o idioma",
                      choices = translator$get_languages(),
                      inline = TRUE,
                      selected = input$selected_language),
          radioButtons(inputId ="sexo",
                       label = i18n()$t("Sex"),
                       choiceNames = c(i18n()$t("Male"),
                                       i18n()$t("Female")),
                       selected = character(0),
                       inline = FALSE,
                       width = NULL,
                       choiceValues = c("0","1")),
          radioButtons(inputId ="has",
                       label = i18n()$t("Hypertension"),
                       choiceNames = c(i18n()$t("Absent"),
                                       i18n()$t("Present")),
                       selected = character(0),
                       inline = FALSE,
                       width = NULL,
                       choiceValues = c("0", "2")),
          radioButtons(inputId ="edema",
                       label = i18n()$t("Edema"),
                       choiceNames =  c(i18n()$t("Absent"),
                                        i18n()$t("Present")),
                       selected = character(0),
                       inline = FALSE,
                       width = NULL,
                       choiceValues = c("0", "2.5")),
          radioButtons(inputId ="rop",
                       label = i18n()$t("Retro-Orbital Pain"),
                       choiceNames = c(i18n()$t("Absent"),
                                       i18n()$t("Present")),
                       selected = character(0),
                       inline = FALSE,
                       width = NULL,
                       choiceValues = c("0", "2")),
          radioButtons(inputId ="age26",
                       label = i18n()$t("Age"),
                       choiceNames = c(i18n()$t("≤ 26 years old"),
                                       i18n()$t(">26 years old")),
                       selected = character(0),
                       inline = FALSE,
                       width = NULL,
                       choiceValues = c("0", "1.5")),
          ),
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel(em(i18n()$t("Results")),
          h4(i18n()$t("SHERA Score Value")),
          verbatimTextOutput("brutescore"),
          br(),
          br(),
          h4(i18n()$t("Which resulted in a prediction of ")),
          verbatimTextOutput("prediction"),
          br(),
          br(),
          br(),
          br(),
          strong(i18n()$t("This Scoring System was designed to predict the risk chronic arthralgia in patients with chikungunya virus infection.")),
          br(),
          br(),
          p(i18n()$t("Read our paper:")), tags$a(href = "https://doi.org/10.1371/journal.pntd.0008467", "https://doi.org/10.1371/journal.pntd.0008467"),
          br(),
          br(),
          p(i18n()$t("If you use the SHERA Scoring System in your work, please cite the following:")),
          em("de Moraes L, Cerqueira-Silva T, Nobrega V, Akrami K, Santos LA, Orge C, Casais P, Cambui L, Rampazzo RD, Trinta KS, Montalbano CA. A clinical scoring system to predict long-term arthralgia in Chikungunya disease: A cohort study. PLOS Neglected Tropical Diseases. 2020 Jul 21;14(7):e0008467."),
          br(),
          br()),
          tabPanel(i18n()$t("About"),
                   strong(i18n()$t("Our model presented a sensitivity of 84% in the derivation cohort and 94% in the validation cohort, 
                   with an accuracy of 81% and 76% in the derivation and validation cohort, respectively.")),
                   br(),
                      strong(i18n()$t("The additional parameters and ROC curve are shown in the figure bellow")),
                   br(),
                   img(src = "static/parameters.jpg", height = 560, width = 800)
                   )))
        )
      )
  })
  

})

shinyApp(ui = ui, server = server)

# library(shinylive)
# library(httpuv)

# shinylive::export(appdir = ".", destdir = "docs")

# httpuv::runStaticServer("docs/", port = 8008)