# HOW TO STYLE UI ELEMENTS INSIDE MODULES.  For some reason selectInput was harder than autonumericInput
# https://stackoverflow.com/questions/72870808/background-color-of-dynamic-ui-input-element-in-shiny-module
# https://stackoverflow.com/questions/44159168/how-to-style-an-single-individual-selectinput-menu-in-r-shiny

library(shiny)
library(shinyWidgets)
library(thematic)
library(bslib)
selectModUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("varinput"))
}

selectModServer <- function(id, label, car, var, color) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    val <- reactive({
      mtcars[car(), var]
    })

    output$varinput <- renderUI({
      ns <- session$ns
      tagList(
        tags$style(paste0("#", ns("numinput"), "{background-color: ", color, " !important;})")),
        autonumericInput(ns("numinput"),
                         label = label,
                         value = val(),
                         align = "right"),
        tags$head(tags$style(HTML(paste0('
    .selectize-input {white-space: nowrap}
    #',ns("selectinput"),'+ div>.selectize-dropdown{width: 100px !important; font-style: italic; font-weight: bold; color: ', color, '; background: brown;}
    #',ns("selectinput"),'+ div>.selectize-input   {width: 100px !important; font-style: italic; font-weight: bold; color: ', color, '; background: brown; margin-bottom: -10px;}
                            ')))),

        selectInput(ns("selectinput"),
                    label = "prout",
                    choices = c("prout", "proutprout")
        )
      )
    })
  })
}

ui <- bslib::page_navbar(
  title = "title",
  bg = "orange", # navbar background
  theme = bslib::bs_theme(
    preset = "shiny",
    "primary" = "orange",
    version = 5
  ),
  bslib::nav_panel(
    title = "Dashboard", # Dashboard nav_panel -----
    bslib::layout_sidebar(
      sidebar =
        bslib::sidebar(
          selectInput("carsel", "Select a car", choices = row.names(mtcars),
                      selected = row.names(mtcars)[1]),
          selectModUI("in1"),
          selectModUI("in2")
        ),
      bslib::layout_column_wrap( ### column wrap   ----
                                 width = 1 / 2,
                                 height = 300,
                                 h1("content goes here")
      )
    )
  )
)


server <- function(input, output) {
  thematic::thematic_shiny(font = "auto")
  selectModServer(id = "in1",
                  label = "Horsepower",
                  car = reactive(input$carsel),
                  var = "hp",
                  color = "darkseagreen")
  selectModServer(id = "in2",
                  label = "Rear axle ratio",
                  car = reactive(input$carsel),
                  var = "drat",
                  color = "khaki")
}

shinyApp(ui = ui, server = server)
