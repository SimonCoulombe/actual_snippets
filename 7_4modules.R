library(shiny)
# pas de call module
# on select le dataset dans un
# module 1 select dataset
# module 2 select colonnes
# module 3 print data
# module 4 download ata

mod_select_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("dataset"), "Choose a dataset:",
                choices = c("rock", "pressure", "cars")
    )
  )
}

mod_select_dataset_server <- function(id, r){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      # Whenever the choice changes, the value inside r is set
      observeEvent( input$dataset , {
        message(input$dataset)
        r$dataset <- switch(input$dataset,
                            "rock" = dplyr::as_tibble(rock),
                            "pressure" = dplyr::as_tibble(pressure),
                            "cars" = dplyr::as_tibble(cars))
      })
    }
  )
}

# module 2 ,select colonnes
mod_select_colonnes_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("colonnes"),label = "Choose some columns", choices = NULL, multiple = TRUE),
    tableOutput(ns("table"))
  )
}

mod_select_colonnes_server <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      colonnes <- names(r$dataset)
      updateSelectInput( session, "colonnes", choices = colonnes)
    })

    # Whenever the select coloumns change changes, the value inside r is set to the updated dataset+colonnes
    observeEvent( input$colonnes , {
      r$dataset_selection <- r$dataset[, input$colonnes]
      message(class(r$dataset_selection))
    })
  }
  )
}

# Module 3, which will display the data
mod_printing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tableOutput(ns("table"))
  )
}

mod_printing_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    # We evaluate the reactiveValue element modified in the
    # first module
    output$table <- renderTable({
      head(r$dataset_selection)
    })
  }
  )
}
# module 4, to download the data

mod_download_ui <- function(id) {
  ns <- NS(id)
  actionButton(
    inputId = ns("render_table"),
    label = "Download table",
    icon = shiny::icon("paper-plane"),
    style = glue::glue("color:#fff; background-color:#555")
  )
}

mod_download_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$render_table, {
      showModal(
        modalDialog(
          title = NULL,
          h3("Render and download the table?", style = "text-align: center;"),
          footer = tagList(
            downloadButton(
              outputId = ns("download_table"),
              label = "Yes"
            ),
            modalButton("Cancel")
          ),
          size = "m"
        )
      )
    })

    # Make the rendered table available for download
    # when the "Download table" button is clicked
    output$download_table <- downloadHandler(
      filename = "table.csv",
      content = function(file) {
        on.exit(removeModal())
        withProgress(message = "Rendering, please wait!", {
          readr::write_csv(r$dataset_selection, file )
        })
      }
    )
  })
}

# Application
library(shiny)
library(dplyr)
app_ui <- function() {
  fluidPage(
    h1("appdemo"),
    mod_select_dataset_ui("select_dataset"),
    mod_select_colonnes_ui("select_colonnes"),
    mod_printing_ui("print_table"),
    mod_download_ui("download")
  )

}


app_server <- function(input, output,session) {
  # List the first level callModules here
  r <- reactiveValues()

  mod_select_dataset_server(id="select_dataset", r = r)
  mod_select_colonnes_server(id = "select_colonnes", r = r)
  mod_printing_server(id = "print_table", r = r)
  mod_download_server(id = "download", r = r)
}




shinyApp(app_ui, app_server)
