library(shiny)
library(shinyWidgets)

#' Returns the about panel.
#'
#' @keywords internal
about_panel <- function () {
    about <- tabPanel("About",
                      "Here we need to add the documentation for the shiny app")
    return(about)
}

#' Returns the upload data panel.
#'
#' @keywords internal
upload_data_panel <- function () {
    upload_panel <- tabPanel(
        "Upload Data",
        h4("Upload Data", style = "color:blue"),
        fileInput("datafile", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/plain")),
        h4("Upload Settings", style = "color:blue"),
        awesomeCheckbox("header", "Header", value = TRUE),
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","))
    return(upload_panel)
}


#' Returns the NeuralNetwork settings panel.
#'
#' @keywords internal
nn_settings_panel <- function () {
    return(tabPanel("Neural Network Settings",
                    br(),
                    uiOutput("networktraining")))
}

#' Returns the visualization panel.
#'
#' @keywords internal
visualization_panel <- function () {
    return(tabPanel("Plot",
                    br(),
                    uiOutput("networkplotting"),
                    plotly::plotlyOutput("plot")))
}

ui <- fluidPage(
    titlePanel("Marginal Effects for Neural Networks"),

    tabsetPanel(
        about_panel(),
        upload_data_panel(),
        nn_settings_panel(),
        visualization_panel(),
        type = "tabs"
    )
)
