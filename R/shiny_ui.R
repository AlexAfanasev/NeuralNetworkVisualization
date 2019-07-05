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

#' Returns the upload nn panel.
#'
#' @keywords internal
upload_nn_panel <- function () {
    upload_panel <- tabPanel(
        "Upload NeuralNetwork",
        h4("Upload NeuralNetwork", style = "color:blue"),
        fileInput("datafile", "Choose NeuralNetwork File",
                  multiple = FALSE))
    return(upload_panel)
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
        upload_nn_panel(),
        visualization_panel(),
        type = "tabs"
    )
)
