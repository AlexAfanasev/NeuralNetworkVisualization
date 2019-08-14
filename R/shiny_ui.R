#' Returns the about panel
#'
#' @import shiny
#' @keywords internal
about_panel <- function () {
    about <- tabPanel("About",
                      "Here we need to add the documentation for the shiny app")
    return(about)
}

#' Returns the upload nn panel.
#'
#' @import shiny
#' @import shinyWidgets
#' @keywords internal
settings_panel <- function () {
    upload_panel <- tabPanel("Settings",
                             h4("Upload NeuralNetwork", style = "color:blue"),
                             fileInput("datafile", "Choose NeuralNetwork File",
                                       multiple = FALSE),
                             h4("Visualization Settings", style = "color:blue"),
                             uiOutput("networkplotting"),
                             actionBttn("go", "Press to plot!"))
    return(upload_panel)
}

#' Returns the visualization panel.
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @keywords internal
visualization_panel <- function () {
    current_visualization_panel <- tabPanel("Visualization",
                                            plotlyOutput("plot",
                                                         width = "100%",
                                                         height = "800"))
    return(current_visualization_panel)

}

#' Creates shiny app ui!
#'
#' @import shiny
#' @keywords internal
create_ui <- function () {
    ui <- tagList(
        fluidPage(
            br(),
            titlePanel("Marginal Effects for Neural Networks"),
            br(),
            tabsetPanel(
                about_panel(),
                settings_panel(),
                visualization_panel(),
                type = "tabs")))
    return(ui)
}
