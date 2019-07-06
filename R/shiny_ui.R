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
                    sidebarLayout(
                      sidebarPanel(width = 5,
                                   br(),
                                   uiOutput("networkplotting"),
                                   br(),
                                   actionBttn("go", "Press to plot!"),
                                   br()
                      ),
                      mainPanel(width = 7,
                                br(),
                        plotly::plotlyOutput("plot")))))

}

ui <- tagList(
      tags$style("html,body{background-color: white;},
                 main{ background-color: white;
                 }
                 .container{
                 width: 100%;
                 margin: 0 auto;
                 padding: 0;
                 }
                 #myimg{
                 width:30%;
                 }
                 @media screen and (min-width: 1200px){
                 .container{
                 width: 1200px;
                 }
                 }"),
        tags$div(class="container",
        fluidPage(
        br(),
        titlePanel("Marginal Effects for Neural Networks"),
        br(),
        tabsetPanel(
        about_panel(),
        upload_nn_panel(),
        visualization_panel(),
        type = "tabs"
       )
     )
    )
  )