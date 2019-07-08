#' Returns the about panel
#' @keywords internal
#' @import shiny
#' @import shinyWidgets
#' 
about_panel <- function () {
    about <- shiny::tabPanel("About",
                      "Here we need to add the documentation for the shiny app")
    return(about)
}

#' Returns the upload nn panel.
#'
#' @keywords internal
upload_nn_panel <- function () {
    upload_panel <- shiny::tabPanel(
        "Upload NeuralNetwork",
        shiny::h4("Upload NeuralNetwork", style = "color:blue"),
        shiny::fileInput("datafile", "Choose NeuralNetwork File",
                  multiple = FALSE))
    return(upload_panel)
}

#' Returns the visualization panel.
#'
#' @keywords internal
visualization_panel <- function () {
    return(shiny::tabPanel("Plot",
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(width = 5,
                                   shiny::br(),
                                   shiny::uiOutput("networkplotting"),
                                   shiny::br(),
                                   shinyWidgets::actionBttn("go", "Press to plot!"),
                                   shiny::br()
                      ),
                      shiny::mainPanel(width = 7,
                                shiny::br(),
                        plotly::plotlyOutput("plot")))))

}

ui <- shiny::tagList(
      shiny::tags$style("html,body{background-color: white;},
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
        shiny::tags$div(class="container",
        shiny::fluidPage(
        shiny::br(),
        shiny::titlePanel("Marginal Effects for Neural Networks"),
        shiny::br(),
        shiny::tabsetPanel(
        about_panel(),
        upload_nn_panel(),
        visualization_panel(),
        type = "tabs"
       )
     )
    )
  )