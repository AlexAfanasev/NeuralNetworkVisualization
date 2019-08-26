library(NeuralNetworkVisualization)
library(shiny)
library(shinyWidgets)
library(plotly)
library(waiter)

# Returns the about panel.
about_panel <- function () {
    about <- tabPanel(
        "About", icon = icon("info-circle"),
        includeMarkdown(file.path("about", "about_page.md")))
    return(about)
}

# Returns the settings panel.
settings_panel <- function () {
    upload_panel <- tabPanel(
        "Settings", icon = icon("cog"), use_waiter(),
        h3("1. Upload NeuralNetwork", style = "color:blue"),
        fileInput("datafile", "Choose NeuralNetwork File", multiple = FALSE),
        uiOutput("networkplotting"),
        br())
    return(upload_panel)
}

# Returns the visualization panel.
visualization_panel <- function () {
    current_visualization_panel <- tabPanel(
        "Visualization", value = "panel3", icon = icon("bar-chart"),
        plotlyOutput("plot", width = "100%", height = "800"))
    return(current_visualization_panel)
}

# Shiny UI
tagList(
    tags$head(
        tags$link(rel = "shortcut icon", type = "image/x-icon",
                  href = "favicon.ico"),
        tags$style(
            HTML(
                ".shiny-notification {
                    position:fixed;
                    top: calc(0%);
                    right: calc(0%);
                }"))
    ),
    div(
        div(
    fluidPage(
        br(),
        titlePanel("Marginal Effects for Neural Networks"),
        br(),
        tabsetPanel(
            id = "inTabset",
            about_panel(),
            settings_panel(),
            visualization_panel(),
            type = "tabs"))
        ),
    style = "margin-left:auto;margin-right:auto;width:90%;"
    )
)
