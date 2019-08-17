#' Run the shiny app for visualizing the marginal effects of the NeuralNewtork
#'
#' @examples
#' \dontrun{
#' # Starts the shiny app
#' run_shiny_app()
#' }
#' @importFrom  shiny shinyApp
#' @name run_shiny_app
#' @export
run_shiny_app <- function () {
    return(shinyApp(ui = create_ui(), server = server))
}
