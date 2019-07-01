# TODO:
# 1. Multiple checkboxes
# 2. Center everything (left and right side of the website should have margins)
# 3. Add submit button to Plot tab
# 4. Add flash message to Neural Network Settings tab when model is sucessfully
#    fitted
# 5. Add clear (model and data) button maybe???

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
    shinyApp(ui = ui, server = server)
}
