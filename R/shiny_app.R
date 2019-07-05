# TODO:
# 1. Shiny widget pickerInput for checkboxes in plot panel
# 2. Add action button to plot panel, 2 input boxes for confidence interval probs, maybe input box for nrepititions?????
# 3. Center everything (left and right side of the website should have margins)

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
