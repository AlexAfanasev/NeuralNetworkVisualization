#' Run the shiny app.
#'
#' @examples
#' \dontrun{
#' # Starts the shiny app
#' run_shiny_app()
#' }
#' @import shiny
#' @name run_shiny_app
#' @export
run_shiny_app <- function(){
    app_directory <- system.file("example",
                                 package = "NeuralNetworkVisualization")
    if (app_directory == "") {
        stop("Could not find example directory. Try re-installing
             `NeuralNetworkVisualization`.", call. = FALSE)
    }
    return(runApp(app_directory, display.mode = "normal"))
}
