# 1. Add posibility to upload data set (csv or txt file)
# 2. Select predictors and dependent variables from columns of file
#    (for formula creation)
# 3. Select layers and scale (and maybe possiblity to add further parameters
#    with json format input)
# 4. Select which predictors to plot
# 5. Plot selected predictors


#' @export
run_shiny_app <- function () {
    library(shiny)
    shinyApp(ui = create_user_interface(),
             server = create_server())
}


#' @export
create_user_interface <- function () {

}

#' @export
create_server <- function () {

}
