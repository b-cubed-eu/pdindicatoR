#' Visualizing PD maps for time periods in tabs
#'
#' This function creates produces an r-shiny app that can showcase
#' multiple PD maps (for separate time periods) in tabs
#' @param pd_indicator List containing PD plots and indicators, produced by
#' function generate_map_and_indicator.R
#' @param plots A list of PD maps produced by the function
#' `generate_map_and_indicator()`, named by their time-period.
#' @return An r-shiny app with PD maps in tabs
#' @import dplyr
#' @examples
#' library(dplyr)
#' ex_data <- retrieve_example_data()
#' mcube <- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
#' mcube <- dplyr::filter(mcube, !is.na(ott_id))
#' PD_cube <- get_pd_cube(mcube, ex_data$tree)
#' pd_indicator<- generate_map_and_indicator(
#'   PD_cube,
#'   ex_data$grid,
#'   taxon="Fagales")
#' plots <- pd_indicator[[1]]
#' indicators <- pd_indicator[[2]]
#' \dontrun{make_shiny_maps(pd_indicator, plots)}
#' @export
#'

make_shiny_maps <- function(pd_indicator, plots) {
  # Create Shiny app to display the plots in tabs
  ui <- shiny::fluidPage(
    shiny::titlePanel("Phylogenetic Diversity (PD) Maps by Time Period"),

      # Top bar with help text
      shiny::fluidRow(
        shiny::column(12, align = "center",
               shiny::helpText(paste("Browse through the different time",
                                     "periods to see the PD indicators."))
        )
      ),
      shiny::mainPanel(
        # Use do.call to pass the list of tabs as separate arguments
        do.call(shiny::tabsetPanel,
                # Dynamically create a tab for each period
                lapply(names(pd_indicator[[1]]), function(period) {
                  shiny::tabPanel(
                    title = paste("Period", period),
                    shiny::plotOutput(outputId = paste0("plot_", period),
                               height = "600px", width = "900px")
                  )
                })
        )
      )
    )

  server <- function(input, output, session) {

    # Render each plot in a separate output
    lapply(names(plots), function(period) {
      output[[paste0("plot_", period)]] <- shiny::renderPlot({
        plots[[period]]
      }, res = 150)
    })
  }

  # Run the Shiny app
  shiny::shinyApp(ui = ui, server = server)
}
