#' Visualizing PD maps for time periods in tabs
#'
#' This function creates produces an r-shiny app that can showcase
#' multiple PD maps (for seperate timeperiods) in tabs
#'
#' @param plots A list of PD maps produced by the function
#' generate_map_and_indicator(), named by their time-period.
#' @return An r-shiny app with PD maps in tabs
#' @example make_shiny_maps(plots)
#'
#'

make_shiny_maps <- function(plots){
# Create Shiny app to display the plots in tabs
ui <- fluidPage(
  titlePanel("Phylogenetic Diversity (PD) Maps by Time Period"),

    # Top bar with help text
    fluidRow(
      column(12, align = "center",
             helpText("Browse through the different time periods to see the PD indicators.")
      )
    ),
    mainPanel(
      # Use do.call to pass the list of tabs as separate arguments
      do.call(tabsetPanel,
              # Dynamically create a tab for each period
              lapply(names(PDindicator[[1]]), function(period) {
                tabPanel(
                  title = paste("Period", period),
                  plotOutput(outputId = paste0("plot_", period),
                             height = "600px", width = "900px")
                )
              })
      )
    )
  )

server <- function(input, output, session) {

  # Render each plot in a separate output
  lapply(names(plots), function(period) {
    output[[paste0("plot_", period)]] <- renderPlot({
      plots[[period]]
    }, res = 150)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)}
