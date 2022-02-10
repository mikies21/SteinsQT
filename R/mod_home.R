#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::valueBox(
        value = prettyNum(x = 2222, big.mark = ","),
        subtitle = "# Solsteins",
        color = "red",
        width = 2,
        icon = icon("credit-card"),
        href = "https://magiceden.io/marketplace/solstein"
      ),
      shinydashboard::valueBox(
        value = prettyNum(x = 8888, big.mark = ","),
        subtitle = "# of Quantum Traders",
        color = "blue",
        width = 2, icon = icon("credit-card"),
        href = "https://magiceden.io/marketplace/quantum_traders"
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "yeald range", width = 6, collapsible = F, collapsed = F,
        plotly::plotlyOutput(outputId = ns("yield_plot"))
      ),
      shinydashboard::box(
        title = "yeald boxplot", width = 6, collapsible = F, collapsed = F,
        plotly::plotlyOutput(outputId = ns("yield_boxplot"))
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, SteinsQT_selection) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$yield_plot <- plotly::renderPlotly({
      p <- plotly::plot_ly(type = 'scatter', mode = 'markers',
        data = yield_data, x = ~Rank, y = ~Total.Yield, color = ~type,
        text = ~ paste("collection: ", type, "<br>ID", ID, "<br>Rank:", Rank, "<br>Total Yield:", Staking.Total)
      ) %>%
        plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

      if (is.null(SteinsQT_selection())) {
        p
      } else {
        new_ponts <- subset(yield_data, subset = Name %in% SteinsQT_selection())
        p <- p %>% plotly::add_trace(new_ponts,
          x = ~Rank, y = ~Total.Yield, color = ~type,
          marker = list(
            color = "rgba(17, 157, 255,0.5)",
            size = 10),
          showlegend = F
        )
        p
      }
    })

    output$yield_boxplot <- plotly::renderPlotly({
      plotly::plot_ly(
        data = yield_data, x = ~type, y = ~Total.Yield, color = ~type, type = "box", boxpoints = "all", jitter = 0.3,
        pointpos = -1.8
      )
    })
  })
}

## To be copied in the UI
# mod_home_ui("home_ui_1")

## To be copied in the server
# mod_home_server("home_ui_1")
