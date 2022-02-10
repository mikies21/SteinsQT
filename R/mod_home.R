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
      ),
      shinydashboard::box(title = "About me",collapsible = F,width = 8,  HTML("Hi all, <br> It's alkemx here. 
                              This tool allows you to see where Steins and QTs are in the total rank and what their Yield over time is. It's not perfect and it's quite slow still,
                                           I will update it in my free time (busy PhD student here). <br>
                                           for any feedback or feature catch me on discord (alkemx.sol#1075) or twitter @alkemx, coffee tips are welcome (alkemx.sol)")
        
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "yield range", width = 6, collapsible = F, collapsed = F,
        plotly::plotlyOutput(outputId = ns("yield_plot"))
      ),
      shinydashboard::box(
        title = "yield boxplot", width = 6, collapsible = F, collapsed = F,
        plotly::plotlyOutput(outputId = ns("yield_boxplot"))
      )
    ),
    fluidRow(
      shinydashboard::box(title = "Yield table",
                          DT::dataTableOutput(ns("yield_table"))),
      shinydashboard::box(title = "Yield table",
                          DT::dataTableOutput(ns("yield_summary")))
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, SteinsQT_selection) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    updated_table <- reactive({
      req(SteinsQT_selection())
      new_ponts <- subset(yield_data, subset = Name %in% SteinsQT_selection())
    })
    
    
    
    output$yield_plot <- plotly::renderPlotly({
      
      p <- ggplot2::ggplot(yield_data, ggplot2::aes(x = Rank,
                                                    y = Total.Yield,
                                                    colour = type,
                                                    text = paste("collection: ", type, "<br>ID", ID, "<br>Rank:", Rank, "<br>Total Yield:", Staking.Total)))+
        ggplot2::geom_point(size = 1)+
        ggplot2::theme_bw()
      
      if (is.null(SteinsQT_selection())) {
        
        p <- plotly::ggplotly(p)
        
      } else {
        new_ponts <- updated_table()
        
        p <- p + ggplot2::geom_point(data = new_ponts, ggplot2::aes(x = Rank,
                                                                    y = Total.Yield, size = 6,
                                                                    label = Name),
                                     colour = 'black')
      }
      
      p <- plotly::ggplotly(p) %>% 
        plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      p
        
    
      ############Plotly option
      #p <- plotly::plot_ly(type = 'scatter', mode = 'markers',
      #  data = yield_data, x = ~Rank, y = ~Total.Yield, color = ~type,
      #  text = ~ paste("collection: ", type, "<br>ID", ID, "<br>Rank:", Rank, "<br>Total Yield:", Staking.Total)
      #) %>%
      #  plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

      #if (is.null(SteinsQT_selection())) {
      #  p
      #} else {
      #  new_ponts <- subset(yield_data, subset = Name %in% SteinsQT_selection())
      #  p <- p %>% plotly::add_trace(new_ponts,
      #    x = ~Rank, y = ~Total.Yield, color = ~type,
      #    marker = list(
      #      color = "rgba(17, 157, 255,0.5)",
      #      size = 10),
      #    showlegend = F
      #  )
      #  p
      #}
    })

    output$yield_boxplot <- plotly::renderPlotly({
      p <- ggplot2::ggplot(yield_data, ggplot2::aes(x = type, y = Total.Yield, fill = type))+
        ggplot2::geom_boxplot(outlier.shape = NA)+
        ggplot2::geom_jitter(position = ggplot2::position_nudge(x = -0.4), ggplot2::aes(colour = type), show.legend = F)+
        ggplot2::theme_bw()
        
      
      p <- plotly::ggplotly(p) %>% 
        plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      p
      #plotly::plot_ly(
      #  data = yield_data, x = ~type, y = ~Total.Yield, color = ~type, type = "box", boxpoints = "all", jitter = 0.3,
      #  pointpos = -1.8
      #)
    })
    
    
    output$yield_table <- DT::renderDataTable({
      filtered_table <- updated_table()[, c("Name", "Rank", "Initial.Airdrop", "Staking.Total", "Total.Yield")]
      DT::datatable(filtered_table, 
                    rownames = F, options = list(
                      autoWidth = FALSE, 
                      scrollX = TRUE)
                    )
    })
    
    output$yield_summary <- DT::renderDataTable({
      summary_table <- aggregate(list(Initial.Airdrop = updated_table()$Initial.Airdrop, 
                                      Staking.Total = updated_table()$Staking.Total,
                                      Total.Yield = updated_table()$Total.Yield),
                                 by = list(type = updated_table()$type),  sum, drop = T)
      summary_table[3,] <- c("Total", sum(summary_table$Initial.Airdrop),sum(summary_table$Staking.Total),sum(summary_table$Total.Yield))

      DT::datatable(summary_table, 
                    rownames = F, options = list(
                      autoWidth = FALSE, 
                      scrollX = TRUE)
      )
    })
    
  })
}

## To be copied in the UI
# mod_home_ui("home_ui_1")

## To be copied in the server
# mod_home_server("home_ui_1")
