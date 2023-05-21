library(shiny)
library(ggplot2)

source("helpers.R")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("select_cluster", ("Select cluster:"), 
                  choices = c("Cluster 1", "Cluster 2", "Cluster 3", 
                              "Cluster 4", "Cluster 5", "Cluster 6", 
                              "Cluster 7", "Cluster 8", "Cluster 9", 
                              "Cluster 10"),
                  selected = "Cluster 1"),
      selectInput("select_stock", "Select stock:",
                  choices = sort(group_lis[[1]]$stock_id),
                  selected = sort(group_lis[[1]]$stock_id)[1]),
      numericInput(inputId = "time_id", label = "Enter index of time ID:", value = 1),
      checkboxGroupInput("checkGroup", 
                         ("Select model to show"), 
                         choices = list("ARIMA" = 1, 
                                        "Linear Regression" = 2, 
                                        "HAV-RV" = 3,
                                        "GARCH" = 4),
                         selected = c(1, 2, 3, 4))
    ),
    mainPanel(
      h2(textOutput(("selected_cluster"))),
      plotOutput(outputId = "plot1")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      radioButtons("method", ("Metrics"),
                   choices = list("RMSE" = "RMSE",
                                  "MAE" = "MAE", "QLIKE" = "QLIKE"),selected = "RMSE")
    ),
    mainPanel(
      h2("Model Performance Comparison for Stocks"),
      plotOutput(outputId = "plot2")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$select_cluster, {
    stock_choices <- switch(input$select_cluster,
                            "Cluster 1" = sort(group_lis[[1]]$stock_id),
                            "Cluster 2" = sort(group_lis[[2]]$stock_id),
                            "Cluster 3" = sort(group_lis[[3]]$stock_id),
                            "Cluster 4" = sort(group_lis[[4]]$stock_id),
                            "Cluster 5" = sort(group_lis[[5]]$stock_id),
                            "Cluster 6" = sort(group_lis[[6]]$stock_id),
                            "Cluster 7" = sort(group_lis[[7]]$stock_id),
                            "Cluster 8" = sort(group_lis[[8]]$stock_id),
                            "Cluster 9" = sort(group_lis[[9]]$stock_id),
                            "Cluster 10" = sort(group_lis[[10]]$stock_id)
                            )
    updateSelectInput(session, "select_stock", choices = stock_choices)
  })
  
  output$selected_cluster <- renderText({
    paste0("Actual volatility vs Predicted volatility (", input$select_cluster, ")")
  })
  
  output$plot1 <- renderPlot({
    time_idx = strtoi(input$time_id)
    time_id <- comm_time_IDs[[time_idx]]
    
    stock_id = as.character(input$select_stock)
    
    show_models <- input$checkGroup
    draw_res_plot(stock_id, time_id, show_models)
  })
  
  output$plot2 <- renderPlot({
    stock_id = as.character(input$select_stock)
    method = input$method
    plot_performance(stock_id, method)
  })
  
}

shinyApp(ui, server)
