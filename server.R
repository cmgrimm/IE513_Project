
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  t_intervals <- 0:100

  l_calculation <- reactive({
    switch(input$l_fun,
           "f(t) = |sin(t)|" = function(t) abs(sin(t)),
           "f(t) = t" = function(t) t,
           "f(t) = log(t)" = function(t) log(t)
           )#end switch
  })#end l_calculation
  
  isolate(l_calculation())
  
  y <- l_calculation(t_intervals)#calculate lambda

  # output$l_fun_hc <- renderHighchart({
  #   hc <- highchart() %>%
  #     hc_xAxis(title = "Time", categories = t_intervals ) %>%
  #     hc_add_series(name = "Lambda", data = y)
  # })
  
  output$test <- renderText({
    paste0(y,sep=",",collapse = ",")
  })
})
