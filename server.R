
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  t_intervals <- 0:100
  
  l_fun <- reactive({
    as.character(input$l_fun)
  })#end l_fun
  
  l_calculation <- function(t,f_t) {
    
    if(f_t == "f(t) = |sin(t)|"){
      abs(sin(t))
    }else if (f_t == "f(t) = t"){
      t
    }else {
      log(t)
    }#end ifelse block
    
    
    # switch(f_t,
    #        "f(t) = |sin(t)|" = abs(sin(t)),
    #        "f(t) = t" = t,
    #        "f(t) = log(t)" = log(t)
    #        )#end switch
  }#end l_calculation
  
  y <- l_calculation(t_intervals,l_fun)

  output$l_fun_hc <- renderHighchart({
    hc <- highchart() %>%
      hc_xAxis(title = "Time", categories = t_intervals ) %>%
      hc_add_series(name = "Lambda", data = y)
  })
  
  output$test <- renderText({
    l_fun
  })
})
