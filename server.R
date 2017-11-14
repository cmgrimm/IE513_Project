
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

packages <- c(
  "shiny",
  "highcharter"
)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

shinyServer(function(input, output) {

  t_intervals <- reactive({ seq(1,input$t_max,1) })
  t_instance <- reactive({ input$t_instance }) #time instance selected
  
  l_calculation <- reactive({
    switch(input$l_fun,
           "f(t) = sin(t/4)+2" = function(t) (sin(t/4)+2),
           "f(t) = t" = function(t) t,
           "f(t) = |log(t)|" = function(t) abs(log(t))
    )#end switch
  })#end l_
  
  #calculate lambda over time
  l_values <- reactive({
    l_function <- l_calculation()
    l_function(t_intervals())
  })
  
  #instance lambda
  l_instance <- reactive({
    l_function <- l_calculation()
    l_function(t_instance())
  })
  
  p_dist <- reactive({
    p_x <- dpois(seq(0,10),lambda = l_instance())
    p_x
  })
  
  # Outputs -----------------------------------------------------------------
  
  output$t_instance_ui <- renderUI({
    sliderInput("t_instance",
                "Select a Time Instance",
                min = 1,
                max = input$t_max,
                value = 1,
                step = 1)
    
  })
  
  output$l_fun_hc <- renderHighchart({
    hc <- highchart() %>%
      hc_xAxis(#categories = t_intervals(),
               plotLines = list(
                list(
                  label = list(text = paste0(c("Lambda: ", round(l_instance(),digits = 2)))),
                  color = "#FF0000",
                  width = 2,
                  value = t_instance()
                 ) 
                )
               ) %>%
      hc_add_series(name = 'Lambda', 
                    data = l_values(),
                    marker=list(enabled=F)) %>%
      hc_title(text = "Lambda as a Function of Time",
               align = 'left') %>%
      hc_exporting(enabled = T)
  })
  
  output$p_dist_instance_hc <- renderHighchart({
    hc <- highchart() %>%
      hc_xAxis(seq(0,10)) %>%
      hc_add_series(name = "P(X = x)", 
                    data = p_dist(), 
                    marker=list(enabled=F)) %>%
      hc_yAxis(min = 0, max = 0.5) %>%
      hc_title(text = "Poisson Distribution",align = "left")# %>%
      # hc_chart(
      #   type = "spline"
      # )
      # 
    
  })
  
})


