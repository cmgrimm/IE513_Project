
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

# Begin server ------------------------------------------------------------

shinyServer(function(input, output) {

  t_intervals <- reactive({ seq(1,input$t_max,1) })
  t_instance <- reactive({ input$t_instance }) #time instance selected
  
  l_calculation <- reactive({
    switch(input$l_fun,
           "f(t) = sin(t/4)+2" = function(t) (sin(t/4)+2),
           "f(t) = t" = function(t) t,
           "f(t) = log(t)" = function(t) log(t)
    )#end switch
  })#end l_calculation
  
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
    p_x #notice to get dpois(x,lambda) call p_x[x+1]
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
      hc_xAxis(plotLines = list(
                list(
                  label = list(text = paste0(c("&lambda;: ", round(l_instance(),digits = 2)),collapse=""),
                               useHTML = T
                               ),
                  color = "#FF0000",
                  width = 2,
                  value = t_instance()
                 ) 
                )
               ) %>%
      hc_add_series(name = '&lambda;', 
                    data = round(l_values(),2),
                    marker=list(enabled=F),
                    useHTML = T) %>%
      hc_title(text = "&lambda; as a Function of Time",
               align = 'left',
               useHTML = T) %>%
      hc_add_theme(hc_theme_tufte()) %>%
      hc_tooltip(headerFormat = '<small>t = {point.key}</small><table>',
                 pointFormat = '<tr><td>{series.name}:  </td><td style="text-align: right"><b>{point.y}</b></td></tr></table>',
                 useHTML = T
                 )
  })
  
  output$p_dist_instance_hc <- renderHighchart({
    hc <- highchart() %>%
      hc_xAxis(seq(0,10)) %>%
      hc_add_series(name = "P(X = x)", 
                    data = round(p_dist(),2), 
                    marker=list(enabled=F),
                    type = "area") %>%
      hc_yAxis(min = 0, max = 0.5) %>%
      hc_title(text = paste0(c("Poisson Distribution: &lambda;=",round(l_instance(),2)),collapse=""),
               align = "left",
               useHTML = T) %>%
      hc_add_theme(hc_theme_tufte()) %>%
      hc_tooltip(headerFormat = '',
                 pointFormat = '<table><tr><td>P(X = {point.x}): <b>{point.y}</b></td></tr></table>')
      # hc_chart(
      #   type = "spline"
      # )
      # 
    
  })
  
})


