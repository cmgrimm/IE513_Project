
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

packages <- c(
  "shiny",
  "highcharter",
  "stats"
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

  t_intervals <- reactive({ seq(1,input$t_max,1) }) #time periods, max set by user
  t_instance <- reactive({ input$t_range[1] }) #time instance selected
  s_instance <- reactive({ input$t_range[2] }) #time instance selected
  
  #set the function of lambda
  l_calculation <- reactive({
    switch(input$l_fun,
           "sin(t/4)+2" = function(t) (sin(t/4)+2),
           "(1/t)+1" = function(t) (1/t)+1,
           "log(t)" = function(t) log(t),
           "custom" = function(t) eval(parse(text = input$l_fun_custom))
    )#end switch
  })#end l_calculation
  
  #calculate lambda over time
  l_values <- reactive({
    l_function <- l_calculation()
    l_function(t_intervals())
  })
  
  #instance lambda at s
  l_instance_s <- reactive({
    l_function <- l_calculation()
    l_function(s_instance()+1)#add one to get correct t, R has array base 1 and our array starts at t=0
  })
  
  #instance lambda at t
  l_instance <- reactive({
    l_function <- l_calculation()
    l_function(t_instance()+1)#add one to get correct t, R has array base 1 and our array starts at t=0
  })
  
  #density values for poisson distribution at a given instance lambda 
  p_dist <- reactive({
    p_x <- dpois(seq(0,10),lambda = l_instance())
    p_x #notice to get P(X=x) call p_x[x+1]
  })
  
  # Outputs -----------------------------------------------------------------
  
  #render s_instance slider, max value set by user
  output$t_range_ui <- renderUI({
    
    sliderInput("t_range",
                "Select a Time Interval",
                min = 1,
                max = input$t_max,
                value = c(1, input$t_max),
                step = 1)
    
  })#end renderUI slider input
  
  #render highchart of lambda over time given selected lambda function
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
                 ),
                list(
                  label = list(text = paste0(c("&lambda;: ", round(l_instance_s(),digits = 2)),collapse=""),
                               useHTML = T
                  ),
                  color = "#FF0000",
                  width = 2,
                  value = s_instance()
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
      hc_tooltip(headerFormat = 't: <b>{point.key}</b><table>',
                 pointFormat = '<tr><td>{series.name}:  </td><td style="text-align: right"><b>{point.y}</b></td></tr></table>',
                 useHTML = T
                 ) %>%
      hc_plotOptions(series=list(animation=F))
  })#end highchart
  
  #render poisson density function given an instance lambda
  output$p_diss_instance_hc <- renderHighchart({
    hc <- highchart() %>%
      hc_xAxis(plotLines = list(
                 list(
                   label = list(text = paste0(c("&mu;: ", round(l_instance(),digits = 2)),collapse=""),
                                useHTML = T
                   ),
                   color = "#FF0000",
                   width = 2,
                   value = l_instance()
                 ) 
               )) %>%
      hc_add_series(name = "P(X = x)", 
                    data = round(p_dist(),2), 
                    marker=list(enabled=F),
                    type = "areaspline") %>%
      hc_yAxis(min = 0, max = 0.5) %>%
      hc_title(text = paste0(c("Poisson Distribution: &lambda;=",round(l_instance(),2)),collapse=""),
               align = "left",
               useHTML = T) %>%
      hc_add_theme(hc_theme_tufte()) %>%
      hc_tooltip(headerFormat = 'x: <b>{point.x}</b>',
                 pointFormat = '<table><tr><td>P(X = x): <b>{point.y}</b></td></tr></table>',
                 useHTML = T)
  })#end highchart
  
  output$integral <- renderUI({
    t <- t_instance()
    s <- s_instance()
    
    value <- round(integrate(f = l_calculation(), lower = t, upper = s, subdivisions = 1000)$value,2)
    
    eq <- if(input$l_fun != "custom") {
      input$l_fun
    } else {
      input$l_fun_custom
    }
    
    withMathJax(
      sprintf(paste0("$$\\int_{",t,"}^{",s,"}[",eq,"]dt = ",value,"$$"))
    )
  })
  
})#end server


