
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

#simulation function for a non homogeneous poisson process
nhpp_sim <- function(start,end,trials,l_func){
  k=1 #index for getting arrival times
  Customers <- NA #total arrival count from t to s
  arrivalTimes <- NA #time of arrival
  
  j=1
  while(j<trials+1){#Runs defined number of simulations
    arrivalvalue=0
    arrivals <- NA #array of arrivals
    i = 1 #used for indexing
    t <- start
    s <- end
    
    while (t < s) {
      #lambda to be changed between options
      l_value <- l_func(t) #lambda value
      arrivalvalue<-rexp(1,rate=l_value)
      #Jumps by incriments of .01 seconds, if the arrival occured in that time frame, document it
      if(arrivalvalue > .01) t=t+.01 else t=t+arrivalvalue
      if(arrivalvalue > .01){
        arrivals[i] <- NA
      } else {
        arrivals[i] <- t
        if(j==1){
          arrivalTimes[k]<-t
          k<-k+1
        }
      }
      i <- i +1
    }#End while s<t
    
    #Count of number of arrivals in duration
    N=length(arrivals[!is.na(arrivals)])
    Customers[j] <- N
    j=j+1 #Move to next simulation (of 100)
  }#End while j<101
  return(Customers)
}



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

  #get lambda function as string
  eq <- reactive({
    if(input$l_fun != "custom") {
      input$l_fun
    } else {
      input$l_fun_custom
    }
  })
  
  mean_value <- reactive({
    t <- t_instance()
    s <- s_instance()
    
    value <- round(integrate(f = l_calculation(), lower = t, upper = s, subdivisions = 1000)$value,2)
    value
  })
  
  #distribution from t to s given lambda(t)
  p_dist <- reactive({
    max <- mean_value() + 30
    x <- rep(NA,max)
    f <- function(t,s,n) {
      ((integrate(f=l_calculation(),lower = t,upper = s)$value)^n) * 
        exp(-integrate(f=l_calculation(),lower = t,upper = s)$value) /
        factorial(n)
    }
    
    for(i in 1:max){
      x[i] <- f(t_instance(),s_instance(),i)
    }
    x
    
  })
  
  # Outputs -----------------------------------------------------------------
  
  #render s_instance slider, max value set by user
  output$t_range_ui <- renderUI({
    
    sliderInput("t_range",
                "Select a Time Interval",
                min = 1,
                max = input$t_max,
                value = c(1, (input$t_max-1)),
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
      # hc_add_series(name = '&lambda;', 
      #               data = round(l_values(),2),
      #               marker=list(enabled=F),
      #               useHTML = T) %>%
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
  output$p_dist_range_hc <- renderHighchart({
    hc <- highchart() %>%
      hc_xAxis(plotLines = list(
                 list(
                   label = list(text = paste0(c("&mu;: ", round(mean_value(),digits = 2)),collapse=""),
                                useHTML = T
                   ),
                   color = "#FF0000",
                   width = 2,
                   value = mean_value()
                 ) 
               )) %>%
      hc_add_series(name = "P(X = x)", 
                    data = round(p_dist(),2), 
                    marker=list(enabled=F),
                    type = "areaspline") %>%
      hc_yAxis(min = 0, max = 0.5) %>%
      hc_title(text = paste0(c("P(N(",t_instance(),",",s_instance(),") = x) when &lambda;(t) = ",eq()),collapse="",sep=""),
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
    
    value <- mean_value()
    
    eq <- eq()
    
    withMathJax(
      sprintf(paste0("$$E[N(",t,",",s,")]=\\int_{",t,"}^{",s,"}[",eq,"]dt = ",value,"$$"))
    )
  })
  
})#end server


