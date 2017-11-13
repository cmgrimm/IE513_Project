
# This is the user-interface definition of a Shiny web application.
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

shinyUI(
  navbarPage(
    title = "Non-Homogenious Poisson Process",
    tabPanel("Home",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   
                   #select lambda function
                   selectInput("l_fun",
                               "Select Lambda Function:",
                               c("f(t) = |sin(t)|","f(t) = t","f(t) = log(t)")
                               ),#end selectInput
                   sliderInput("t_instance",
                               "Select t",
                               min = 0,
                               max = 100,
                               value = 10
                               )#end sliderInput
                   
                 ),#end sidebarPanel
                 mainPanel(
                   highchartOutput("l_fun_hc"),#end highchartOutput 
                   textOutput("test")
                 )#end mainPanel
                 
               )#end sidebarLayout
               
             )#end fluidPage
             
    )#end tabPanel 'Home'
    
  )#end navbarPage
  
)#end shinyUI
