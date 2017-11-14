
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


# Begin UI ----------------------------------------------------------------

shinyUI(
  fluidPage(
    titlePanel("Non-Homogenious Poisson Process"),
             fluidPage(
               fluidRow(
                 column(width = 4,
                   #select lambda function
                   selectInput("l_fun",
                               HTML("Select &lambda;(t) Function:"),
                               c("sin(t/4)+2","t","log(t)")
                               )#end selectInput
                 ),#end column
                 column(width = 4,
                        numericInput("t_max",
                                     "Select the Maximum Time",
                                     min = 1,
                                     max = 10000,
                                     value = 50,
                                     step = 1
                        )#end numeric input t_instance
                 ),#end column
                 column(width = 4,
                        uiOutput("t_instance_ui")#end uiOutput
                 )#end column

               ),#end fluidRow
               fluidRow(
                 highchartOutput("l_fun_hc")#end highchartOutput 
               ),#end fluidRow
               fluidRow(
                 highchartOutput("p_dist_instance_hc")#end highchartOutput 
               )#end fluidRow
             )#end fluidPage
             
  )#end navbarPage
  
)#end shinyUI
