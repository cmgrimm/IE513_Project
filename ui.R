
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


packages <- c(
  "shiny",
  "highcharter",
  "stats",
  "shinyjs"
  )
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)


# Begin UI ----------------------------------------------------------------

shinyUI(fluidPage(
  titlePanel("Non-Homogeneous Poisson Process"),
  sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   #select lambda function
                   selectInput("l_fun",
                               HTML("Select &lambda;(t) Function:"),
                               c("sin(t/4)+2","(1/t)+1","log(t)","[Custom]" = "custom")
                               ),#end selectInput
                   # Only show this panel if Custom is selected
                   conditionalPanel(
                     condition = "input.l_fun == 'custom'",
                     textInput("l_fun_custom", HTML("Custom &lambda;(t) Function:"),value="sin(log(t/2))")
                   )
                 ),#end fluidRow
                 fluidRow(
                        numericInput("t_max",
                                     "Select the Maximum Time",
                                     min = 5,
                                     max = 100,
                                     value = 25,
                                     step = 1
                        )#end numeric input t_instance
                 ),#end fluidRow
                 fluidRow(
                        uiOutput("t_range_ui")#end uiOutput
                 ),#end fluidRow
                 fluidRow(
                   numericInput("trials",
                                "Simulation Trials",
                                min = 10,
                                max = 500,
                                value = 100,
                                step = 1)
                 )
               ),#end sidebarPanel
               
               mainPanel(
                 inlineCSS(".btn { margin-top: 10px;
                                   margin-bottom: 50px }"),
                 tabsetPanel(
                   tabPanel("Equations", uiOutput("equations")),
                   tabPanel("Theoreticl Graphs",
                            highchartOutput("l_fun_hc"),#end highchartOutput 
                            highchartOutput("p_dist_range_hc")#end highchartOutput 
                            ),
                   tabPanel("Simulation",
                              fluidRow(
                                actionButton("simulate","Run Simulation")
                              ),
                              fluidRow(
                                highchartOutput("sim_hist")
                              ),
                              fluidRow(
                                highchartOutput("sim_independance")
                              )
                            )
                 )#end tabsetPanel
               )#end mainPanel

  )#end sidebarLayout
)#end fluidPage
  
)#end shinyUI
