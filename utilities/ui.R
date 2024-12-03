#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


# header <- dashboardHeader()
# 
# sidebar <- dashboardSidebar(
#   
#   collapsed = FALSE, width = 4,
#   sidebarMenu(
#     
#     menuItem(
#       "Side 1",
#       tabName = "tab1",
#       icon = icon("dashboard"),
#       uiOutput("Side1")
#     ),
#     
#     menuItem(
#       "Side 2",
#       tabName = "tab2",
#       icon = icon("dashboard"),
#       uiOutput("Side2")
#     )
#     
#   )
#   
# )
# 
# body <- dashboardBody(
#   
#   
#   
# )
# 
# dashboardPage(
#   header = header,
#   sidebar = sidebar,
#   body = body
# )

navbarPage(title = "Temp_analysis",
           tabPanel(title = "Methods comparison",
                    uiOutput("Tab3")),
           tabPanel(title = "Silverman's method",
                    uiOutput("Tab1")),
           tabPanel(title = "Gaussian Mixtures",
                    uiOutput("Tab2")),
           tabPanel(title = "Seasonal Separation",
                    uiOutput("Tab4")),
           tabPanel(title = "Mode Tree",
                    uiOutput("Tab5")),
           tabPanel(title = "Number of modes",
                    uiOutput("Tab6")),
           tabPanel(title = "Map",
                    uiOutput("Tab7")),
           tabPanel(title = "Fixed number of bins",
                    uiOutput("Tab8")),
           tabPanel(title = "Number of modes vs Number of bins",
                    uiOutput("Tab9")),
           tabPanel(title = "Chapter 3",
                    uiOutput("chap_3_1"))
)