
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)



ui <- dashboardPage(
  
  dashboardHeader(title = "Roche Interview"),
  
  # ============================================================================
  dashboardSidebar(
    sidebarMenu(
      # icons: https://getbootstrap.com/docs/3.3/components/#glyphicons
      menuItem("About", tabName = "About", icon = icon("user", lib="glyphicon")),
      menuItem("Motivation", tabName = "Motivation", icon = icon(name="star", lib="glyphicon")),
      menuItem("Coding", tabName = "Coding", icon = icon(name="console", lib="glyphicon"),
               menuSubItem("Challenge", tabName = "Challenge"),
               menuSubItem("Output", tabName = "Output"),
               menuSubItem("Source code", tabName = "Source_code") 
              )
    )
  ),
  
  
  # ============================================================================
  dashboardBody(
    
    tabItems(
      
      # ------------------------------ ABOUT -----------------------------------
      tabItem(tabName = "About", 
              
              fluidRow(
                
                # column widths need to add up to 12!
                column(width = 4, # ............... column 1 ...................
                       
                      userbox() # see global.R
                         
                ),
                
                column(width = 4, # ............... column 2 ...................
                       
                       tabBox(
                         title = "",
                         # The id lets us use input$tabset1 on the server to find the current tab
                         id = "tabset1", width = "250px", #height = "250px",
                         tabPanel("Academia", icon = icon(name="graduation-cap"),
                                  
                                  timeline_academia() # see global.R
                                  
                                  ),
                         tabPanel("Experience", icon = icon(name="briefcase"),
                                  
                                  timeline_experience() # see global.R
                                  
                                  ),
                         
                         tabPanel("", icon = icon(name="house"),
                                  
                                  plotOutput('plot_cv', height = "230px") # see server.R
                                  
                                  )
                       )
                ),
                
                
                column(width = 4, # ............... column 3 ...................
                       
                       box(title = 'Skills',
                           icon = icon('screwdriver-wrench'),
                           width = NULL,
                           status = "primary",
                           solidHeader = FALSE,
                           collapsible = FALSE,
                           plotOutput('plot_spider') # see server.R
                       ),
                       
                       box(title = "Languages", icon = icon('earth-americas'), 
                           width = NULL, status = "primary",
                           plotOutput('plot_dot', height = "120px") # see server.R
                       )
                       
                ) # column end 
                
              ) # fluidrow end
              
      ), # tabItem end    
              
      
      tabItem(tabName = "Motivation", h2("Motivation content")
      ),
      
      tabItem("Challenge", h2("Challenge content")),
      
      tabItem("Output", h2("Output content")),
      
      tabItem("Source_code", h2("Source code content"))
    
      
      
    ) # tabItems
  ) #dashboardBody
  
  
  
  
)

