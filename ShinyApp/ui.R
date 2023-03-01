

ui <- dashboardPage(
  
  dashboardHeader(title = "Presentation"),
  
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
      
      # ------------------------------------------------------------------------
      # ------------------------------ ABOUT -----------------------------------
      tabItem(tabName = "About", 
              
              fluidRow(
                
                # column widths need to add up to 12!
                column(width = 4, # ............... column 1 ...................
                      userbox() # see global.R
                ),
                
                column(width = 4, # ............... column 2 ...................
                       
                       tabBox(title = "", id = "tabset1", width = "250px",
                         
                         tabPanel("Academia", icon = icon(name="graduation-cap"),
                                  timeline_academia() # see global.R
                                  ),
                         
                         tabPanel("Experience", icon = icon(name="briefcase"),
                                  timeline_experience() # see global.R
                                  ),
                         
                         tabPanel("CV", 
                                  plotOutput('plot_cv', height = "230px") # see server.R
                         )
                         
                       )
                ),
                
                
                column(width = 4, # ............... column 3 ...................
                       
                       tabBox(title = "", id = "tabset2", width = "250px",
                              
                              tabPanel("Technical Skills", icon = icon(name="screwdriver-wrench"),
                                       plotOutput('plot_spider_tech') # see server.R
                              ),
                              
                              tabPanel("Statistical Skills", icon = icon(name="square-root-variable"),
                                       plotOutput('plot_spider_stats') # see server.R
                              )
                       ),
                       
                       box(title = "Languages", icon = icon('earth-americas'), 
                           width = NULL, status = "primary",
                           plotOutput('plot_dot', height = "120px") # see server.R
                       )
                       
                ) # column end 
                
              ) # fluidrow end
              
      ), # tabItem end    
              
      
      # ------------------------------------------------------------------------
      # ------------------------------ MOTIVATION ------------------------------
      
      tabItem(tabName = "Motivation", 
              
              
              h3("Where I am"),
      
              fluidRow(
                column(
                  width = 4, 
                  tags$head(tags$style(HTML('.info-box {min-height: 150px;} .info-box-icon {height: 150px; line-height: 150px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
                  infoBoxOutput("ordersbox", width = NULL)),
                column(width = 4, infoBoxOutput("progressBox", width = NULL)),
                column(width = 4, infoBoxOutput("approvalBox", width = NULL))
                ),
              
              tags$hr(), # -----------------------------------------------------
              
              h3("Where I wanna go"),
              
              fluidRow(
                column(width = 4, 
                      tags$head(tags$style(HTML('.info-box {min-height: 150px;} .info-box-icon {height: 150px; line-height: 150px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
                      infoBoxOutput("Boxfour", width = NULL)
                      ),
                column(width = 4, infoBoxOutput("Boxfive", width = NULL)),
                column(width = 4, infoBoxOutput("Boxsix", width = NULL))
              ),
              
              h3("Position Roche"),
              
              fluidRow(
                column(width = 4, 
                       tags$head(tags$style(HTML('.info-box {min-height: 150px;} .info-box-icon {height: 150px; line-height: 150px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
                       infoBoxOutput("Boxseven", width = NULL)
                       ),
                column(width = 4, infoBoxOutput("Boxeight", width = NULL)),
                column(width = 4, infoBoxOutput("Boxnine", width = NULL))
              )
              
            ),
      
      
      # ------------------------------------------------------------------------
      # ------------------------------- CODING ---------------------------------
      
      tabItem("Challenge", 
              
              h2("Challenge content"),
              
              actionButton("next_pic", "Next"),
              hr(),
              uiOutput("image")
              
              ),
      
      tabItem("Output", h2("Output content")),
      
      tabItem("Source_code", h2("Source code content"))
    
      
      
    ) # tabItems
  ) # dashboardBody
)

