

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
               menuSubItem("Results", tabName = "Output")#,
               #menuSubItem("Source code", tabName = "Source_code") 
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
                column(width = 3, # ............... column 1 ...................
                      userbox() # see global.R
                ),
                
                column(width = 5, # ............... column 2 ...................
                       
                       tabBox(title = "", id = "tabset1", width = "250px",
                         
                         tabPanel("Academia", icon = icon(name="graduation-cap"),
                                  timeline_academia() # see global.R
                                  ),
                         
                         tabPanel("Experience", icon = icon(name="briefcase"),
                                  timeline_experience() # see global.R
                                  ),
                         
                         tabPanel("CV", 
                                  plotOutput('plot_cv', height = "270px") # see server.R
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
              
              br(), br(),
              
              # ============================ Organization ==============================
              fluidRow( 
                
                column(12,
                       box(width = 12, collapsible = TRUE, collapsed = TRUE,
                           title = h3('Organization', align = 'center'),
                           
                           column(4, 
                                  fluidRow(align = 'center',img(src='Bild4.png')),
                                  br(),p("Commitment to patients.", align = 'center')
                           ),
                           column(4, 
                                  fluidRow(align = 'center',img(src='Bild5.png')),
                                  p("Integer, fair & sustainable culture.", align = 'center')
                           ),
                           column(4, 
                                  fluidRow(align = 'center',img(src='Bild6.png')),
                                  p("International & diverse environment.", align = 'center')
                           )
                           
                       )
                )
              ),
              
              br(), 
              
              fluidRow(   
                
                # ========================= Impact on patients ========================
                column(4, 
                       box(width = NULL, height = 600, collapsible = TRUE, collapsed = TRUE,
                           title = h3('Impact on patients', align = 'center'),
                           fluidRow(align = 'center',img(src='Bild1.png')),
                           p("Making a difference in peoples' lives.", align = 'center'),
                           # Doing what patients need next.
                           
                           tags$hr(), 
                           
                           fluidRow( 
                             tags$div(
                               tags$ul(
                                 # influence medial practice
                                 tags$li("Being at the pulse of medical progress"),  
                                 tags$li("Drug development"),  
                                 # something which fascinates me, very often we thinking to easy of it (we take it and it works),
                                 # but its so much more complex than that, thinking of interactions with other medication,
                                 # taking them too long, or in case of certain comorbidities 
                                 # not for every person the medication has the same effect, 
                                 # being able to contribute to the challenge of finding medicaiton that fits all 
                                 tags$li("Oncology") 
                                 # kind of scary disease, already as a child this had like a big effect on me, since its so common, 
                                 # and often being patients are being exposed helplessly, and also being able to actively do smth against 
                                 # that I imagine to be very fulfilling, feeling less helplessly exposed
                               )
                             )
                           )
                       )
                ),
                
                
                # =================== Team work & diverse stakeholders ================
                column(4, 
                       box(width = 12, height = 600, collapsible = TRUE, collapsed = TRUE,
                           title = h3('Team work & diverse stakeholders', align = 'center'),
                           fluidRow(align = 'center',img(src='Bild2.png')),
                           p("Collaborate to achieve goals & develope.", align = 'center'),
                           
                           tags$hr(), # ---------------------------------------------
                           
                           fluidRow( 
                             tags$div(
                               tags$ul(
                                 tags$li("Northstar"),
                                 tags$li("Swarm intelligence"),
                                 tags$li("Learning from others")
                               )
                             )
                           )
                       )
                ),
                
                
                # ========================= Hands-on Programming ======================  
                column(4, 
                       box(width = 12, height = 600, collapsible = TRUE, collapsed = TRUE,
                           title = h3('Hands-on programming', align = 'center'),
                           fluidRow(align = 'center', img(src='Bild3.png')),
                           p("Problem solving & learning.", align = 'center'),
                           br(),
                           
                           tags$hr(), # ---------------------------------------------
                           
                           fluidRow( 
                             # analytical challenges / problem solving/  -->  taking things apart and figuring out how they work
                             # joy of continuous learning --> Book Less bad programming, exploring new data sources and applying new methodologies
                             # nice balance between theory and practice
                             # Breaking down complex information into easy-to-understand content
                             tags$div(
                               tags$ul(
                                 tags$li("generating data driven insights"),
                                 tags$li("analytical challenges & problem solving"),
                                 tags$li("breaking down complex information into easy-to-understand content"),
                                 tags$li("balance between theory and practice"),
                                 tags$li("continuous learning")
                                 
                               )
                             )
                           )
                           
                       )
                )
              ) 
              
            ),
      
      
      # ------------------------------------------------------------------------
      # ------------------------------- CODING ---------------------------------
      
      tabItem("Challenge", 
              
              h2("Medication during pregnancy"),
              #  
              actionButton("next_pic", "Next"),
              hr(),
              uiOutput("image")
              
              
              
              # fluidRow(
              #   column(2),
              #   column(8, 
              #          box(width= 12, title = h3('Medication during pregnancy', align = 'center'),
              #               
              #               column(6, br(), br(),
              #                      fluidRow(align = 'center', img(src='pregnancy_0.png', width = 270))
              #                       ),
              # 
              #               column(6,
              #                      h4('Motivation'),
              #                      tags$div(
              #                        tags$ul(
              #                          tags$li("pregnant women/ unborn childern = sensitive group of patients"),
              #                          tags$li("low empirical evidence"),
              #                          tags$li("Switzerland: missing overview")
              #                        )
              #                      ),
              #                      
              #                      h4('Aim'),
              #                      tags$div(
              #                        tags$ul(
              #                          tags$li("What medications are prescribed during pregnancy (acute & chronic conditions)?"),
              #                          tags$li("Special focus on potentially teratogenic/fetotoxic drugs"),
              #                          tags$li("When and how often?")
              #                               )
              #                           )
              #                      
              #                       )
              # 
              #             )
              # 
              #          ),
              #   column(2)
              # 
              # ),
              
              # br(), br(), br(),
              # 
              # fluidRow(column(4, 
              #                 box(width = 12, title = h4('Identification pregnancy period', align = 'center'),
              #                     collapsible = TRUE, collapsed = TRUE,
              #                     fluidRow(align = 'center', img(src='challenge_1.png', width = 110)),
              #                     br(),
              #                     fluidRow(align = 'center', actionButton("more1", "Show more"))
              #                     )
              #                 ),
              #          
              #          column(4, 
              #                 box(width = 12, title = h4('Identification teratogenic drugs', align = 'center'),
              #                     collapsible = TRUE, collapsed = TRUE,
              #                     fluidRow(align = 'center', img(src='challenge_2.png', width = 130)),
              #                     br(),
              #                     fluidRow(align = 'center', actionButton("more2", "Show more"))
              #                     )
              #          ),
              #          
              #          column(4, 
              #                 box(width = 12, title = h4('Presentation of results', align = 'center'),
              #                     collapsible = TRUE, collapsed = TRUE,
              #                     fluidRow(align = 'center', img(src='challenge_3.png', width = 110)),
              #                     br(),
              #                     fluidRow(align = 'center', actionButton("more3", "Show more"))
              #                     )
              #                 )
              #          
              #          )
 
              ),
      
      
      # ------------------------------------------------------------------------
      tabItem("Output", #h2("Results based on Gerbier et al. 2021"),
              
              fluidRow(
                    
                # column(1,  
                #     radioButtons(
                #       "top",
                #       "Conditions",
                #       choices = unique(d.acute.chronic$Type),
                #       selected = 'acute',
                #       inline = FALSE
                #     )
                #   ),
                
                column(4,
                       
                       box(title = 'Publications',
                         
                         #title = 'Total no. pregnancies (2014-2018)',
                         solidHeader = FALSE,
                         collapsible = FALSE,
                         width = NULL,
                         #status = 'primary',
                         fluidRow(column(width = 12,
                         tags$div(
                           tags$ul(
                             tags$li("Spoendlin et al. 2021"),
                             tags$li("Gerbier et al. 2021, 2022")
                           )
                         )
                         )),
                         
                         tags$hr(),
                         
                         h4("Data"),
                         fluidRow(column(width = 12,
                                         '2014 - 2018, extrapolated to whole of Switzerland'
                         )
                         ),
                         
                         tags$hr(),
                         
                         h4('Total no. pregnancies'),
                         fluidRow(column(width = 12,
                                         "369'371"
                         )
                         ),
                         
                         tags$hr(), # -----------------------------------------
                         #h4("Conditions"),
                         fluidRow(column(width = 12,
                                         radioButtons(
                                           "top",
                                           h4("Conditions"),
                                           choices = unique(d.acute.chronic$Type),
                                           selected = 'acute',
                                           inline = FALSE
                                         )
                         )
                         )#,
                         
                         
                         
                         # tags$hr(), # -----------------------------------------
                         # h4('asldkfjsldkj'),
                         # fluidRow(column(width = 12,
                         #                 'sdlfkjsd'
                         # )
                         # )#,
                         
                         # tags$hr(), # -----------------------------------------
                         # h4('Pregnancy experience'),
                         # fluidRow(column(width = 12,
                         #                 textOutput("experience")
                         #                 )
                         #         )
                         
                       )
                ),
                  
                column(8, plotlyOutput("barPlot1")
                )
              ),
              
              br(), 

             fluidRow(

                 # column(3,
                 #    #helpText("Choose ATC to display distribution of prescriptions during pregnancy."),
                 #    selectInput('ATC', 'Drug Substance (ATC)', unique(d.acute.chronic$ATC), selected = 'N02BE01')
                 #  ),

                 column(4,
                  
                       box(
                 
                           title = 'Drug substance (ATC)', #textOutput("substanz"),
                           solidHeader = FALSE,
                           collapsible = FALSE,
                           width = NULL,
                           #status = 'primary',
                           selectInput('ATC', '', unique(d.acute.chronic$ATC), selected = 'N02BE01'),
                           h5(textOutput("substanz")),
                           p(textOutput("prescr")),
                           #tags$hr(),
                           # h4('Total no. prescriptions'),
                           # fluidRow(column(width = 12,
                           #                 textOutput("prescr")
                           #                 )
                           #          ),

                          tags$hr(), # -----------------------------------------
                          h4("Indications"),
                          fluidRow(column(width = 12,
                                          textOutput("indikation")
                                           )
                                    ),

                          tags$hr(), # -----------------------------------------
                          h4(textOutput("experience")),
                          fluidRow(column(width = 12,
                                          textOutput("signal")
                                          )
                          )#,

                          # tags$hr(), # -----------------------------------------
                          # h4('Pregnancy experience'),
                          # fluidRow(column(width = 12,
                          #                 textOutput("experience")
                          #                 )
                          #         )
      
                         )
                ),
                  
                column(8, plotlyOutput("barPlot2")

                       # box(
                       # 
                       #   title = NULL,
                       #   icon = icon("user"),
                       #   solidHeader = FALSE,
                       #   collapsible = FALSE,
                       #   width = NULL,
                       #   status = 'primary',
                       #   plotlyOutput("barPlot2")
                       # )
                       )
             #  
             )
          )#,
      
      
      
      # ------------------------------------------------------------------------
      #tabItem("Source_code", h2("Source code content"))
    
      
      
    ) # tabItems
  ) # dashboardBody
)

