

server <- function(input, output) {
  
  # =========================== Plots in ABOUT =================================
  
  # non-interactive figure
  output$plot_cv <- renderPlot({ 
    cv_plot() # see global.R
    })
  
  # non-interactive figure
  output$plot_spider_tech <- renderPlot({ 
    spider_plot_tech() # see global.R
  })
  
  # non-interactive figure
  output$plot_spider_stats <- renderPlot({ 
    spider_plot_stats() # see global.R
  })
  
  # non-interactive figure
  output$plot_dot <- renderPlot({ 
    dot_plot() # see global.R
  })
  
  
  # =========================== Boxes in MOTIVATION ============================
  
  # 1. Box
  output$ordersbox <- renderInfoBox({
    infoBox(
      "Stakeholder", "120", icon = icon("users"),
      color = "light-blue", fill =TRUE, width = 3
    )
  }) 
  
  # 2. Box
  output$progressBox <- renderInfoBox({
    invalidateLater(as.integer(1000))
    infoBox("Time",
            paste(format(Sys.time(), "%H:%M:%S"), "h"), 
            icon = icon("time", lib = "glyphicon"),
            color = "teal", fill =TRUE, width = 3
    )
  })
  
  # 3. Box
  output$approvalBox <- renderInfoBox({
    infoBox(
      "KPI 2", "120", icon = icon("check-square"),
      color = "yellow", fill =TRUE,width = 3
    )
  })
  
  # 4. Box
  output$Boxfour <- renderInfoBox({
    infoBox(
      "Bla", "120", icon = icon("check-square"),
      color = "red", fill =TRUE,width = 3
    )
  })
  
  # 5. Box
  output$Boxfive <- renderInfoBox({
    infoBox(
      "Bli", "120", icon = icon("check-square"),
      color = "blue", fill =TRUE,width = 3
    )
  })
  
  # 6. Box
  output$Boxsix <- renderInfoBox({
    infoBox(
      "Blu", "120", icon = icon("check-square"),
      color = "orange", fill =TRUE,width = 3
    )
  })
  
  # 7. Box
  output$Boxseven <- renderInfoBox({
    infoBox(
      "Blu", "120", icon = icon("check-square"),
      color = "green", fill =TRUE,width = 3
    )
  })
  
  # 8. Box
  output$Boxeight <- renderInfoBox({
    infoBox(
      "Blu", "120", icon = icon("check-square"),
      color = "green", fill =TRUE,width = 3
    )
  })
  
  # 9. Box
  output$Boxnine <- renderInfoBox({
    infoBox(
      "Blu", "120", icon = icon("check-square"),
      color = "green", fill =TRUE,width = 3
    )
  })
  
  
  # ================== reactive output in coding challenge =====================
  
  ## easy way for one picture --------------------------------------------------
  # observeEvent(input$next_pic, {
  #   output$image1 <- renderUI({
  # 
  #     img(src="pregnancy_1.png", width = 350)
  # 
  #     })
  # })
  
  
  ## alternative way -----------------------------------------------------------
  # https://stackoverflow.com/questions/40283682/multiple-reactivevalues-in-a-shiny-app
  
  rv <- reactiveValues()
  
  my_images <- c("pregnancy_1.png", "pregnancy_2.png")

  output$image <- renderUI({
    
    if(rv$img == 'pregnancy_2.png'){
      fluidRow(column(width = 6,
                      img(src=rv$img, width = 790)), 
               column(width = 6,
                      br(), br(),
                      fluidRow(rv$button),
                      br(), br(), br(), br(), 
                      fluidRow(rv$button2),
                      br(), br(), br(), br(), 
                      fluidRow(rv$button3)
                      )
      )
      
    }else{
      fluidRow(column(width = 6,
                      img(src=rv$img, width = 450)), 
               column(width = 6,
                      br(), br(), 
                      fluidRow(rv$button),
                      br(), br(), br(), br(), 
                      fluidRow(rv$button2),
                      br(), br(), br(), br(), 
                      fluidRow(rv$button3)
                      )
              )
      }

    })
  

  observe({
    
    nclick <- sum(as.numeric(input$next_pic))

    if (nclick == 0) { # initial display
      rv$img = my_images[1]
    }else if (nclick > 0 & nclick < length(my_images)+1) {
      rv$img <- my_images[nclick]
    }else{
      rv$img <- my_images[length(my_images)] # show the last one
    }
    
  })
  
  
  observe({
    
    nclick <- sum(as.numeric(input$next_pic))
    
    if (nclick > length(my_images)) {
      # tags$style(
      #   type = 'text/css',
      #   '.modal-dialog.test{ width: fit-content !important; }'
      # )
      rv$button <- actionButton("more1", "Read more") #'some text'
      rv$button2 <- actionButton("more2", "Read more") #'some text'
      rv$button3 <- actionButton("more3", "Read more") #'some text'
    }else{
      rv$button <- NULL
      rv$button2 <- NULL
      rv$button3 <- NULL
    }
  })
  
  
  # =========================== pop up window I ==============================
  
  output$pregnancyalgo <- renderUI({
    img(src="pregnancy_algo.png", width = 850)
  })
  
  observeEvent(input$more1, {
    
    showModal(modalDialog(
      uiOutput("pregnancyalgo"),
      footer = 'Thats plot 1',
      easyClose = TRUE,
      size = 'l'
    ))
  })
  
  
  # =========================== pop up window II ==============================
  
  output$embryotox <- renderUI({
    img(src="embryotox.png", width = 800)
  })
  
  
  observeEvent(input$more2, {
    
    showModal(modalDialog(
      uiOutput("embryotox"),
      footer = 'embryotox.de',
      easyClose = TRUE,
      size = 'l'
    ))
  })
  
  
  
  # ----------------------------- Plot 1 -------------------------------------
  output$barPlot1 <- renderPlotly({
 
    # data2 <- d.acute.prescr %>%
    #               arrange(-Prescriptions) %>% 
    #               top_n(., input$top)
    
    data2 <- d.acute.chronic.prescr %>%
      filter(Type == input$top) %>%
      select(ATC, `Drug substance`, Prescriptions) %>%
      top_n(., 10)

    plot_ly(data2,
            x = ~Prescriptions, y = ~ATC,
            type = 'bar',
            orientation = 'h',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1)
            )
    ) %>%
    layout(title = paste0('Top 10 prescriptions T1-T3 to treat ', input$top, ' conditions'),
             xaxis = list(title = ""),
             yaxis = list(title = "", categoryorder = "total ascending"),
             margin = list(l = 70, r = 60, t = 80, b = 90, pad = 8)#,
             #paper_bgcolor = 'rgba(0, 0, 0, 0.03)', # rgb(248, 248, 255)
             #plot_bgcolor = 'rgba(0, 0, 0, 0)'
      ) %>%
      add_annotations(xref = 'paper', yref = 'paper',
                      x = 0, y = -0.38,
                      text = paste('Source: Gerbier et al. 2021. Numbers represent extrapolations for the whole of Switzerland.'),
                      font = list(size = 10, color = 'rgb(150,150,150)'),
                      showarrow = FALSE)
  })
  

  # ----------------------------- Plot 2 -------------------------------------
  output$barPlot2 <- renderPlotly({

    data <- d.acute.chronic %>% filter(Time != 'T1-T3' & ATC == input$ATC)
    drug <- unique(data$`Drug substance`)

    fig <- plot_ly(data, x = ~Time, y = ~Prescriptions, type = 'bar',
                   marker = list(color = 'rgb(158,202,225)',
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5)))
    
    fig %>% layout(#title = paste0(drug, ' (', input$ATC, ") prescriptions during pregnancy"),
                   xaxis = list(title = "Time pregnancy"),
                   yaxis = list(title = "No. of prescriptions"),
                   margin = list(l = 70, r = 60, t = 50, b = 90, pad = 8)#,
                   #paper_bgcolor = 'rgba(0, 0, 0, 0.03)', # rgb(248, 248, 255)
                   #plot_bgcolor = 'rgba(0, 0, 0, 0)'
                  )

  })
  

  # ----------------------------- Text Embryotox  ----------------------------
  output$substanz <-  renderText({
                              d.acute.chronic %>%
                                filter(ATC == input$ATC)%>%
                                pull(`Drug substance`) %>% unique()
                        })
  
  output$prescr <-  renderText({
                              d.acute.chronic %>%
                                filter(Time == 'T1-T3' & ATC == input$ATC) %>%
                                pull(Prescriptions) %>% unique()
                         })
  

  output$indikation <- renderText({
    
                          ind <- d.acute.chronic %>%
                            filter(ATC == input$ATC) %>%
                            pull(indikation) %>% unique()
                         
                          ifelse(is.na(ind), 'no information on embryotox.de', ind)
                        })
  
  output$experience <- renderText({
                          exp <- d.acute.chronic %>%
                            filter(ATC == input$ATC) %>%
                            pull(erfahrungsumfang) %>% unique()
                          
                          ifelse(is.na(exp), 'Drug safety', paste0('Drug safety (', exp, ' experience)'))
  })
  
  
  output$signal  <- renderText({ #renderInfoBox({
                        
                sign <- d.acute.chronic %>%
                          filter(ATC == input$ATC) %>%
                          pull(signal) %>% unique()

                if(is.na(sign)){

                  'no information on embryotox.de'
                }
                
                else if(sign == 'rot'){ 'teratogenic/ fetotoxic'
                  # infoBox('Drug safety', 
                  #         'teratogenic/ fetotoxic',
                  #          icon = icon("triangle-exclamation"), 
                  #         color = "red")
                  
                } else if(sign == 'grau'){ 'contradictory/ insufficient study results'
                  # infoBox('Drug safety', 
                  #         'contradictory/ insufficient study results',
                  #         icon = icon("smile"), 
                  #         color = "teal")
            
                } else if(sign == 'grün'){ 'Ok, but careful risk-benefit assessment necessary.'
                  # infoBox('Drug safety',
                  #         'Ok, careful risk-benefit assessment necessary.',
                  #         icon = icon("smile"),
                  #         color = "green")
                  
                } else{ 'no information on embryotox.de'
                  # infoBox('Drug safety',
                  #         'no information on embryotox.de',
                  #          icon = icon("smile"), color = "yellow")
                }
    
     })
}