

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
  
  rv <- reactiveValues(img=NULL)
  btn <- reactiveValues(button=NULL)
  
  my_images <- c("pregnancy_1.png", "pregnancy_2.png")

  
  output$image <- renderUI({
    
    if(rv$img == 'pregnancy_2.png'){
      fluidRow(column(width = 8,
                      img(src=rv$img, width = 640)), 
               column(width = 4,
                      btn$button)
               )
      #img(src=rv$img, width = 640)
      
    # } else if(rv$img == 'pregnancy_3.png'){
    #   fluidRow(column(width = 8,
    #                   img(src=rv$img, width = 620)), 
    #            column(width = 4,
    #                   btn$button)
    #   )
      #img(src=rv$img, width = 620)
      
    }else{
      fluidRow(column(width = 8,
                      img(src=rv$img, width = 350)), 
               column(width = 4,
                      btn$button)
      )
      #img(src=rv$img, width = 350)
      }

    })
  
  # https://stackoverflow.com/questions/40283682/multiple-reactivevalues-in-a-shiny-app
  observe({
    
    nclick <- sum(as.numeric(input$next_pic))
    
    if (nclick > length(my_images)) {
      btn$button <- actionButton("more", "More") #'some text'
    }else{
      btn$button <- NULL
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
  
  
  
}