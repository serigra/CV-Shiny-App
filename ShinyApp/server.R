

library(shiny)

server <- function(input, output) {
  
  # non-interactive figure
  output$plot_cv <- renderPlot({ 
    cv_plot() # see global.R
    })
  
  # non-interactive figure
  output$plot_spider <- renderPlot({ 
    spider_plot() # see global.R
  })
  
  
  # non-interactive figure
  output$plot_dot <- renderPlot({ 
    dot_plot() # see global.R
  })
  
  
}