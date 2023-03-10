
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(magrittr)
library(fmsb) # radarchart / spider-plot
library(plotly)

# ==============================================================================
#                        DATA for Programming Challenge
# ==============================================================================

load('/Users/sereina/Documents/03_Projects/04_Interviews/02_Roche/CV Shiny App/data_embryotox.Rda')

d.acute <- readxl::read_xlsx('/Users/sereina/Documents/03_Projects/04_Interviews/02_Roche/CV Shiny App/Test_data.xlsx', sheet = 'acute')
d.chronic <- readxl::read_xlsx('/Users/sereina/Documents/03_Projects/04_Interviews/02_Roche/CV Shiny App/Test_data.xlsx', sheet = 'chronic')

d.chronic %<>%
  filter(!is.na(ATC)) %>%
  mutate(Pre = as.numeric(str_replace(Pre, " \\s*\\([^\\)]+\\)", "")),
         T1 = as.numeric(str_replace(T1, " \\s*\\([^\\)]+\\)", "")),
         T2 = as.numeric(str_replace(T2, " \\s*\\([^\\)]+\\)", "")),
         T3 = as.numeric(str_replace(T3, " \\s*\\([^\\)]+\\)", "")),
         `T1-T3` = as.numeric(str_replace(`T1-T3`, " \\s*\\([^\\)]+\\)", ""))
  )

d.acute.chronic <- rbind(d.acute, d.chronic)

d.acute.chronic %<>%
  pivot_longer(., cols = c('Pre', 'T1', 'T2', 'T3', 'T1-T3'), names_to = "Time", values_to = "Prescriptions")

d.acute.chronic %<>%
  left_join(d.embryotox, by = c('Drug substance' = 'agent')) %>% 
    mutate(erfahrungsumfang = case_when(erfahrungsumfang %in% c('KEINE', 'KEINER') ~ 'no',
                                        erfahrungsumfang == 'GERING' ~ 'few',
                                        erfahrungsumfang == 'MITTEL' ~ 'medium',
                                        erfahrungsumfang == 'HOCH' ~ 'high',
                                        erfahrungsumfang == 'SEHR HOCH' ~ 'very high',
                                        TRUE ~ '-'
                                        )
    )

d.acute.chronic.prescr <- d.acute.chronic %>%
  filter(Time == 'T1-T3') %>%
  mutate(percentage = paste0(round(100*(Prescriptions/Total_pregnancies), 0), '%')) %>%
  select(-Time, -Total_pregnancies) %>%
  arrange(-Prescriptions)
# d.acute <- readxl::read_xlsx('/Users/sereina/Documents/03_Projects/04_Interviews/02_Roche/CV Shiny App/Test_data.xlsx', sheet = 'Tabelle2')
# 
# d.acute %<>%
#   pivot_longer(., cols = c('Pre', 'T1', 'T2', 'T3', 'T1-T3'), 
#                names_to = "Time", 
#                values_to = "Prescriptions")
# 
# d.acute %<>%
#   left_join(d.embryotox, by = c('Drug substance' = 'agent')) %>% 
#   mutate(erfahrungsumfang = case_when(erfahrungsumfang %in% c('KEINE', 'KEINER') ~ 'no',
#                                       erfahrungsumfang == 'GERING' ~ 'few',
#                                       erfahrungsumfang == 'MITTEL' ~ 'medium',
#                                       erfahrungsumfang == 'HOCH' ~ 'high',
#                                       erfahrungsumfang == 'SEHR HOCH' ~ 'very high',
#                                       TRUE ~ 'no information on embryotox.de'
#                                       )
#   )
# 
# d.acute.prescr <- d.acute %>%
#                     filter(Time == 'T1-T3') %>%
#                     select(-Time) %>%
#                     arrange(-Prescriptions)
# 


# ==============================================================================
#                                 MODAL Function
# ==============================================================================

mymodal <- function (..., title = NULL, footer = modalButton("Dismiss"), 
                     size = c("m", "s", "l"), easyClose = FALSE, fade = TRUE, idcss = "") 
{
  size <- match.arg(size)
  cls <- if (fade) 
    "modal fade"
  else "modal"
  div(id = "shiny-modal", class = cls, tabindex = "-1", `data-backdrop` = if (!easyClose) 
    "static", `data-keyboard` = if (!easyClose) 
      "false", div(class = paste("modal-dialog", idcss), class = switch(size, 
                                                                        s = "modal-sm", 
                                                                        m = NULL, 
                                                                        l = "modal-lg"), 
                   div(class = "modal-content", 
                       if (!is.null(title)) 
                         div(class = "modal-header", tags$h4(class = "modal-title", 
                                                             title)
                         ), 
                       div(class = "modal-body", ...), 
                       if (!is.null(footer)) 
                         div(class = "modal-footer", footer))
      ), 
    tags$script("$('#shiny-modal').modal().focus();"))
}

# ==============================================================================
#                                 USER BOX
# ==============================================================================

userbox <- function(){
  
  box(title = h4("Sereina M. Graber", align="center"),
      icon = icon("user"),
      status = "primary",
      solidHeader = FALSE,
      collapsible = FALSE,
      width = NULL,
      
      p("Data Scientist, PhD", align = 'center'), 
      
      # ------------------------------------------------------------------------
      
      fluidRow(
              align = 'center',
              img(src="suki_sereina.jpg", width = 170),
              br()
              ), # .jpg needs to be in the folder called www)
      
      tags$hr(), # -------------------------------------------------------------
      
      p(icon("pagelines"), "36"),
      p(icon("location-dot"), "Zurich"),
      p(icon("heart"), "programming, visualizing, outdooring & Suky"),
      
      tags$hr(), # -------------------------------------------------------------
      
      fluidRow(column(width = 12,
                      "initiative · proactive · creative · curious
                      · exact · passionate · integer · social · empathic"
              # I am a data scientist and part of the health services research 
              #  team of one of the biggest health insurance companies in Switzerland. 
              #  I am a biostatistician and biologist by training. 
              #  And my passion lies in R programming and expressing quantitative information
              #  using numbers, models and visual displays."
                )
               ),
      
      tags$hr(), # -------------------------------------------------------------
      
      fluidRow(column(width = 12, p('Links'))),
      
      fluidRow(
 
        column(width = 3,
               shiny::actionButton(inputId='ab1', label="",
                                   icon = icon("globe"), 
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                   onclick = "location.href='https://github.com/serigra';"
               )
        ),
        
        column(width = 3,
        shiny::actionButton(inputId='ab1', label="", 
                            icon = icon("github"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                            onclick = "location.href='https://github.com/serigra';"
                            )
        ),
        
        column(width = 3,
          shiny::actionButton(inputId='ab1', label="", 
                              icon = icon("linkedin"), 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                              onclick = "location.href='https://www.linkedin.com/in/sereina-maria-graber-078701bb/';"
                              )
          ),
        
        column(width = 3,
               shiny::actionButton(inputId='ab1', label="", 
                                   icon = icon("researchgate"), 
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                   onclick = "location.href='https://www.researchgate.net/profile/Sereina-Graber/research';"
               )
    
               )
        
      )
      
      )
  
}



# ==============================================================================
#                          Timeline ShinyDashboard
# ==============================================================================

### ----------------------- TIMELINE ACADEMIA ---------------------------

timeline_academia <- function() {
  
  timelineBlock(width = "250px", reversed = FALSE,
    
    timelineLabel('2013 - 2017', color = "gray"),
    timelineItem(title = "PhD Biology",#icon = icon("book"),
                 #color = "gray",
                "Thesis: Social and ecological aspects of brain size evolution."
                #br(), br(),
                #appButton(inputId = "ReadMore", label = "Read more")
                ),
    
    timelineLabel('2012 - 2015', color = "gray"),
    timelineItem(title = "Msc Biostatistics", footer = "",
                 "Thesis: Phylogenetic comparative methhods for discrete responses in Evolutionary Biology."),
    
    timelineLabel('2010 - 2012', color = "gray"),
    timelineItem(title = "Msc Biology", footer = "",
                 "Thesis: Cooperative breeding and the Evolution of Brain Size in Birds."),
  
    timelineLabel('2007 - 2010', color = "gray"),
    timelineItem(title = "Bsc Biology")
  )
  
}


### -------------------------- TIMELINE EXPERIENCE -----------------------------

timeline_experience <- function() {
  
  timelineBlock(width = "250px", reversed = FALSE,
    
    timelineLabel('2019 - current', color = "gray"),
    
    timelineItem(title = "Data Scientist @ Helsana",
                footer = "",
                "Lead, design and implementation of data analyses for internal projects
                and scientific studies at the health science department, 
                with a focus on understanding the effectiveness and the quality of 
                treatment in the Swiss health care system."
                ),
    
    timelineLabel('2017-2019', color = "gray"),
    timelineItem(title = "Data Scientist @ Sanitas",
                 "Lead, design and implementation of advanced analytical projects, 
                 with a focus on the operational business. 
                 In-house consulting and teaching of statistical and analytical skills."
                ),
    
    timelineLabel('2012 - 2016 | projectwise', color = "gray"),
    timelineItem(title = "Biostatistican @ USZ, KSA",
                 "Design, implementation of and consulting on statistics and 
                 data analyses for clinical trials/ scientific publications."
                 )
  )

}



# ==============================================================================
#                                CV plot
# ==============================================================================

# data for CV plot
activity.studies <- c("Bsc Biology", "Msc Biology (Anthropology)", "Msc Biostatistics", "PhD Biology (Evolution)", "Academic Background")
activity.work <- c("Biostatistician USZ, KSA", "Data Scientist - Sanitas", "Data Scientist - Helsana", "Work Experience")
activity.skills <- c("R", "SQL", "Skills")
activity <- c(activity.studies, activity.work, activity.skills)
chapter = c(rep("Academic Background", length(activity.studies)), 
            rep("Work Experience", length(activity.work)), 
            rep("Skills", length(activity.skills))
)
d.cv <- tibble(chapter = chapter,
               activity = factor(activity, levels = activity),
               start_date = as.Date(c('2007-09-01', '2010-09-01', '2012-09-01', '2013-08-01', NA, '2012-03-01', '2017-08-01', '2019-08-01', NA, '2010-09-01', '2017-08-01', NA), format = "%Y-%m-%d"),
               end_date = as.Date(c('2010-05-01', '2012-05-01', '2015-05-01', '2017-07-01', NA, '2016-07-01', '2019-07-01', '2023-03-01', NA, '2023-03-01', '2023-03-01', NA), format = "%Y-%m-%d")
)

cv_plot <- function(){
  
  # cv plot
  p <- d.cv %>%
    ggplot2::ggplot(., aes(x = start_date, y =  activity,
                           xend = end_date, yend = activity,
                           colour = chapter)
    ) +
    # background shaded bands
    # ggplot2::geom_rect(data = date_range_df, ggplot2::aes(xmin = start,
    #                                                       xmax = end,
    #                                                       ymin = -Inf,
    #                                                       ymax = Inf),
    #                    inherit.aes=FALSE,
    #                    alpha = 0.4,
    #                    fill = colour_stripe) +
    ggplot2::geom_segment(lineend = 'round', size = 5.5) +
    ggplot2::scale_y_discrete("") +
    scale_x_date(date_breaks = '1 year', date_labels = "%Y") +
    xlab('') + 
    ggplot2::theme_minimal() +
    ggplot2::scale_colour_manual(values = wesanderson::wes_palette("Darjeeling1")) +
    ggplot2::theme(text = element_text(family = "sans"),
                   axis.text.x = element_text(angle = 45, vjust = -0.1, size = 12),
                   axis.text.y.left = element_text(face = ifelse(activity == chapter, yes = "bold", no = "plain"), size = 14),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.y = element_blank(),
                   legend.position = "none")
  
  return(p)
  
}



# ==============================================================================
#                               Spider Plot Skills
# ==============================================================================


# --------------------------- TECHNICAL SKILLS ---------------------------------

# data
scores <- data.frame(
  row.names = c("Sereina"),
  R = c(5), RShiny = c(3.5), Python = c(2),
  SQL = c(4), LaTex = c(3), Git = c(3.5),
  Bash_Shell = c(2), SPSS = c(2), SAS = c(0), MSOffice = c(4)
)

max_min <- data.frame(
  R = c(5, 0), RShiny = c(5, 0), Python = c(5, 0),
  SQL = c(5, 0), LaTex = c(5, 0), Git = c(5, 0),
  Bash_Shell = c(5, 0), SPSS = c(5, 0), SAS = c(5, 0), MSOffice = c(5, 0)
)
rownames(max_min) <- c("Max", "Min")

d.skills.tech <- rbind(max_min, scores)


# plot
spider_plot_tech <- function(){

  op <- par(mar = c(0, 0, 0, 0))
  fmsb::radarchart(d.skills.tech, axistype = 1,
                  pcol = "#00AFBB", pfcol = scales::alpha("#00AFBB", 0.5), 
                  plwd = 2, plty = 1,
                  # Customize the grid
                  cglcol = "grey", cglty = 1, cglwd = 0.8,
                  axislabcol = "grey", 
                  vlcex = 1.1, vlabels = colnames(d.skills.tech),
                  caxislabels = c(0, 1, 2, 3, 4, 5), 
                  title = NULL
                  )
  par(op)
  
}


# --------------------------- STATISTICAL SKILLS -------------------------------

# data
scores <- data.frame(
  row.names = c("Sereina"),
  LM = c(5), GLM = c(4), GLMM = c(3), GEE = c(3),
  RF = c(2), Survival = c(2), PSM = c(3.5)
)

max_min <- data.frame(
  LM = c(5, 0), GLM = c(5, 0), GLMM = c(5, 0),
  GEE = c(5, 0), RF = c(5, 0), Survival = c(5, 0), PSM = c(5, 0)
)
rownames(max_min) <- c("Max", "Min")

d.skills.stats <- rbind(max_min, scores)


# plot
spider_plot_stats <- function(){
  
  op <- par(mar = c(0, 0, 0, 0))
  fmsb::radarchart(d.skills.stats, axistype = 1,
                   pcol = "#00AFBB", pfcol = scales::alpha("#00AFBB", 0.5), 
                   plwd = 2, plty = 1,
                   # Customize the grid
                   cglcol = "grey", cglty = 1, cglwd = 0.8,
                   axislabcol = "grey", 
                   vlcex = 1, vlabels = colnames(d.skills.stats),
                   caxislabels = c(0, 1, 2, 3, 4, 5), 
                   title = NULL
  )
  par(op)
  
}


# ==============================================================================
#                               Dot Plot Languages
# ==============================================================================

d.lang <- data.frame (
  lang = rep(c('German', 'English', 'French'), each = 5),
  score_germ = rep(c(1:5), 3),
  binary = factor(c(rep(1, 9), 0, 1, 0,0,0,0) )
)

dot_plot <- function() {
  
  d.lang %>% 
    mutate(lang = factor(lang, levels = c('French', 'English', 'German'))) %>% 
    ggplot(., aes(x=score_germ, y = lang, color = binary)) +
    geom_point(size = 6.5) +
    ggplot2::scale_color_manual(values = c('grey', 'black')) +
    xlab('') + 
    ylab('') + 
    ggplot2::theme_minimal() +
    ggplot2::theme(text = element_text(family = "sans"),
                   axis.text.y = element_text(size = 15,
                                              hjust = -0),
                   axis.text.x = element_blank(),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.y = element_blank(),
                   legend.position = "none")
  
}
 

