library(shinythemes); library(shinyBS)
library(DT); library(dplyr);library(readr);
library(stringr)
source("utils.R")
# source("kdData.R")



ui <-
  navbarPage(
    "RVU Calculator",
    theme = shinytheme("flatly"),
    collapsible = T,
    header =
      tags$head(# includeHTML("google-analytics.js"),
        tags$style(
          HTML(
            "
                        #test {
                          padding: 100px;
                        }
                        .navbar {
                          margin: 0px;
                        }
                        .footer {
                            position: relative;
                            left: 0;
                            bottom: 0;
                            width: 100%;
                            background-color: #d7dfea;
                            # color: white;
                            text-align: center;
                        }
                        "
          )
        )),
    
    tabPanel("Convert", id = "test", sidebarLayout(
      sidebarPanel(width = 3, bsCollapse(
        open = "panel",
        bsCollapsePanel(
          p(icon("bars"), HTML('&nbsp;'), "Visit details"),
          value = "panel",
          selectInput(
            "label_VISIT",
            label = h3("Visit type"),
            choices = list(
              "New visit" = "new",
              "Consult" = "cs",
              "Follow up" = "fu"
            ),
            selected = "new"
          ),
          numericInput(
            "label_VISIT",
            label = h3("Length of encounter (minutes)"),
            value = 30
          ),
          numericInput(
            "label_PREP",
            label = h3("Length of preparation (minutes)"),
            value = 0
          )
        )
      )), 
      mainPanel(
        fluidPage(
          fluidRow(textOutput('extra_encounter_time')),
          fluidRow(textOutput('extra_encounter_rvu')),
          fluidRow(DT::dataTableOutput("rvu_filter")),
          fluidRow(DT::dataTableOutput("rvu_table"))
          ))
    ))
  )

server <- function(input, output) {
  df <- read_csv("rvu_data.csv")
  df_data <- df %>% 
    mutate(min_minutes = str_extract(description, '[0-9].'))
  
  extra_encounter_time <- df_data[df_data$cpt=='99417',]$min_minutes
  extra_encounter_rvu <- df_data[df_data$cpt=='99417',]$rvu
  
  
  # rvu_prep_encounter_time <- df_data[df_data$cpt=='99417', 'rvu']
  # rvu_prep_encounter_rvu <- df_data[df_data$cpt=='99417', 'rvu']
  
  output$extra_encounter_time <- renderText({extra_encounter_time})
  output$extra_encounter_rvu <- renderText({extra_encounter_rvu})
  
  
  df_output <- reactive({
    df_data 
  })
  
  output$rvu_filter = DT::renderDataTable({
    datatable(
      df_output(),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style =
          'caption-side: top;
          text-align: left;
          color: black;
          font-size:150%',
        "Filtered RVUs"
      )
    )
  })
  
  output$rvu_table = DT::renderDataTable({
    datatable(
      df %>% rename(
        CPT = cpt,
        Description = description,
        RVUs = rvu
      ),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style =
          'caption-side: top;
          text-align: left;
          color: black;
          font-size:150%',
          "All RVUs"
      )
    )
  })
}

shinyApp(ui, server)
