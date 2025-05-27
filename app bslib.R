library(shiny)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(data.table)
library(ggplot2)
#library(patchwork)
library(waffle)
library(ggrepel)
library(grid) # for 'rectGrob'
library(gridExtra) # for 'arrangeGrob' and 'grid.arrange'
library(gt)
#library(gtExtras)

# load functions and prerequisites
source("./functions/plot_functions.R")
source("./functions/prereqs.R")

# ---- Load Synthetic Data ----
kidney <- readRDS("./data/kidney.rds")

ui <- page_sidebar(
  # Header area
  title = "Kidney Waiting Times",   # Appears at the top of the page
  window_title = "Risk Communication Tool", # Shows in the browser tab
  
  # Sidebar area
  sidebar = sidebar(
    title = "Enter details about the patient",
    # Informational text
    p("Other things might influence these results, for example other health conditions."),
    p("The tool cannot take into account all these factors."),
    p("The tool uses data collected by NHSBT. Some data that is collected has not been used in the tool."),
    p("Changes to the kidney offering scheme in September 2019 are not yet reflected in the tool."),
    # create action button with blue background
    actionButton("factors", "Show factors considered but not included",
                 icon = icon("info"), 
                 style = "color: #fff; background-color: #337ab7; border-color: #337ab7"),
    # add vertical space
    #tags$br(), tags$br(),
    
    # create action button with red background
    actionButton("reset", "Reset all",
                 icon = icon("redo"), 
                 style = "color: #fff; background-color: #d9534f; border-color: #d9534f"),
    #
    radioGroupButtons(
      inputId = "age",
      label = "Age", 
      choices = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
      selected = character(0),
      justified = TRUE
    ),
    
    radioGroupButtons(
      inputId = "sex",
      label = "Sex", 
      choices = c("Male", "Female"),
      selected = character(0),
      justified = TRUE
    ),
    
    radioGroupButtons(
      inputId = "ethnicity",
      label = "Ethnicity", 
      choices = c("White", "Mixed", "Asian", "Black", "Other"),
      selected = character(0),
      justified = TRUE
    ),
    
    radioGroupButtons(
      inputId = "blood",
      label = "Blood Group", 
      choices = c("O", "A", "B", "AB"),
      selected = character(0),
      justified = TRUE
    ),
    #
    #selectInput("example", "Choose an option:", c("A", "B", "C")),
    width = 700
  ),
  
  # Main content area
  "Your main content will go here. This area can contain plots, tables, text, or any other outputs.",
  withSpinner(uiOutput("barPlotUI"))
)

server <- function(input, output) {
  # Data reactives
  source(file.path("server/kidney_data.R"), local = TRUE)$value
  
  # pop-up to show factors that have not been considered
  modal_confirm <- modalDialog(
    factors_not_included,
    title = "Factors considered but not included",
    footer = tagList(
      actionButton("ok", "OK", class = "btn btn-success")
    )
  )
  observeEvent(input$factors, {
    showModal(modal_confirm)
  })
  observeEvent(input$ok, {
    removeModal()
  })
  # reset the radioGroupButtons when the reset button is clicked
  observeEvent(input$reset, {
    updateRadioGroupButtons(inputId = "age", selected = character(0))
    updateRadioGroupButtons(inputId = "sex", selected = character(0))
    updateRadioGroupButtons(inputId = "ethnicity", selected = character(0))
    updateRadioGroupButtons(inputId = "blood", selected = character(0))
  })
  
  
  # ----------------- Bar Plot -------------------------
  output$barPlotUI <- renderUI({
    if (any(sapply(list(input$age, input$sex, input$ethnicity, input$blood), is.null))) {
      tags$p("Results will appear here once all inputs have been selected.", 
             style = "color: red; font-style: italic;")
    } else {
      plotOutput("barPlot")
    }
  })
  
  output$barPlot <- renderPlot({
    
    dt_labels <- kidney_outcomes()[year == 5][
      order(-outcome)
    ][
      , ypos := cumsum(proportion) - proportion / 2
    ]
    
    ggplot(kidney_outcomes(), aes(x = year, y = proportion, fill = outcome)) +
      geom_col(width = 0.8)  +
      geom_label_repel( # add percentage labels for each section of box plot
        aes(label = scales::percent(proportion, accuracy = 1)),
        position = position_stack(vjust = 0.5),
        size = 6,
        colour = "black",
        direction = "x",
        box.padding = 0.5,
        segment.color = NA,
        min.segment.length = Inf,
        max.overlaps = Inf
      ) +
      scale_x_discrete(labels = function(x) paste("By the end of year", x)) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_fill_manual(values = colours) +
      geom_text_repel(# add legend labels on last bar
        data = dt_labels,
        aes(x = year, y = ypos, label = outcome, color = outcome),
        inherit.aes = FALSE,
        hjust = 0,
        direction = "y",
        xlim = c(3.42, 5),
        size = 7,
        fontface = "bold",
        min.segment.length = Inf,
        max.overlaps = Inf
      ) +
      scale_color_manual(values = colours) +
      coord_cartesian(clip = "off") +   
      labs(caption = "Outcomes after listing for a kidney transplant",
           #fill = "Outcome",
           #x = "Time after listing", 
           y = "Proportion of patients") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, 
                                   margin = margin(t = -7, b = 10), 
                                   size = 14,
                                   face = "bold"),
        legend.position = "none",
        # remove x axis ticks
        axis.ticks.x = element_blank(),
        # remove x-axis and y-axis title
        axis.title = element_blank(),
        # remove y-axis labels
        axis.text.y = element_blank(),
        # change plot.caption to left alligned
        plot.caption = element_text(hjust = 0, size = 14, face = "bold"),
        # remove grid lines
        panel.grid = element_blank(),
        # increase right-hand margin
        plot.margin = margin(t = 0, r = 90, b = 0, l = 0, unit = "pt")
      )
  })
}

shinyApp(ui, server)
