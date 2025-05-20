library(shiny)
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

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Waiting Times"),
  
  # Two columns layout
  fluidRow(
    
    ##############################################.
    # KIDNEY, WAITING TIMES, PAGE ----
    ##############################################.
    column(4,
           # Informational text
           h2("Enter details about the patient."),
           p("Other things might influence these results, for example other health conditions."),
           p("The tool cannot take into account all these factors."),
           p("The tool uses data collected by NHSBT. Some data that is collected has not been used in the tool."),
           p("Changes to the kidney offering scheme in September 2019 are not yet reflected in the tool."),
           
           # create action button with blue background
           actionButton("factors", "Show factors considered but not included",
                        icon = icon("info"), 
                        style = "color: #fff; background-color: #337ab7; border-color: #337ab7"),
           
           # add vertical space
           tags$br(), tags$br(),
           
           # create action button with red background
           actionButton("reset", "Reset all",
                        icon = icon("redo"), 
                        style = "color: #fff; background-color: #d9534f; border-color: #d9534f"),
           
           # add vertical space
           tags$br(),
           
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
           )
    ),
    column(8,
           # Informational text
           h2("What might happen if you are listed for a transplant?"),
           p("The results below show what happened to people like you in the past. 
             It is not a prediction of what will happen in the future."),
           p("There are many factors that can influence these results and make the numbers higher or lower for you."),

           tabsetPanel(
             tabPanel("Bar Chart", withSpinner(uiOutput("barPlotUI"))),
             tabPanel("Area Chart", withSpinner(uiOutput("cumulativePlotUI"))),
             tabPanel("Icon Display", withSpinner(uiOutput("iconPlotUI", height = "600px"))),
             tabPanel("Table", withSpinner(uiOutput("tableUI"))),
             tabPanel("Text", htmlOutput("text"))
           )
    )
  )
)

####################################################
# Server
#####################################################
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
  
  output$cumulativePlotUI <- renderUI({
    if (any(sapply(list(input$age, input$sex, input$ethnicity, input$blood), is.null))) {
      tags$p("Results will appear here once all inputs have been selected.", 
             style = "color: red; font-style: italic;")
    } else {
      plotOutput("cumulativePlot")
    }
  })
  
  output$cumulativePlot <- renderPlot({
    # create years 1 3 and 5 label data
    dt_labels <- kidney_outcomes()[year %in% c(1, 3, 5)][
      order(year, -outcome)
    ][
      , ypos := cumsum(proportion) - proportion / 2, by = year
    ][
      , xpos := fifelse(year == 1, 1, fifelse(year == 3, 3, 5))
    ][
      , label := scales::percent(proportion, accuracy = 1)
    ]
    # draw the histogram with the specified number of bins
    ggplot(kidney_ribbon(), aes(x = waittime_year, ymin = ymin, ymax = ymax, fill = outcome)) +
      geom_ribbon() +
      scale_fill_manual(values = colours) +
      scale_color_manual(values = colours) +
      geom_segment(data = data.table(x = c(1, 3, 5)),
                   aes(x = x, xend = x, y = 0, yend = 1),
                   inherit.aes = FALSE,
                   colour = "grey60",
                   linetype = "solid") +
      ylim(0, 1) +
      geom_label_repel( # add percentages at yrs 1, 3, 5
        data = dt_labels,
        aes(x = xpos, y = ypos, label = label, fill = outcome),
        colour = "black",
        inherit.aes = FALSE,
        size = 6,
        direction = "both",
        segment.color = NA,
        max.overlaps = Inf
      ) +
      geom_text_repel( # add outcome labels on RH side
        data = dt_labels[year == 5],
        aes(x = xpos, y = ypos, label = outcome, color = outcome),
        inherit.aes = FALSE,
        xlim = c(5.3, 7),
        size = 7,
        fontface = "bold",
        direction = "y",
        segment.color = NA,
        max.overlaps = Inf
      ) +
      labs(caption = "Outcomes after listing for a kidney transplant"#,
           #x = "Wait time (years)", 
           #y = "Cumulative proportion", 
           #fill = "Outcome"
           ) +
      scale_x_continuous(
        limits = c(0, 5),
        breaks = c(1, 3, 5),
        labels = function(x) paste("End of year", x)
      ) +
      coord_cartesian(clip = "off") +   
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
        plot.margin = margin(t = 0, r = 200, b = 0, l = 0, unit = "pt")
      )
  })
  
  output$iconPlotUI <- renderUI({
    if (any(sapply(list(input$age, input$sex, input$ethnicity, input$blood), is.null))) {
      tags$p("Results will appear here once all inputs have been selected.", 
             style = "color: red; font-style: italic;")
    } else {
      plotOutput("iconPlot")
    }
  })

  output$iconPlot <- renderPlot({
    # ---- Generate Status Display Grobs for years 1, 3 and 5 ----
    outcome_str <- as.character(unique(kidney_outcomes()[, outcome]))
    years <- c(1, 3, 5)
    
    status_panels <- lapply(years, function(y) {
      plots <- lapply(outcome_str, function(o) {
        row <- kidney_outcomes()[year == y & outcome == o]
        create_status_display(row$outcome, row$count, row$colour)
      })
      arrangeGrob(
        grobs = plots,
        ncol = 1,
        heights = unit(rep(0.7, length(plots)), "cm"),
        top = "", bottom = "", left = "", right = ""
      )
    })
    # ---- Combine Status Display Columns ----
    combined_plot <- arrangeGrob(
      grobs = status_panels,
      ncol = 3,
      widths = unit(rep(1, 3), "null"),
      top = ""
    )
    
    # ---- Create Waffle Plots ----
    waffle_grobs <- lapply(years, function(y) {
      create_waffle_plot(dt = kidney_outcomes(), y, col = colours)
      })
    
    # ---- Combine Waffle Plots ----
    waffle_row <- arrangeGrob(
      grobs = waffle_grobs,
      ncol = 3,
      widths = unit(rep(1, 3), "null"),
      top = ""
    )
    
    # ---- Final Layout ----
    final_arrangement <- grid.arrange(
      combined_plot,
      waffle_row,
      nrow = 2,
      heights = unit(c(1, 3), "null")
    )
  })
  
  
  output$tableUI <- renderUI({
    if (any(sapply(list(input$age, input$sex, input$ethnicity, input$blood), is.null))) {
      tags$p("Results will appear here once all inputs have been selected.", 
             style = "color: red; font-style: italic;")
    } else {
      gt_output("table")
    }
  })
  
  output$table <- render_gt({
    kidney_table() |>
      gt() |>
      ## format the three year-columns as % (underlying values stay numeric)
      fmt_percent(
        columns      = c(`1`, `3`, `5`),
        decimals     = 1,
        scale_values = TRUE          # multiply by 100
      ) |>
      ## first col of 'cols_merge' is the target colmns
      cols_merge(columns = c(`1`, phrase), pattern = "{1} {2}") |>
      cols_merge(columns = c(`3`, phrase), pattern = "{1} {2}") |>
      cols_merge(columns = c(`5`, phrase), pattern = "{1} {2}") |>
      cols_hide(columns = c(outcome, outcome_order, phrase)) |>
      ## relabel headers, add title, etc.
      cols_label(
        `1` = md("**By the end of year 1**"),
        `3` = md("**By the end of year 3**"),
        `5` = md("**By the end of year 5**")
      ) |>
      cols_align(align = 'left') |>
      tab_header(title = "Outcomes after listing for a kidney transplant") |>
      tab_options(
        # https://gt.albert-rapp.de/styling
        data_row.padding = px(8),
        #summary_row.padding = px(3), # A bit more padding for summaries
        #row_group.padding = px(12)    # And even more for our groups
        heading.align = 'left',
        heading.padding = px(20)
      ) |>
      tab_style(
        style = list(
          cell_fill(color = colours_trans["Waiting"])
        ),
        locations = cells_body(
          #columns = vars(V1, V2), # not needed if coloring all columns
          rows = 1)
      ) |>
      tab_style(
        style = list(cell_fill(color = colours_trans["Transplanted"])),
        locations = cells_body(rows = 2)
      ) |>
      tab_style(
        style = list(cell_fill(color = colours_trans["Removed"])),
        locations = cells_body(rows = 3)
      ) |>
      tab_style(
        style = list(cell_fill(color = colours_trans["Died"])),
        locations = cells_body(rows = 4)
      )
  }, align = "left")
  
  output$text <- renderText(
      paste(
        "<b>By the end of year 1 </b> <br>",
        sprintf("Hello! %s %s %s %s", 
                input$age, input$sex, input$ethnicity, input$blood),
      "<br><br><b>By the end of year 3 </b> <br>",
      sprintf("Hello! %s %s %s %s", 
              input$age, input$sex, input$ethnicity, input$blood),
      "<br><br><b>By the end of year 5 </b> <br>",
      sprintf("Hello! %s %s %s %s", 
              input$age, input$sex, input$ethnicity, input$blood)
      )
    )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
