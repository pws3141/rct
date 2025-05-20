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
install.packages("gt")
library(gt)

# load 'plot_functions.R' file
source("./R/plot_functions.R")

# ---- Load Synthetic Data ----
kidney <- readRDS("./data/kidney.rds")

colours <- c(
  "Waiting" = "#66CCEE",
  "Removed" = "#DDCC77",
  "Died" = "#BBBBBB",
  "Transplanted" = "#228833"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Waiting Times"),
  
  # Two columns layout
  fluidRow(
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
             tabPanel("Table", gt_output("table")),
             tabPanel("Text", "Hello! :)")
           )
    )
  )
)

# factors considered text:
factors_not_included <- HTML("
<strong>Recipient BMI</strong> – Tested and not found to be significant in the model.<br><br>

<strong>Creatinine</strong> – The kidney function of donor is available but not
included. This is because it's not possible to know how many were on titration in
ITU. This would give a falsely low creatinine and potentially be misleading.<br><br>

<strong>Comorbidities (cardiovascular disease, vascular disease, stroke,
MI)</strong> – Not collected, looked into those that are, have a high proportion of
missing data.<br><br>

<strong>Time on dialysis</strong> – Recipient waiting time (years). Time waiting on
deceased donor kidney waiting list until time of transplant (active and suspended).
This can serve as a proxy for 'time on dialysis' as most patients are either already
on dialysis or due to commence dialysis within 6 months at the time of listing for
transplantation.")


# Define server logic required to draw a histogram
server <- function(input, output) {
  kidney_data <- reactive({
    # don't create until all options selected
    req(input$age, input$sex, input$ethnicity, input$blood)
    kidney[age_group == input$age & 
             sex == input$sex & 
             ethnicity == input$ethnicity &
             blood_group == input$blood]
  })
  
  kidney_ribbon <- reactive({
    # don't create until kidney_data() has been created
    req(kidney_data())
    
    kidney_ribbon <- copy(kidney_data())
    setorder(kidney_ribbon, waittime)
    # Calculate cumulative proportions
    kidney_ribbon[, txd_cum := cumsum(txd) / .N]
    kidney_ribbon[, remove_cum := cumsum(remove) / .N]
    kidney_ribbon[, death_cum := cumsum(death) / .N]
    kidney_ribbon[, waiting_cum := 1 - (txd_cum + remove_cum + death_cum)]
    
    # Create long-format ribbon data
    kidney_ribbon <- rbindlist(list(
      data.table(waittime_year = kidney_ribbon$waittime_year,
                 ymin = 0,
                 ymax = kidney_ribbon$txd_cum,
                 outcome = "Transplanted"),
      data.table(waittime_year = kidney_ribbon$waittime_year,
                 ymin = kidney_ribbon$txd_cum,
                 ymax = kidney_ribbon$txd_cum + kidney_ribbon$waiting_cum,
                 outcome = "Waiting"),
      data.table(waittime_year = kidney_ribbon$waittime_year,
                 ymin = kidney_ribbon$txd_cum + kidney_ribbon$waiting_cum,
                 ymax = 1 - kidney_ribbon$remove_cum,
                 outcome = "Died"),
      data.table(waittime_year = kidney_ribbon$waittime_year,
                 ymin = 1 - kidney_ribbon$remove_cum,
                 ymax = 1,
                 outcome = "Removed")
    ))[, outcome := factor(outcome,
                           levels = c("Removed", "Died", "Waiting", "Transplanted"))]
    
    # find values closest to year 5, and change them to be exactly 5
    rows_to_update <- kidney_ribbon[waittime_year <= 5, .I[which.max(waittime_year)], by = outcome]$V1
    # Set waittime_year := 5 in those rows
    kidney_ribbon[rows_to_update, waittime_year := 5]
    kidney_ribbon
  })
  
  kidney_outcomes <- reactive({
    # don't create until kidney_data() has been created
    req(kidney_data())
    
    copy(kidney_data())[
      , `:=`(
        year_1 = waittime_year <= 1,
        year_3 = waittime_year <= 3,
        year_5 = waittime_year <= 5
      )
    ][
      , .(
        # Prop of txd, died, removed for each period
        # Year 1
        txd_1 = mean(year_1 & txd == 1),
        death_1 = mean(year_1 & death == 1),
        remove_1 = mean(year_1 & remove == 1),
        waiting_1 = 1 - (mean(year_1 & txd == 1) + mean(year_1 & death == 1) + mean(year_1 & remove == 1)),
        # Year 2
        txd_3 = mean(year_3 & txd == 1),
        death_3 = mean(year_3 & death == 1),
        remove_3 = mean(year_3 & remove == 1),
        waiting_3 = 1 - (mean(year_3 & txd == 1) + mean(year_3 & death == 1) + mean(year_3 & remove == 1)),
        # Year 5
        txd_5 = mean(year_5 & txd == 1),
        death_5 = mean(year_5 & death == 1),
        remove_5 = mean(year_5 & remove == 1),
        waiting_5 = 1 - (mean(year_5 & txd == 1) + mean(year_5 & death == 1) + mean(year_5 & remove == 1))
      )
    ][
      , melt(.SD, measure.vars = patterns(".*"),
             variable.name = "name", value.name = "proportion")
    ][
      , c("outcome", "year") := tstrsplit(name, "_")
    ][
      , `:=`(
        count = round(proportion * 100),
        year_str = paste0(year, ifelse(year == "1", " year", " years")),
        outcome = factor(outcome,
                         levels = c("remove", "death", "waiting", "txd"),
                         labels = c("Removed", "Died", "Waiting", "Transplanted"))
      )
    ][
      order(outcome, year)
    ][
      , .(outcome, year, proportion, count, year_str)
    ][
      , colour := colours[as.character(outcome)]
      ]
  })

  kidney_table <- reactive({
    # don't create until kidney_data() has been created
    req(kidney_outcomes())
    
    kidney_table <- dcast(kidney_outcomes(), outcome ~ year, value.var = 'proportion')
    kidney_table[, outcome_order := fcase(
      outcome == "Removed", 3,
      outcome == "Died", 4,
      outcome == "Waiting", 1,
      outcome == "Transplanted", 2
    )]
    setorder(kidney_table, outcome_order)
  
    # add the phrase column
    kidney_table[, phrase := fcase(
      outcome == "Removed",      "have been removed from the list",
      outcome == "Died",         "have died",
      outcome == "Waiting",      "are still waiting",
      outcome == "Transplanted", "have been transplanted"
    )]
    kidney_table
  })
  
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
      tab_header(title = "Outcomes after listing for a kidney transplant") 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
