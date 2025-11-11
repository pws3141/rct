library(shiny)
library(bslib)
library(bsicons)
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

ui <- withMathJax(page_navbar(
  theme = bs_theme(version = 5),
  # Header area
  title = "Risk Communication Tool",   # Appears at the top of the page
  window_title = "Risk Communication Tool", # Shows in the browser tab
  id = "main_nav",
  
  nav_panel(title = "Home",
            layout_columns(
              col_widths = c(7, 5),
              #row_heights = c(1, 2),
              !!!homeCards,
            ),
            icon = bsicons::bs_icon("house")
  ),
  
  nav_panel(title = "About",
            !!!aboutCards,
            icon = bsicons::bs_icon("question-circle")
            ),
  
  nav_panel(title = "Publications",
            !!!publicationCard,
            icon = bsicons::bs_icon("file-text")
  ),
  
  nav_panel(title = "Technical",
            navset_pill_list(
              #title = "Technical Details for the Kidney Tool",
              !!!techPanels,
              widths = c(3, 9)
            ), # navset_pill_list
            icon = bsicons::bs_icon("gear"),
            id = "technical"
  ), # nav_panel
  
  nav_panel(
    title = "Dashboards",
    navset_card_underline(
      # Kidney Waiting Time nav_panel
      nav_panel("Kidney Waiting Times",
                layout_sidebar(
                  sidebar = sidebar(
                    title = "Kidney Waiting Times",
                    h4("Enter details about the patient"),
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
                    width = 700
                  ),
                  # Main content area
                  # Informational text
                  h2("What might happen if you are listed for a transplant?"),
                  p("The results below show what happened to people like you in the past. 
                         It is not a prediction of what will happen in the future."),
                  p("There are many factors that can influence these results and make the numbers higher or lower for you."),
                  navset_card_tab(
                    nav_panel("Bar Chart", withSpinner(uiOutput("barPlotUI"))),
                    nav_panel("Area Chart", withSpinner(uiOutput("cumulativePlotUI"))),
                    nav_panel("Icon Display", withSpinner(uiOutput("iconPlotUI"))),
                    nav_panel("Table", withSpinner(uiOutput("tableUI"))),
                    nav_panel("Text", withSpinner(uiOutput("textUI"))),
                    full_screen = TRUE
                  )
                ), # layout_sidebar
                icon = bsicons::bs_icon("clock-history")
      ), # nav_panel kidney WT
      nav_panel("How long might the kindey last?", "TODO"),
      nav_panel("How long might the patient survive?", "TODO")
    ), # navset_card_tab
    icon = bsicons::bs_icon("activity"),
  ), # nav_panel

  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_nhsbt),
    nav_item(link_otdt)
  ), # nav_menu
) 
)

server <- function(input, output) {
  # Data reactives
  source(file.path("server/kidney_data.R"), local = TRUE)$value
  
  observe({
    if (input$main_nav == "Dashboards")  {
      showModal(modalDialog(
        title = "Transplant Centre",
        div(id = "aa", style = "width: 1100px; height: 100px;", 
            HTML("<b>Choose the transplant centre before continuing:</b>")),
        
        easyClose = FALSE
      ))
    }
  }) |> bindEvent(input$main_nav) # observe
    
  
  observe({
    if (input$main_nav == "Dashboards")  {
      showModal(modalDialog(
        title = "Transplant Centre",
        div(
          id = "aa", 
          style = "width: 1100px;", 
          HTML("<b>Choose the transplant centre before continuing:</b>"),
          br(), br(),
          selectInput(
            inputId = "selected_centre",
            label = "Select Transplant Centre:",
            choices = c("Choose a transplant centre" = "", transplantCentres),
            selected = NULL
          )
        ),
        easyClose = FALSE,
        fade = FALSE,
        footer = tagList(
          #modalButton("Cancel"),
          actionButton("confirm_centre", "Confirm", class = "btn-primary")
        )
      ))
    }
  }) |> bindEvent(input$main_nav)
  
  # Handle the confirmation
  observeEvent(input$confirm_centre, {
    if (input$selected_centre != "") {
      removeModal()
      # You can now use input$selected_centre in your app
      showNotification(paste("Selected:", input$selected_centre), type = "message")
    } else {
      showNotification("Please select a transplant centre", type = "warning")
    }
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
    
    create_bar_plot(plot.data = kidney_outcomes(), label.data = dt_labels)
    
  })
  
  # --------------------- Area Chart --------------------
  output$cumulativePlotUI <- renderUI({
    if (any(sapply(list(input$age, input$sex, input$ethnicity, input$blood), is.null))) {
      tags$p("Results will appear here once all inputs have been selected.", 
             style = "color: red; font-style: italic;")
    } else {
      plotOutput("cumulativePlot")
    }
  }) # renderUI
  
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
    # 
    create_area_chart(plot.data = kidney_ribbon(), label.data = dt_labels)
  }) # renderPlot
  
  # ------------------------ Icon Plot -----------------
  output$iconPlotUI <- renderUI({
    if (any(sapply(list(input$age, input$sex, input$ethnicity, input$blood), is.null))) {
      tags$p("Results will appear here once all inputs have been selected.", 
             style = "color: red; font-style: italic;")
    } else {
      plotOutput("iconPlot")
    }
  })
  
  output$iconPlot <- renderPlot({
    # generate Status Display Grobs for years 1, 3 and 5 
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
    # combine Status Display Columns
    combined_plot <- arrangeGrob(
      grobs = status_panels,
      ncol = 3,
      widths = unit(rep(1, 3), "null"),
      top = ""
    )
    
    # Create Waffle Plots
    waffle_grobs <- lapply(years, function(y) {
      create_waffle_plot(dt = kidney_outcomes(), y, col = colours)
    })
    
    # Combine Waffle Plots 
    waffle_row <- arrangeGrob(
      grobs = waffle_grobs,
      ncol = 3,
      widths = unit(rep(1, 3), "null"),
      top = ""
    )
    
    # Final Layout 
    final_arrangement <- grid.arrange(
      combined_plot,
      waffle_row,
      nrow = 2,
      heights = unit(c(1, 3), "null")
    )
  })
  
  # ---------------------- Table ---------------------
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
        decimals     = 0,
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
  
  
  # --------------------------- Text -----------------------------
  
  output$textUI <- renderUI({
    if (any(sapply(list(input$age, input$sex, input$ethnicity, input$blood), is.null))) {
      tags$p("Results will appear here once all inputs have been selected.", 
             style = "color: red; font-style: italic;")
    } else {
      htmlOutput("text")
    }
  })
  output$text <- renderText(
    paste(
      "<br><b style='font-size:18px;'>By the end of year 1</b><br>",
      sprintf("%.0f%% are still waiting <br>",
              kidney_outcomes()[year == 1 & outcome == "Waiting", c(percent)]),
      sprintf("%.0f%% have been transplanted <br>",
              kidney_outcomes()[year == 1 & outcome == "Transplanted", c(percent)]),
      sprintf("%.0f%% have been removed from the list <br>",
              kidney_outcomes()[year == 1 & outcome == "Removed", c(percent)]),
      sprintf("%.0f%% have died <br>",
              kidney_outcomes()[year == 1 & outcome == "Died", c(percent)]),
      #
      "<br><br><b style='font-size:18px;'>By the end of year 3 </b> <br>",
      sprintf("%.0f%% are still waiting <br>",
              kidney_outcomes()[year == 3 & outcome == "Waiting", c(percent)]),
      sprintf("%.0f%% have been transplanted <br>",
              kidney_outcomes()[year == 3 & outcome == "Transplanted", c(percent)]),
      sprintf("%.0f%% have been removed from the list <br>",
              kidney_outcomes()[year == 3 & outcome == "Removed", c(percent)]),
      sprintf("%.0f%% have died <br>",
              kidney_outcomes()[year == 3 & outcome == "Died", c(percent)]),
      #
      "<br><br><b style='font-size:18px;'>By the end of year 5 </b> <br>",
      sprintf("%.0f%% are still waiting <br>",
              kidney_outcomes()[year == 5 & outcome == "Waiting", c(percent)]),
      sprintf("%.0f%% have been transplanted <br>",
              kidney_outcomes()[year == 5 & outcome == "Transplanted", c(percent)]),
      sprintf("%.0f%% have been removed from the list <br>",
              kidney_outcomes()[year == 5 & outcome == "Removed", c(percent)]),
      sprintf("%.0f%% have died <br>",
              kidney_outcomes()[year == 5 & outcome == "Died", c(percent)])
    )
  )
  
} # server

shinyApp(ui, server)
