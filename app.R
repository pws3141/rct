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

# --------------------------------------------------
# ------------ Home cards
homeCards <- list(
  card(card_header("Kidney Transplants: Understanding the Numbers"),
       #
       h4(tags$b("Who is this tool for?")),
       p("This tool is for people over 18 who are considering kidney transplantation."),
       p("It should be used with a tranplant doctor, specialist nurse or other healthcare professional."),
       h4(tags$b("What does this tool do?")),
       p("This is a communication tool. It will help clinicians explain numbers about transplants to patients."),
       p("It will show graphs and charts of waiting times and survival outcomes that are more personalised to each patient."),
       p("Results of the tool can be printed for patients to take home."),
       h4(tags$b("What will it show?")),
       tags$ul(
         tags$li("Likely waiting time"),
         tags$li("How long the kidney might last"),
         tags$li("How long the patient might survive after a transplant")
       ),
       h4(tags$b("How does it work?")),
       p("The tool takes information about the patient and calculates what happened to people in the registry who had the same characteristics."),
       #p("Even more words, with a ",
       #  tags$a(href = "https://digital.nhs.uk/data-and-information/statistical-publications-open-data-and-data-products",
       #                        "link"), 
       #  " to the webpage with the report on."),
       p(sprintf("This dashboard was last updated on %s.", Sys.Date())),
       br()
  ),
  card(card_header("What will it show?"),
       tagList(
         tags$ul(
           tags$li("Likely waiting time"),
           tags$li("How long the kidney might last"),
           tags$li("How long the patient might survive after a transplant")
         )
       )
  )
)

aboutCards <- list(
  card(card_header("Model development"),
        h4("Overview"),
        p("The tool takes information about you, such as age, blood group, disease, and it looks at people who had these same characteristics, and shows what happened to these people. For example, how many people 'like you' received a transplant within one year of being listed."),
        p("It is not showing you what will happen to you, it is showing you what happened to people like you, in the past."),
        p("It’s important to remember that the tool cannot take into account everything about you, for example, whether you have other health conditions. The tool will ask for some medical information such as blood group, or recent test results. The tool will be less accurate if you don't have all the relevant information."),
        p("There are many factors that can influence how well a transplanted organ does, for example taking your medication properly, diet and whether you exercise."),
        p("If you want to know more about the models and data behind the tools, please read the Technical section. Data about transplant patients were used to create statistical models. When you enter information into the tool, the calculator looks at these models and produces results."),
        p("⚠ Changes to the UK Kidney Offering Scheme in September 2019 are not reflected in these models"),
        h4("Using the tool offline"),
        p("You need an internet connection to access the tool for the first time, but once you have visited the site once, you can access it offline (just don't close the browser)."),
        p("The tool can produce a printout of results for later reference."),
        h4("Who is this site for?"),
        p("The tool is suitable for kidney patients who are over 18 years old. This is because we use past data from the NHS transplant registry. Fewer children have transplants than adults and there is not enough data yet to make a tool for children."),
        p("The tool should be used by patients alongside their transplant doctors or coordinator."),
        h4("Who developed the tool?"),
        p("The tool was developed by the Winton Centre for Risk and Evidence Communication and currently displays models disclosed by NHSBT under a data sharing agreement."),
        p("We wish to thank all the transplant patients and their partners, as well as clinical teams at transplant centres in England who took part in researching the tool design.")
       )
)

techPanels <- list(
  nav_panel(title = "Model development",
       p("The models behind the tool were developed using UK Transplant Registry (UKTR) data which is held by NHS Blood and Transplant (NHSBT). The UKTR database contains information on all patients who are listed for transplantation in the UK, and all patients who are transplanted with a solid organ transplant in the UK with follow-up data."),
       p("NHSBT Statisticians work closely with transplant clinicians to compile a large list of potential variables (e.g. age, primary renal disease) from the UK Transplant Registry to test in their models. Each of these variables are statistically tested and kept in the model if found to have an important relationship with the outcome of interest (e.g. post-transplant survival). These variables are referred to as ‘risk factors’. Some of the models used by the tool are also used regularly by NHSBT in their organ specific annual reports (",
       tags$a(href = "https://www.odt.nhs.uk/statistics-and-reports/organ-specific-reports/", "https://www.odt.nhs.uk/statistics-and-reports/organ-specific-reports/"),
       ") and in other analyses."),
       p("At the end of the modelling process values were obtained called ‘parameter estimates’ which quantify the estimated impact of each risk factor upon the outcome of interest. Please refer to the Mathematical Section below to see exactly how a change in parameter estimates affects the outcome of interest. There will also be an estimated baseline risk curve plotted over time that represents an ‘average’ patient in the study cohort. The most common/mean value from the model development dataset for each risk factor is indicated as the baseline value as this value is represented by the baseline curve. The parameter estimates are then used by the tool to essentially shift this baseline curve when the values of the risk factors are changed from the ‘average’ values. This way, the patient can plot a curve for values of the risk factors that are relevant to their own circumstances. For all models, transplant centre was treated as a stratifying factor, i.e. a separate baseline curve was produced for each centre."),
       p("Although the tool is based on reputable models, it cannot say what the outcomes for a particular patient will be. It can only provide a summary of survival and waiting list outcomes for people in the past with similar characteristics."),
       p("This tool has been developed using retrospective registry data. Therefore, changes to the Kidney Offering Scheme in 2019 are NOT reflected in these models."),
       p("All statistical analyses for this website were generated using SAS Enterprise Guide software, Version 7.1. SAS and all other SAS Institute Inc. product or service names are registered trademarks or trademarks of SAS Institute Inc., Cary, NC, USA.")
  )
)

ui <- page_navbar(
  theme = bs_theme(version = 5),
  # Header area
  title = "Risk Communication Tool",   # Appears at the top of the page
  window_title = "Risk Communication Tool", # Shows in the browser tab
  id = "nav",
  

  
  nav_panel(title = "Home",
            layout_columns(
              col_widths = c(8, 4),
              #row_heights = c(1, 2),
              homeCards[[1]],
              homeCards[[2]],
            ),
            icon = bsicons::bs_icon("house")
  ),
  
  nav_panel(title = "About",
            aboutCards[[1]],
            icon = bsicons::bs_icon("question-circle")
            ),
  
  nav_panel(title = "Technical",
            navset_card_underline(
              title = "Technical Details for the Kidney Tool",
              techPanels[[1]]
            ) # navset_card_underline
            ), # nav_panel
  
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
  ) # nav_panel kidney WT
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
