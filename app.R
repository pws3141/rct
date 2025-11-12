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
  ), # nav_panel
  
  nav_panel(title = "About",
            !!!aboutCards,
            icon = bsicons::bs_icon("question-circle")
            ),
  
  nav_panel(title = "Publications",
            !!!publicationCard,
            icon = bsicons::bs_icon("file-text")
  ), # nav_panel
  
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
                source(file.path("pages/kidney_waiting_time_ui.R"), local = TRUE)$value,
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
  
  
  # Get content for individual pages
  source(file.path("pages/kidney_waiting_time_server.R"), local = TRUE)$value
  
  
} # server

shinyApp(ui, server)
