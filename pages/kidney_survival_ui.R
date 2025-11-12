layout_sidebar(
  sidebar = sidebar(
    #title = "How long might the kidney last?",
    title = "Enter details about the patient and donor (if known)",
    #h4("Enter details about the patient and donor (if known)"),
    # Informational text
    p("This tool uses data collected by NHS Blood and Transplant. Some data that is collected has not been included in the tool."),
    # create action button with blue background
    actionButton("ksurv_factors", "Show factors considered but not included",
                 icon = icon("info"), 
                 style = "color: #fff; background-color: #337ab7; border-color: #337ab7"),
    # add vertical space
    #tags$br(), tags$br(),
    
    # create action button with red background
    actionButton("ksurv_reset", "Reset all",
                 icon = icon("redo"), 
                 style = "color: #fff; background-color: #d9534f; border-color: #d9534f"),
    #
    radioGroupButtons(
      inputId = "ksurv_age",
      label = "Age", 
      choices = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
      selected = character(0),
      justified = TRUE
    ),
    
    radioGroupButtons(
      inputId = "ksurv_pkd",
      label = "Primary kidney disease", 
      choices = c("Diabetes", "PKD", "Glomerulonephritis", "Other"),
      selected = character(0),
      justified = TRUE
    ),
    
    radioGroupButtons(
      inputId = "ksurv_waiting_time",
      label = "Waiting Time (years)", 
      choices = c("1 year or less", "1 to 3 years", "3 to 5 years", "5 to 7 years", "over 7 years"),
      selected = character(0),
      justified = TRUE
    ),
    
    radioGroupButtons(
      inputId = "ksurv_prevtx",
      label = "Previous kidney transplant?", 
      choices = c("No", "Yes"),
      selected = character(0),
      justified = TRUE
    ),
    #
    width = 700
  ),
  # Main content area
  # Informational text
  h2("How long might the kidney last after a transplant from a deceased donor?"),
  p("The results here show, on average, what happened to people 'like you' in the past. 
    It is not a prediction of what will happen to you in the future"),
  p("There are many factors that can influence these results and make the numbers higher or lower for you."),
  navset_card_tab(
    nav_panel("Bar Chart", "Hello"), #withSpinner(uiOutput("barPlotUI"))),
    nav_panel("Area Chart", "Hello"), #withSpinner(uiOutput("cumulativePlotUI"))),
    nav_panel("Icon Display", "Hello"), #withSpinner(uiOutput("iconPlotUI"))),
    nav_panel("Table", "Hello"), #withSpinner(uiOutput("tableUI"))),
    nav_panel("Text", "Hello"), #withSpinner(uiOutput("textUI"))),
    full_screen = TRUE
  )
) # layout_sidebar