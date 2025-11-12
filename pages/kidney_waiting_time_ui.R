layout_sidebar(
  sidebar = sidebar(
    #title = "Kidney Waiting Times",
    title = "Enter details about the patient",
    #h4("Enter details about the patient"),
    # Informational text
    p("Other things might influence these results, for example other health conditions."),
    p("The tool cannot take into account all these factors.
      The tool uses data collected by NHSBT. Some data that is collected has not been used in the tool."),
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
) # layout_sidebar