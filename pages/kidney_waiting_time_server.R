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