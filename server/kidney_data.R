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
    , percent := proportion * 100
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