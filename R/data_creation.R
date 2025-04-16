library(here)
library(data.table)

# set working directory using here
here::i_am("data_creation.R")


# create data
set.seed(100)
n_samples <- 50000

kidney <- data.table(id = 1:n_samples)[
  , txd := sample(c(0, 1), size = .N, replace = TRUE, prob = c(0.25, 0.75))
][
  , waittime := round(rweibull(.N, shape = 1.5, scale = 1000))
][
  , waittime_year := waittime / 365.25
][
  , death := fifelse(
    txd == 0,
    rbinom(.N, size = 1, prob = 0.2),
    0L
  )
][
  , remove := fifelse(
    txd == 0 & death == 0,
    rbinom(.N, size = 1, prob = 0.1),
    0L
  )
][
  , age := sample(18:89, size = .N, replace = TRUE)
][
  , age_group := cut(
    age,
    breaks = c(18, 29, 39, 49, 59, 69, Inf),
    labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
    right = TRUE
  )
][
  , sex := factor(sample(c("Male", "Female"), size = .N, replace = TRUE))
][
  , ethnicity := factor(sample(
    c("White", "Mixed", "Asian", "Black", "Other"),
    size = .N, replace = TRUE, prob = c(0.6, 0.1, 0.1, 0.1, 0.1)
  ))
][
  , blood_group := factor(sample(c("O", "A", "B", "AB"), size = .N, replace = TRUE))
]

outcomes <- copy(kidney)[
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
    # Year 3
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
]

# create ribbon dataset (required to map the fill to a factor within aes())
kidney_ribbon <- copy(kidney)
setorder(kidney_ribbon, waittime)
# Calculate cumulative proportions
kidney_ribbon[, txd_cum := cumsum(txd) / .N]
kidney_ribbon[, remove_cum := cumsum(remove) / .N]
kidney_ribbon[, death_cum := cumsum(death) / .N]
kidney_ribbon[, waiting_cum := 1 - (txd_cum + remove_cum + death_cum)]

# Create long-format ribbon data
ribbon_data <- rbindlist(list(
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
                       levels = c("Transplanted", "Waiting", "Removed", "Died"))
]

# save data as RDS
saveRDS(kidney, here("data", "kidney.rds"))
saveRDS(outcomes, here("data", "kidney_outcomes.rds"))
saveRDS(ribbon_data, here("data", "kidney_ribbon.rds"))
