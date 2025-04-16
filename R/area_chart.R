library(data.table)
library(waffle)
library(here)
here::i_am("data_creation.R")

# load kidney.rds, kidney_outcomes.rds and kidney_ribbon.rds from ./data/
kidney <- readRDS(here("data", "kidney.rds"))
outcomes <- readRDS(here("data", "kidney_outcomes.rds"))
ribbon_data <- readRDS(here("data", "kidney_ribbon.rds"))

# find values closest to year 5, and change them to be exactly 5
rows_to_update <- ribbon_data[waittime_year <= 5, .I[which.max(waittime_year)], by = outcome]$V1
# Set waittime_year := 5 in those rows
ribbon_data[rows_to_update, waittime_year := 5]

# find the closest values to waittime_year = 5, and create new rows with waittime_year = 5
# find values closest to year 5, and change them to be exactly 5
rows_to_update <- ribbon_data[waittime_year <= 5, .I[which.max(waittime_year)], by = outcome]$V1
# Set waittime_year := 5 in those rows
ribbon_data[rows_to_update, waittime_year := 5]
ribbon_data

dt_labels <- outcomes[year %in% c(1, 3, 5)][
  order(year, -outcome)
][
  , ypos := cumsum(proportion) - proportion / 2, by = year
][
  , xpos := fifelse(year == 1, 1, fifelse(year == 3, 3, 5))
][
  , label := scales::percent(proportion, accuracy = 1)
]

ggplot(ribbon_data, aes(x = waittime_year, ymin = ymin, ymax = ymax, fill = outcome)) +
  geom_ribbon() +
  scale_fill_manual(values = c(
    "Removed" = "#DDCC77",
    "Died" = "#BBBBBB",
    "Waiting" = "#66CCEE",
    "Transplanted" = "#228833"
  )) +
  geom_segment(data = data.table(x = c(1, 3, 5)),
               aes(x = x, xend = x, y = 0, yend = 1),
               inherit.aes = FALSE,
               colour = "grey60",
               linetype = "solid") +
  ylim(0, 1) +
  geom_label_repel(
    data = dt_labels,
    aes(x = xpos, y = ypos, label = label, fill = outcome),
    colour = "black",
    inherit.aes = FALSE,
    size = 7,
    direction = "both",
    segment.color = NA,
    fontface = "bold",
    max.overlaps = Inf
  ) +
  labs(x = "Wait time (years)", 
       y = "Cumulative proportion", 
       fill = "Outcome") +
  scale_x_continuous(
    limits = c(0, 5),
    breaks = c(1, 3, 5),
    labels = function(x) paste("End of year", x)
  ) +
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
    panel.grid = element_blank()
    # increase right-hand margin
    #plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  )

# Create waffle plot

outcomes_5yr <- outcomes[year == 5]

# Create pictogram waffle plot
ggplot(outcomes_5yr, aes(fill = outcome, values = count)) +
  geom_waffle(aes(color = outcome), 
              n_rows = 10, make_proportional = TRUE, flip = TRUE) +
  scale_fill_manual(values = c(
    "Transplanted" = "#228833",
    "Waiting" = "#66CCEE",
    "Died" = "#BBBBBB",
    "Removed" = "#DDCC77"
  )) +
  scale_color_manual(values = c(
    "Transplanted" = "#fff",
    "Waiting" = "#fff",
    "Died" = "#fff",
    "Removed" = "#fff"
  )) +
  coord_equal() +
  theme_minimal() +
  theme_enhance_waffle() 

ggplot(outcomes_5yr, aes(label = outcome, values = count)) +
  geom_pictogram(
    n_rows = 10,
    aes(color = outcome),
    family = "sans",
    flip = TRUE,
    size = 6,
    show.legend = FALSE
  ) +
  scale_label_pictogram(values = c("square")) +
  scale_color_manual(values = c(
    "Transplanted" = "#228833",
    "Waiting" = "#66CCEE",
    "Died" = "#BBBBBB",
    "Removed" = "#DDCC77"
  )) +
  coord_equal() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) +
  ggtitle("Outcomes after 5 years\n(each icon represents one person)")
