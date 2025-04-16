# ---- Setup ----
library(data.table)
library(waffle)
library(here)
library(ggplot2)
library(gridExtra)
library(grid)

here::i_am("data_creation.R")

# ---- Load Data ----
outcomes <- readRDS(here("data", "kidney_outcomes.rds"))

# ---- Colour Mapping ----
colours <- c(
  "Waiting" = "#66CCEE",
  "Removed" = "#DDCC77",
  "Died" = "#BBBBBB",
  "Transplanted" = "#228833"
)
outcomes[, colour := colours[as.character(outcome)]]

# ---- Create Label Plot ----
create_status_display <- function(status, count, colour) {
  ggplot() + 
    theme_void() +
    xlim(0, 3) + ylim(0, 1) +
    annotation_custom(rectGrob(width = unit(0.5, "cm"), height = unit(0.5, "cm"),
                               gp = gpar(fill = colour, col = NA)),
                      xmin = 0.5, xmax = 0.5, ymin = 0.5, ymax = 0.5) +
    annotation_custom(textGrob(paste(count, status), gp = gpar(fontsize = 12),
                               hjust = 0),
                      xmin = 0.9, xmax = 0.9, ymin = 0.5, ymax = 0.5)
}

# ---- Generate Status Display Grobs per Year ----
outcome_str <- as.character(unique(outcomes[, outcome]))
years <- c(1, 3, 5)

status_panels <- lapply(years, function(y) {
  plots <- lapply(outcome_str, function(o) {
    row <- outcomes[year == y & outcome == o]
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

# ---- Helper for Waffle Plots ----
create_waffle_plot <- function(y) {
  data_tmp <- outcomes[year == y]
  # reverse the factors such that transplant is at the bottom of the icon plots
  setorder(data_tmp, -outcome)
  ggplot(data_tmp, aes(fill = outcome, values = count)) +
    geom_waffle(aes(color = outcome), n_rows = 10, make_proportional = TRUE, flip = TRUE) +
    scale_fill_manual(values = colours) +
    scale_color_manual(values = rep("#fff", 4)) +
    labs(caption = paste("At the end of year", y)) +
    guides(color = "none", fill = "none") +
    coord_equal() +
    theme_minimal() +
    theme_enhance_waffle() +
    theme(plot.caption = element_text(hjust = 0.1, size = 14, face = "bold"))
}


# ---- Combine Waffle Plots ----
waffle_grobs <- lapply(years, create_waffle_plot)

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
