## Plotting functions

## Icon plot

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

# ---- Helper for Waffle Plots ----
create_waffle_plot <- function(dt, y, col) {
  data_tmp <- dt[year == y]
  # Set outcome as factor with all expected levels
  data_tmp[, outcome := factor(outcome, levels = names(col))]
  # reverse the factors such that transplant is at the bottom of the icon plots
  setorder(data_tmp, -outcome)
  ggplot(data_tmp, aes(fill = outcome, values = count)) +
    geom_waffle(aes(color = outcome), n_rows = 10, make_proportional = TRUE, flip = TRUE) +
    scale_fill_manual(values = col) +
    scale_color_manual(values = rep("#fff", 4)) +
    labs(caption = paste("At the end of year", y)) +
    guides(color = "none", fill = "none") +
    coord_equal() +
    theme_minimal() +
    theme_enhance_waffle() +
    theme(plot.caption = element_text(hjust = 0.1, size = 14, face = "bold"))
}