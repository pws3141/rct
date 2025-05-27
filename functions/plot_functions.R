## Plotting functions

## Bar Plot

# ---- Create Bar Plot ----
create_bar_plot <- function(plot.data) {
  ggplot(plot.data, aes(x = year, y = proportion, fill = outcome)) +
  geom_col(width = 0.8)  +
  geom_label_repel( # add percentage labels for each section of box plot
    aes(label = scales::percent(proportion, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 6,
    colour = "black",
    direction = "x",
    box.padding = 0.5,
    segment.color = NA,
    min.segment.length = Inf,
    max.overlaps = Inf
  ) +
  scale_x_discrete(labels = function(x) paste("By the end of year", x)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = colours) +
  geom_text_repel(# add legend labels on last bar
    data = dt_labels,
    aes(x = year, y = ypos, label = outcome, color = outcome),
    inherit.aes = FALSE,
    hjust = 0,
    direction = "y",
    xlim = c(3.42, 5),
    size = 7,
    fontface = "bold",
    min.segment.length = Inf,
    max.overlaps = Inf
  ) +
  scale_color_manual(values = colours) +
  coord_cartesian(clip = "off") +   
  labs(caption = "Outcomes after listing for a kidney transplant",
       #fill = "Outcome",
       #x = "Time after listing", 
       y = "Proportion of patients") +
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
    panel.grid = element_blank(),
    # increase right-hand margin
    plot.margin = margin(t = 0, r = 90, b = 0, l = 0, unit = "pt")
  )
}

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
  #data_tmp[, outcome := factor(outcome, levels = names(col))]
  # Set levels in desired visual bottom-to-top order
  lvl <- c("Transplanted", "Waiting", "Died", "Removed")
  data_tmp[, outcome := factor(outcome, levels = lvl)]

  # Order so that first row = bottom row of waffle
  setorder(data_tmp, outcome)

  # reverse the factors such that transplant is at the bottom of the icon plots
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
