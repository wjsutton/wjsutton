# Career Timeline Gantt Chart Recreation
# Install and load required packages

library(ganttrify)
library(dplyr)
library(ggplot2)
library(lubridate)

# Prepare the data based on the timeline shown in your image
df <- data.frame(
  wp = c(
    "Work",
    "Work",
    "Work",
    "Work",
    "Work",
    "Work",
    "Education",
    "Work",
    "Work",
    "Work",
    "Education"
  ),
  activity = c(
    "Consultant, The Information Lab",
    "WorldRemit, Senior Data Analyst",
    "BBC, Senior Data Analyst",
    "Cerulli Associates, Senior Analyst",
    "Killik Asset Management, Dealing & Portfolio Admin",
    "Placement Year, Internships & Temporary Work",
    "Loughborough University, Maths & Management",
    "Placement Year, Internships & Temporary Work",
    "Placement Year, Internships & Temporary Work",
    "Placement Year, Internships & Temporary Work",
    "Loughborough University, Maths & Management"
  ),
  start_date = as.Date(c(
    "2022-06-01", # TIL
    "2021-09-01", # WorldRemit, 
    "2017-04-01", # BBC, 
    "2013-09-01", # Cerulli
    "2011-10-01", # Killik
    "2011-08-01", # argo
    "2010-09-01",  # Loughborough University
    "2011-04-01",  # JPM
    "2009-11-01", # ABC
    "2008-07-01", # building
    "2007-09-01" # Loughborough University
  )),
  end_date = as.Date(c(
    Sys.Date(), # Consultant end
    "2022-06-01", # WorldRemit end
    "2021-09-01", # BBC end
    "2017-04-01", # Cerulli end
    "2013-10-01", # Killik end
    "2011-10-01", # argo
    "2011-07-01", # Uni
    "2011-05-01",  # JPM 
    "2010-09-01", # ABC
    "2008-09-01", # building
    "2009-07-01" # Uni
  )),
  stringsAsFactors = FALSE
)

# Create unique identifiers for activities to avoid duplication issues
df$activity_id <- paste0(df$activity, " (", format(df$start_date, "%Y"), ")")

start_date <- as.Date('2007-09-01')
df$start_date <- as.Date(df$start_date)
df$end_date <- as.Date(df$end_date)

# Work out length of time in current job (started June 2022)
# Calculate quarters properly - only increment at quarter boundaries
current_job_start <- as.Date("2022-06-01")
current_date <- Sys.Date()

# Calculate total months since start
years_diff <- as.numeric(format(current_date, "%Y")) - as.numeric(format(current_job_start, "%Y"))
months_diff <- as.numeric(format(current_date, "%m")) - as.numeric(format(current_job_start, "%m"))

# Adjust if we haven't reached the anniversary month
if (as.numeric(format(current_date, "%m")) < as.numeric(format(current_job_start, "%m")) || 
    (as.numeric(format(current_date, "%m")) == as.numeric(format(current_job_start, "%m")) && 
     as.numeric(format(current_date, "%d")) < as.numeric(format(current_job_start, "%d")))) {
  years_diff <- years_diff - 1
  months_diff <- months_diff + 12
}

total_months <- years_diff * 12 + months_diff
quarters <- floor(total_months / 3)
rounded_years <- quarters * 0.25
years_in_analytics = round(rounded_years + 0.75 + 4.5 + 3.5)
latest_job_label = paste0(rounded_years,' years')

# Create the enhanced Gantt chart
career_chart <- ganttrify(
  project = df, 
  project_start_date = "2007-10-01",
  hide_wp = TRUE,
  by_date = TRUE,
  month_breaks = 18,
  month_number_label = FALSE,
  month_date_label = TRUE,
  show_vertical_lines = FALSE,  # This removes vertical gridlines
  x_axis_position = "bottom",
  colour_palette = c("#6ACCEA", "#00FFB8"),
  size_text_relative = 2.0, 
  alpha_wp = 0,              
  alpha_activity = 1,
  colour_stripe = "white"
) 

# Get the base plot from ganttrify
base_plot <- career_chart

# Create custom labels - you can edit these as needed
custom_labels <- c(
  latest_job_label,     # Consultant, The Information Lab
  "0.75 years",      # WorldRemit, Senior Data Analyst  
  "4.5 years",     # BBC, Senior Data Analyst
  "3.5 years",     # Cerulli Associates, Senior Analyst
  "2 years",     # Killik Asset Management, Dealing & Portfolio Admin
  "1.25 years",      # Placement Year, Internships & Temporary Work (2011)
  "3 years",     # Loughborough University, Maths & Management (2010)
  "",       # Placement Year, Internships & Temporary Work (2011) - JPM
  "",     # Placement Year, Internships & Temporary Work (2009) - ABC
  "",      # Placement Year, Internships & Temporary Work (2008) - building
  ""      # Loughborough University, Maths & Management (2007)
)

# Create label data frame with custom labels
label_data <- data.frame(
  activity = df$activity,
  label_x = df$end_date + days(60),  # Position labels after bar end
  y_position = 1:nrow(df),           # Match ganttrify's ordering (1 = top row)
  custom_label = custom_labels,
  stringsAsFactors = FALSE
)

label_data <- filter(label_data, custom_label != '') 
label_data$y_position <- nrow(label_data):1

# Add enhancements with stronger theme overrides
enhanced_plot <- base_plot +
#  ggplot2::labs(
#   title = paste0("Career Timeline, Sep 2007 - ",format(Sys.Date(), "%b %Y")),
#    subtitle = paste0(years_in_analytics," Years in Data & Analytics"),
#    caption = "Sectors: 7 Years in Finance, 4.5 Years in Media & Broadcast, 3 Years in Consulting | Full details available on LinkedIn"
#  ) +
  # More aggressive theme overrides
  ggplot2::theme(
    # Reduce row spacing
    panel.spacing.y = unit(-0.5, "lines"),
    # Remove ALL possible grid elements
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # Remove panel elements
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    # Remove axis elements that might create lines
    axis.ticks = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(color = "black"),
    # Keep plot elements
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(5, 20, 5, 20, "pt"),
    # Keep text elements
    plot.title = element_text(size = 20, hjust = -0.5),
    plot.subtitle = element_text(size = 14, hjust = -0.37),
    plot.caption = element_text(size = 10, hjust = 1)
  ) +
  ggplot2::scale_x_date(expand = expansion(mult = c(0.02, 0.15)))

final_plot <- enhanced_plot +
  ggplot2::geom_text(
    data = label_data,
    aes(x = label_x, y = y_position, label = custom_label),
    hjust = 0,     # Left-align text
    vjust = 0.5,   # Center vertically
    size = 6,      # Text size
    color = "black",
    inherit.aes = FALSE  # Don't inherit aesthetics from base plot
  )

# Add manual legend elements
legend_data <- data.frame(
  x = as.Date(c("2023-06-01", "2023-06-01")),
  y = c(1.5, 1),
  label = c("Work Experience", "Education"),
  color = c("#6ACCEA", "#00FFB8")
)

final_plot <- final_plot +
  # Add legend rectangles
  ggplot2::geom_rect(
    data = legend_data,
    aes(xmin = x, xmax = x + 200, ymin = y - 0.15, ymax = y + 0.15, fill = color),
    inherit.aes = FALSE
  ) +
  # Add legend text
  ggplot2::geom_text(
    data = legend_data,
    aes(x = x + 250, y = y, label = label),
    hjust = 0,
    vjust = 0.5,
    size = 6,
    inherit.aes = FALSE
  ) +
  # Add legend title
  ggplot2::annotate(
    "text",
    x = as.Date("2023-06-01"), 
    y = 2, 
    label = "Activity Type",
    hjust = 0,
    vjust = 0.5,
    size = 6
  ) +
  ggplot2::scale_fill_identity() # Use colors as-is

# Display the chart
print(final_plot)

# Optional: Save the chart with larger dimensions to accommodate labels
ggsave("cards/work_timeline.png", final_plot, width = 15, height = 5, dpi = 300)