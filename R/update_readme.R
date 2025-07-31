# Update README.md with dynamic career timeline values
# This script calculates current values and updates the README.md file

library(lubridate)

# Calculate dynamic values (reusing logic from work_timeline.R)
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
consulting_years <- quarters * 0.25

# Total years in data analytics (started September 2013)
data_start <- as.Date("2013-09-01") 
total_data_years <- as.numeric(difftime(Sys.Date(), data_start, units = "days")) / 365.25
years_in_analytics <- round(total_data_years)

# Current month and year
current_month_year <- format(Sys.Date(), "%b %Y")

# Read the current README.md
readme_path <- "README.md"
readme_content <- readLines(readme_path, warn = FALSE)

# Find and update the three dynamic values
for (i in seq_along(readme_content)) {
  line <- readme_content[i]
  
  # Update: "## Career Timeline, Sep 2007 - Jul 2025"
  if (grepl("^## Career Timeline, Sep 2007 -", line)) {
    readme_content[i] <- paste0("## Career Timeline, Sep 2007 - ", current_month_year)
  }
  
  # Update: "12 Years in Data & Analytics"  
  if (grepl("^[0-9]+ Years in Data & Analytics$", line)) {
    readme_content[i] <- paste0(years_in_analytics, " Years in Data & Analytics")
  }
  
  # Update: "Sectors: 7 Years in Finance, 4.5 Years in Media & Broadcast, 3 Years in Consulting"
  if (grepl("Sectors: .* Years in Consulting", line)) {
    readme_content[i] <- paste0("Sectors: 7 Years in Finance, 4.5 Years in Media & Broadcast, ", 
                                consulting_years, " Years in Consulting | Full details available on [LinkedIn](https://www.linkedin.com/in/will-sutton-14711627/)")
  }
}

# Write the updated content back to README.md
writeLines(readme_content, readme_path)

# Print summary of updates
cat("README.md updated successfully!\n")
cat("Current date:", current_month_year, "\n")
cat("Years in Data & Analytics:", years_in_analytics, "\n") 
cat("Years in Consulting:", consulting_years, "\n")