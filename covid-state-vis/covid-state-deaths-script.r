library(readr)
library(deplyr)
library(purrr)
library(ggplot2)

data <- read.csv("us-states.csv")

# Show case:death ratio by state
death_ratio_state_data <- data%>%
  group_by(state)%>%
  summarize(
    sum_cases=sum(cases),
    sum_deaths=sum(deaths)
  )

death_ratio_state_plot <- ggplot(data=death_ratio_state_data,
  aes(x=sum_cases, y=sum_deaths, color=state)) +
  geom_point() +
  # Only show label if over 1m cases; position label
  geom_text(aes(label=ifelse(sum_cases>10000000,as.character(state),'')),hjust=0.5,vjust=1) +
  theme(legend.position = "none") +
  # Set axes to show standard numbers, not formulaic numbers; Set axes limits
  scale_y_continuous(name="Cases", labels = scales::comma, expand = c(0, 0), limits = c(0, 3000000)) +
  scale_x_continuous(name="Deaths", labels = scales::comma, expand = c(0, 0), limits = c(0, 40000000))
            
death_ratio_state_plot

# Isolate cluster
# Isolate timespan

# Show deaths per weekday
deaths_per_day_data <- data%>%
  mutate(
    # Convert YYYY-MM-DD to days of the week
    day_of_the_week = format(as.Date(data[["date"]]), "%A"),
    day_of_the_week = factor(day_of_the_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  )%>%
  group_by(day_of_the_week)%>%
  summarize(
    sum_cases=sum(cases),
    sum_deaths=sum(deaths)
  )

deaths_per_day_bar <- ggplot(data=deaths_per_day_data,
  aes(x=day_of_the_week, y=sum_deaths)) +
  geom_bar(stat="identity") +
  scale_y_continuous(name="Cases", labels = scales::comma, expand = c(0, 0), limits = c(0, NA))

deaths_per_day_bar

# Show coloured map
