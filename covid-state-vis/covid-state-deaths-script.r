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
  scale_alpha(range = c(0.4, 0.8))+
  geom_text(aes(label=ifelse(sum_cases>10000000,as.character(state),'')),hjust=0.5,vjust=1)+
  theme(legend.position = "none") +
  scale_y_continuous(name="Cases", labels = scales::comma) +
  scale_x_continuous(name="Deaths", labels = scales::comma)
            
death_ratio_state_plot



# Show deaths per weekday
# Show coloured map
