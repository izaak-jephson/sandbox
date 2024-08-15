library(sf)
library(dplyr)
library(here)
library(ggplot2)
library(sssstats)
library(stringr)
library(readxl)
library(plotly)

# Read in tables from April 2024 ADP publication
load(here("data","adp_2024_04.R"))
data_table <- data$`Table 8` %>% make_all_string_numeric() %>% 
  mutate(`Local authority` = str_replace_all(`Local authority`,"&", "and")) %>% 
  mutate(`Local authority` = case_when(
             `Local authority` == "Edinburgh, City of" ~ "City of Edinburgh", # to match LA boundary data file
             TRUE ~ `Local authority`))

# ONS Population estimates
pop <- read_xlsx(here("data","myebtablesuk20112022.xlsx"), sheet = "MYEB1", skip = 1) %>% 
  group_by(ladcode23, laname23) %>% 
  summarise(population_2022 = sum(population_2022))

# Local Authority Districts (May 2024) Boundaries UK BSC
la <- st_read(here("data","Local_Authority_Districts_May_2024_Boundaries__UK_BSC_4981612463203497213.geojson"))

# Join the three datasets
la_data <- merge(la, data_table, by.x = "LAD24NM", by.y = "Local authority") %>% 
  merge(pop, by.x = "LAD24NM", by.y ="laname23") %>% 
  mutate(`Applications per 10,000 people` = round(`Total part 1 applications registered` /  population_2022  * 10000)) %>%
 
  # Odd behaviour of ggplotly means hoveron = "fill" does not work correctly if two areas have same value (and therefore fill colour). 
  # Aberdeen City and Argyll & Bute have same value. Therefore perturb one of these by 1.
  mutate(`Applications per 10,000 people` = 
          case_when(LAD24NM == "Aberdeen City" ~ `Applications per 10,000 people` + 1,
        TRUE ~ `Applications per 10,000 people`)) %>% 
  
  # Create hover labels
  mutate(Label = paste0(LAD24NM, ": ",`Applications per 10,000 people`))

# Plot with ggplot2's sf geom
applied_chart <- ggplot() +
  geom_sf(data = la_data, 
          mapping = aes(fill = `Applications per 10,000 people`, text = Label),
          color = "white") + theme(panel.grid.major = element_line(colour = "transparent"))

# Turn into plotly chart
applied_chart_int <- ggplotly(applied_chart, tooltip = "text")

applied_chart_int <- applied_chart_int %>% 
  style(hoveron = "fill",
        line.width = 0.5,
        traces = 2:length(applied_chart_int$x$data))
        
ggsave(here("output","ADP_applications_map.svg"), plot = applied_chart, height = 10, width = 10)

htmlwidgets::saveWidget(widget = applied_chart_int, file = here("output","ADP_applications_map.html"))
