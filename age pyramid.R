library(tidycensus)
library(plotly)
library(cowplot)
library(patchwork)
##----wisconsin-data---------

wisconsin <- get_estimates(
  geography = "state",
  state = "55",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
)
wisconsin

##---prepare-data-------
wisconsin_filtered <- filter(wisconsin, str_detect(AGEGROUP, "^Age"),
                             SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

wisconsin_filtered

##---wisconsin-age-pyramid------
wisconsin_pyramid <- ggplot(wisconsin_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) +
  geom_col(width = 0.95, alpha = 0.75) +
  theme_minimal(base_family = "verdana") +
  scale_x_continuous(labels = function(y) paste0(abs(y / 1000), "k")) +
  scale_y_discrete(labels = function(x) gsub("Age | years", "",x)) +
  labs(x ="",
       y = "2010 Census Bureau population estimates",
       title = "Population structure in Wisconsin",
       fill = "",
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")
wisconsin_pyramid

##--interactive--plot
ggplotly(wisconsin_pyramid)

##--dane-county-data
dane <- get_estimates(
  geography = "county",
  state = "55",
  county = "025",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE
)
dane

##---prepare-data-------
dane_filtered <- filter(dane, str_detect(AGEGROUP, "^Age"),
                             SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

dana_filtered

##--dane-county-age-pyramid
dane_pyramid <- ggplot(dane_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) +
  geom_col(width = 0.95, alpha = 0.75) +
  theme_minimal(base_family = "verdana") +
  scale_x_continuous(labels = function(y) paste0(abs(y / 1000), "k")) +
  scale_y_discrete(labels = function(x) gsub("Age | years", "",x)) +
  labs(x ="",
       y = "2010 Census Bureau population estimates",
       title = "Population structure in Dane county (Madison)",
       fill = "",
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")
dane_pyramid

## interactive age pyramid
ggplotly(dane_pyramid)


