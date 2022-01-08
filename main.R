library(tidyverse)
library(tidycensus)
library(sf)
library(RColorBrewer)
library(showtext)

font_add_google("Cinzel", "cz")
showtext_auto()

`%+%` <- function(x, y) paste(x, y)

census_vars_2020 <- load_variables(year = 2020, dataset = "pl")

d <- get_decennial(geography = "county",
                   variables = "P1_001N",
                   year = 2020,
                   geometry = TRUE)

d_state <- d %>%
  filter(!str_detect(NAME, "District of Columbia")) %>%
  mutate(state = str_extract(NAME, "(?<=, ).*$"),
         name = str_remove_all(NAME, " [:alpha:]*, .*$"),
         name = str_remove_all(name, " City .*$| Census$"),
         type = str_extract(NAME, "[:alpha:]*(?=, )"))

x <- d_state %>%
  as_tibble() %>%
  filter(!str_detect(NAME, "County|Parish")) %>%
  select(NAME, state, name, type)


top_10_n <- d_state %>%
  as_tibble() %>%
  group_by(name) %>%
  summarise(n = n(),
            state_n = length(unique(state))) %>%
  arrange(desc(n)) %>%
  head(10)

t10 <- top_10_n %>%
  .[["name"]]

x <- d_state %>%
  filter(name %in% top_10_n$name)

dd <- d_state %>%
  filter(!state %in% c("Hawaii", "Alaska", "Puerto Rico"))


state_counts <- d_state %>%
  as_tibble() %>%
  filter(name %in% top_10_n$name) %>%
  select(state, name) %>%
  group_by(state) %>%
  summarise(n_unique = length(unique(name))) %>%
  arrange(desc(n_unique))

states <- tigris::states()

l <- rnaturalearth::ne_download(type = "lakes", category = "physical", scale = "large")  %>%
  st_as_sf(., crs = st_crs(states))

lakes <- c("Lake Erie",
           "Lake Michigan",
           "Lake Superior",
           "Lake Huron",
           "Lake Ontario")
gl <- l %>%
  filter(name %in% lakes) %>%
  st_transform(crs = st_crs(states))

not_states <- c("Commonwealth of the Northern Mariana Islands",
                "Alaska",
                "Puerto Rico",
                "Hawaii",
                "American Samoa",
                "United States Virgin Islands",
                "Guam")

states_skinny <- states %>%
  filter(!NAME %in% not_states) 


for (i in 1:nrow(gl)) {
  states_skinny <- st_difference(states_skinny, gl[i,])
}

s <- states_skinny %>%
  select(state = NAME,
         geometry) %>%
  left_join(., state_counts) %>%
  mutate(group = case_when(n_unique == 10 ~ "All 10",
                           n_unique > 4 ~ "5-9",
                           n_unique > 0 ~ "1-4",
                           TRUE ~ "None"),
         group = factor(group, levels = c("None",
                                          "1-4",
                                          "5-9",
                                          "All 10")))

tmp <- s %>%
  as_tibble() %>%
  select(state, n_unique)

top_10_n %>%
  ggplot(aes(reorder(name, state_n), state_n)) +
  geom_col(fill = "#CA0020", width = .75) +
  geom_text(aes(label = name, y = 1), hjust = 0, color = "white", size = 5) +
  geom_text(aes(label = state_n, y = state_n + 1)) +
  coord_flip() +
  labs(title = "Top 10 county names in the United States",
       y = "State Count",
       caption = "The 'county' designation here is inclusive of other entities used by states (e.g. Parish).\n" %+%
         "Graph by Spencer Schien (@MrPecners) | Data from US Census Bureau") +
  theme_minimal() +
  theme(axis.title.x = element_text(family = "cz", size = 14),
        plot.title = element_text(size = 16),
        text = element_text(family = "cz"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave("top10_county_names.png", device = "png", bg = "white")


p <- s %>%
  ggplot() +
  geom_sf(aes(fill = group), 
          color = "white", size = .1) +
  theme_void() +
  scale_fill_brewer(palette = "RdGy", direction = -1) +
  labs(fill = "Count by State",
       title = "States with the 10 most common county names",
       subtitle = "Five states have all 10",
       caption = "Graph by Spencer Schien (@MrPecners) | Data from US Census Bureau") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5, family = "cz", size = 16),
        text = element_text(family = "cz"),
        plot.caption = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5, size = 14)) +
  coord_sf(crs = st_crs(3347))

#ggsave("plot.pdf", plot = p, device = "pdf")
ggsave("top10_map.png", plot = p, device = "png", bg = "white")
