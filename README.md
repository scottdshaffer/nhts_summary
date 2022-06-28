# nhts_summary
Bar graph visualizing National Household Travel Survey statistics
---
```
#load packages
library(tidyverse)
library(janitor)
library(showtext)
library(ggtext)
showtext_auto(enable = TRUE)
showtext_opts(dpi = 96)

#get url for NHTS data
url <- "https://nhts.ornl.gov/assets/2016/download/csv.zip"

#download and unzip the data
temp <- tempfile()
download.file(url, temp)
unzip(temp, list = TRUE)

#create data frames for household, trip, and person tables
hh_df <- read_csv(unz(temp, "hhpub.csv"))
trip_df <- read_csv(unz(temp, "trippub.csv"))
person_df <- read_csv(unz(temp, "perpub.csv"))
unlink(temp)

#calculate traveled by household by mode from the trip table
trip_summ <- trip_df |> group_by(HOUSEID, TRPTRANS) |>
  summarize(distance = sum(WTTRDFIN * TRPMILES)) |>
  ungroup()

#join with the household table
join <- left_join(hh_df, trip_summ, by = "HOUSEID") |>
  mutate(distance = replace_na(distance, 0),
         TRPTRANS = replace_na(TRPTRANS, '-9'))

#recode population density, transportation mode, calculate averages
join_summ <- join |>
  filter(HBPPOPDN != -9 &
           !HHFAMINC %in% c('-7', '-8', '-9')) |>
  mutate(
    density = case_when(
      HBPPOPDN %in% c(50, 300, 750) ~ "<1,000",
      HBPPOPDN %in% c(1500, 3000) ~ "1,000 - 4,999",
      HBPPOPDN %in% c(7000, 17000) ~ "5,000 - 24,999",
      HBPPOPDN %in% c(30000) ~ "25,000+"
    ),
    density = factor(
      density,
      levels = c("<1,000",
                 "1,000 - 4,999",
                 "5,000 - 24,999",
                 "25,000+")
    ),
    mode = case_when(
      TRPTRANS %in% c('01', '02', '11', '12', '13', '14', '15', '16') ~ "Walk, bike, bus, or train",
      TRPTRANS %in% c('03', '04', '05', '06', '17', '18') ~ "Car or truck",
      TRUE ~ "Other"
    ),
    income = case_when(
      HHFAMINC %in% c('01', '02', '03', '04', '05') ~ "Households earning less than $50,000/year",
      HHFAMINC %in% c('06', '07', '08', '09') ~ "$50,000 - $149,999",
      HHFAMINC %in% c('10', '11') ~ "$150,000 or more"
    ),
    income = factor(
      income,
      levels = c(
        "Households earning less than $50,000/year",
        "$50,000 - $149,999",
        "$150,000 or more"
      )
    )
  ) %>%
  group_by(density, income, mode) %>%
  summarize(
    households = sum(WTHHFIN),
    tot_distance = sum(distance),
    avg_distance = tot_distance / households
  ) %>%
  ungroup() %>%
  filter(mode != "Other")

#set up the fonts
font_add_google("Fira Sans Condensed",
                "fira cnd light",
                regular.wt = 300,
                bold.wt = 500)
font_add_google("Fira Sans Condensed",
                "fira cnd heavy",
                regular.wt = 400,
                bold.wt = 700)
font_add_google("Fira Sans")

#visualize it
join_summ %>%
  ggplot(aes(x = density, y = avg_distance, fill = mode)) +
  geom_col(position = "dodge") +
  facet_wrap(~ income, nrow = 3) +
  labs(title = "People living in denser neighborhoods drive less",
       subtitle = "Annual miles traveled by <span style='color:#941C2F;'>**car or truck**</span> or by <span style='color:#20A4F3;'>**walking, biking, or transit**</span>",
       caption = "Scott Shaffer | June 2022 | 2017 National Household Travel Survey") +
  scale_fill_manual(name = "Transportation mode",
                    values = c("#941C2F",
                               "#20A4F3")) +
  scale_x_discrete(name = "Neighborhood population density (pop./sq.mi.)") +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid = element_line(color = "white", size = .4),
    text = element_text(family = "fira cnd light"),
    plot.title = element_text(family = "Fira Sans",
                              face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "fira cnd light"),
    plot.caption = element_text(family = "fira cnd light",
                                color = "gray30"),
    axis.text = element_text(family = "fira cnd light", size = 8),
    axis.title.y = element_blank(),
    axis.title.x = element_text(family = "fira cnd light"),
    strip.text = element_text(family = "fira cnd heavy"),
    strip.background = element_rect(fill = "gray95", color = "gray95"),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "gray95", color = "gray95")
  )
showtext_opts(dpi = 300)
ggsave(
  "nhts_bargraph.png",
  height = 4.8,
  width = 4.8,
  units = "in",
  bg = "white",
  dpi = 300
)
```
---
