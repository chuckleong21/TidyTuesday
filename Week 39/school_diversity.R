library(tidyverse)
library(ggalluvial)

# df_school_diversity <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")
# speed up data loading through local file
df_raw <- read_csv(here::here("Week 39", "school_diversity.csv"))

df_school_diversity <- 
        df_raw %>% 
        janitor::clean_names() %>% 
        select(leaid, st, school_year, diverse) %>% 
        filter(st != "DC") %>% 
        # mutate(school_year = ifelse(school_year == "1994-1995", "before", "after")) %>% 
        # pivot_wider(leaid, school_year, values_from = diverse) %>% 
        count(leaid, st, school_year, diverse, name = "freq")

# should implicit missing values be filled?
# df_school_nebraska <- 
#         df_school_diversity %>% 
#         filter(st == "NE") %>% 
#         complete(leaid, school_year, fill = list(st = "NE", freq = 1))

# df_school_diversity %<>% 
#         filter(st != "NE") %>% 
#         bind_rows(df_school_nebraska)


sub <- str_wrap("Below is the visualization for changes in racial diversity 
                in schools across states between school years during 1994 and 2016. 
                Although more schools become more racial-diverse, white students remain
                the majority race in schools.", 80)

df_school_diversity %>% 
        mutate(st = as.factor(st)) %>% 
        ggplot(aes(x = school_year, 
                   stratum = diverse, 
                   alluvium = leaid, 
                   y = freq, 
                   fill = diverse)) + 
        geom_flow(alpha = .7) + 
        geom_stratum() + 
        scale_x_discrete(expand = c(0, 0)) +
        scale_fill_manual(name = "", 
                          limits = c("Extremely undiverse", "Undiverse", "Diverse"), 
                          values = c("firebrick", "darkorange", "forestgreen")) +
        facet_wrap(~st, scales = "free_y", nrow = 10, ) +
        theme_minimal(base_family = "Nimrod Cyr-Bold") + 
        labs(x = "", y = "", 
             title = str_to_title("baby steps: more racial-diverse schools"), 
             subtitle = sub, 
             caption = "Data: NCES | Graphic: @chucc900") + 
        theme(plot.margin = margin(20, 20, 20, 10), 
              plot.title = element_text(size = 28), 
              plot.subtitle = element_text(size = 20),
              plot.caption = element_text(size = 18),
              panel.grid = element_blank(), 
              legend.position = c(.9, 1.15),
              legend.key.size = unit(.8, "cm"), 
              legend.text = element_text(size = 15),
              axis.text.x = element_text(size = 11),
              axis.text.y = element_blank(),
              strip.text = element_text(size = 14))

ggsave(here::here("Week 39", "diversity.png"), height = 29.9, width = 42.9, units = "cm")


