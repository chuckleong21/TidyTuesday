library(tidyverse)
library(highcharter)
tt <- tidytuesdayR::tt_load(2024, 27)

sizing <- function(file) {
        ext <- tools::file_ext(file)
        obj <- switch(ext, 
                      "xlsx" = readxl::read_excel(file), 
                      "xls" = readxl::read_excel(file), 
                      "csv" = read_csv(file, show_col_types = F), 
                      "rds" = read_rds(file), 
                      warning("unrecognized file extension"))
        size <- object.size(obj) / 1024^2
        size
}
relabel <- function(x) {
        if(x$level == 2) {
                max_dim <- slice_max(filter(datasets, id == x$id), dimension)
                vars <- pull(max_dim, variables)
                obs <- pull(max_dim, observations)
                list(name = paste0(
                        "Week: ", x$name, "<br>",
                        "Variables: ", ifelse(length(vars) != 1, vars[1], vars), "<br>",
                        "Observations: ", ifelse(length(obs) != 1, obs[1], obs)
                ),
                id = x$id,
                parent = x$parent, 
                value = x$value,
                level = x$level
                )
        } else x
}

# data object sizes
datasets_file <- tibble(
        file = list.files("tidytuesday-master/data", pattern = "xlsx|xls|csv|rds", recursive = T, full.names = T), 
        year = str_extract(file, "(\\d{4})\\-\\d{2}\\-\\d{2}", group = 1)
) %>% 
        group_by(year) %>% 
        mutate(week = row_number()) %>% 
        ungroup() %>% 
        mutate(year = as.numeric(year))


datasets <- tt$tt_datasets %>% 
        mutate(id = paste0(year, "_", week), 
               dimension = variables * observations) %>% 
        left_join(datasets_file, join_by(year, week)) %>% 
        mutate(obj_size = map_dbl(file, sizing))

# Dimensionality treemap
treemap_by_dimension <- datasets %>% 
        data_to_hierarchical(c(year, week), dimension) %>% 
        map(relabel) %>% 
        hchart(
                type = "treemap",
                # allowDrillToNode = TRUE, # Deprecated according to API Docs
                allowTraversingTree = TRUE,
                layoutAlgorithm = "squarified",
                levelIsConstant = FALSE,
                levels = list(
                        list(level = 1, dataLabels = list(enabled = TRUE, 
                                                          format = "{point.name}")),
                        list(level = 2, dataLabels = list(enabled = FALSE))
                )
        ) %>% 
        hc_tooltip(enabled = FALSE) %>% 
        hc_title(text = "Dimensions of TidyTuesday datasets")


treemap_by_objsize <- datasets %>% 
        data_to_hierarchical(c(year, week), obj_size) %>% 
        map(relabel) %>% 
        hchart(
                type = "treemap", 
                allowTraversingTree = TRUE,
                layoutAlgorithm = "squarified",
                levelIsConstant = FALSE,
                levels = list(
                        list(level = 1, dataLabels = list(enabled = TRUE, 
                                                          format = "{point.name}")),
                        list(level = 2, dataLabels = list(enabled = FALSE))
                )
        ) %>% 
        hc_tooltip(pointFormat = '{point.name}<br>{point.value:..2f} MB') %>%  # rounds to two decimals format
        hc_title(text = "Object sizes of TidyTuesday datasets")

# facets
hw_grid(list(treemap_by_dimension, treemap_by_objsize), ncol = 2)
