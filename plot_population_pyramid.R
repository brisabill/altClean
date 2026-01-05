# Load necessary packages
library(tidyr)
library(dplyr)
library(ggplot2)

# Function to plot population pyramid for any folder and country code
plot_population_pyramid <- function(folder, ccode,
                                    results_dir = "C:/Users/mirandaa/Desktop/estican/estican/result",
                                    global_pop_file = "//inti/CIN/DataShare/Globocan2022/source/population.csv",
                                    year_model = 2022, year_pop = 2024) {
  
  # 1. Read in the modelled population (folder/CCp.csv)
  pop_path <- file.path(results_dir, folder, paste0(ccode, "p.csv"))
  if (!file.exists(pop_path)) stop("Model file not found: ", pop_path)
  population <- read.csv(pop_path, stringsAsFactors = FALSE)
  
  # 2. Read & subset the global population
  df <- read.csv(global_pop_file, stringsAsFactors = FALSE)
  df_cc <- df %>% filter(country_code == ccode)
  
  # 3. Melt to long format
  df_cc_long <- df_cc %>%
    pivot_longer(
      cols = starts_with("age"),
      names_to   = "age",
      names_prefix = "age",
      values_to  = "py"
    ) %>%
    mutate(age = as.integer(age)) %>%
    filter(sex != 0)   # drop total if present
  
  # 4. Build age_group factor
  age_labels <- c(
    "0-4","5-9","10-14","15-19","20-24","25-29",
    "30-34","35-39","40-44","45-49","50-54","55-59",
    "60-64","65-69","70-74","75-79","80-84","85+"
  )
  df_cc_long <- df_cc_long %>%
    mutate(age_group = factor(age, levels = 1:18, labels = age_labels))
  population <- population %>%
    mutate(age_group = factor(age, levels = 1:18, labels = age_labels))
  
  # 5. Tag years & combine datasets
  df_cc_long$year    <- year_model
  population$year    <- year_pop
  combined_data <- bind_rows(
    df_cc_long   %>% mutate(source = as.character(year_model)),
    population   %>% mutate(source = as.character(year_pop))
  )
  
  # 6. Prepare for pyramid: flip female values negative
  pyramid_data <- combined_data %>%
    mutate(
      sex_label = ifelse(sex == 1, "Male", "Female"),
      pop_value = ifelse(sex == 2, -py, py)
    )
  
  # 7. Plot
  # Correct color mapping using setNames()
  year_colors <- setNames(
    c("#1f78b4", "#e31a1c"),
    c(as.character(year_pop), as.character(year_model))
  )
  
  p <- ggplot(pyramid_data, aes(x = age_group, y = pop_value, fill = source)) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.7, width = 0.7) +
    coord_flip() +
    scale_y_continuous(
      labels = abs,
      expand = expansion(mult = c(0.02, 0.05)),
      breaks = pretty(c(-max(abs(pyramid_data$pop_value)), max(abs(pyramid_data$pop_value))))
    ) +
    scale_fill_manual(values = year_colors) +
    labs(
      title = paste0("Population Pyramid – Country ", ccode),
      x = "Age Group",
      y = "Person-Years",
      fill = "Year"
    ) +
    facet_grid(~sex_label, switch = "x", scales = "free_x", space = "free_x") +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major      = element_blank(),
      panel.grid.minor      = element_blank(),
      axis.text.x           = element_text(size = 11),
      axis.text.y           = element_text(size = 11),
      axis.title            = element_text(face = "bold", size = 13),
      strip.placement       = "outside",
      strip.text            = element_text(face = "bold", size = 13),
      plot.title            = element_text(face = "bold", size = 16, hjust = 0.5),
      legend.position       = "bottom",
      legend.text           = element_text(size = 12),
      panel.spacing         = unit(0, "lines")
    )
  
  return(p)
}



