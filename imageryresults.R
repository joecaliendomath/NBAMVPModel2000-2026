library(ggplot2)
library(dplyr)
library(forcats)

results <- results %>%
  mutate(Race = 2027-ceiling(row_number() / 3))

races <- unique(results$Race)

for (r in races) {
  race_data <- results %>%
    filter(Race == r) %>%
    mutate(
      Player = fct_reorder(Player, desc(Fitted)),
      Rank = rank(Fitted, ties.method = "first"),
      Bar_Color = case_when(
        Rank == 1 ~ "#185FA5",
        Rank == 2 ~ "#7F77DD",
        Rank == 3 ~ "#A32D2D",
        TRUE ~ "#888780"
      )
    )
  
  p <- ggplot(race_data, aes(x = Player, y = Fitted, fill = Bar_Color)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = round(Fitted, 2)), hjust = -0.15, size = 4, color = "gray20") +
    coord_flip() +
    scale_fill_identity() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(
      title = paste0("MVP Race ", r),
      subtitle = "Regression Model Prediction Graph",
      x = NULL, y = "Fitted Value from the Model"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "gray40", size = 10)
    )
  
  ggsave(paste0("mvp_race_", r, ".png"), p, width = 7, height = 4, dpi = 150)
}

# --- 2. Leaderboard dataframe ranked by fitted value ---
leaderboard <- results %>%
  arrange(Fitted) %>%
  mutate(Fitted_Rank = row_number()) %>%
  select(Fitted_Rank, Player, Race, Actual_Rank, Fitted, Residual)

print(leaderboard)
