library(dplyr)

# Net media sentiment
mvp_2000on$Media <- mvp_2000on$PositiveMedia - mvp_2000on$NegativeMedia

# Predict rank from win shares/48 and media sentiment
model2000s <- lm(100 * Rank ~ WS48 + Media, data = mvp_2000on)
summary(model2000s)

# Actual vs. predicted rank per player
results <- data.frame(Player = mvp_2000on$Player, Actual_Rank = mvp_2000on$Rank, Fitted = fitted(model2000s) / 100, Residual = residuals(model2000s) / 100)
print(results)

# Model's pick per season = lowest fitted value
mvp_2000on$Fitted <- fitted(model2000s)
predictions <- mvp_2000on %>% group_by(SeasonID) %>% slice_min(Fitted, n = 1, with_ties = FALSE) %>% ungroup()

# How often that pick was the real MVP
correct <- sum(predictions$Rank == 1)
total <- nrow(predictions)
cat("Model correctly predicted", correct, "of", total, "MVPs (", round(100 * correct / total, 2), "%)\n")
