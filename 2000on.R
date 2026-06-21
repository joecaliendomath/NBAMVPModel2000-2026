mvp_2000_2009$Predicted_Rank <- predict(
  mvp_model,
  newdata = mvp_2000_2009
)
library(dplyr)

predictions <- mvp_2000_2009 %>%
  group_by(SeasonID) %>%
  slice_min(Predicted_Rank, n = 1, with_ties = FALSE) %>%
  ungroup()
predictions %>%
  select(Player, Predicted_Rank)
#________Above is how 2000's players are judged by 2010s standards
mvp_2000on[55,31]=2
mvp_2000on[61,31]=1
mvp_2000on[75,32]=1
mvp_2000on$Media=mvp_2000on$PositiveMedia-mvp_2000on$NegativeMedia
mvp_2000_2009$pra=mvp_2000_2009$PTS+mvp_2000_2009$TRB+mvp_2000_2009$AST

model2000s <- lm(100*Rank ~WS48	+	Media,data = mvp_2000on)

summary(model2000s)
results <- data.frame(
  Player = mvp_2000on$Player,
  Actual_Rank = mvp_2000on$Rank,
  Fitted = fitted(model2000s) / 100,  # divide by 100 since you multiplied Rank by 100
  Residual = residuals(model2000s) / 100
)

print(results)
library(dplyr)
#______
mvp_2000on$Fitted <- fitted(model2000s)


predictions <- mvp_2000on %>%
  group_by(SeasonID) %>%
  slice_min(Fitted, n = 1, with_ties = FALSE) %>%
  ungroup()

correct <- sum(predictions$Rank == 1)
total <- nrow(predictions)

cat("Model correctly predicted", correct,
    "of", total, "MVPs (",
    round(100 * correct / total, 2),
    "%)\n")



#______
predictions %>%
  mutate(Correct = Rank == 1) %>%
  select(SeasonID, Player, Rank, Fitted, Correct)

