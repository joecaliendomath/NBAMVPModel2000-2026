
write.csv(mvp_Sheet2, "mvp.csv", row.names=FALSE)
names(mvp_Copy_of_Sheet2)[(ncol(mvp_Copy_of_Sheet2)-1):ncol(mvp_Copy_of_Sheet2)]<- c("PositiveMedia","NegativeMedia")

mvp_Copy_of_Sheet2[31,36]=2

mvp_Copy_of_Sheet2$Media = mvp_Copy_of_Sheet2$PositiveMedia-mvp_Copy_of_Sheet2$NegativeMedia
mediamodel <- lm(100*Rank ~ pra +WinPct_Rounded+Media,
              data = mvp_Copy_of_Sheet2)
summary(mediamodel)

results <- data.frame(
  Player = mvp_Copy_of_Sheet2$Player,
  Actual_Rank = mvp_Copy_of_Sheet2$Rank,
  Fitted = fitted(mediamodel) / 100,  # divide by 100 since you multiplied Rank by 100
  Residual = residuals(mediamodel) / 100
)

print(results)
library(dplyr)
#______
mvp_Copy_of_Sheet2$Fitted <- fitted(mediamodel)

mvp_Copy_of_Sheet2$SeasonID <- rep(1:(nrow(mvp_Sheet2)/3), each = 3)

predictions <- mvp_Copy_of_Sheet2 %>%
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
 mvp_Sheet2$WinPct_Rounded <- round(mvp_Sheet2$WinPct, 1)
