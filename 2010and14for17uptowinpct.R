library(readr)

# Skip the blank first row
mvp_Sheet2 <- mvp_Sheet2_2_

# Rename columns
colnames(mvp_Sheet2) <- c("Rank", "Player", "Age", "Tm", "First", "Pts_Won", "Pts_Max", 
                          "Share", "G", "MP", "PTS", "TRB", "AST", "STL", "BLK", 
                          "FG_pct", "X3P_pct", "FT_pct", "WS", "WS48", 
                          "MVP_last3", "MVP_career", "WinPct", "PrevWinPct","NonWinsLast3","ChampionshipsLast3Yrs")

# Convert numeric columns (they may have read in as character)
numeric_cols <- c("Rank", "Age", "First", "Pts_Won", "Pts_Max", "Share", "G", "MP", 
                  "PTS", "TRB", "AST", "STL", "BLK", "FG_pct", "X3P_pct", 
                  "FT_pct", "WS", "WS48", "MVP_last3", "MVP_career","WinPct", "PrevWinPct","NonWinsLast3","ChampionshipsLast3Yrs")

mvp_Sheet2[numeric_cols] <- lapply(mvp_Sheet2[numeric_cols], as.numeric)
mvp_Sheet2 <- mvp_Sheet2[-1,]


mvp_Sheet2$pra = mvp_Sheet2$PTS+mvp_Sheet2$TRB+mvp_Sheet2$AST

model <- lm(Rank ~ WinPct + pra, data = mvp_Sheet2)

summary(model)
install.packages("car")  # uncomment if needed
library(car)
vif(model)






#__________________

# See fitted values alongside actual rank and player name
results <- data.frame(
  Player = mvp_Sheet2$Player,
  Actual_Rank = mvp_Sheet2$Rank,
  Fitted = fitted(model) / 100,  # divide by 100 since you multiplied Rank by 100
  Residual = residuals(model) / 100
)

print(results)
library(dplyr)
#______
mvp_Sheet2$Fitted <- fitted(model)

mvp_Sheet2$SeasonID <- rep(1:(nrow(mvp_Sheet2)/3), each = 3)

predictions <- mvp_Sheet2 %>%
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
