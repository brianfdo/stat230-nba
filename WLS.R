player_2024 = read.csv('final_2024_player.csv')
player_2024

player_2024 <- player_2024[, !(names(player_2024) %in% c("Salary", "Guaranteed", "Next_Year_Guaranteed", "Awards"))]
player_2024 <- na.omit(player_2024)

player_2024$log_salary <- log(player_2024$Next_Year_Salary)
hist(player_2024$log_salary)
hist(player_2024$Next_Year_Salary)

# weight approach using leverage
ols_model <- lm(log_salary ~ ., data = player_2024)
lev <- hatvalues(ols_model)
player_2024$wts <- 1 / (lev + 1e-6)
player_2024$wts <- 1 / log(player_2024$Next_Year_Salary)
# weight approach using Experience
player_2024$wts <- player_2024$NumOfAwards/ max(player_2024$NumOfAwards, na.rm = TRUE)

wls_model <- lm(log_salary ~ . -Next_Year_Salary -Rk_x -Player -Team -Season, 
                data = player_2024, weights = wts)
summary(wls_model)



ols_model <- lm(log_salary ~ . -Next_Year_Salary -Player -Team -Season, data = player_2024)
plot(fitted(ols_model), residuals(ols_model))
summary(ols_model)

qqnorm(residuals(ols_model))
qqline(residuals(ols_model))

# Histogram of residuals
hist(residuals(ols_model), main = "Histogram of Residuals", xlab = "Residuals")

library(car)
ncvTest(ols_model)

# Identify top outliers
outliers <- ols_model$residuals[abs(ols_model$residuals) > 3 * sd(ols_model$residuals)]
outliers_players <- rownames(outliers)

# Print details of outlier players
player_2024[outliers_players, ]



# Box score model
box_score_vars <- c("Age", "Pos", "G", "MP_x", "PTS", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV.", "FG.", "X3P.", "FT.", "X2P.")

# Advanced metrics model
advanced_vars <- c("Age", "PER", "TS.", "eFG.", "VORP", "WS", "BPM", "OBPM", "DBPM", "USG.", "FTr", "NumOfAwards",
                   "All.Star", "AwardWinner", "FirstTeam", "SecondTeam", "ThridTeam", "DefTeam1", "DefTeam2", "X2023.24_contract_year")

# Build design matrices
X_box <- model.matrix(player_2024$log_salary ~ ., data = player_2024[box_score_vars])
X_adv <- model.matrix(player_2024$log_salary ~ ., data = player_2024[advanced_vars])

# Fit models
wls_box <- lm(player_2024$log_salary ~ ., data = as.data.frame(X_box), weights = player_2024$wts)
wls_adv <- lm(player_2024$log_salary ~ ., data = as.data.frame(X_adv), weights = player_2024$wts)

# Check summaries
summary(wls_box)
summary(wls_adv)