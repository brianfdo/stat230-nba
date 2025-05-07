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

# weight approach using Experience
player_2024$wts <- player_2024$NumOfAwards/ max(player_2024$NumOfAwards, na.rm = TRUE)

wls_model <- lm(log_salary ~ . -Next_Year_Salary -Player -Team -Pos -Season, 
                data = player_2024, weights = wts)
summary(wls_model)
