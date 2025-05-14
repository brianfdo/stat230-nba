library(ggplot2)

#helper function for stratified train test split
# we want a sufficient number of "stars" in both train and test, and we define a star as players with any accolades
# (MVP, all star, all nba defensive team ,etc)

train_test_split_stratified=function(data, y){
  x_reg=data[which(data$accolade==0),]
  y_reg=y[which(data$accolade==0)]
    
  x_star=data[which(data$accolade==1),]
  y_star=y[which(data$accolade==1)]
  
  set.seed(0)
  reg_train_index=sample(seq_len(nrow(x_reg)), size=0.8*nrow(x_reg))
  
  train_data_reg=x_reg[reg_train_index, ]
  test_data_reg=x_reg[-reg_train_index, ]
  ytr_reg=y_reg[reg_train_index]
  yts_reg=y_reg[-reg_train_index]
  
  star_train_index=sample(seq_len(nrow(x_star)), size=0.8*nrow(x_star))
  train_data_star=x_star[star_train_index, ]
  test_data_star=x_star[-star_train_index, ]
  ytr_star=y_star[star_train_index]
  yts_star=y_star[-star_train_index]
  
  xtr=rbind(train_data_reg, train_data_star)
  xts=rbind(test_data_reg, test_data_star)
  ytr=c(ytr_reg, ytr_star)
  yts=c(yts_reg, yts_star)
  return(list(xtr, xts, ytr, yts))
}

# helper function for calculating test r squared after fitting any of the ols, wls, or lasso models on training set
rsq_test=function(pred, yts){
  ss_res =sum((pred - yts)^2)
  ss_tot <- sum((yts - mean(yts))^2)
  return(1-ss_res / ss_tot)
}


player_2024 = read.csv('final_2024_player.csv')
player_2024

player_2024 <- player_2024[, !(names(player_2024) %in% c("Salary", "Guaranteed", "Next_Year_Guaranteed", "Awards"))]
player_2024 <- na.omit(player_2024)

player_2024$accolade=as.numeric(player_2024$All.Star+player_2024$AwardWinner+player_2024$FirstTeam+player_2024$SecondTeam+player_2024$ThridTeam+player_2024$DefTeam1+player_2024$DefTeam2+player_2024$NumOfAwards>0)


player_2024$log_salary <- log(player_2024$Next_Year_Salary)
hist(player_2024$log_salary)
hist(player_2024$Next_Year_Salary)






# weight approach using Awards
player_2024$wts <- player_2024$NumOfAwards/ max(player_2024$NumOfAwards, na.rm = TRUE)

# Box score model
box_score_vars <- c("accolade", "wts", "Age", "Pos", "G", "MP_x", "PTS", "TRB", "AST", "STL", "BLK", "TOV.", "FG.", "FT.")

# Advanced metrics model
advanced_vars <- c("accolade", "wts", "Age", "eFG.", "WS", "BPM", "DBPM", "USG.", "FTr", "NumOfAwards", "X2023.24_contract_year")

# Build feature matrices







X_box <- model.matrix(player_2024$log_salary ~ ., data = player_2024[box_score_vars])
X_adv <- model.matrix(player_2024$log_salary ~ ., data = player_2024[advanced_vars])

xtr_box=train_test_split_stratified(data.frame(X_box), player_2024$log_salary)[[1]]
xts_box=train_test_split_stratified(data.frame(X_box), player_2024$log_salary)[[2]]
ytr_box=train_test_split_stratified(data.frame(X_box), player_2024$log_salary)[[3]]
yts_box=train_test_split_stratified(data.frame(X_box), player_2024$log_salary)[[4]]

xtr_adv=train_test_split_stratified(data.frame(X_adv), player_2024$log_salary)[[1]]
xts_adv=train_test_split_stratified(data.frame(X_adv), player_2024$log_salary)[[2]]
ytr_adv=train_test_split_stratified(data.frame(X_adv), player_2024$log_salary)[[3]]
yts_adv=train_test_split_stratified(data.frame(X_adv), player_2024$log_salary)[[4]]


# Fit models

#OLS first

ols_box=lm(ytr_box~., data=cbind(ytr_box = ytr_box, xtr_box[, -c(1, 2, 3)]))
pred=predict(ols_box, xts_box)
print(summary(abs(exp(pred)-exp(yts_box))))

rsq_test(pred, yts_box)

ols_adv=lm(ytr_adv~., data=cbind(ytr_adv = ytr_adv, xtr_adv[, -c(1, 2, 3)]))
pred=predict(ols_adv, xts_adv)
print(summary(abs(exp(pred)-exp(yts_adv))))

rsq_test(pred, yts_adv)

plot(fitted(ols_box), resid(ols_box), xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values, OLS")
abline(h = 0, col = "black")

# WLS

wls_box <- lm(ytr_box ~ ., data = as.data.frame(xtr_box[,c(-1, -2, -3)]), weights = xtr_box[,3])
wls_adv <- lm(ytr_adv ~ ., data = as.data.frame(xtr_adv[,c(-1, -2, -3)]), weights = xtr_adv[,3])

# Check summaries
summary(wls_box)
summary(wls_adv)
vif(wls_box)


box_pred=predict(wls_box, as.data.frame(xts_box[,c(-1, -2, -3)]))
print(summary((abs(exp(box_pred)-exp(yts_box)))))

rsq_test(box_pred, yts_box)
adv_pred=predict(wls_adv, as.data.frame(xts_adv[,c(-1, -2, -3)]))
print(summary(abs(exp(adv_pred)-exp(yts_adv))))
rsq_test(adv_pred, yts_adv)

# LASSO
x_mat=model.matrix(~ . - 1, data = xtr_box[, -c(1, 2, 3)])
x_mat_test=model.matrix(~ . -1, data=xts_box[, -c(1, 2, 3)])
model=cv.glmnet(x_mat, ytr_box, alpha=1)
pred=predict(model, newx=x_mat_test, s="lambda.min")
summary(abs(exp(pred)-exp(yts_box)))

rsq_test(pred, yts_box)

x_mat=model.matrix(~ . - 1, data = xtr_adv[, -c(1, 2, 3)])
x_mat_test=model.matrix(~ . -1, data=xts_adv[, -c(1, 2, 3)])
model=cv.glmnet(x_mat, ytr_adv, alpha=1)
pred=predict(model, newx=x_mat_test, s="lambda.min")
summary(abs(exp(pred)-exp(yts_adv)))

rsq_test(pred, yts_adv)


# Last bit of testing with 10 selected top free agents from
# summer 2024. 5 stars and 5 role players (non-stars)
# This time, players were classified as stars or non-stars with the authors' domain knowledge of the NBA


top_FA_i=which(player_2024$Player %in% c("Paul George", "LeBron James", "Tyrese Maxey", "DeMar DeRozan", "James Harden", "Kentavious Caldwell-Pope", "Tyus Jones", "Malik Beasley", "Jonas Valančiūnas", "Tobias Harris"))
top_FA_x=rbind(xtr_box[c("112", "201", "274", "308", "320", "391", "446", "462"),], xts_box[c("240", "461"),])
top_FA_y=player_2024$log_salary[top_FA_i]

ols_pred=predict(ols_box, top_FA_x[,-c(1, 2, 3)], interval="prediction")
colnames(ols_pred)=c("ols_fit", "ols_lwr", "ols_upr")

wls_pred=predict(wls_box, top_FA_x[,c(-1, -2, -3)], interval="prediction")
colnames(wls_pred)=c("wls_fit", "wls_lwr", "wls_upr")

ints=as.data.frame(cbind(wls_pred, ols_pred))
ints$name=c("DeMar DeRozan", "James Harden", "Kentavious Caldwell-Pope", "LeBron James", "Malik Beasley", "Paul George", "Tobias Harris", "Tyus Jones", "Jonas Valančiūnas", "Tyrese Maxey")
player_2024[top_FA_i, c(2,64)]

ints$actual=c(16.96825, 17.33164, 16.94038, 17.70178, 15.60727, 17.71152, 17.04891, 14.55149, 16.10805, 17.37505)
library(ggplot2)



ggplot(ints, aes(x = as.factor(name))) +
  
  # WLS prediction interval
  geom_pointrange(aes(y = wls_fit, ymin = wls_lwr, ymax = wls_upr, color = "WLS Interval"),
                  position = position_nudge(x = -0.2)) +
  
  # OLS prediction interval
  geom_pointrange(aes(y = ols_fit, ymin = ols_lwr, ymax = ols_upr, 
                      color = "OLS Interval"),
                  position = position_nudge(x = 0.2)) +
  
  # Actual log salaries
  geom_point(aes(y = actual, color = "Actual Salary"),
             size = 2) +
  
  # Manual legend mappings
  scale_color_manual(name = "Legend", 
                     values = c("WLS Interval" = "red", 
                                "OLS Interval" = "blue", 
                                "Actual Salary" = "black")) +
  
  # Labels and theme
  labs(title = "WLS and OLS Confidence Intervals and Actual 2024 log(Salaries), Selected Top Free Agents",
       y = "log(Salary)",
       x = "Player Name") +
  theme_minimal() +
  scale_y_continuous(limits = c(13.7, 20.2))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Checking assumptions for OLS, WLS, LASSO

#OLS
#linear relationship check
plot(fitted(ols_box), resid(ols_box), xlab="OLS Fitted Vals", ylab="OLS Residuals", main="OLS Fitted Vals vs Residuals")
abline(h = 0, col = "red")

plot(fitted(ols_adv), resid(ols_adv), xlab="OLS Fitted Vals", ylab="OLS Residuals", main="OLS Fitted Vals vs Residuals, Advanced Stats")
abline(h = 0, col = "red")

# independent errors
library(lmtest)
dwtest(ols_box)
dwtest(ols_adv)
#p>0.05, assumption holds

#multicolinearity check
library(car)
vif(ols_box)
vif(ols_adv)

#homoskedastic errors check
qqnorm(residuals(ols_box))
qqline(residuals(ols_box))

qqnorm(residuals(ols_adv))
qqline(residuals(ols_adv))

#Lower tail deviates from the line significantly: suggests heteroskedastic errors

# WLS
#Same assumptions as OLS, we are checking 
qqnorm(residuals(wls_box))
qqline(residuals(wls_box))


