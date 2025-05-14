ols_box=readRDS("ols_box.rds")
ols_adv=readRDS("ols_adv.rds")
ints=read.csv("ints.csv")

library(ggplot2)


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


#experiment at the end

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
