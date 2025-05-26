# Heteroscedasticity

# setup
library(here)
library(tidyverse)
library(plotly)
rm(list = ls()) 
load(here("dBdat.Rda"))

# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

model <- lm(dB ~ device + x + y + eye + visit, data = dBdat)

plot(fitted(model), residuals(model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, lty = 2) # Add a horizontal line at zero

plot(dBdat$dB[dBdat$device=="hfa"], dBdat$dB[dBdat$device=="retinalogik"],
     main = "Regression Comparison", xlab = "HFA", ylab = "Retinalogik")

boxplot(residuals(model) ~ dBdat$device,
        xlab = "Device", ylab = "Residuals",
        main = "Residuals by Device")

library(lmtest)
bptest(model)

# A small p-value (typically < 0.05) suggests rejecting the null hypothesis, indicating heteroscedasticity.

bptest(model, ~ device + eye + visit + x + y + I(x^2) + I(y^2) + x:y, data = dBdat)
# Adjust the formula based on your model's predictors

library(ggplot2)

# Install if needed: install.packages("MethComp")
library(MethComp)

# Run Deming regression with assumed lambda = 1
# deming_result <- Deming(widedat$hfa, widedat$retinalogik, vr=lambda)
# 
# summary(deming_result)
# intercept <- deming_result[1]
# slope <- deming_result[2]
# 
# p1 <- dBdat %>%
#   select(id, device, visit, eye, x, y, dB) %>%
#   # mutate(dB = case_when(
#   #   dB < 15 ~ 15,
#   #   dB > 40 ~ 40,
#   #   TRUE ~ dB # This catches all other cases and keeps the original dB value
#   # )) %>%
#   pivot_wider(., names_from = device, values_from = dB) %>%
#   mutate(retinalogik = as.numeric(retinalogik), hfa = as.numeric(hfa)) %>%
#   na.omit() %>%
#   ggplot(., aes(x=hfa, y=retinalogik)) + 
#   #geom_point(aes(color=id), position = "jitter") +
#   scale_x_continuous(breaks = seq(0, 44, by=5), limits=c(14,40)) +
#   scale_y_continuous(breaks = seq(0, 44, by=5), limits=c(14,40)) +
#   geom_abline(intercept = 0, slope = 1, color = "gray50", linetype="dashed") +
#   geom_abline(intercept = intercept, slope = slope, color = "red", linetype = "solid") +  # Deming line
#   geom_count(color = "blue") +
#   labs(title = "Set <15 dB to 15 dB",
#        x = "HFA",
#        y = "Retinalogik") +
#   theme_bw()
# ggplotly(p1)

### Set <15 dB to 15 dB

# Estimate within-location variance for dB
widedat <- dBdat %>%
  mutate(dB = case_when(
    dB < 15 ~ 15,
    dB > 40 ~ 40,
    TRUE ~ dB # This catches all other cases and keeps the original dB value
  )) %>%
  select(id, eye, visit, x, y, device, dB) %>%
  pivot_wider(names_from = device, values_from = dB) %>%
  drop_na() 

error_var_df <- widedat %>%
  group_by(id, eye, x, y) %>%
  summarise(
    n = n(),
    var_H = ifelse(n > 1, var(hfa, na.rm = TRUE), NA),
    var_R = ifelse(n > 1, var(retinalogik, na.rm = TRUE), NA),
    .groups = "drop"
  ) %>%
  filter(n > 1)

# Mean within-location variances
sigma2_H <- mean(error_var_df$var_H, na.rm = TRUE)
sigma2_R <- mean(error_var_df$var_R, na.rm = TRUE)

# Error ratio for Deming regression
lambda <- sigma2_R / sigma2_H

cat("Estimated error variance (HFA):", sigma2_H, "\n")
cat("Estimated error variance (Retinalogik):", sigma2_R, "\n")
cat("Lambda (Retinalogik / HFA):", lambda, "\n")


# Install if needed: install.packages("MethComp")

# Run Deming regression with assumed lambda = 1
deming_result <- Deming(widedat$hfa, widedat$retinalogik, vr=lambda)

summary(deming_result)
intercept <- deming_result[1]
slope <- deming_result[2]

p1 <- dBdat %>%
  select(id, device, visit, eye, x, y, dB) %>%
  mutate(dB = case_when(
    dB < 15 ~ 15,
    dB > 40 ~ 40,
    TRUE ~ dB # This catches all other cases and keeps the original dB value
  )) %>%
  pivot_wider(., names_from = device, values_from = dB) %>%
  mutate(retinalogik = as.numeric(retinalogik), hfa = as.numeric(hfa)) %>%
  na.omit() %>%
  ggplot(., aes(x=hfa, y=retinalogik)) + 
  #geom_point(aes(color=id), position = "jitter") +
  scale_x_continuous(breaks = seq(0, 45, by=5), limits=c(14,40)) +
  scale_y_continuous(breaks = seq(0, 45, by=5), limits=c(14,40)) +
  geom_abline(intercept = 0, slope = 1, color = "gray50", linetype="dashed") +
  geom_abline(intercept = intercept, slope = slope, color = "red", linetype = "solid", size = 1) +  # Deming line
  geom_count(color = "blue") +
  annotate("text", x = 20, y = 38, label = paste("Slope =", round(slope, 2)), color = "red") +
  labs(title = "Set <15 dB to 15 dB",
       x = "HFA",
       y = "Retinalogik") +
  theme_bw()
ggplotly(p1)

###########################
### Include all dBs
###########################

widedat <- dBdat %>%
  select(id, eye, visit, x, y, device, dB) %>%
  pivot_wider(names_from = device, values_from = dB) %>%
  drop_na() 

error_var_df <- widedat %>%
  group_by(id, eye, x, y) %>%
  summarise(
    n = n(),
    var_H = ifelse(n > 1, var(hfa, na.rm = TRUE), NA),
    var_R = ifelse(n > 1, var(retinalogik, na.rm = TRUE), NA),
    .groups = "drop"
  ) %>%
  filter(n > 1)

# Mean within-location variances
sigma2_H <- mean(error_var_df$var_H, na.rm = TRUE)
sigma2_R <- mean(error_var_df$var_R, na.rm = TRUE)

# Error ratio for Deming regression
lambda <- sigma2_R / sigma2_H

cat("Estimated error variance (HFA):", sigma2_H, "\n")
cat("Estimated error variance (Retinalogik):", sigma2_R, "\n")
cat("Lambda (Retinalogik / HFA):", lambda, "\n")

# Run Deming regression with assumed lambda = 1
deming_result <- Deming(widedat$hfa, widedat$retinalogik, vr=lambda)

summary(deming_result)
intercept <- deming_result[1]
slope <- deming_result[2]

p2 <- dBdat %>%
  select(id, device, visit, eye, x, y, dB) %>%
  pivot_wider(., names_from = device, values_from = dB) %>%
  mutate(retinalogik = as.numeric(retinalogik), hfa = as.numeric(hfa)) %>%
  na.omit() %>%
  ggplot(., aes(x=hfa, y=retinalogik)) + 
  #geom_point(aes(color=id), position = "jitter") +
  scale_x_continuous(breaks = seq(0, 50, by=5), limits=c(-2,50)) +
  scale_y_continuous(breaks = seq(0, 50, by=5), limits=c(-2,50)) +
  geom_abline(intercept = 0, slope = 1, color = "gray50", linetype="dashed") +
  geom_abline(intercept = intercept, slope = slope, color = "red", linetype = "solid", size = 1) +  # Deming line
  geom_count(color = "blue") +
  annotate("text", x = 20, y = 40, label = paste("Slope =", round(slope, 2)), color = "red") +
  labs(title = "Include all dBs",
       x = "HFA",
       y = "Retinalogik") +
  theme_bw()
ggplotly(p2)

# to isolate the outliers
p3 <- dBdat %>%
  select(id, device, visit, eye, x, y, dB) %>%
  pivot_wider(., names_from = device, values_from = dB) %>%
  mutate(retinalogik = as.numeric(retinalogik), hfa = as.numeric(hfa)) %>%
  na.omit() %>%
  ggplot(., aes(x=hfa, y=retinalogik)) + 
  geom_point(aes(color=id), position = "jitter") +
  scale_x_continuous(breaks = seq(0, 44, by=5), limits=c(-2,50)) +
  scale_y_continuous(breaks = seq(0, 44, by=5), limits=c(-2,50)) +
  geom_abline(intercept = 0, slope = 1, color = "gray50", linetype="dashed") +
  #geom_abline(intercept = intercept, slope = slope, color = "red", linetype = "solid", size = 1) +  # Deming line
  #geom_count(color = "blue") +
  #annotate("text", x = 20, y = 38, label = paste("Slope =", round(slope, 2)), color = "red") +
  labs(title = "Include all dBs",
       x = "HFA",
       y = "Retinalogik") +
  theme_bw()
ggplotly(p3)

