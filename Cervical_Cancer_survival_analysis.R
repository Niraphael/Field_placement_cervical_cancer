#***************************************************************************
# Field placement II                                                       *
# May 21, 2024                                                             *

# Group Member: Raphael Ndahimana, Melissa Uwase                                  *

# CERVICAL CANCER SURVIVAL ANALYSIS                                        *

#***************************************************************************

# Setting working directory

wd = "C:/Users/RAPHAEL PRO/Desktop/Field_placement_II"
data_wd = paste0(wd,"/data")
output_wd = paste0(wd,"/output")

# Load needed libraries
library(readxl)
library(tidyverse)
library(tidyr)
library(lubridate)
library(janitor)
library(dplyr)
library(skimr)
library(gtsummary)
library(survival)
library(survminer)
install.packages("forestmodel")
install.packages("ggplot2")
library(forestmodel)
library(ggplot2)

# TO Clear environment
#rm(list = ls())

# Upload the data
setwd(data_wd)

Cervical_Cancer_data <- read_excel("C:/Users/RAPHAEL PRO/Desktop/Field_placement_II/data/Cervical_Cancer_data.xlsx") %>%
  janitor::clean_names()

#computing age in Months from two different dates

    # converting dates variables in dates format

Cervical_Cancer_data$date_of_diagnosis <- as.Date(Cervical_Cancer_data$date_of_diagnosis, format="%Y-%m-%d")
Cervical_Cancer_data$date_of_last_contact_6 <- as.Date(Cervical_Cancer_data$date_of_last_contact_6, format="%Y-%m-%d")

 # creating different in Months

Cervical_Cancer_data$diff_months <- interval(Cervical_Cancer_data$date_of_diagnosis, Cervical_Cancer_data$date_of_last_contact_6) %/% months(1)

head(Cervical_Cancer_data)

summary (Cervical_Cancer_data $ diff_months)

#Renaming variable and Removing duplicated observations

Cervical_Cancer_data<-Cervical_Cancer_data %>%
  
rename(survival_time = diff_months)  

distinct(Cervical_Cancer_data)

# creating age groups
Cervical_final <- Cervical_final %>%
  mutate(age_category = case_when(
    age < 20 ~ "<20",
    age >= 20 & age < 40 ~ "20-39",
    age >= 40 & age < 60 ~ "40-59",
    age >= 60 & age < 80 ~ "60-79",
    age >= 80 ~ "80+"
  ))

# creating age groups with other categories
Cervical_final <- Cervical_final %>%
  mutate(age_category2 = case_when(
    age < 46 ~ "<46",
    age >= 46 & age < 56 ~ "46-55",
    age >= 56 & age <= 63 ~ "56-63",
    age >= 64 ~ "64+"
  ))

#Re-coding the outcome variable

Cervical_Cancer_data <- Cervical_Cancer_data %>%
  mutate(vital_status_numeric = recode(vital_status, "Alive" = 0, "Died" = 1)) 
# Re-coding treatment medication variable

Cervical_Cancer_data <- Cervical_Cancer_data %>%
  mutate_at(vars(surgery, chemotherapy, immunotherapy, radiotherapy, hormothrapy,palliative_care), ~ recode(., "YES" = 1, "NO" = 0)) %>%
  rename(
    new_surgery = surgery,
    new_Immunotherapy = immunotherapy,
    new_radiotherapy = radiotherapy,
    new_hormothrapy = hormothrapy,
    new_palliative_care = palliative_care
  )
#Re-coding the received treatment
  Cervical_Cancer_data <- Cervical_Cancer_data %>%
  mutate(received_treatment = recode(received_treatment, "Yes" = 1, "No" = 0))
  
  ######### Data analysis####################
  
  # 1. Descriptive analysis 
  
  percentage_frequency <- prop.table(table(Cervical_final$method_of_diagnosis)) * 100
  
  print(percentage_frequency)
  
  
  # 2.Fitting the Survival data
  
      # Use Surv() syntax for right-censored data
  Cervical_survival <- Surv(time = Cervical_final$survival_time,
                          event = Cervical_final$vital_status_numeric)
  # start of the analysis by creating the survival fit object
  Cervical_survival_fit <-survival::survfit(Cervical_survival ~ 1)
  
summary(Cervical_survival_fit)
  
  # We can also use the print() function. The print.rmean = TRUE argument is used to obtain the mean survival time and its standard error (se)
# print Cervical_survival_fit object with mean survival time and it's se. 

print(Cervical_survival_fit, print.rmean = TRUE)

#plotting Kaplan Meir curve
plot(
  Cervical_survival_fit,
  xlab = "Month of follow-up",       
  ylab = "Survival Probability",       
  mark.time = TRUE,              # mark events on the curve: a "+" is printed at every event
  conf.int = FALSE,              # do not plot the confidence interval
  censor.shape = 124,           # Shape of censoring marks
  censor.size = 4,              # Size of censoring marks
  main = "Overall survival curve and cumulative mortality"
)

# draw an additional curve to the previous plot
lines(
  Cervical_survival_fit,
  lty = 3,             # use different line type for clarity
  fun = "event",       # draw the cumulative events instead of the survival 
  mark.time = FALSE,
  conf.int = FALSE
)

# add a legend to the plot
legend(
  "topright",                               # position of legend
  legend = c("Survival", "Cum. Mortality"), # legend text 
  lty = c(1, 3),                            # line types to use in the legend
  cex = .85,                                # parameters that defines size of legend text
  bty = "n"                                 # no box type to be drawn for the legend
)

## conducting the survival fit by grouping with other variables using log-rank test

# create the new survfit object based on gender
Cervical_survival_fit_age_category2 <-survfit(Surv(survival_time, vital_status_numeric) ~ age_category2, data = Cervical_final)
  

# set colors
col_age_category2 <- c("lightgreen", "darkgreen", "yellow","black")

# create plot
plot(
  Cervical_survival_fit_age_category2,
  col = col_age_category2,
  conf.int = FALSE,              # Show confidence intervals
  censor.shape = 124,           # Shape of censoring marks
  censor.size = 4,              # Size of censoring marks
  title = "Kaplan-Meier Curve",
  xlab = "Months of follow-up",
  ylab = "Survival Probability")

# add legend
legend(
  "topright",
  legend = c("<46","46-55","56-63","64+"),
  col = col_age_category2,
  lty = 1,
  cex = .9,
  bty = "n")
# Checking for overlapping and p-value
survival::survdiff(
  Surv(survival_time, vital_status_numeric) ~ age_category2, 
  data = Cervical_final
)



# Alternatively, Drawing the survival curve with censoring using ggsurv plot package

# Fit the Kaplan-Meier estimator

fit <- survfit(Surv(survival_time, vital_status_numeric) ~ 1, data = Cervical_final)

     # Plot the Kaplan-Meier curve
ggsurvplot(
  fit,
  conf.int = FALSE,              # Show confidence intervals
  censor.shape = 124,           # Shape of censoring marks
  censor.size = 4,              # Size of censoring marks
  ggtheme = theme_minimal(),    # Use a minimal theme for the plot
  title = "Kaplan-Meier Curve",
  xlab = "Time",
  ylab = "Survival Probability"
)

   ## Grouping with other variable

fit1 <- survfit(Surv(survival_time, vital_status_numeric) ~ age_category2, data = Cervical_final)

ggsurvplot(
  fit1,
  conf.int = FALSE,              # Show confidence intervals
  censor.shape = 124,           # Shape of censoring marks
  censor.size = 4,              # Size of censoring marks
  ggtheme = theme_minimal(),    # Use a minimal theme for the plot
  title = "Kaplan-Meier Curve by Age",
  xlab = "Time",
  ylab = "Survival Probability",
  legend.title = "Age_category2",      # Title for the legend
  legend.labs = c("<46","46-55","56-63","64+")  # Labels for the legend
)

#compute the test of the difference between the survival curves
survival::survdiff(
  Surv(survival_time, vital_status_numeric) ~ age_category2, 
  data = Cervical_final
)

## other alternative using survminer package

survminer::ggsurvplot(
  fit1, 
  data = Cervical_final,          # again specify the data used to fit Cervical_final 
  conf.int = FALSE,              # do not show confidence interval of KM estimates
  surv.scale = "percent",        # present probabilities in the y axis in %
  break.time.by = 10,            # present the time axis with an increment of 10 days
  xlab = "Follow-up months",
  ylab = "Survival Probability",
  pval = T,                      # print p-value of Log-rank test 
  pval.coord = c(40,.91),        # print p-value at these plot coordinates
  risk.table = T,                # print the risk table at bottom 
  legend.title = "Age",       # legend characteristics
  legend.labs = c("<46","46-55","56-63","64+"),
  font.legend = 10, 
  palette = "Dark2",             # specify color palette 
  surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
  ggtheme = theme_light()        # simplify plot background
)

## Kaplan Meir curve of the Cervical cancer stage at Dx
fit2 <- survfit(Surv(survival_time, vital_status_numeric) ~ stage, data = Cervical_final)


survminer::ggsurvplot(
  fit2, 
  data = Cervical_final,          # again specify the data used to fit Cervical_final 
  conf.int = FALSE,              # do not show confidence interval of KM estimates
  surv.scale = "percent",        # present probabilities in the y axis in %
  break.time.by = 10,            # present the time axis with an increment of 10 days
  xlab = "Follow-up months",
  ylab = "Survival Probability",
  pval = T,                      # print p-value of Log-rank test 
  pval.coord = c(40,.91),        # print p-value at these plot coordinates
  risk.table = T,                # print the risk table at bottom 
  legend.title = "stage of cervical cancer",       # legend characteristics
  legend.labs = c("Stage I","Stage II","Stage III","Stage IV","Unknown"),
  font.legend = 10, 
  palette = "Dark2",             # specify color palette 
  surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
  ggtheme = theme_light()        # simplify plot background
)

## Kaplan Meir curve of the Cervical cancer treatment profile at time of Dx
fit3 <- survfit(Surv(survival_time, vital_status_numeric) ~ received_treatment, data = Cervical_final)
survminer::ggsurvplot(
  fit3, 
  data = Cervical_final,          # again specify the data used to fit Cervical_final 
  conf.int = FALSE,              # do not show confidence interval of KM estimates
  surv.scale = "percent",        # present probabilities in the y axis in %
  break.time.by = 10,            # present the time axis with an increment of 10 days
  xlab = "Follow-up months",
  ylab = "Survival Probability",
  pval = T,                      # print p-value of Log-rank test 
  pval.coord = c(40,.91),        # print p-value at these plot coordinates
  risk.table = T,                # print the risk table at bottom 
  legend.title = "stage of cervical cancer",       # legend characteristics
  legend.labs = c("0","1"),
  font.legend = 10, 
  palette = "Dark2",             # specify color palette 
  surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
  ggtheme = theme_light()        # simplify plot background
)

## Kaplan Meir curve of the Cervical method of dx  profile a
fit4 <- survfit(Surv(survival_time, vital_status_numeric) ~ method_of_diagnosis, data = Cervical_final)
survminer::ggsurvplot(
  fit4, 
  data = Cervical_final,          # again specify the data used to fit Cervical_final 
  conf.int = FALSE,              # do not show confidence interval of KM estimates
  surv.scale = "percent",        # present probabilities in the y axis in %
  break.time.by = 10,            # present the time axis with an increment of 10 days
  xlab = "Follow-up months",
  ylab = "Survival Probability",
  pval = T,                      # print p-value of Log-rank test 
  pval.coord = c(40,.91),        # print p-value at these plot coordinates
  risk.table = T,                # print the risk table at bottom 
  legend.title = "stage of cervical cancer",       # legend characteristics
  legend.labs = c("Clinically","Histology"),
  font.legend = 10, 
  palette = "Dark2",             # specify color palette 
  surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
  ggtheme = theme_light()        # simplify plot background
)
### Modeling the event with Covariates in survival analysis 
 # Let us start with crude analysis ,considering the following potential risk factors( stage of cervical cancer, Age at diagnosis, Received treatment,province,method of diagnosis)
 
#1.Cox Regression analysis where Covariates are considered
  
  #fitting the cox model for the 
  cervical_cox_sexage <- survival::coxph(
    Surv(survival_time, vital_status_numeric) ~ age_category2 + stage+province+method_of_diagnosis+received_treatment, 
    data = Cervical_final
  )
 #printing the model fitted
  print(cervical_cox_sexage)
  summary(cervical_cox_sexage)
  
  # Evaluating the model performance
  
  ## Using the cox regression model coefficients and are stated in the results(check in the field placement folders)
  
  ### Using the ROC time dependent curves
  
  # Load necessary libraries
  install.packages("timeROC")
  library(timeROC)
  library(survival)
  
  # Fit the Cox model
  fit_ROC <-coxph(Surv(survival_time, vital_status_numeric) ~ 
                 age_category2 + stage + province + method_of_diagnosis + 
                 received_treatment, data = Cervical_final)
  
  # Predict risk scores
  risk_scores <- predict(fit_ROC, type = "risk")
  
  # Compute time-dependent ROC curves
  times <- Cervical_final$survival_time  # Replace with relevant time points for your data
  roc_results <- timeROC(T = Cervical_final$survival_time,
                         delta = Cervical_final$vital_status_numeric,
                         marker = risk_scores,
                         cause = 1,
                         times = times,
                         iid = TRUE)
  
xxxxxxxxxxxxxxxxxxxx

# Load necessary libraries
install.packages("glmnet")
install.packages("timeROC")
library(glmnet)
library(timeROC)

# Check the initial summary of the data
summary(Cervical_final)

# Remove rows with non-positive event times and any missing data
Cervical_final_clean <- Cervical_final[Cervical_final$survival_time > 0, ]
Cervical_final_clean <- Cervical_final_clean[complete.cases(Cervical_final_clean), ]

# Confirm the number of observations after cleaning
nrow(Cervical_final_clean)

# Prepare the data for the Cox model
X_clean <- model.matrix(~ age_category2 + stage + province + method_of_diagnosis + received_treatment, 
                        data = Cervical_final_clean)[, -1]  # Remove the intercept term
Y_clean <- with(Cervical_final_clean, Surv(survival_time, vital_status_numeric))

# Confirm the number of observations in X_clean and Y_clean
nrow(X_clean)
length(Y_clean)

# Fit regularized Cox model (lasso) with cleaned data
fit_lasso_clean <- cv.glmnet(X_clean, Y_clean, family = "cox", alpha = 1)

# View the results
print(fit_lasso_clean)

# Extract the linear predictor (risk scores)
risk_scores_clean <- predict(fit_lasso_clean, newx = X_clean, s = "lambda.min")

# Calculate time-dependent ROC
roc_clean <- timeROC(T = Cervical_final_clean$survival_time, 
                     delta = Cervical_final_clean$vital_status_numeric, 
                     marker = risk_scores_clean, 
                     cause = 1, 
                     times = c(1, 2, 3,4,5,6,7,8,9,10,11,12,13,26),  # Specify time points of interest
                     iid = TRUE)
# Plot the ROC curve
plot(roc_clean, time = 26)  # Example: ROC at time = 2

  ## Building Forest plots of the cox model 

  # Fit the Cox proportional hazards model
  fit9 <- coxph(Surv(survival_time, vital_status_numeric) ~ 
                 age_category2 + stage + province + method_of_diagnosis + 
                 received_treatment, data = Cervical_final)
  
  # Create the forest plot of the cox model
  plot <- forest_model(fit9) +
    ggtitle("Hazard Ratios from Cox Proportional Hazards Model") +
    theme_minimal() +
    coord_cartesian(xlim = c(0.1, 10)) +  # Adjust x-axis limits
    theme(plot.title = element_text(hjust = 0.5))
  
  print(plot)
  
  # Optional: extract data and customize plot manually
  cox_summary <- summary(fit9)
  cox_data <- data.frame(
    term = rownames(cox_summary$coefficients),
    estimate = exp(cox_summary$coefficients[, "coef"]),
    conf.low = exp(cox_summary$conf.int[, "lower .95"]),
    conf.high = exp(cox_summary$conf.int[, "upper .95"]),
    p.value = cox_summary$coefficients[, "Pr(>|z|)"]
  )
  # Create a basic forest plot with manual customization
  plot_manual <- ggplot(cox_data, aes(x = term, y = estimate)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 1, linetype = "dashed") +  # HR of 1 as reference
    coord_flip() +
    scale_y_log10() +  # Log scale for hazard ratios
    ggtitle("Hazard Ratios from Cox Proportional Hazards Model") +
    theme_minimal() +
    xlab("Variables") +
    ylab("Hazard Ratio (log scale)") +
    geom_text(aes(label = ifelse(p.value < 0.05, sprintf("%.3f", p.value), "")), 
              hjust = -0.2, vjust = -0.2, size = 3) +  # Display significant p-values
    theme(plot.title = element_text(hjust = 0.5))
  
  print(plot_manual)
  
  
  
  
  
