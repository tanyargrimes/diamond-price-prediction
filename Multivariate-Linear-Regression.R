##################################################
#                                                #
# Multivariate Regression                        #
#                                                #
# Written by Tanya Grimes                        #
#                                                #
##################################################



#-------------------------------------------------------------------
# Clean Slate & Environment Initialization                                  

# if a list of plots exists, clears plots
if(!is.null(dev.list())) dev.off()

# clears entire console
cat("\014")

# clears all objects in the current workspace and global environment window
rm(list=ls())

# sets the current working directory for any files read from or saved to
setwd("C:/Users/SetYourOwn")

# prevents use of exponential notation until the numeric value width
# is greater than the scipen limit specified
options(scipen=9)

# controls the number of digits to print. Default is 7.
options(digits=5)



#-------------------------------------------------------------------
# Package Installation & Load

# using for summary statistics
if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

# using for advanced correlations
if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")



#-------------------------------------------------------------------
# Read in Diamond data

diamond_data_TG <- read.delim("diamond_val.txt", header = TRUE, sep = "\t")

# get initial look at data
head(diamond_data_TG)

# get structure of initial data set to confirm data is there
str(diamond_data_TG)



#-------------------------------------------------------------------
# Assignment Task 1 - Data Transformation

# convert int Price to a numeric data type
diamond_data_TG$Price <- as.numeric(diamond_data_TG$Price)
str(diamond_data_TG)

# convert int Year to a numeric data type
diamond_data_TG$Year <- as.numeric(diamond_data_TG$Year)
str(diamond_data_TG)

# convert factor Source to index dummy variables
source_dummies_TG <- model.matrix(~ Source -1, data = diamond_data_TG)
head(source_dummies_TG)

# combine the datasets 
diamond_data_TG <- cbind(diamond_data_TG, source_dummies_TG)
head(diamond_data_TG)

# drop unnecessary factor variable
diamond_data_TG <- diamond_data_TG[-c(3)]

head(diamond_data_TG)

# rename variables with _TG suffix
# names(diamond_data_TG) <- c("Price_TG", "Carat_TG", "Year_TG", "Clarity_TG", 
#                             "Color_TG", "Cut_TG", "Val_TG", "Alrosa_TG", "DeBeers_TG", 
#                             "Debswana_TG", "Petra_TG", "RioTinto_TG", 
#                             "Rockwell_TG")

names(diamond_data_TG) <- c("Pri_TG", "Crt_TG", "Yr_TG", "Clr_TG", "Col_TG", "Cut_TG", 
                            "Val_TG", "Alr_TG", "Bee_TG", "Deb_TG", "Pet_TG", "Rio_TG",
                            "Roc_TG")


head(diamond_data_TG)

# confirm variable data types
str(diamond_data_TG)



#-------------------------------------------------------------------
# Assignment Task 2 - Descriptive Data Analysis

# generate summary statistics on the variables
summary(diamond_data_TG)

# generate 3 x 3 grid for graphs
par(mfrow = c(3,3))

# generate histograms for all numeric variables
# loop over column *names* instead of actual columns
sapply(names(diamond_data_TG), function(cname){
  # plot only the numeric columns
  if (is.numeric(diamond_data_TG[[cname]]))
    # set column name as plot title with 'main' param
    print(hist(diamond_data_TG[[cname]], main = cname, xlab = cname))
})

# reset to individuals plot sizes afterwards
par(mfrow=c(1,1))



#-------------------------------------------------------------------
# Assignment Task 3 - Outliers Search

# generate 3 x 3 grid for graphs
par(mfrow = c(3,3))

# generate box plots for all numeric variables
# loop over column *names* instead of actual columns
sapply(names(diamond_data_TG), function(cname){
  # plot only the numeric columns
  if (is.numeric(diamond_data_TG[[cname]]))
    # set column name as plot title with 'main' param
    print(boxplot(diamond_data_TG[[cname]], main = cname, xlab = cname))
})

# reset to individuals plot sizes afterwards
par(mfrow=c(1,1))



#-------------------------------------------------------------------
# Assignment Task 4 - Exploratory Analysis

# Tests for Normality

# generate 3 x 3 grid for graphs
par(mfrow = c(3,3))

# generate QQ Norm plot for normality visual
# loop over column *names* instead of actual columns
sapply(names(diamond_data_TG), function(cname){
  # plot only the numeric columns
  if (is.numeric(diamond_data_TG[[cname]]))
    # set column name as plot title with 'main' param
    qqnorm(diamond_data_TG[[cname]], main = cname)
    qqline(diamond_data_TG[[cname]])
})

# reset to individuals plot sizes afterwards
par(mfrow=c(1,1))


# run Shapiro Wilks tests for normality
diamond_norm_TG <- lapply(diamond_data_TG, shapiro.test)

# group all the tests together
diamond_ngroup_TG <- sapply(diamond_norm_TG, `[`, c("statistic","p.value"))

# transpose the group for an easier read
diamond_ngroupt_TG <- t(diamond_ngroup_TG)
diamond_ngroupt_TG


# Spearman Correlations

# run spearman correlation table since all data is not normally distributed
diamond_corr_TG <- cor(diamond_data_TG, method="spearman")
round(diamond_corr_TG, 2)

# generate visual correlation representation
corrgram(diamond_data_TG, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main = "Diamond Variable Correlations", cor.method="spearman")



#-------------------------------------------------------------------
# Assignment Task 5 - Model Development

# Model with all variables included
diamond_all_lm_TG = lm(Pri_TG ~ Crt_TG + Yr_TG + Clr_TG + Col_TG + 
                       Cut_TG + Val_TG + Alr_TG + Bee_TG + Deb_TG + Pet_TG +
                       Roc_TG, data=diamond_data_TG, na.action=na.omit)
diamond_all_lm_TG
summary(diamond_all_lm_TG)



# Model with all forward selection
diamond_min_lm_TG <- lm(Pri_TG ~ 1, data=diamond_data_TG, na.action=na.omit)
diamond_fwd_lm_TG = step(diamond_min_lm_TG, direction="forward", scope =(
                        ~ Crt_TG + Yr_TG + Clr_TG + Col_TG + 
                        Cut_TG + Val_TG + Alr_TG + Bee_TG + Deb_TG + 
                        Pet_TG + Roc_TG), details=TRUE)

diamond_fwd_lm_TG
summary(diamond_fwd_lm_TG)



# Model with all stepwise selection
diamond_all_lm_TG = lm(Pri_TG ~ Crt_TG + Yr_TG + Clr_TG + Col_TG + 
                         Cut_TG + Val_TG + Alr_TG + Bee_TG + Deb_TG + Pet_TG +
                         Roc_TG, data=diamond_data_TG, na.action=na.omit)

diamond_step_lm_TG <- step(diamond_all_lm_TG)
diamond_step_lm_TG
summary(diamond_step_lm_TG)



#-------------------------------------------------------------------
# Assignment Task 6 - Model Evaluation

# Create model and residual vectors
diamond_all_fit_TG <- predict(diamond_all_lm_TG)
diamond_all_res_TG <- residuals(diamond_all_lm_TG)

diamond_fwd_fit_TG <- predict(diamond_fwd_lm_TG)
diamond_fwd_res_TG <- residuals(diamond_fwd_lm_TG)

diamond_step_fit_TG <- predict(diamond_step_lm_TG)
diamond_step_res_TG <- residuals(diamond_step_lm_TG)



# Test normality of residuals - none has normal fit
shapiro.test(diamond_all_res_TG)
shapiro.test(diamond_fwd_res_TG)
shapiro.test(diamond_step_res_TG)



# Run diagnositcs and plot graphs on models
par(mfrow = c(2, 2))
plot(diamond_all_lm_TG)
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
plot(diamond_fwd_lm_TG)
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
plot(diamond_step_lm_TG)
par(mfrow = c(1, 1))



#-------------------------------------------------
# Package Unload

# uncomment to unload packages
# detach(package:pastecs)
# detach(package:corrgram)

