#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
install.packages("tidyverse")
lapply(c("stringr", "tidyverse"),  pkgTest)

#set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")
View(inc.sub)


#QUESTION 1
#Run a regression where the outcome variable is voteshare and the 
#explanatory variable is difflog.
q1 <- lm(voteshare~difflog, data=inc.sub)
summary(q1)
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.579031   0.002251  257.19   <2e-16 ***
#difflog     0.041666   0.000968   43.04   <2e-16 ***

#Make a scatterplot & Regression Line 
# Scatter plot
scatterq1<-
  ggplot(data = inc.sub, 
         mapping = aes(x = difflog, 
                       y = voteshare)) + 
  geom_point(size = 1) +
  geom_smooth(method='lm',col="darkgreen") + #Regression line
  labs(x = "Difference in Spending", #Labels
       y = "Incumbent's Vote Share", 
       title =  "Incumbent's Vote Share by Difference in Spending") +
  theme_classic()  #Theme
ggsave(scatterq1, file = "ScatterQ1.pdf", width = 8, height = 4)

scatterq1

#Save the residuals of the model in a separate object.
residualsq1 <- lm(voteshare~difflog, data=inc.sub)$residuals
residualsq1

#Prediction Equation
summary(q1) #Incumbent's Vote Share = 0.579031 + 0.041666*Difference in Spending



#QUESTION 2
#Run a regression where the outcome variable is presvote and 
#the explanatory variable is difflog
q2 <- lm(presvote~difflog, data=inc.sub)
summary(q2)
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.507583   0.003161  160.60   <2e-16 ***
#difflog     0.023837   0.001359   17.54   <2e-16 ***

#Make a scatterplot & Regression Line 
# Scatter plot
scatterq2<-
  ggplot(data = inc.sub, 
         mapping = aes(x = difflog, 
                       y = presvote)) + 
  geom_point(size = 1) +
  geom_smooth(method='lm',col="darkblue") + #Regression line
  labs(x = "Difference in Spending", #Labels
       y = "Presidential Candidate's Vote Share", 
       title =  "Presidential Candidate's Vote Share by Difference in Spending") +
  theme_classic()  #Theme
ggsave(scatterq2, file = "ScatterQ2.pdf", width = 8, height = 4)

scatterq2

#Save the residuals of the model in a separate object.
residualsq2 <- lm(presvote~difflog, data=inc.sub)$residuals
residualsq2

#Prediction Equation
summary(q2) #Presidential Candidate's Vote Share = 0.507583 + 0.023837*Difference in Spending

#QUESTION 3
#Run a regression where the outcome variable is \texttt{voteshare} 
#and the explanatory variable is \texttt{presvote}.
q3 <- lm(voteshare~presvote, data=inc.sub)
summary(q3)
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.441330   0.007599   58.08   <2e-16 ***
#presvote    0.388018   0.013493   28.76   <2e-16 ***

#Make a scatterplot & Regression Line 
# Scatter plot
scatterq3<-
  ggplot(data = inc.sub, 
         mapping = aes(x = presvote, 
                       y = voteshare)) + 
  geom_point(size = 1) +
  geom_smooth(method='lm',col="purple") + #Regression line
  labs(x = "Presidential Candidate's Vote Share", #Labels
       y = "Incumbent's Vote Share", 
       title =  "Incumbent's Vote Share by Presidential Candidate's Vote Share") +
  theme_classic()  #Theme
ggsave(scatterq3, file = "ScatterQ3.pdf", width = 8, height = 4)

scatterq3

#Prediction Equation
summary(q3) #Incumbent's Vote Share = 0.441330 + 0.388018 * Presidential Candidate's Vote Share


#QUESTION 4 
#Run a regression where the outcome variable is the residuals from 
#Question 1 and the explanatory variable is the residuals from Question 2
q4 <- lm(residualsq1~residualsq2)
summary(q4)
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -5.934e-18  1.299e-03    0.00        1    
#residualsq2  2.569e-01  1.176e-02   21.84   <2e-16 ***

#Make a scatterplot & Regression Line 
# Scatter plot
scatterq4<-
  ggplot(data = inc.sub, 
         mapping = aes(x = residualsq2, 
                       y = residualsq1)) + 
  geom_point(size = 1) +
  geom_smooth(method='lm',col="turquoise") + #Regression line
  labs(x = "Q2 Residuals", #Labels
       y = "Q1 Residuals", 
       title =  "Variation in Residuals") +
  theme_classic()  #Theme
ggsave(scatterq4, file = "ScatterQ4.pdf", width = 8, height = 4)

scatterq4

#Prediction Equation
summary(q4) #Residuals Q1 = -5.934e-18 +  2.569e-01 * Residuals Q2 



#QUESTION 5
# Run a regression where the outcome variable is the incumbent's 
#voteshare and the explanatory variables are difflog and presvote.
q5 <- lm(voteshare~difflog+presvote, data=inc.sub)
summary(q5)
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.4486442  0.0063297   70.88   <2e-16 ***
#difflog     0.0355431  0.0009455   37.59   <2e-16 ***
#presvote    0.2568770  0.0117637   21.84   <2e-16 ***

#Prediction Equation 
#Incumbent's Vote Share = 0.4486442 + 0.0355431 * Difference in Spending + 0.2568770 * Presidential Candidate's Vote Share
