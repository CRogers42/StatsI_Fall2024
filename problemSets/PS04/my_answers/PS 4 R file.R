#In this question, use the \texttt{prestige} dataset in the \texttt{car} library.
install.packages(car)
library(car)
data(Prestige)
help(Prestige)
install.packages(stargazer)
library(stargazer)

#QUESTION 1
#We would like to study whether individuals with higher levels of income have 
#more prestigious jobs. Moreover, we would like to study whether professionals 
#have more prestigious jobs than blue and white coln lar workers.
#Create a new variable {professional} by recoding the variable {type} so that 
#professionals are coded as 1, and blue and white collar workers are coded 
#as 0 (Hint: {ifelse}).
professional <- ifelse(Prestige$type == "prof", 1, 0)
professional

#Run a linear model with {prestige} as an outcome and {income}, {professional},
#and the interaction of the two as predictors (Note: this is a continuous 
#$\times$ dummy interaction.)
interaction_model <- lm(prestige ~ income + professional + income:professional, 
                        data = Prestige)
summary(interaction_model)
stargazer((interaction_model))

#Prediction Equation
#Y = B0 + B1Xi +B2Di +B3XiDi + e
#Prestige = 21.1422589 + 0.0031709*Income + 37.7812800*Professional - 0.0023257*Income*Professional     

#What is the effect of a \$1,000 increase in income on prestige score for 
#professional occupations? In other words, we are interested in the marginal 
#effect of income when the variable \texttt{professional} takes the value of 
#$1$. Calculate the change in $\hat{y}$ associated with a \$1,000 increase in 
#income based on your answer for (c).

#Prestige = 21.1422589 + 0.0031709*Income + 37.7812800*Professional - 0.0023257*Income*Professional     
#Y <- 0.0031709*Income - 0.0023257*Income*Professional 
Y <- 0.0031709*(1000) - 0.0023257*(1000)*(1) 
Y #0.8452

#What is the effect of changing one's occupations from non-professional to 
#professional when her income is \$6,000? We are interested in the marginal 
#effect of professional jobs when the variable \texttt{income} takes the value 
#of $6,000$. 
Y <- 37.7812800*1 - 0.0023257*(6000)*(1)
Y #23.82708

#Question 2
TSa <- 0.042/0.016
p_value_a <- 2*pt(abs(TSa), 131-2-1, lower.tail = FALSE)
p_value_a #0.00972002

TSb <- 0.042/0.013
p_value_b <- 2*pt(abs(TSb), 131-2-1, lower.tail = FALSE)
p_value_b #0.00156946

