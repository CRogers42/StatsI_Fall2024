###QUESTION 1

Q1 <- as.table(rbind (c(14, 6, 7), c(7, 7, 1)))
dimnames(Q1) <- list(c("Upper class", "Lower class"),
                    c("Not stopped", "Bribe requested", "Stopped/warning")
)
Q1



#(a) calculate chi-square 
chisq.test(Q1)

q1 <- addmargins(Q1, margin = seq_along(dim(Q1)), 
                      FUN = sum, quiet = FALSE)
q1
#expected freq.
fe1 <- (27/42)*21
fe2 <- (15/42)*21
fe3 <- (27/42)*13
fe4 <- (15/42)*13
fe5 <- (27/42)*8
fe6 <- (15/42)*8

chisquare <- sum(((14-fe1)^2)/fe1,
    ((7-fe2)^2)/fe2,
    ((6-fe3)^2)/fe3,
    ((7-fe4)^2)/fe4,
    ((7-fe5)^2)/fe5,
    ((1-fe6)^2)/fe6
)
chisquare # 3.791168

#b)Now calculate the p-value from the test statistic you just created (in R).
# What do you conclude if a = 01?
p_value <- pchisq(3.791168, df=2, lower.tail=F)
p_value #0.1502306



              
              
#c) Calculate the standardized residuals 
stan_res <- c((14-fe1)/sqrt(fe1*((1-27/42)*(1-21/42))), 
              (7-fe2)/sqrt(fe2*((1-15/42)*(1-21/42))) ,
              (6-fe3)/sqrt(fe3*((1-27/42)*(1-13/42))) ,
              (7-fe4)/sqrt(fe4*((1-15/42)*(1-13/42))) ,
              (7-fe5)/sqrt(fe5*((1-27/42)*(1-8/42))) ,
              (1-fe6)/sqrt(fe6*((1-15/42)*(1-8/42))) 
)
stan_res #0.3220306 -0.3220306 -1.6419565  1.6419565  1.5230259 -1.5230259

chisq.test(Q1)$stdres






###QUESTION 2
q2 <- read.csv(url("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"))

lm <- lm(formula = q2$water~ q2$reserved)
summary (lm)
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   14.738      2.286   6.446 4.22e-10 ***
#q2$reserved    9.252      3.948   2.344   0.0197 *  
