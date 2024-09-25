#QUESTION 1 - EDUCATION
setwd("C:/Users/141CR/OneDrive/Documents/GitHub/StatsI_Fall2024ForkCR/problemSets/PS01/my_answers")
getwd()

IQ <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98,
       80, 97, 95, 111, 114, 89, 95, 126, 98)

n<-length(IQ)
sd<-sd(IQ)
mean(IQ)

#for 90% CI...T distribution
t_score <- qt(0.95, df=length(IQ)-1)
t_score
t_lower_99<- mean (IQ) -(t_score)*(sd(IQ)/sqrt(n))
t_upper_99<- mean (IQ) +(t_score)*(sd(IQ)/sqrt(n))
t_lower_99
t_upper_99

#[93.96, 102.92]

#Null hypothesis: the average IQ of students in this specific 
#school is equal to the average IQ among all the schools in the country.
#Alternative hypothesis: the average IQ of students in this specific 
#school is greater than the average IQ among all the schools in the country.
#Test statistic 
t_stat <- (mean(IQ)-100)/(sd/sqrt(n))
t_stat #-0.5957439
p_value <- (pt(abs(t_stat), 24))
p_value # 0.7215383










#This line needs to be at 41-------


#QUESTION 2
#Y{per capita expenditure on shelters/housing assistance in state}
#X1{per capita personal income in state
#X2{Number of residents per 100,000 that are "financially insecure" in state}
#X3{Number of people per thousand residing in urban areas in state} 
#Region{1=Northeast, 2= North Central, 3= South, 4=West} 
expenditure <- read.delim("expenditure.txt")
View(expenditure)

png("output/graphsA1.png"
    )
par(mfrow = c(2,3))
plot(expenditure$X1, expenditure$Y,
     col=2, 
     main = "Personal Income by Expenditure", 
     xlab= "Personal Income",
     ylab= "Expenditure", 
     cex.main = .75
)
plot(expenditure$X2, expenditure$Y,
     col=3, 
     main = "Financially Insecure Residents by Expenditure", 
     xlab= "'Financially insecure'",
     ylab= "Expenditure",
     cex.main = .75
)
plot(expenditure$X3, expenditure$Y,
     col=4,
     main = "Population in Urban Areas by Expenditure", 
     xlab= "Urban population",
     ylab= "Expenditure",
     cex.main = .75
)
plot(expenditure$X1, expenditure$X2,
     col=5,
     main = "Personal income by 'Financially Insecure'", 
     xlab= "Personal Income",
     ylab= "'Financially insecure'",
     cex.main = .75
)
plot(expenditure$X1, expenditure$X3,
     col=6,
     main = "Financially insecure by Urban Population", 
     xlab= "Personal Income",
     ylab= "Urban Population",
     cex.main = .75
)
plot(expenditure$X2, expenditure$X3,
     col=7,
     main = "Personal income by 'Urban Population", 
     xlab= "'Financially insecure'",
     ylab= "Urban Population",
     cex.main = .75
)
dev.off()

dev.new()


    
#Please plot the relationship between \emph{Y} and \emph{Region}? 
#On average, which region has the highest per capita expenditure on 
#housing assistance?

class(expenditure$Region)
region_names <- c("Northeast", "North Central", "South", "West")
region_names
levels (expenditure$Region) <- region_names
expenditure$Region
png("output/box.png")

boxplot(Y~ Region, data=expenditure, col = "grey",
        xlab= "Regions",
        ylab= "Expenditure on Shelters/Housing assistance", 
        names = c("Northeast", "North Central", "South", "West")
)
dev.off()

mean(expenditure$Y[expenditure$Region == "1"])
mean(expenditure$Y[expenditure$Region == "2"])
mean(expenditure$Y[expenditure$Region == "3"])
mean(expenditure$Y[expenditure$Region == "4"])

png("output/YandX1.png",
    res=45)
plot(expenditure$X1, expenditure$Y,
     col=1, 
     main = "Personal Income by Expenditure", 
     xlab= "Personal Income",
     ylab= "Expenditure"
)
dev.off()


class(expenditure$Region)
region <- as.factor(expenditure$Region)
colors <- c("pink", "blue", "green", "purple")

png("output/Region.png")
plot(expenditure$X1, expenditure$Y,
     col= colors[factor(expenditure$Region)],
     pch = 1:4, 
     main = "Personal Income by Expenditure in various Regions", 
     xlab= "Personal Income in State per capita",
     ylab= "Expenditure in State per capita"
)
legend("topleft", 
       legend = c("Northeast", "North Central", "South", "West"),
       col= colors,
       pch=1,        # Marker type (1 is default)
       cex = 0.5)

dev.off()
