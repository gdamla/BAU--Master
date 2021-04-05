# EUROPEAN SALES DATA / LINEAR REGRESSION MODEL
# Gülhan Damla Aşık

getwd()

# SETTING # working directory
setwd("C:/Users/user/.../1- European Sales Dataset R")

# READING # data
EuropeanSalesData <- read.csv("EuropeanSales.csv", header =T)

library(dplyr)
library(ggplot2)
library(gplots)
library(gapminder)
library(reshape2)
install.packages("moments")   #Skewness and Kurtosis
library(moments)
install.packages("caTools")   #Split data
library(caTools)  


# UNDERSTANDING # data
View(EuropeanSalesData)
glimpse(EuropeanSalesData)
attributes(EuropeanSalesData)
head(EuropeanSalesData)
unique(EuropeanSalesData$Country)
table(EuropeanSalesData$Country)

# NULL value check
is.null(EuropeanSalesData)
# FALSE

# Data Visualization 
ggplot(EuropeanSalesData, aes(x = GDPperHead  , y = SalesPerCapita)) + geom_point(size =3) + geom_smooth(method=lm)
ggplot(EuropeanSalesData, aes(x = EducationSpending    , y = SalesPerCapita)) + geom_point(size =3) + geom_smooth(method=lm)
ggplot(EuropeanSalesData, aes(x = Population , y = SalesPerCapita , color = "red")) + geom_point(size =3)+ geom_smooth(method=lm)
ggplot(EuropeanSalesData, aes(x = UnemploymentRate   , y = SalesPerCapita)) + geom_point(size =3) + geom_smooth(method=lm)
ggplot(EuropeanSalesData, aes(x = ComputerSales    , y = SalesPerCapita)) + geom_point(size =3) + geom_smooth(method=lm)
ggplot(EuropeanSalesData, aes(x= SalesPerCapita , y = Country, color = SalesPerCapita)) + 
  geom_point(size = 8) + 
  geom_segment(aes( yend= Country ,xend = 0 ), size=2) + 
  geom_text(aes(label = SalesPerCapita), color = "White" , size = 3) + 
  labs(title = "SalesPerCapita per Country")+ 
  geom_vline( xintercept = mean(EuropeanSalesData$SalesPerCapita) , color = "red" , linetype = 5)


plot(EuropeanSalesData)
# We can see there is a relation with GDPperHead.

# Find correlations
cor(EuropeanSalesData[,])
# This gives an error because Country is categorical

# Drop country
EuropeanSalesData2 <- EuropeanSalesData[c("Population", "GDPperHead","UnemploymentRate","EducationSpending","SalesPerCapita","ComputerSales")]
EuropeanSalesData2

cor(EuropeanSalesData$SalesPerCapita,EuropeanSalesData$Population)
cor(EuropeanSalesData$SalesPerCapita, EuropeanSalesData$GDPperHead)
cor(EuropeanSalesData$SalesPerCapita, EuropeanSalesData$UnemploymentRate)
cor(EuropeanSalesData$SalesPerCapita, EuropeanSalesData$EducationSpending)
cor(EuropeanSalesData$SalesPerCapita, EuropeanSalesData$ComputerSales)
# GDPperHead(0.66) and EducationSpending(0.61) are positively correlated with SalesPerCapita. Other attributes has weak relation.

# Aproach 2;
cormat <- round(cor(EuropeanSalesData2),2)
head(cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x =Var1 , y =Var2, fill=value )) + 
  geom_tile(color="white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white" ,midpoint = 0, limit = c(-1,1), space = "Lab" , name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + 
  geom_text(aes(label = value), color = "black" , size = 4) +
  coord_fixed()
# Target1: As shown in the matrix there are high correlation between SalesPerCapita and GDPperHead,EducationSpending.
# Target2: As shown in the matrix there are high correlation between ComputerSales and Population.

# OUTLIERS # 
boxplot(EuropeanSalesData2$GDPperHead, main= "GDPperHead")
boxplot(EuropeanSalesData2$EducationSpending, main= "EducationSpending")
boxplot(EuropeanSalesData2$Population, main= "Population")
boxplot(EuropeanSalesData2$UnemploymentRate, main= "UnemploymentRate")
boxplot(EuropeanSalesData2$ComputerSales, main= "ComputerSales")
boxplot(EuropeanSalesData2$SalesPerCapita, main= "SalesPerCapita")
# ComputerSales (2), EducationSpending (1) , SalesPerCapita (1)

boxplot.stats(EuropeanSalesData2$ComputerSales)$out
# shows the outliers (6824 9887)
Q1 <- quantile(EuropeanSalesData2$ComputerSales, 0.25)
Q3 <- quantile(EuropeanSalesData2$ComputerSales, 0.75)
IQR <- IQR(EuropeanSalesData2$ComputerSales)
lower_limit <- Q1 - 1.5*IQR
upper_limit <- Q3 + 1.5*IQR
which(EuropeanSalesData2$ComputerSales < lower_limit | EuropeanSalesData2$ComputerSales > upper_limit)
# shows the index of outliers (8 ve 21)
EuropeanSalesData3 <- subset(EuropeanSalesData2, EuropeanSalesData2$ComputerSales > lower_limit & EuropeanSalesData2$ComputerSales < upper_limit)
EuropeanSalesData3

boxplot.stats(EuropeanSalesData2$EducationSpending)$out
# shows the outlier (8.4)
Q12 <- quantile(EuropeanSalesData2$EducationSpending, 0.25)
Q32 <- quantile(EuropeanSalesData2$EducationSpending, 0.75)
IQR2 <- IQR(EuropeanSalesData2$EducationSpending)
lower_limit2 <- Q12 - 1.5*IQR2
upper_limit2 <- Q32 + 1.5*IQR2
which(EuropeanSalesData2$EducationSpending < lower_limit2 | EuropeanSalesData2$EducationSpending > upper_limit2)
# shows the index of an outlier (5)
EuropeanSalesData4 <- subset(EuropeanSalesData3, EuropeanSalesData3$EducationSpending > lower_limit2 & EuropeanSalesData3$EducationSpending < upper_limit2)
EuropeanSalesData4

boxplot.stats(EuropeanSalesData2$SalesPerCapita)$out
# shows the outliers (372)
Q13 <- quantile(EuropeanSalesData2$SalesPerCapita, 0.25)
Q33 <- quantile(EuropeanSalesData2$SalesPerCapita, 0.75)
IQR3 <- IQR(EuropeanSalesData2$SalesPerCapita)
lower_limit3 <- Q13 - 1.5*IQR3
upper_limit3 <- Q33 + 1.5*IQR3
which(EuropeanSalesData2$SalesPerCapita < lower_limit3 | EuropeanSalesData2$SalesPerCapita > upper_limit3)
# shows the index of outliers (6)
EuropeanSalesData5 <- subset(EuropeanSalesData4, EuropeanSalesData4$SalesPerCapita > lower_limit3 & EuropeanSalesData4$SalesPerCapita < upper_limit3)
EuropeanSalesData5

glimpse(EuropeanSalesData5)


# Check the heatmap again
cormat2 <- round(cor(EuropeanSalesData5),2)
head(cormat2)
melted_cormat2 <- melt(cormat2)
head(melted_cormat2)
ggplot(data = melted_cormat2, aes(x =Var1 , y =Var2, fill=value )) + 
  geom_tile(color="white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white" ,midpoint = 0, limit = c(-1,1), space = "Lab" , name="Pearson\nCorrelation2") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + 
  geom_text(aes(label = value), color = "black" , size = 4) +
  coord_fixed()


# Distrubution of the Data
ggplot(EuropeanSalesData5, aes(x=SalesPerCapita)) + 
  geom_histogram(aes(y=..density..), binwidth = 5, color ="black", fill= "white") +
  geom_density(alpha=0.2, fill= "#FF6666")
ggplot(EuropeanSalesData5, aes(x=Population)) + 
  geom_histogram(aes(y=..density..), binwidth = 5, color ="black", fill= "white") +
  geom_density(alpha=0.2, fill= "#FF6666")
ggplot(EuropeanSalesData5, aes(x=GDPperHead)) + 
  geom_histogram(aes(y=..density..), binwidth = 10, color ="black", fill= "white") +
  geom_density(alpha=0.2, fill= "#FF6666") 
ggplot(EuropeanSalesData5, aes(x=UnemploymentRate)) + 
  geom_histogram(aes(y=..density..), binwidth = 10, color ="black", fill= "white") +
  geom_density(alpha=0.2, fill= "#FF6666") 
ggplot(EuropeanSalesData5, aes(x=EducationSpending)) + 
  geom_histogram(aes(y=..density..), binwidth = 10, color ="black", fill= "white") +
  geom_density(alpha=0.2, fill= "#FF6666") 
ggplot(EuropeanSalesData5, aes(x=ComputerSales)) + 
  geom_histogram(aes(y=..density..), binwidth = 10, color ="black", fill= "white") +
  geom_density(alpha=0.2, fill= "#FF6666") 

skewness(EuropeanSalesData5)
# If the coefficient of skewness is equal to 0 or approximately close to 0, then the graph is said to be symmetric and data is normally distributed.
# If the coefficient of skewness is less than 0, then the graph is said to be negatively skewed with the majority of data values greater than mean. Most of the values are concentrated on the right side of the graph.
# Population        GDPperHead  UnemploymentRate EducationSpending    SalesPerCapita     ComputerSales 
# 1.15634800        0.01514868        0.61713255        0.42006692        0.74330592        1.11267597

kurtosis(EuropeanSalesData5)
# For normal distribution, kurtosis value is approximately equal to 3.
# If the coefficient of kurtosis is greater than 3, shows a sharp peak on the graph.
# Population        GDPperHead  UnemploymentRate EducationSpending    SalesPerCapita     ComputerSales 
# 2.843448          1.631036          2.430625          2.988237          3.318658          3.266758

#There is a slightly problem with the distribution.

EuropeanSalesData5_log <- log(EuropeanSalesData5)

ggplot(EuropeanSalesData5_log, aes(x=Population )) + 
  geom_histogram(aes(y=..density..), binwidth = 5, color ="black", fill= "white") +
  geom_density(alpha=0.2, fill= "#FF6666")

skewness(EuropeanSalesData5_log)
# Population        GDPperHead  UnemploymentRate EducationSpending    SalesPerCapita     ComputerSales 
# 0.54714573       -0.61413773       -0.08766091       -0.07587471       -0.56754510       -0.35451323

kurtosis(EuropeanSalesData5_log)
# Population        GDPperHead  UnemploymentRate EducationSpending    SalesPerCapita     ComputerSales 
# 1.946457          2.047032          2.302800          2.381886          2.721732          2.730386



################ Linear Regression Modelling for SalesPerCapita #
# Split the Data
X <- EuropeanSalesData5_log[c("Population","GDPperHead","UnemploymentRate","EducationSpending","SalesPerCapita","ComputerSales")]
X

set.seed(123)
sampleX <- sample.split(X, SplitRatio = 0.75)
trainX <- subset(X , sampleX == TRUE)
testX <- subset(X , sampleX == FALSE)

dim(trainX)
# 11 6
dim(testX)
# 6 6

# Model 1 (all included except Country)
LRModelSalesCapital1 <- lm(SalesPerCapita ~ . , data = trainX)
summary(LRModelSalesCapital1)
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# R-squared:(high is good) How well the model is fitting the actual data. The R2 will always increase as more variables are included in the model.
# Adjusted R-squared:(high is good) Any variable without a strong correlation will make adjusted R-squared decrease.
# p-value: (low is good) A variable is considered as insignificant if p-value > 5%. Population and ComputerSales has *** next to p-value. This means they have highly statistical significance.
# t-value : (high is good) A measure of how many standard deviations our coefficient estimate is far away from 0.

LRModelPredSalesCapital1 <- predict(LRModelSalesCapital1, newdata = testX)
rmse1S <- sqrt(sum((exp(LRModelPredSalesCapital1) - testX$SalesPerCapita)^2)/length(testX$SalesPerCapita))
c(RMSE = rmse1S, R2=summary(LRModelSalesCapital1)$r.squared)
# RMSE          R2 
# 122.3445404   0.9999787 
# RMSE: lower the value, better is the performance of the model


# Model 2 (ComputerSales excluded)
LRModelSalesCapital2 <- lm(SalesPerCapita ~ Population + GDPperHead + UnemploymentRate + EducationSpending  , data = trainX)
summary(LRModelSalesCapital2)
# Multiple R-squared:  0.9335,	Adjusted R-squared:  0.8891
# GDPperHead has *** next to p-value. This means GDPperHead has highly statistical significance.

LRModelPredSalesCapital2 <- predict(LRModelSalesCapital2, newdata = testX)
rmse2S <- sqrt(sum((exp(LRModelPredSalesCapital2) - testX$SalesPerCapita)^2)/length(testX$SalesPerCapita))
c(RMSE = rmse2S, R2=summary(LRModelSalesCapital2)$r.squared)
# RMSE          R2 
# 88.2450874  0.9334517 


# Model 3 (Only Population and ComputerSales)
LRModelSalesCapital3 <- lm(SalesPerCapita ~ Population + ComputerSales  , data = trainX)
summary(LRModelSalesCapital3)
# Multiple R-squared:      1,	Adjusted R-squared:      1 

LRModelPredSalesCapital3 <- predict(LRModelSalesCapital3, newdata = testX)
rmse3S <- sqrt(sum((exp(LRModelPredSalesCapital3) - testX$SalesPerCapita)^2)/length(testX$SalesPerCapita))
c(RMSE = rmse3S, R2=summary(LRModelSalesCapital3)$r.squared)
# RMSE          R2 
# 122.9931158   0.9999602 


layout(matrix(c(1,2,3,4),2,2)) 
plot(LRModelSalesCapital1)
plot(LRModelSalesCapital2)
plot(LRModelSalesCapital3)
# Model 1 and Model 3 has same R2 and adjR2 values. I would chose Model 3 because it explains same with less variable.



################ Linear Regression Modelling for ComputerSales #

# Model 1 (all included except Country)
LRModelComputerSales1 <- lm(ComputerSales ~ . , data = trainX)
summary(LRModelComputerSales1)
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# R-squared:(high is good) How well the model is fitting the actual data. The R2 will always increase as more variables are included in the model.
# Adjusted R-squared:(high is good) Any variable without a strong correlation will make adjusted R-squared decrease.
# p-value: (low is good) A variable is considered as insignificant if p-value > 5%. Population and SalesPerCapita has *** next to p-value. This means they have highly statistical significance.
# t-value : (high is good) A measure of how many standard deviations our coefficient estimate is far away from 0.

LRModelPredComputerSales1 <- predict(LRModelComputerSales1, newdata = testX)
rmse1C <- sqrt(sum((exp(LRModelPredComputerSales1) - testX$ComputerSales)^2)/length(testX$ComputerSales))
c(RMSE = rmse1C, R2=summary(LRModelComputerSales1)$r.squared)
# RMSE          R2 
# 3196.8167891    0.9999896
# RMSE: lower the value, better is the performance of the model


# Model 2 (SalesPerCapita excluded)
LRModelComputerSales2 <- lm(ComputerSales ~ Population + GDPperHead + UnemploymentRate + EducationSpending  , data = trainX)
summary(LRModelComputerSales2)
# Multiple R-squared:  0.9675,	Adjusted R-squared:  0.9459 
# GDPperHead and Population have *** next to p-value. This means they have highly statistical significance.

LRModelPredComputerSales2 <- predict(LRModelComputerSales2, newdata = testX)
rmse2C <- sqrt(sum((exp(LRModelPredComputerSales2) - testX$ComputerSales)^2)/length(testX$ComputerSales))
c(RMSE = rmse2C, R2=summary(LRModelComputerSales2)$r.squared)
# RMSE            R2 
# 3051.6380916    0.9675166


# Model 3 (Only Population and SalesPerCapita)
LRModelComputerSales3 <- lm(ComputerSales ~ Population + SalesPerCapita  , data = trainX)
summary(LRModelComputerSales3)
# Multiple R-squared:      1,	Adjusted R-squared:      1 

LRModelPredComputerSales3 <- predict(LRModelComputerSales3, newdata = testX)
rmse3C <- sqrt(sum((exp(LRModelPredComputerSales3) - testX$ComputerSales)^2)/length(testX$ComputerSales))
c(RMSE = rmse3C, R2=summary(LRModelComputerSales3)$r.squared)
# RMSE            R2 
# 3192.8231789    0.9999812 


layout(matrix(c(1,2,3,4),2,2)) 
plot(LRModelComputerSales1)
plot(LRModelComputerSales2)
plot(LRModelComputerSales3)
# Model 1 and Model 3 has same R2 and adjR2 values. I would chose Model 3 because it explains same with less variable.
