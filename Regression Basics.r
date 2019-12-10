## Note: The original source code is replicated from https://stats.idre.ucla.edu/wp-content/uploads/2019/02/r-code.r

#read the data from csv file
d <- read.csv(
"https://stats.idre.ucla.edu/wp-content/uploads/2019/02/elemapi2v2.csv")

#We can also read the data from the local computer
#we can set the working directory by setwd()
#d <- read.csv("C:/.../regR/elemapi2v2.csv")
class(d)  #class of object d, returns data.frame
names(d)  #retuns names of variables (columns)
dim(d)    #returns number of rows and columns of data frame


str(d)      #returns structure of variables in the data frame d
summary(d)  #Summary statistics of variables in d
help(lm)    #shows R Documentation for function lm()

#multiple regression model of DV api00 and DVs enroll, meals, and full
m1 <- lm(api00 ~ enroll, data = d) 
print(m1)   #Prints coefficients of the model
summary(m1) #Prints summary of the model
r <- cor(d$api00, d$enroll) #correlation coefficient of api00 and enroll
r ^ 2       #this is equal to r-squared in simple regression 
ls(m1)      #list of components in object class lm
m1$coefficients # returns coefficients of the model
m1$fitted.values[1:10] #a vector of fitted values
residuals <- m1$resid  #a vector of residuals
coefficients(m1)     # returns coefficients of the model
confint(m1)          # returns a matrix of Confidence Interval for coefficients

plot(api00 ~ enroll, data = d) #scatter plot of api00 vs. enroll
abline(m1, col = "blue")    #Add regression line to the scatter plot
#adding labels(school number) to the scatter plot
plot(api00 ~ enroll, data = d)
text(d$enroll, d$api00+20, labels = d$snum, cex = .7)
abline(m1, col = "blue")


anova(m1) #Anova table
#multiple regression model of DV api00 and DVs enroll, meals, and full
m2 <- lm(api00 ~  enroll + meals + full, data = d)
summary(m2) #summary of model m2
anova(m2)   #anova table of model m2

sum(anova(m2)$Sum) #sum of RSS and SSreg
(400 - 1) * var(d$api00) #Total sum of squre 

#Standardized regression model
m2.sd <- lm(scale(api00) ~  scale(enroll) + scale(meals) + scale(full), data = d)
summary(m2.sd)#coefficients are standardized



#---------------------------------------------------------------

# install packages for part 2, Regression Diagnostics

#install.packages("car")
#install.packages("alr3")
#install.packages("faraway")

#loading packages into working environment
library(car)
library(faraway)
library(alr3)
#scatter plot matrix from package car
scatterplotMatrix(~ api00 + enroll + meals + full, data =d)
#studentized residuals
er.std <- rstandard(m2)
#plot of studentized residuals
plot(er.std, ylab="Standardized Residual", ylim=c(-3.5,3.5))
#adding horizental lines for cut of value of outliers
abline(h =c(-3,0,3), lty = 2)
#determine which row is outlier(outside of cut of values of -3 and 3)
index <- which(er.std > 3 | er.std < -3)
#label School name to points that are out of bounds 
text(index-20, er.std[index] , labels = d$snum[index])
#print row number of values that are out of bounds
index
#print school names that are out of bounds
d$snum[index]

#Bonferroni p-values for testing outliner
outlierTest(m2)

#a vector containing the diagonal of the 'hat' matrix
h <- influence(m2)$hat

#half normal plot of leverage from package faraway
halfnorm(h, ylab = "leverage")


#the cut of value for cook's distance
cutoff <- 4/((nrow(d)-length(m2$coefficients)-2))
#plot cook's distance
plot(m2, which = 4, cook.levels = cutoff)

#cook's distance, studentized residuals, and leverage in the same plot
influencePlot(m2, main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

#4 diagnostic plots to intentify influential points
infIndexPlot(m2)

#residual vs. fitted value plot for Homoscedasticity
plot(m2$resid ~ m2$fitted.values)
#add horizental line from 0
abline(h = 0, lty = 2)

#residual vs. fitted value and all predictors plus test for curvature
residualPlots(m2)

#residual plot vs. school id 
plot(m2$resid ~ d$snum)
abline(h = 0, lty = 2)

#plot(m2$resid[-1] ~ m2$resid[-400])
#Normal Quantile to Quantile plot
qqnorm(m2$resid)
qqline(m2$resid)


car::vif(m2) #variance inflation factor

#model specification added variable plot
avPlots(m2)

#codes for part 3
#------------------------------------------------------------
class(d$api00)   #class of api00
class(d$yr_rnd)  #class of yr_rnd
class(d$mealcat) #class of mealcat

#summary of api00, yr_rnd, and mealcat
summary(d$api00)   
summary(d$yr_rnd)  
summary(d$mealcat) 

table(d$yr_rnd)    #frequency table for yr_rnd
table(d$mealcat)   #frequency table for mealcat

#creat two new variables as factor and add to the data frame
d$yr_rnd_F <- factor(d$yr_rnd)
d$mealcat_F <- factor(d$mealcat)

class(d$yr_rnd_F)
class(d$mealcat_F)

#levels of factor
levels(d$yr_rnd_F)
levels(d$mealcat_F)

#summary of factor
summary(d$yr_rnd_F)
summary(d$mealcat_F)

#regression of api00 with yr_rnd_F
m3 <- lm(api00 ~ yr_rnd_F, data = d)
summary(m3)

#scatter plot api00 against yr_rnd
plot(api00 ~ yr_rnd, data = d)
abline(m3)  # add regression line to the plot

# mean of api00 when yr_rnd_F is at level "0". school is not year round
mean(d$api00[d$yr_rnd_F == "0"])
# mean of api00 when yr_rnd_F is at level "1". school is year round
mean(d$api00[d$yr_rnd_F == "1"])


#using aggregate to find the mean for each group of year school round
aggregate(api00 ~ yr_rnd_F, FUN=mean, data = d)

#t test for equality of mean of api00 for two group of year round and not year round 
#with equal variance assumption
t.test(api00 ~ yr_rnd_F, data = d, var.equal = TRUE)


#anova table
anova(m3)


#square of t value from the t-test is the same as F value from anova
10.7815 ^ 2

#regression model of api00 against categorical variable mealcat_F with 3 levels
m4 <- lm(api00 ~ mealcat_F, data = d)
summary(m4)

#aggregate the mean of api00 for each category in mealcat_F
aggregate(api00 ~ mealcat_F, FUN=mean, data = d)


#relevel factor mealcat_F and make group "3" as the reference group
d$mealcat_F <- relevel(d$mealcat_F, ref = "3")
m5 <- lm(api00 ~ mealcat_F, data = d)
summary(m5)