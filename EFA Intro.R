#Original code sourced from https://www.r-bloggers.com/exploratory-factor-analysis-in-r/


#Installing the Psych package and loading it
install.packages("psych")
library(psych)
#Loading the dataset
bfi_data=bfi

#Remove rows with missing values and keep only complete cases
bfi_data=bfi_data[complete.cases(bfi_data),]

#Create the correlation matrix from bfi_data
bfi_cor <- cor(bfi_data)

#Factor analysis of the data
factors_data <- fa(r = bfi_cor, nfactors = 6)
#Getting the factor loadings and model analysis
factors_data
