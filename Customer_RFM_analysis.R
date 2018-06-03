#import the data

library(readr)
data <- read_csv("C:/Users/manit_patel/Downloads/Spring 18/R SCRIPTS REPOSITORY/KAGGLE PROJECTS/RFM_analysis/data.csv/data.csv")
View(data)

# Exploring missing values

colSums(is.na(data))

#Removing description 

data = data[ , -3]

#removing missing customerID

data=na.omit(data)

colSums(is.na(data))


#loading the libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)

install.packages("DT")

library(DT)
library(tidyr)

install.packages("knitr")

library(knitr)

install.packages("rmarkdown")

library(rmarkdown)

#Data

str(data)
summary(data)

#replacing negative values of Quantity and price by NA's####

data= data %>%
  mutate(Quantity= replace(Quantity, Quantity <= 0 , NA), UnitPrice= replace(UnitPrice, UnitPrice <=0, NA))


data= na.omit(data)

#convert all character data types into factors
data = data %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))

#Feature engineering
data$total_dolar = data$Quantity*data$UnitPrice

glimpse(data)


#calculate RFM

df_RFM = data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequenci=n_distinct(InvoiceNo), monitery= sum(total_dolar)/n_distinct(InvoiceNo)) 

summary(df_RFM)

head(df_RFM)

kable(head(df_RFM))

#Recency: How recently did the customer purchase?
hist(df_RFM$recency)
#Frequency: How often do they purchase?
hist(df_RFM$frequenci, breaks = 50)
# Monitery: 
hist(df_RFM$monitery, breaks = 50)

#high frequency: Loyal Customers
#High Monetary value and high recency: High Value customers
#High Monetary Value and low recency: Potential Customers


#Bining Frequency
df_RFM$F_binned= df_RFM$frequenci

table(df_RFM$F_binned)
quantile(df_RFM$F_binned)

df_RFM$F_binned[df_RFM$F_binned >= 5] = 5

class(df_RFM$F_binned)


#Binning Monetary
df_RFM$M_binned=df_RFM$monitery

quantile(df_RFM$monitery)
table(df_RFM$monitery)

df_RFM$M_binned[df_RFM$M_binned <= 100] =1
df_RFM$M_binned[df_RFM$M_binned > 100 & df_RFM$M_binned <= 178.62] = 2
df_RFM$M_binned[df_RFM$M_binned > 178.62 & df_RFM$M_binned <= 293] = 3
df_RFM$M_binned[df_RFM$M_binned > 293 & df_RFM$M_binned <= 430.1137] = 4
df_RFM$M_binned[df_RFM$M_binned > 430.1137] = 5

table(df_RFM$M_binned)

#Binning Recency
df_RFM$R_binned=df_RFM$recency

class(df_RFM$R_binned)


table(df_RFM$R_binned)
quantile(df_RFM$R_binned)

df_RFM$R_binned[df_RFM$R_binned <= 30] =1
df_RFM$R_binned[df_RFM$R_binned > 30 & df_RFM$R_binned <= 40] = 2
df_RFM$R_binned[df_RFM$R_binned > 40 & df_RFM$R_binned <= 73] = 3
df_RFM$R_binned[df_RFM$R_binned > 73 & df_RFM$R_binned <= 164] = 4
df_RFM$R_binned[df_RFM$R_binned > 164 & df_RFM$R_binned <= 396] = 5

table(df_RFM$R_binned)
table(df_RFM$F_binned)
table(df_RFM$M_binned)

#calculating customer value
df_RFM$customer_value= df_RFM$R_binned + df_RFM$F_binned + df_RFM$M_binned

# calculating customer_score
df_RFM$customer_score= df_RFM$customer_value/3




