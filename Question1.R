# Question 1: What is the approximate probability distribution between the test group and the control group

#loading the required libraries
library(BSDA)
library(dplyr)
library(readr)
library(ggplot2)

# Importing testSamples.CSV file and store it as ts
tS <- read_csv("testSamples.csv")
# Importing transData.CSV file and store it as tD
tD <- read_csv("transData.csv")

#Cleaning data

#Replacing probable negative transaction amount values for REBILL by NA
tD[tD$transaction_type=="REBILL" & tD$transaction_amount <0]<- NA
#Replacing probable positive transaction amount values for REFUND and ECHARGABLE by NA
tD[tD$transaction_type!="REBILL" & tD$transaction_amount >0]<- NA

#Removing Not Avaialble (NA) data 
tS=na.omit(tS)
tD=na.omit(tD)

# Joining tD data frame to tS data frame by commun field (sample_id)
data=merge(x = tD, y = tS, by = "sample_id", all.x = TRUE)

#saving the data in a data frame named "data.RData"
save(data, file = "data.RData")

#Changing the title of the 5th column 
colnames(data)[5] <- "Group"

#Recoding the Group field values 
data$Group=recode(data$Group,"0"="Control group","1"="Test group")

#Plotting density of users based on transaction maount for each of the transaction types
ggplot(data, aes(x=transaction_amount, fill=Group)) +
  geom_histogram(aes(y=..density..), binwidth=2, position=position_jitter(0.1), alpha=0.4)+
  facet_grid(. ~ transaction_type)

#Keeping columns 1, 4 and 5 og the "data" data frame and removing the rest
data=data[c(1,4,5)]

#Calculating the totla amount transacted per user in each test groups
DATA=aggregate(data$transaction_amount, by=list(sample_id=data$sample_id),  FUN=sum)

#Renaming the 2nd column of DATA data frame
colnames(DATA)[2] <- "transaction_amount"

# Joining DATA data frame to tS data frame by commun field (sample_id)
DATA=merge(x = DATA, y = tS, by = "sample_id", all.x = TRUE)

#Renaming the 3rd column of DATA data frame
colnames(DATA)[3] <- "Group"

#Recoding the Group field values 
DATA$Group=recode(DATA$Group,"0"="Control group","1"="Test group")

#Plotting the distribution 
ggplot(DATA, aes(x=transaction_amount, fill=Group)) +
  geom_histogram(aes(y=..density..), binwidth=5, position="dodge", alpha=0.7)

