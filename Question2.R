# Question 2: Is a user that must call-in to cancel more likely to generate at least 1 addition REBILL?

#loading the required libraries
library(readr)

#loading "data.RData" data frame
load("data.RData")

# Importing testSamples.CSV file and store it as ts
tS <- read_csv("testSamples.csv")

# Grouping data by sample_id (each sample_id appears only once)
data_grouped=summarise(group_by(data, sample_id, transaction_type, test_group))

# Creating a table to fill with the results
Summary_Question2=summarise(group_by(data_grouped),
P_test_group=100*sum(data_grouped$transaction_type=="REBILL" & data_grouped$test_group==1)/nrow(data_grouped),    # Calculating the probability to generate at least 1 addition REBILL in test group
P_control_group=100*sum(data_grouped$transaction_type=="REBILL" & data_grouped$test_group==0)/nrow(data_grouped)) # Calculating the probability to generate at least 1 addition REBILL in control group

# Printing the result
ifelse(Summary_Question2$P_test_group>Summary_Question2$P_control_group, "The answer to Question 2 is YES","The answer to Question 2 is NO")

#Calculating the statisticl significancy of the results

#Keeping columns 1, 4 and 5 og the "data" data frame and removing the rest
data=data[c(1,4,5)]

#Calculating the totla amount transacted per user in each test groups
DATA=aggregate(data$transaction_amount, by=list(sample_id=data$sample_id),  FUN=sum)

#Renaming the 2nd column of DATA data frame
colnames(DATA)[2] <- "transaction_amount"

# Joining DATA data frame to tS data frame by commun field (sample_id)
DATA=merge(x = DATA, y = tS, by = "sample_id", all.x = TRUE)

#Extracting data for Control group
Ctrl=subset(DATA, test_group==0)
#Extracting data for test group
Test=subset(DATA, test_group==1)

#Z.test to calculate the statistical significance level of the results
z.test(Ctrl$transaction_amount, y = Test$transaction_amount, alternative = "greater", mu = 0, sigma.x = sd(Ctrl$transaction_amount),
       sigma.y = sd(Test$transaction_amount), conf.level = 0.95)

