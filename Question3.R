# Question 3: Is a user that must call-in to cancel more likely to generate more revenues?

#loading "data.RData" data frame
load("data.RData")

# Grouping data by sample_id (each sample_id appears only once)
data_grouped=summarise(group_by(data, sample_id, transaction_type, test_group))

# Grouping the transactions per sample_id
trans_sample_id=summarise(group_by(data, sample_id, transaction_amount, test_group))
Revenue=subset(trans_sample_id, transaction_amount<0)     # Extracting the revenue transaction information

Summary_Question3=summarise(group_by(Revenue),
          mean_transaction_test_group=   mean((subset(Revenue, test_group==1))$transaction_amount), # Calculating the mean revenue transaction amount per user in test group
          mean_transaction_control_group=mean(subset(Revenue, test_group==0)$transaction_amount))   # Calculating the mean revenue transaction amount per user in control group

# Printing the summary result
ifelse(Summary_Question3$mean_transaction_test_group>Summary_Question3$mean_transaction_control_group, "The answer to Question 3 is YES","The answer to Question 3 is NO")

#Calculating the statisticl significancy of the results

#Extracting Revenue data frame for Control group
Ctrl=subset(Revenue, test_group==0)
#Extracting Revenue data for test group
Test=subset(Revenue, test_group==1)

#Z.test to calculate the statistical significance level of the results
z.test(Ctrl$transaction_amount, y = Test$transaction_amount, alternative = "greater", mu = 0, sigma.x = sd(Ctrl$transaction_amount),
       sigma.y = sd(Test$transaction_amount), conf.level = 0.95)

