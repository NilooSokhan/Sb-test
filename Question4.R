# Question 4: Is a user that must call-in more likely to produce a higher chargeback rate(CHARGEBACKs/REBILLs)?

#loading "data.RData" data frame
load("data.RData")

# Grouping data by sample_id (each sample_id appears only once)
data_grouped=summarise(group_by(data, sample_id, transaction_type, test_group))

#Summarising the data from data_grouped data frame
Summary_Question4=summarise(group_by(data),
            Nbr_REBILL_test_group        =nrow(data_grouped[(data_grouped$transaction_type=="REBILL") & (data_grouped$test_group==1),]),         # Counting the number of rebill cases in test group
            Nbr_CHARGEBACK_test_group    =nrow(data_grouped[(data_grouped$transaction_type=="CHARGEBACK") & (data_grouped$test_group==1),]),     # Counting the number of chargeback cases in test group
            RATIO_test_group             =Nbr_CHARGEBACK_test_group/Nbr_REBILL_test_group,                                                       # Calculating the chargeback/rebill ratio in test group
            Nbr_REBILL_control_group     =nrow(data_grouped[(data_grouped$transaction_type=="REBILL") & (data_grouped$test_group==0),]),         # Counting the number of rebill cases in control group
            Nbr_CHARGEBACK_control_group =nrow(data_grouped[(data_grouped$transaction_type=="CHARGEBACK") & (data_grouped$test_group==0),]),     # Counting the number of chargeback cases in control group
            RATIO_control_group          =Nbr_CHARGEBACK_control_group/Nbr_REBILL_control_group)                                                 # Calculating the chargeback/rebill ratio in control group

# Printing the result
ifelse(Summary_Question4$RATIO_test_group>Summary_Question4$RATIO_control_group, "The answer to Question 4 is YES","The answer to Question 4 is NO")

