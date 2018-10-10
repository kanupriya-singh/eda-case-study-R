#----------------------------- Load libraries ----------------------------
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)

# ---------------------------- Load the data  ----------------------------
loan_data <- read.csv("loan.csv", stringsAsFactors = F)

#  ---------------------------- Clean the data  ----------------------------
# Check structure of data
#View(loan_data)
str(loan_data)
head(loan_data)

#Check if there are any duplicate rows 
nrow(loan_data)
nrow(unique(loan_data))
#No Duplicate Rows Present

#Remove unnecessary columns
#Many 0 and NA values in later columns. Let us see how many have non-zero and non-NA values
length(which(loan_data$collections_12_mths_ex_med!=0)) #All 0 so this column can be discarded
length(which(!is.na(loan_data$mths_since_last_major_derog))) #All NA so this column can be discarded
length(which(loan_data$application_type!="INDIVIDUAL")) #All "Individual" so this column can be discarded
length(which(!is.na(loan_data$annual_inc_joint))) #All NA so this column can be discarded
length(which(!is.na(loan_data$dti_joint))) #All NA so this column can be discarded
length(which(!is.na(loan_data$verification_status_joint))) #All NA so this column can be discarded
length(which(loan_data$acc_now_delinq!=0)) #All 0 so this column can be discarded
length(which(!is.na(loan_data$tot_coll_amt))) #All NA so this column can be discarded
length(which(!is.na(loan_data$tot_cur_bal))) #All NA so this column can be discarded
length(which(!is.na(loan_data$open_acc_6m))) #All NA so this column can be discarded
length(which(!is.na(loan_data$open_il_6m))) #All NA so this column can be discarded
length(which(!is.na(loan_data$open_il_12m))) #All NA so this column can be discarded
length(which(!is.na(loan_data$open_il_24m))) #All NA so this column can be discarded
length(which(!is.na(loan_data$mths_since_recent_il))) #All NA so this column can be discarded
length(which(!is.na(loan_data$total_bal_il))) #All NA so this column can be discarded
length(which(!is.na(loan_data$il_util))) #All NA so this column can be discarded
length(which(!is.na(loan_data$open_rv_12m))) #All NA so this column can be discarded
length(which(!is.na(loan_data$open_rv_24m))) #All NA so this column can be discarded
length(which(!is.na(loan_data$max_bal_bc))) #All NA so this column can be discarded
length(which(!is.na(loan_data$all_util))) #All NA so this column can be discarded
length(which(!is.na(loan_data$total_rev_hi_lim))) #All NA so this column can be discarded
length(which(!is.na(loan_data$inq_fi))) #All NA so this column can be discarded
length(which(!is.na(loan_data$total_cu_tl))) #All NA so this column can be discarded
length(which(!is.na(loan_data$inq_last_12m))) #All NA so this column can be discarded
length(which(!is.na(loan_data$acc_open_past_24mths))) #All NA so this column can be discarded
length(which(!is.na(loan_data$avg_cur_bal))) #All NA so this column can be discarded
length(which(!is.na(loan_data$bc_open_to_buy))) #All NA so this column can be discarded
length(which(!is.na(loan_data$bc_util))) #All NA so this column can be discarded
length(which(loan_data$chargeoff_within_12_mths!=0)) #All NA so this column can be discarded
length(which(loan_data$delinq_amnt!=0)) #All NA so this column can be discarded
length(which(!is.na(loan_data$mo_sin_old_il_acct))) #All NA so this column can be discarded
length(which(!is.na(loan_data$mo_sin_old_rev_tl_op))) #All NA so this column can be discarded
length(which(!is.na(loan_data$mo_sin_rcnt_rev_tl_op))) #All NA so this column can be discarded
length(which(!is.na(loan_data$mo_sin_rcnt_tl))) #All NA so this column can be discarded
length(which(!is.na(loan_data$mort_acc))) #All NA so this column can be discarded
length(which(!is.na(loan_data$mths_since_recent_bc))) #All NA so this column can be discarded
length(which(!is.na(loan_data$mths_since_recent_bc_dlq))) #All NA so this column can be discarded
length(which(!is.na(loan_data$mths_since_recent_inq))) #All NA so this column can be discarded
length(which(!is.na(loan_data$mths_since_recent_revol_delinq))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_accts_ever_120_pd))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_actv_bc_tl))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_actv_rev_tl))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_bc_sats))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_bc_tl))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_il_tl))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_op_rev_tl))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_rev_accts))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_rev_tl_bal_gt_0))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_sats))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_tl_120dpd_2m))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_tl_30dpd))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_tl_90g_dpd_24m))) #All NA so this column can be discarded
length(which(!is.na(loan_data$num_tl_op_past_12m))) #All NA so this column can be discarded
length(which(!is.na(loan_data$pct_tl_nvr_dlq))) #All NA so this column can be discarded
length(which(!is.na(loan_data$percent_bc_gt_75))) #All NA so this column can be discarded

length(which(loan_data$pub_rec_bankruptcies!=0)) #Has to be retained

length(which(loan_data$tax_liens!=0)) #All 0 so this column can be discarded
length(which(!is.na(loan_data$tot_hi_cred_lim))) #All NA so this column can be discarded
length(which(!is.na(loan_data$total_bal_ex_mort))) #All NA so this column can be discarded
length(which(!is.na(loan_data$total_bc_limit))) #All NA so this column can be discarded
length(which(!is.na(loan_data$total_il_high_credit_limit))) #All NA so this column can be discarded
length(which(!is.na(loan_data$tot_hi_cred_lim))) #All NA so this column can be discarded

#Store selected data in a new data frame 
loan_data_new <- loan_data[,c(1:49, 106)]

#Remove other unnecessary columns

colnames(loan_data_new)

#"id" and "member_id" are distinct to each loan and can be dropped
#"url" is distinct to the each loan and can be dropped
#"emp_title" is a descriptive column and can be dropped
#"desc" is a descriptive column and can be dropped
#"title" is a descriptive column and can be dropped
#"zip_code" even though categorical; has too many values and can't be used for analysis

#Find unique values of "pymnt_plan"
unique(loan_data_new$pymnt_plan)
#All are "n" and thus this column can be dropped

#Find unique values of "initial_list_status"
unique(loan_data_new$initial_list_status)
#All are "f" and thus this column can be dropped

#Below variables are about the current loan status and thus can be dropped for this analysis:
#"out_prncp", "out_prncp_inv", #"total_pymnt", "total_pymnt_inv", "recoveries", "total_rec_prncp", 
#"total_rec_int", "total_rec_late_fee", "collection_recovery_fee",
#"last_pymnt_d", "last_pymnt_amnt", "next_pymnt_d"

#Remove the above identified columns
drops <- c("id", "member_id", "emp_title", "url", "desc", "title", "zip_code", "pymnt_plan", "initial_list_status", "out_prncp", "out_prncp_inv", "total_pymnt", "total_pymnt_inv", "recoveries", "total_rec_prncp", "total_rec_int", "total_rec_late_fee", "collection_recovery_fee", "last_pymnt_d", "last_pymnt_amnt", "next_pymnt_d")
loan_data_new <- loan_data_new[, !(names(loan_data_new) %in% drops)]

colnames(loan_data_new)

#------------------ Correct the formats and derive new metrics ----------------------

#Correct the date format for issue_d
loan_data_new$issue_date <- ifelse(substr(loan_data_new$issue_d,1,1)==1, loan_data_new$issue_d, paste("0",loan_data_new$issue_d,sep = ""))
loan_data_new$issue_date <- paste(loan_data_new$issue_date, "01", sep = "-")
loan_data_new$issue_date <- as.Date(loan_data_new$issue_date, format = "%y-%b-%d")

#Introduce a column unfunded_amnt = loan_amnt - funded_amnt_inv
#This column is introduced to quantify when the investors are not funding the full amount and if this has any impact on loan default
loan_data_new$unfunded_amnt <- loan_data_new$loan_amnt - loan_data_new$funded_amnt_inv

#remove percentage sign from interest rate and save it as a numeric data
loan_data_new$interest_rate <- as.numeric(sub("%", "", loan_data_new$int_rate))

#remove percentage sign from revol_util and save it as a numeric data
loan_data_new$revol_util <- as.numeric(sub("%", "", loan_data_new$revol_util))

#If loan_status is "Charged Off" set flag "defaulted" as Y; otherwise as "N"
loan_data_new$defaulted <- ifelse(loan_data_new$loan_status=="Charged Off","Y","N")

#If pub_rec > 0 set flag "pub_rec_exists" as Y
loan_data_new$pub_rec_exists <- ifelse(loan_data_new$pub_rec>0,"Y","N")

#Bring all earliest_cr_line to the same format as years after 2000 are in the format "1-Jan" and others in "Jan-00"
#Identify the format
loan_data_new$ecl_format <- ifelse(str_detect(substr(loan_data_new$earliest_cr_line,1,1), "([1-9])"), "1", "0")
#Insert zero at first position whenever required
loan_data_new$earliest_cr_line <- ifelse(loan_data_new$ecl_format=="1", paste("0", loan_data_new$earliest_cr_line, sep = ""), loan_data_new$earliest_cr_line)
#Bring all dates to same format
loan_data_new$earliest_cr_line <- ifelse(loan_data_new$ecl_format=="1", substr(paste(loan_data_new$earliest_cr_line, substr(loan_data_new$earliest_cr_line, 1, 2), sep = "-"), 4, 9), loan_data_new$earliest_cr_line)
#paste "01" (date) at the beginning of all dates
loan_data_new$earliest_cr_line <- paste("01", loan_data_new$earliest_cr_line, sep = "-")
#store it as a date
loan_data_new$ecl_date <- as.Date(loan_data_new$earliest_cr_line, format = "%d-%b-%y")
#Check if all dates are converted correctly
length(which(is.na(loan_data_new$ecl_date)))

#Some ecl_dates are extending to beyong current date, to correct this:
loan_data_new$ecl_date <- as.Date(ifelse(loan_data_new$ecl_date > "2018-07-31", format(loan_data_new$ecl_date, "19%y-%m-%d"), format(loan_data_new$ecl_date)))

#Get the year
loan_data_new$ecl_year <- format(loan_data_new$ecl_date,"%Y")
#Get the decade
loan_data_new$ecl_decade <- ifelse(loan_data_new$ecl_year>=2000, "2000s",
                                   ifelse(loan_data_new$ecl_year>=1940 & loan_data_new$ecl_year<1950, "1940s",
                                          ifelse(loan_data_new$ecl_year>=1950 & loan_data_new$ecl_year<1960, "1950s",
                                                 ifelse(loan_data_new$ecl_year>=1960 & loan_data_new$ecl_year<1970, "1960s",
                                                        ifelse(loan_data_new$ecl_year>=1970 & loan_data_new$ecl_year<1980, "1970s",
                                                               ifelse(loan_data_new$ecl_year>=1980 & loan_data_new$ecl_year<1990, "1980s", "1990s"))))))

#Check annual income range
range(loan_data_new$annual_inc) 

#Let's remove the outliers and create a subset called income_outlier_removed
inc95 <- quantile(loan_data_new$annual_inc, probs = 0.95) #to treat outliers
income_outlier_removed <- subset(loan_data_new, annual_inc<=inc95)

#correct format for last_credit_pull_d
loan_data_new$last_credit_pull_d <- as.Date(paste("01", loan_data_new$last_credit_pull_d, sep = "-"), format = "%d-%y-%b")

colnames(loan_data_new)

#--------------------------------- Preliminary analysis ---------------------------------------------#
#Tally loan counts by loan_status - to see how many loans are being defaulted
loan_data_new %>% group_by(loan_status) %>% tally()
ggplot(loan_data_new, aes(loan_status, fill=loan_status)) + geom_bar() + geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)), y= ((..count..)/sum(..count..))), stat="count", vjust = -.25)
#5627 loans out of 39717 have been "Charged Off" ~ around 14%

#Loan count by year
loan_data_new %>% group_by(format(loan_data_new$issue_date,"%Y")) %>% tally()
ggplot(loan_data_new, aes(x=format(loan_data_new$issue_date,"%Y"), fill=format(loan_data_new$issue_date,"%Y"))) + geom_bar() + geom_text(stat='count', aes(label=..count..))
#Numbers of loans taken are increasing every year almost exponentially

#Loan count by grade 
loan_data_new %>% group_by(loan_data_new$grade) %>% tally()
ggplot(loan_data_new, aes(x=grade, fill=grade)) + geom_bar() + geom_text(stat='count', aes(label=..count..))
#Most of the loans are in A, B or C grades with very few in the F or G grade

#Loan count by interest rate
ggplot(loan_data_new, aes(x=interest_rate)) + geom_histogram(binwidth = 1)
ggplot(loan_data_new, aes(x=interest_rate, fill=grade)) + geom_histogram(binwidth = 1) + labs(x="Grade", y= "Number of Loans")
#Loans with better grades have lower interest rates. Let's analyse this further

#Spread of interest rates for various grades
ggplot(loan_data_new, aes(x=grade, y=interest_rate)) + geom_boxplot(aes(fill=grade)) + labs(x="Grade", y= "Interest Rate")

#What is the average interest rate by various grades / sub-grades?
ir_by_grade <- loan_data_new %>% group_by(grade) %>% summarise(mean_interest_rate = mean(interest_rate, na.rm = T))
ir_by_subgrade <- loan_data_new %>% group_by(sub_grade) %>% summarise(mean_interest_rate = mean(interest_rate, na.rm = T))
#View(ir_by_grade)
#View(ir_by_subgrade)
#Interest rate increases as the Grades / Sub-Grades get worse. 

#What is the annual income of the borrowers?
ggplot(income_outlier_removed, aes(x=annual_inc)) + geom_histogram(binwidth = 10000) + labs(x="Annual Income", y= "Number of Loans")

#What is the debt-to-income ratio of the borrowers?
ggplot(loan_data_new, aes(x=dti)) + geom_histogram(binwidth = 1) 
#People wit dti>25 get very few loans

#Loan count by purpose
ggplot(loan_data_new, aes(x=purpose, fill=purpose)) + geom_bar() + geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)), y= ((..count..)/sum(..count..))), stat="count", vjust = -.25)
#Around 60% loans are for the purpose of debt consolidation or credit cards.

#Loan count by states
ggplot(loan_data_new, aes(x=addr_state)) + geom_bar() + geom_text(stat='count', aes(label=..count..))
#Top 5 states: California, New York, Texas, Florida, New Jersey

#Loan count by term
ggplot(loan_data_new, aes(x=term, fill=term)) + geom_bar() + geom_text(stat='count', aes(label=..count..))
#Loans with a term of 3 years are almost 3 times the 5-year loans

#-------------------------------- Analysis of Loan Attributes ---------------------------------------------#

#Objective: Understand how Consumer Attributes and Loan Attributes influence the tendency to default
#For this, first let us observe loan_status against various loan attributes 
#List of loan attributes: loan_amnt, funded_amnt, funded_amnt_inv, installment, term, int_rate, grade, sub_grade, unfunded_amnt

#loan_amnt - The listed amount of the loan applied for by the borrower.
ggplot(loan_data_new, aes(x=loan_amnt, fill=defaulted)) + geom_histogram(binwidth=4000, position = "fill")
#There is an increase in loan defaults % as loan amount increases
#So loan_amnt is a strong indicator for loan default

cor(loan_data_new$loan_amnt, loan_data_new$funded_amnt)
#Funded data and loan amount have a correlation of 0.98
cor(loan_data_new$loan_amnt, loan_data_new$funded_amnt_inv)
#Loan amount and amount funded by investors have a correlation of 0.94

#funded_amnt - The total amount committed to that loan at that point in time.
ggplot(loan_data_new, aes(x=funded_amnt, fill=defaulted)) + geom_histogram(binwidth=4000, position = "fill")
#Shows a plot similar to that of loan_amnt

#funded_amnt_inv - The total amount committed by investors for that loan at that point in time.
ggplot(loan_data_new, aes(x=funded_amnt_inv, fill=defaulted)) + geom_histogram(binwidth=4000, position = "fill")
#data is inconlusive (though there is a spike at 35000)

#installment - The monthly payment owed by the borrower if the loan originates.
ggplot(loan_data_new, aes(x=installment, fill=defaulted)) + geom_histogram(binwidth = 100)
ggplot(loan_data_new, aes(x=installment, fill=defaulted)) + geom_histogram(binwidth = 100, position = "fill")
#Data is inconclusive

#unfunded_amnt (derived metric) - When the funded_amnt_inv is less than the loan_amnt does it have any implication on the defaulters' %?
ggplot(loan_data_new, aes(x=unfunded_amnt, fill=defaulted)) + geom_histogram(binwidth = 4000, position="fill")
#There seems to be a rise in loan defaults where the amount committed by the investors is less than the requested amount
#So funded_amnt_inv is a strong indicator for loan default

#interest_rate - Interest Rate on the loan
ggplot(loan_data_new, aes(x=interest_rate, fill=defaulted)) + geom_histogram(binwidth = 1, position = "fill")
#With % with higher interest rates, there is a high chance of default ("Charged Off" cases increase)
#But this also means these are the borrowers who had a history of defaulting and were thus assigned a bad grade and offered a worse interest rate
#So int_rate is a strong indicator for loan default

#Interest rate spread for defaulters vs non-defaulters
ggplot(loan_data_new, aes(x=defaulted, y=interest_rate)) + geom_boxplot()
#On an average defaulters have a higher interest rate

#term- The number of payments on the loan. Values are in months and can be either 36 or 60.
ggplot(loan_data_new, aes(x=term, fill=defaulted)) + geom_bar(position = "fill")
#Almost double the Charged-Off cases when term is 5 years rather than 3 years
#So term is a strong indicator for loan default

#grade - LC assigned loan grade
ggplot(loan_data_new, aes(x=grade, fill=defaulted)) + geom_bar(position = "fill")
#With worse grades, there is a higher chance that the person will default
#So grade is a strong indicator for loan default

#sub_grade - LC assigned loan sub-grade
ggplot(loan_data_new, aes(x=sub_grade, fill=defaulted)) + geom_bar(position = "fill")
#With worse sub-grades, there is a higher chance that the person will default
#So sub-grade is a strong indicator for loan default

#Purpose - A category provided by the borrower for the loan request.
ggplot(loan_data_new, aes(x=purpose, fill=defaulted)) + geom_bar(position = "fill")
#Loans taken for different purposes have different rates of defaulting; 
#specifically loans taken for small businesses have a high rate of default.
#So purpose is a strong indicator for loan default

#-----------------------------------Consumer Attributes-----------------------------------------#

#Let us now observe loan_status against various consumer attributes
#List of consumer attributes: emp_length, home_ownership, annual_inc, verification_status, 
#dti, earliest_cr_line (we are going to use derived column ecl_year or ecl_decade), 
#inq_last_6mths, delinq_2yrs, mths_since_last_delinq, mths_since_last_record, open_acc, pub_rec, 
#revol_bal, revol_util, total_acc, last_credit_pull_d

#emp_length - Employment length in years.
ggplot(loan_data_new, aes(x=emp_length, fill=defaulted)) + geom_bar(position = "fill")
#Years of employments don't make a difference; 
#but borrowers who chose to not disclose their employed years; tend to default more
#So emp_length = n/a should be focused on

#home_ownership - The home ownership status provided by the borrower during registration. 
ggplot(loan_data_new, aes(x=home_ownership, fill=defaulted)) + geom_bar()
ggplot(loan_data_new, aes(x=home_ownership, fill=defaulted)) + geom_bar(position = "fill")
#We can ignore "OTHER" or "NONE" as there is insufficient data
#Otherwise there doesn't seem to be much effect on loan default

#annual_inc - The self-reported annual income provided by the borrower during registration.
ggplot(income_outlier_removed, aes(x=annual_inc, fill=defaulted)) + geom_histogram(binwidth = 10000, position = "stack")
ggplot(income_outlier_removed, aes(x=annual_inc, fill=defaulted)) + geom_histogram(binwidth = 10000, position = "fill")
#Borrowers with lower income tend to default a lot more than borrowers in the higher income grid 
#So annual_inc is an strong indicator of loan default

#verification_status - Indicates if income was verified by LC, not verified, or if the income source was verified
ggplot(loan_data_new, aes(x=verification_status, fill=defaulted)) + geom_bar(position = "fill") 
#Counter-intuitively, verified borrowers are defaulting slightly more 
#But that doesn't mean that we can start giving weightage to non-verified borrowers
#So this can't be considered an indicator

#dti - A ratio calculated using the borrower's total monthly debt payments on the total debt obligations, excluding mortgage and the requested LC loan, divided by the borrower's self-reported monthly income
ggplot(loan_data_new, aes(x=dti, fill=defaulted)) + geom_histogram(binwidth = 2)
ggplot(loan_data_new, aes(x=dti, fill=defaulted)) + geom_histogram(binwidth = 2, position = "fill")
#We see that as dti increases; so do the chances of the borrower defaulting
#(There is a sudden drop in borrowers with dti >= 25; and at that point the defaulting % also decreases)
#dti seems to be a strong indicator of loan default

#pub_rec - Number of derogatory public records (we are using derived metric pub_rec_exists)
ggplot(loan_data_new, aes(x=pub_rec_exists, fill=defaulted)) + geom_bar(position = "fill")
#Borrowers with a public record are more likely to default
#So this is an important indicator of loan default

#pub_rec_bankruptcies - Number of public record bankruptcies
ggplot(loan_data_new, aes(x=pub_rec_bankruptcies, fill=defaulted)) + geom_bar(position = "fill")
#Borrowers with more publically recorded bankruptcies are indeed more likely to default
#So this is an important indicator of loan default

#total_acc - The total number of credit lines currently in the borrower's credit file
ggplot(loan_data_new, aes(x=total_acc, fill=defaulted)) + geom_bar(position = "stack")
ggplot(loan_data_new, aes(x=total_acc, fill=defaulted)) + geom_histogram(binwidth=4, position = "fill")
#no clear trend

#open_acc - The number of open credit lines in the borrower's credit file.
ggplot(loan_data_new, aes(x=open_acc, fill=defaulted)) + geom_bar(position = "stack")
ggplot(loan_data_new, aes(x=open_acc, fill=defaulted)) + geom_histogram(binwidth=4, position = "fill")
#if a borrower has more than 25 open credit lines; chance of him defaulting is higher
length(which(loan_data_new$open_acc>25))
#Not enough such borrowers. So this attribute is not a strong indicator

#delinq_2_yrs - The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years
ggplot(loan_data_new, aes(x=delinq_2yrs, fill=defaulted)) + geom_bar(position = "stack")
ggplot(loan_data_new, aes(x=delinq_2yrs, fill=defaulted)) + geom_bar(position = "fill")
length(which(loan_data_new$delinq_2yrs>=8))
#no clear trend

#mths_since_last_delinq - The number of months since the borrower's last delinquency.
ggplot(loan_data_new, aes(x=mths_since_last_delinq, fill=defaulted)) + geom_bar(position = "fill")
#Data is inconclusive

#inq_last_6mths - The number of inquiries in past 6 months (excluding auto and mortgage inquiries)
ggplot(loan_data_new, aes(x=inq_last_6mths, fill=defaulted)) + geom_bar(position = "stack")
ggplot(loan_data_new, aes(x=inq_last_6mths, fill=defaulted)) + geom_bar(position = "fill")
length(which(loan_data_new$inq_last_6mths>4))
#no clear trend

#revol_util - Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit.
ggplot(loan_data_new, aes(x=revol_util, fill=defaulted)) + geom_histogram(binwidth=10, position = "fill")
#Borrowers with higher Revolving line utilization rate are defaulting more 
#So this is a strong indicator of loan default

#revol_bal - Total credit revolving balance
ggplot(loan_data_new, aes(x=revol_bal, fill=defaulted)) + geom_histogram(position = "fill")
#Data is inconclusive

#earliest_cr_line - The month the borrower's earliest reported credit line was opened
#We are using derived metric ecl_decade
ggplot(loan_data_new, aes(x=ecl_decade, fill=defaulted)) + geom_bar(position = "stack")
ggplot(loan_data_new, aes(x=ecl_decade, fill=defaulted)) + geom_bar(position = "fill")
#Borrowers with more recent "earliest credit line" seem to be defaulting slightly more.
#We can ignore borrowers having earliest credit lines in the 40s, 50s and 60s due to less number of records
#So this is a indicator of loan default, albeit not a strong one

#addr_state - The state provided by the borrower in the loan application
ggplot(loan_data_new, aes(x=addr_state, fill=defaulted)) + geom_bar(position = "stack")
ggplot(loan_data_new, aes(x=addr_state, fill=defaulted)) + geom_bar(position = "fill")
#Different states have different default %
#So this can be considered while calculating chances of default

#last_credit_pull_d - The most recent month LC pulled credit for this loan
ggplot(loan_data_new, aes(x=format(loan_data_new$last_credit_pull_d,"%Y"), fill=defaulted)) + geom_bar(position = "fill")
#no clear trend in data

#----------------------------- Bivariate Analysis -----------------------------------

#annual_inc and loan_amnt
ggplot(income_outlier_removed, aes(x=annual_inc, y=loan_amnt, color=defaulted)) + geom_point(alpha=0.1) 
#We can see more defaults when loan amount to income ratio is big
#Let's analyse this further by creating a new derived metric loan_inc_ratio
income_outlier_removed$loan_inc_ratio <- income_outlier_removed$loan_amnt / income_outlier_removed$annual_inc
ggplot(income_outlier_removed, aes(x=loan_inc_ratio, fill=defaulted)) + geom_histogram(position = "fill") 
#When income is less and loan_amnt is more; it results in higher loan default %

#term and interest_rate
ggplot(loan_data_new,aes(x=term,y=interest_rate)) + geom_boxplot(aes(fill=defaulted)) 
#The average interest rate is much higher for a 5-year loan
#For both 3-year and 5-year, defaulters had a higher interest_rate

#grade and interest_rate
ggplot(loan_data_new,aes(x=grade, y=interest_rate)) + geom_boxplot(aes(fill=defaulted)) 
#The average interest rate is much higher for a 5-year loan
#For both 3-year and 5-year, defaulters had a higher interest_rate

#annual_inc and grade
ggplot(income_outlier_removed,aes(x=grade,y=annual_inc)) + geom_boxplot(aes(fill=defaulted)) 
#Defaulters have the lower income when compared to non-defaulters irrespective of grades

#Relation of grades to term
ggplot(loan_data_new, aes(x = grade, y = term)) + geom_jitter(alpha=0.1, width = 0.45, aes(color=defaulted)) 
#Borrowers with "A" grade take very few loans with a term of 5 year
#Borrowers with "G" grade get fewer loans with a term of 3 year


#------------------- Export data for plotting in Tableau ---------------------------
write.csv(income_outlier_removed, file="output.csv")
