####Project Data Cleaning#

setwd("~/Work/Project File/Project Data")

read.csv("TwinDetails_060917.csv")

twin_details_data <- read.csv("TwinDetails_060917.csv")

#calculating date
colnames(twin_details_data[4])

yrborn = twin_details_data[4]

library(data.table)
library(eeptools)
library(lubridate)

library(data.table)
library(lubridate)

##test
DOB1<-"07/12/1940"
as.numeric(format(as.Date(Sys.time()),format="%Y"))-as.numeric(format(as.Date(DOB1,format="%m/%d/%Y"),format="%Y"))
class(yrborn)
age_matrix = matrix(ncol = 1, nrow = nrow(yrborn))


for (i in 1:dim(yrborn)[1])
{
  age_matrix[i,] <- as.numeric(format(as.Date(Sys.time()),format="%Y"))-as.numeric(format(as.Date(yrborn[i,],format="%d/%m/%Y"),format="%Y"))
}

tail(yrborn)
head(yrborn)
head(age_matrix)

###merging age into dataset

library(tibble)


twin_details_data = add_column(twin_details_data, age_matrix, .after = "DATE_BORN")

colnames(twin_details_data)[5] = "Age_today"

colnames(twin_details_data)[2] = "ParticipantID"

head(twin_details_data)

##loading in the data fixed by ruth

new_fill = read.csv("BCQ_LCQ_combined_No_Skip_all_PID.csv")

dim(new_fill)

colnames(new_fill)

pheno_codes = colnames(new_fill)

#loading in phenotype q names 

code_Q_names =read.csv("Phenotype_Q_and_code.csv")

dim(code_Q_names)

code_Q_names

pheno_codes = pheno_codes[-c(1,52,53)]

pheno_codes = pheno_codes[-c(1)]

colnames(new_fill)

#putting cols in the correct order to correspond with phenotype codes

new_fill_correct_order = new_fill[c(1,53,52,2:37,54,38:50,55,51)]

colnames(new_fill_correct_order[1:3])

colnames(new_fill_correct_order)

dim(new_fill_correct_order)

code_Q_names[1]

new_fill_correct_order_cols = colnames(new_fill_correct_order[-c(1:3)])

duplicate_check = data.frame(code_Q_names, new_fill_correct_order_cols)

dim(duplicate_check)

###use phenotype codes for now, however use duplicate check or code_Q_names to cross reference
#phenotype codes have erradicated the fact that we initially had duplicate questions, which has been an issue since the start of this analysis 

#####Merging twin details with Alpha Diversity####

Alpha_diversity = read.csv("alpha_diversity_20180209.tsv.csv")

twin_details_AlphaD = merge(twin_details_data, Alpha_diversity, by = "ParticipantID", all = F)

###Merging everything together
dim(twin_details_AlphaD)

colnames(new_fill_correct_order)

CQ_twin.details_merged = merge(new_fill_correct_order, twin_details_AlphaD, by = "ParticipantID")


dim(CQ_twin.details_merged)

##merging the twin_details_AlphaD with new_fill_correct order has resulted in 927 subjects

#just to be sure I'm going to check how many participant ID's common between both datasets

colnames(twin_details_AlphaD)

colnames(new_fill_correct_order)

participant_ID_datasets = merge(twin_details_AlphaD[1], new_fill_correct_order[1])

dim(participant_ID_datasets)

dim(participant_ID_datasets)

##seems to be correct in relation to the full dataset merge

###calculating age of participants from when alphadiversity was done and samples were taken relative to the data the questionarre for LCQ or BCQ was taken

CQ.twin.details_merged_2 = CQ_twin.details_merged[c(1,56:106,2,3:55)]

colnames(CQ.twin.details_merged_2[54])

study_date_LCQ = CQ.twin.details_merged_2[54]

colnames(CQ.twin.details_merged_2[53])

study_date_BCQ = CQ.twin.details_merged_2[53]

date_born = CQ.twin.details_merged_2[4]

head(date_born)
head(study_date_BCQ)
head(study_date_LCQ)


age_matrix3 = matrix(ncol = 1, nrow = nrow(study_date_BCQ))

for (i in 1:dim(study_date_BCQ)[1])
{
  age_matrix3[i,] <- as.numeric(format(as.Date(study_date_BCQ[i,],format="%d/%m/%Y"),format="%Y"))-as.numeric(format(as.Date(date_born[i,],format="%d/%m/%Y"),format="%Y"))
}

head(age_matrix3)
head(date_born)


colnames(age_matrix3)[1] <- c("Age At Time of Response")

head(age_matrix3)

library(tibble)

colnames(CQ.twin.details_merged_2)

RD_Age_CQ = add_column(CQ.twin.details_merged_2, age_matrix3, .after = "ResponseDateBCQ")

colnames(RD_Age_CQ)

colnames(RD_Age_CQ)[54] <- "Age At Time Response BCQ"

age_matrix3b = matrix(ncol = 1, nrow = nrow(study_date_LCQ))

for (i in 1:dim(study_date_LCQ)[1])
{
  age_matrix3b[i,] <- as.numeric(format(as.Date(study_date_LCQ[i,],format="%d/%m/%Y"),format="%Y"))-as.numeric(format(as.Date(date_born[i,],format="%d/%m/%Y"),format="%Y"))
}

age_matrix3b

colnames(RD_Age_CQ)

RD_Age_CQ = add_column(RD_Age_CQ, age_matrix3b, .after = "ResponseDateLCQ")

colnames(RD_Age_CQ)[56] <- "Age At Time Response LCQ"

dim(RD_Age_CQ)

summary(RD_Age_CQ)


library(plyr)

str(RD_Age_CQ)


summary(RD_Age_CQ)

##checking to see if any of the participant ID's are dupliacted 

duplicated(RD_Age_CQ$ParticipantID)

#deleting duplicated row

dim(CQ_twin.details_merged)

merged_CQ_alphaD_final = RD_Age_CQ[-c(504),]

dim(merged_CQ_alphaD_final)

duplicated(merged_CQ_alphaD_final$ParticipantID)

colnames(merged_CQ_alphaD_final)

#seperating variables from dataset to generate binary values 

colnames(merged_CQ_alphaD_final)


colnames(merged_CQ_alphaD_final[57:108])

variables = merged_CQ_alphaD_final[57:108]

colnames(variables)

#code established by ruth to establish a binary list

#double checking to see if new dataset is fixed and doesnt have numbers in excess of 1 as many questions are intended to be binary variables


write.csv(merged_CQ_alphaD_final, "fixed_dataset.csv")

dim(merged_CQ_alphaD_final)

colnames(variables)

Binary_variables =sapply(variables, function(col) !any(diff(which(!is.na(col))) == 1))

dim(Binary_variables)

dim(Binary_variables)

dim(variables)

dim(dude)

dude = apply(variables, 2, function(x){
  max=max(x, na.rm = T)
})

binary = variables[,which(dude==1)]

dim(binary)

dim(binary)

colnames(binary)

BL = apply(binary, 2, function(x){
  table(x, useNA = "ifany")
})

rownames(BL) = c("No","Yes","NA")


dim(BL)

dim(BL)

write.csv(BL, "BinaryList.csv")

colnames(BL)

dim(BL)

length(which(variables$PH0000769 == 1))

dim(merged_CQ_alphaD_final)

length(which(merged_CQ_alphaD_final$SEX.x == "NA"))

891 + 35

##Binary list generated seems right, however, there are 3 phenotypes excluded from the list due to them having only no's and no yesses, these are PH0000600, PH0000766, PH0000769

#however there are some questions that accidentaly translate to binary variables, an example being "lifetime occurance of hystorectomy operation"
#everyone who has had a hystorectomy in this dataset only has had one in their life, 
#interestingly this has more yesses than no's but whether its included in the binary values list is a technicality, the question could be reworded to have you ever had a hystorectomy, however, it would
#as there is not a second question asking if you have ever had a hystorectomy I will reword this question 

##creating a table of the age of the participantsthat said yes 

colnames(merged_CQ_alphaD_final[1])

age16s = na.omit(merged_CQ_alphaD_final[c(1,17)])

dim(age16s)

dim(age16s)

head(age16s)

BCQ_age = na.omit(merged_CQ_alphaD_final[54])

LCQ_age = na.omit(merged_CQ_alphaD_final[56])

dim(BCQ_age)

dim(LCQ_age)

colnames(CQ.twin.details_merged_2)

#checking to see if the 16s age from alpha diverstiy and BCQ ages line up 

age_check = data.frame(age16s, BCQ_age)

dim(age_check)

#they dont, however, this is because 16s age is taken from 2017, the year of the study, 
#whereas, the BCQ study was calculated using todays date, which is 2020. These coincide with the fact that
#the age16s seems to be approximately 3 years in difference from for now I'm gonna use age16s as its the age provided by the alpha diversity dataset, however, 16s age may be age calculated from DNA as opposed to real age at the 
#time of study, CONFIRMED

#creating dataset of ages and Phenotypes for later use

variables_PID = (merged_CQ_alphaD_final[c(1,57:108)])

head(variables_PID)

dim(variables_PID)

variables_age = merge(age16s,variables_PID, by = "ParticipantID")

head(variables_age)

dim(variables_age)

dim(variables_age)

colnames(variables_age)

dim(BL)

#useful bit of code

non_binary = variables[,-which(colnames(variables)%in%colnames(binary))]

dim(non_binary)

dim(non_binary)

colnames(binary)

colnames(non_binary)


length(which(merged_CQ_alphaD_final$PH0000766 == 1))

binary_yes_age = data.frame(age16s, binary)

dim(binary_yes_age)

dim(binary_yes_age)

colnames(binary_yes_age)

binary_SD_M_Yes_matrix = matrix(ncol = 2, nrow = 29)

for(i in c(3:ncol(binary_yes_age)))
    {
  only_ones = binary_yes_age[binary_yes_age[,i] == 1,]
  mean_dude = mean(only_ones$age.16S, na.rm = T)
  sd_dude = sd(only_ones$age.16S, na.rm = T)
  binary_SD_M_Yes_matrix[i,1] = mean_dude
  binary_SD_M_Yes_matrix[i,2] = sd_dude
}

dim(binary_SD_M_Yes_matrix)


colnames(binary_SD_M_Yes_matrix) = c("Mean", "SD")

Age_SD_Mean_Table = binary_SD_M_Yes_matrix[-c(1,2),]

dim(Age_SD_Mean_Table)

rownames(Age_SD_Mean_Table) <- colnames(binary_yes_age[c(3:29)])

dim(Age_SD_Mean_Table)

mean(age16s$age.16S)
sd(age16s$age.16S)

###The next objective is to find the mean and standard  for the richness measures of chao1, fisher_alpha, simpson, observed otus and shannon
##These are a part of the alpha diversity measures

#The following richness measures will be used

#simpson

#chao1 

#shannon 

#fisherA 

#otus

###using old code with by function to figure out the mean and std deviation of the yesses and no's seperately
## or using the subset function

colnames(merged_CQ_alphaD_final)


Alpha_D_specific = merged_CQ_alphaD_final[c(23,32,41,48,49)]


alpha_D_binary = data.frame(Alpha_D_specific,binary)

dim(alpha_D_binary)

colnames(alpha_D_binary)

##this is the subset experiment, however, if i already have a loop that works I dont think its worth spending time on this 

#subset experiment didnt work, using old loop with tapply
#chao1 matrix for 1's and 0s


Chao1_mean_matrix = matrix(nrow = 32, ncol = 2)

for(i in c(6:ncol(alpha_D_binary))){
Chao1_mean = tapply(alpha_D_binary$chao1, alpha_D_binary[i], FUN = mean, na.rm = TRUE)
Chao1_mean_matrix[i,] = Chao1_mean
print(Chao1_mean)
}

colnames(Chao1_mean_matrix) = c("No","Yes")

dim(Chao1_mean_matrix)


#chao1 SD matrix

Chao1_sd_matrix = matrix(nrow = 32, ncol = 2)

for(i in c(6:ncol(alpha_D_binary))){
Chao1_sd = tapply(alpha_D_binary$chao1, alpha_D_binary[i], FUN = sd, na.rm = TRUE)
Chao1_sd_matrix[i,] = Chao1_sd
}

colnames(Chao1_sd_matrix) = c("No","Yes")

dim(Chao1_sd_matrix)

#shannon mean

shannon_mean_matrix = matrix(nrow = 32, ncol = 2)

for(i in c(6:ncol(alpha_D_binary))){
shannon_mean = tapply(alpha_D_binary$shannon, alpha_D_binary[i], FUN = mean, na.rm = TRUE)
shannon_mean_matrix[i,] = shannon_mean
}

colnames(shannon_mean_matrix) = c("No","Yes")

dim(shannon_mean_matrix)

#shannon sd

shannon_sd_matrix = matrix(nrow = 32, ncol = 2)

for(i in c(6:ncol(alpha_D_binary))){
shannon_sd = tapply(alpha_D_binary$shannon, alpha_D_binary[i], FUN = sd, na.rm = TRUE)
shannon_sd_matrix[i,] = shannon_sd
}

colnames(shannon_sd_matrix) = c("No","Yes")

dim(shannon_sd_matrix)

#simpson mean 

simpson_mean_matrix = matrix(nrow = 32, ncol = 2)

for(i in c(6:ncol(alpha_D_binary))){
simpson_mean = tapply(alpha_D_binary$simpson, alpha_D_binary[i], FUN = mean, na.rm = TRUE)
simpson_mean_matrix[i,] = simpson_mean
}

colnames(simpson_mean_matrix) = c("No","Yes")

dim(simpson_mean_matrix)

#simpson sd

simpson_sd_matrix = matrix(nrow = 32, ncol = 2)

for(i in c(6:ncol(alpha_D_binary))){
simpson_sd = tapply(alpha_D_binary$simpson, alpha_D_binary[i], FUN = sd, na.rm = TRUE)
simpson_sd_matrix[i,] = simpson_sd
}

colnames(simpson_sd_matrix) = c("No","Yes")

dim(simpson_sd_matrix)

#fisher_alpha mean

fisher_alpha_mean_matrix = matrix(nrow = 32, ncol = 2)

for(i in c(6:ncol(alpha_D_binary))){
fisher_alpha_mean = tapply(alpha_D_binary$fisher_alpha, alpha_D_binary[i], FUN = mean, na.rm = TRUE)
fisher_alpha_mean_matrix[i,] = fisher_alpha_mean
}

colnames(fisher_mean_matrix) = c("No","Yes")

dim(fisher_alpha_mean_matrix)

#fisher_alpha sd

fisher_alpha_sd_matrix = matrix(nrow = 32, ncol = 2)

for(i in c(6:ncol(alpha_D_binary))){
fisher_alpha_sd = tapply(alpha_D_binary$fisher_alpha, alpha_D_binary[i], FUN = sd, na.rm = TRUE)
fisher_alpha_sd_matrix[i,] = fisher_alpha_sd
}

colnames(fisher_sd_matrix) = c("No","Yes")
dim(fisher_alpha_sd_matrix)

#observed otus mean

otus_mean_matrix = matrix(nrow = 32, ncol = 2)

for(i in c(6:ncol(alpha_D_binary))){
otus_mean = tapply(alpha_D_binary$observed_otus, alpha_D_binary[i], FUN = mean, na.rm = TRUE)
otus_mean_matrix[i,] = otus_mean
}

colnames(otus_mean_matrix) = c("No","Yes")

dim(otus_mean_matrix)

#observed otus sd

otus_sd_matrix = matrix(nrow = 32, ncol = 2)

for(i in c(6:ncol(alpha_D_binary))){
otus_sd = tapply(alpha_D_binary$observed_otus, alpha_D_binary[i], FUN = sd, na.rm = TRUE)
otus_sd_matrix[i,] = otus_sd
}

dim(otus_sd_matrix)

colnames(otus_sd_matrix) = c("No","Yes")


#table with all the means, sd's of all richness measures put together

mean_sd_richness_table = data.frame(Chao1_mean_matrix,Chao1_sd_matrix,shannon_mean_matrix,shannon_sd_matrix,simpson_mean_matrix,simpson_sd_matrix,
                                    fisher_alpha_mean_matrix,fisher_alpha_sd_matrix,otus_mean_matrix,otus_sd_matrix)


dim(mean_sd_richness_table)

##making a table of all the means and SD's to draw from later
rownames(mean_sd_richness_table) = colnames(alpha_D_binary)

rownames(mean_sd_richness_table)


write.csv(mean_sd_richness_table, "table_updated.csv")

dim(mean_sd_richness_table)

##next, a t.test will be performed on phenotypes that have a yes value of 10 or greater.
dim(BL)

colnames(BL)

colnames(alpha_D_binary[6:32])

which(BL[2,]>=10)

BL_less_10 = which(BL[2,]<10)

class(BL_less_10)

less_than_10 = as.data.frame(BL_less_10)

class(less_than_10)

BL[2,]

print(length(which(merged_CQ_alphaD_final$PH0000600== 0)))

colnames(alpha_D_binary)

rownames(less_than_10)

Alpha_D_binary_10 = alpha_D_binary[,-which(colnames(alpha_D_binary)%in%rownames(less_than_10))]


print(length(which(Alpha_D_binary_10$PH0000800 == 1)))

dim(Alpha_D_binary_10)

colnames(Alpha_D_binary_10)

##performing t.tests aka calculating p.values to determine significance ie, if it lower than 0.05 its deemed
#statistically significant 

dim(Alpha_D_binary_10)

for(i in c(6:ncol(Alpha_D_binary_10))){
  chao1_tt = (t.test(Alpha_D_binary_10$chao1 ~ Alpha_D_binary_10[,i]))
  print(chao1_tt$p.value)
  print(chao1_tt$statistic)
}

t.test(Alpha_D_binary_10$chao1 ~ Alpha_D_binary_10$PH0000779)

##significant t.test for phenotype "PH0000779" 
#aka(Has a doctor ever told you that you have/had any of the following conditions? \ An abnormal cervical smear)

colnames(Alpha_D_binary_10)

t.test(Alpha_D_binary_10$chao1 ~ Alpha_D_binary_10[,16])


#data:  Alpha_D_binary_10$chao1 by Alpha_D_binary_10[, 16]
#t = -2.2461, df = 165.1, p-value = 0.02602
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -5.3666567 -0.3454891
#sample estimates:
#  mean in group 0 mean in group 1 
#24.15435        27.01042 
#t.test for shannon

dim(Alpha_D_binary_10)

for(i in c(6:ncol(Alpha_D_binary_10))){
  shannon_tt = (t.test(Alpha_D_binary_10$shannon ~ Alpha_D_binary_10[,i]))
  print(shannon_tt$p.value)
}


t.test(Alpha_D_binary_10$shannon ~ Alpha_D_binary_10$PH0000520)

#data:  Alpha_D_binary_10$shannon by Alpha_D_binary_10[, 6]
#t = 2.1063, df = 443.64, p-value = 0.03574
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.009538198 0.275525385
#sample estimates:
#  mean in group 0 mean in group 1 
#3.337002        3.194470


t.test(Alpha_D_binary_10$shannon ~ Alpha_D_binary_10[,16])


#reworked data has shown 2 phenotypes with significant P.values

#the same seems to be the case for shannon t.test ie. phenotype PH0000779" 

#Welch Two Sample t-test

#data:  Alpha_D_binary_10$shannon by Alpha_D_binary_10[, 16]
#t = -3.6908, df = 226.41, p-value = 0.0002801
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.4268430 -0.1297061
#sample estimates:
#  mean in group 0 mean in group 1 
#3.198619        3.476894 



#t.test for simpson

for(i in c(6:ncol(Alpha_D_binary_10))){
  simpson_tt = (t.test(Alpha_D_binary_10$simpson ~ Alpha_D_binary_10[,i]))
  print(simpson_tt$p.value)
}


#The case seems to be different with simpson, phenotype "PH0000767" aka "	Has a doctor ever told you that you have/had any of the following conditions? \ An ectopic pregnancy"
#reworked data shows more phenotypes hav has a p.value that is of statistical significance

colnames(Alpha_D_binary_10[6])

t.test(Alpha_D_binary_10$simpson ~ Alpha_D_binary_10[,6])

#PH0000520 (Lifetime occurance of hystorectomy operation/Have you ever had a hystorectomy operation)

#data:  Alpha_D_binary_10$simpson by Alpha_D_binary_10[, 6]
#t = 2.0817, df = 426.59, p-value = 0.03796
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.001408549 0.049057543
#sample estimates:
#mean in group 0 mean in group 1 
#0.8330821       0.8078491


colnames(Alpha_D_binary_10[12])

t.test(Alpha_D_binary_10$simpson ~ Alpha_D_binary_10[,12]) #PH0000767 "	Has a doctor ever told you that you have/had any of the following conditions? \ An ectopic pregnancy"

#data:  Alpha_D_binary_10$simpson by Alpha_D_binary_10[, 12]
#t = -4.0311, df = 30.57, p-value = 0.0003415
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.07762819 -0.02544782
#sample estimates:
#  mean in group 0 mean in group 1 
#0.8154789       0.8670169 


t.test(Alpha_D_binary_10$simpson ~ Alpha_D_binary_10[,16]) #phenotype PH0000779 (Has a doctor ever told you that you have/had any of the following conditions? \ An abnormal cervical smear) 

#data:  Alpha_D_binary_10$simpson by Alpha_D_binary_10[, 16]
#t = -3.3909, df = 283.71, p-value = 0.0007957
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.06580322 -0.01746618
#sample estimates:
#  mean in group 0 mean in group 1 
#0.8099171       0.8515518 

#t.test for fisher_alpha 

for(i in c(6:ncol(Alpha_D_binary_10))){
  fisher_alpha_tt = (t.test(Alpha_D_binary_10$fisher_alpha ~ Alpha_D_binary_10[,i]))
  print(fisher_alpha_tt$p.value)
}

t.test(Alpha_D_binary_10$fisher_alpha ~ Alpha_D_binary_10[,16]) #phenotype PH0000779" (ectopic pregnancy)

#data:  Alpha_D_binary_10$fisher_alpha by Alpha_D_binary_10[, 16]
#t = -3.2443, df = 174.46, p-value = 0.001411
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -2.0779933 -0.5060199
#sample estimates:
#  mean in group 0 mean in group 1 
#7.273436        8.565442 


#t.test for otus

for(i in c(6:ncol(Alpha_D_binary_10))){
  otus_tt = (t.test(Alpha_D_binary_10$observed_otus ~ Alpha_D_binary_10[,i]))
  print(otus_tt$p.value)
}

t.test(Alpha_D_binary_10$observed_otus ~ Alpha_D_binary_10[,16]) #phenotype PH0000779" 



#data:  Alpha_D_binary_10$observed_otus by Alpha_D_binary_10[, 16]
#t = -3.5712, df = 194.12, p-value = 0.000448
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -2.9158456 -0.8410432
#sample estimates:
#  mean in group 0 mean in group 1 
#17.29137        19.16981 



#reworked code has produced additional values of significance, those being: PH0000520,PH0000779,PH0000767

#Next steps
#put together linear regression table like last time,
#Then Do linear regression on the phenotypes with relevant p.values, 
#Then figure out how to do linear regression with species and such
#if you simply have no way of figuring out your next steps before tomorrow
#then do as much of the writing and editing as possible 

#ParticipantID	age.16S	Family_No.x	SEX.x	ZYGOSITY	BMI	chao1	shannon	fisher_alpha	observed_otus
  

colnames(merged_CQ_alphaD_final)

t_test_lin_reg_table = merged_CQ_alphaD_final[c(1,17,2,6,13,18,23,48,32,49,41,57,87,75)]

dim(t_test_lin_reg_table)

colnames(t_test_lin_reg_table)

#linear regression code from last time

###James' code
##values would be effect size, standard error and a p value which helps you to determine whether there is any change ( increase or decrease)
##in each of

data <- t_test_lin_reg_table
colnames(data)
library(lmerTest)
library(lme4)
head(data)

colnames(data)

data[is.na(data)] <- 0

#PH0000520 linear regression model

output_PH0000520 <- apply(data[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000520 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = data, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000520_new = t(output_PH0000520)

colnames(output_PH0000520_new )=c("effect_size", "std_error","p.value")

dim(output_PH0000520_new)

#metrics below 0.05 in simpson however with a negative effect size

#PH0000779 linear regression model

output_PH0000779 <- apply(data[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000779 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = data, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000779_new = t(output_PH0000779)

colnames(output_PH0000779_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000779_new)

#p.values below 0.005 in all metrics except chao1 with positive effect sizes

#2nd one with PH0000767

output_PH0000767 <- apply(data[, 7:11], 2, function(i){
  fit <- lmer(i ~ PH0000767 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = data, REML=F, na.action=na.omit )
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000767_new = t(output_PH0000767)

colnames(output_PH0000767_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000767_new)

d#no p.value below 0.05 in this one therefore not viable

##7:9 being the columns with the three alpha diversity metrics, chao, shannon and fishers. 

#next steps to do linear regression on all phenotypes that have more than 10 yes
#then do linear regression all non-binary phenotypes with continous data i.e age

##beta diverstiy read

beta_species = read.csv("Species_Only.csv")


#merging beta diversity with just participants and no alpha diversity data

twin_details_BetaD = merge(twin_details_data, beta_species, by = "ParticipantID", all = F)

dim(twin_details_BetaD)

dim(twin_details_BetaD)

colnames(twin_details_BetaD)

twin_details_BetaD_rm = twin_details_BetaD[-c(11)]

dim(twin_details_BetaD_rm)



twin_details_BetaD_CQ = merge(twin_details_BetaD_rm, new_fill_correct_order, by = "ParticipantID")

dim(new_fill_correct_order)
dim(twin_details_BetaD_CQ)

wdim(merged_CQ_alphaD_final)

#just checking to see if everything is lined up

head(merged_CQ_alphaD_final$ParticipantID)
head(twin_details_BetaD_CQ$ParticipantID)
tail(merged_CQ_alphaD_final$ParticipantID)
tail(twin_details_BetaD_CQ$ParticipantID)


#merging beta_diversity with participants and alpha diversity data

alpha_beta_CQ_all_data = merge(beta_species, merged_CQ_alphaD_final, by = "ParticipantID")

dim(alpha_beta_CQ_all_data)

dim(merged_CQ_alphaD_final)

dim(alpha_beta_CQ_all_data)

#Ok so now we have two datasets to work from, the problem now is to figure out how to implement the 
#same techniques we used on alpha diverstiy
#Last time Caroline said we need to use 5 specicif metrics, those being chao1, shannon, simpson, fisher_alpha and
#otus
#Now I've been told to start with all species present in at least 50% of the population.
#lets figure out how to do this 
#From Caroline: Could you please run linear regression the same way as you did for alpha diversity - starting with all species present in at least 50% of the population. 
#Only look at phenotypes that were significantly associated with alpha diversity from your linear regression results (P < 0.05, for any of the alpha diversity metrics that you considered). 

colnames(twin_details_BetaD_CQ)

species = (twin_details_BetaD_CQ[c(1,15:556)])

dim(species)

#< LESS THAN 
#> GREATER THAN
#you fucking idiot

dim(twin_details_BetaD_CQ)

#test for percentage calculation 

length(which(twin_details_BetaD_CQ[15] > 0.000000000))

dim(twin_details_BetaD_CQ[15])


write.csv(twin_details_BetaD, "twin_details_BetaD.csv")

w582/926*100

colnames(twin_details_BetaD_CQ)

#defining only the taxa whos percentage is to be calculated

only_taxa = twin_details_BetaD_CQ[c(15:556)]

dim(only_taxa)

dim(only_taxa)

#making a matrix to put percentage calculations in 

percentage_matrix = matrix(nrow = 542, ncol = 1,)

rownames(percentage_matrix) = colnames(only_taxa)

dim(only_taxa)

dim(percentage_matrix)

head(colnames(only_taxa))

dim(only_taxa)

for(i in c(1:ncol(only_taxa))){
amount = length(which(only_taxa[i] > 0.000000000)) 
 percentage_matrix[i,] = amount/926*100
}

length(which(only_taxa[3] == 1))


dim(percentage_matrix)

over_50_club= which((percentage_matrix >= 50))

over_80_club = which((percentage_matrix >= 20))

over_90_club = which((percentage_matrix >= 10))

dim(over_80_club)

dim(over_50_club)

percentage_matrix[520]

dim(percentage_matrix[512])

colnames(twin_details_BetaD_CQ[15:556])

rownames(percentage_matrix)
colnames(only_taxa)

#well its elaborate and over complicated but it works
#We have 83 measurements over 50%

#so to do this you need to take the over 50 club, tack it on to the relevant phenotype you found, and then 

over_50_club



over_50_cols = only_taxa[c(1,30,35,42,50,56,67,69,76,77,81,86,92,105,106,107,108,109,110,111,118,120,123,151,152,153,154,155,156,238,249
                          ,252,255,259,273,299,300,303,304,306,307,309,311,314,319,320,321,324,325,328,329,335,336,339,341,348,349,350,355,
                          356,360,365,366,369,373,374,376,377,388,392,396,398,410,424,426,427,448,452,453,472,497,511)]

dim(over_50_cols)

length(which(over_50_cols[3] > 0.000000000))
length(which(twin_details_BetaD_CQ[49] > 0.000000000))

details = merged_CQ_alphaD_final[c(1,17,2,6,13,18)]

dim(details)

phenotypes = merged_CQ_alphaD_final[c(57,87,75)]


data_b = data.frame(details, over_50_cols, phenotypes)

dim(data_b)

dim(merged_CQ_alphaD_final)

data_b[is.na(data_b)] <- 0

colnames(data_b)

#linear regression for beta diversity and PH0000520

beta_PH0000520 <- apply(data_b[, 7:88], 2, function(i){
  fit <- lmer(i ~ PH0000520 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = data_b, REML=F, na.action=na.omit )
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_beta_PH0000520 = t(beta_PH0000520)

colnames(output_beta_PH0000520)=c("effect_size", "std_error","p.value")

dim(output_beta_PH0000520)


#relevant species for PH0000520

taxa_PH0000520 = as.data.frame(output_beta_PH0000520)

dim(taxa_PH0000520)

which(taxa_PH0000520$p.value <= 0.05)

p.value_microbes_PH0000520 = t(taxa_PH0000520[c(8,11,17,29,43,49,57,63,68,76),])

PH0000520_microbes = data_b[,which(colnames(data_b)%in%colnames(p.value_microbes_PH0000520))]

rownames(taxa_PH0000520[c(8,11,17,29,43,49,57,63,68,76),])
colnames(PH0000520_microbes)

which(colnames(data_b)== "PH0000520")

PH0000520_statistically_relevant_taxa = data.frame(data_b[c(1,89)],PH0000520_microbes)

dim(PH0000520_statistically_relevant_taxa)

length(which(PH0000520_statistically_relevant_taxa$PH0000520  == 1))

dim(PH0000)


#linear regression for beta diversity and PH0000779


beta_PH0000779 <- apply(data_b[, 7:88], 2, function(i){
  fit <- lmer(i ~ PH0000779 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = data_b, REML=F, na.action=na.omit )
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_beta_PH0000779 = t(beta_PH0000779)

colnames(output_beta_PH0000779)=c("effect_size", "std_error","p.value")

dim(output_beta_PH0000779)

#relevant species for PH0000779

taxa_PH0000779 = as.data.frame(output_beta_PH0000779)

dim(taxa_PH0000779)

which(taxa_PH0000779$p.value <= 0.05)

p.value_microbes_PH0000779 = t(taxa_PH0000779[c(5,6,10,18,24,29,39,40,46,49,51,56,59,66),])

PH0000779_microbes = data_b[,which(colnames(data_b)%in%colnames(p.value_microbes_PH0000779))]

rownames(taxa_PH0000779[c(5,6,10,18,24,29,39,40,46,49,51,56,59,66),])
colnames(PH0000779_microbes)

which(colnames(data_b)== "PH0000779")

PH0000779_statistically_relevant_taxa = data.frame(data_b[c(1,90)],PH0000779_microbes)

dim(PH0000779_statistically_relevant_taxa)

length(which(PH0000779_statistically_relevant_taxa$PH0000779  == 1))




#for pnenotype PH0000767

beta_PH0000767 <- apply(data_b[, 7:88], 2, function(i){
  fit <- lmer(i ~ PH0000767 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = data_b, REML=F, na.action=na.omit )
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_beta_PH0000767 = t(beta_PH0000767)

colnames(output_beta_PH0000767)=c("effect_size", "std_error","p.value")

dim(output_beta_PH0000767)



#making dataset of all relevant binary phenotypes

#Those being "PH0000788","PH0000789","PH0000792","PH0000522","PH0000598","PH0000767","PH0000773","PH0000775"
#if you have time, do the rest. It wont take too much time and may give you valuable data


which(colnames(merged_CQ_alphaD_final)=="observed_otus")

hrt_ooph_binary = merged_CQ_alphaD_final[c(1,17,2,6,13,18,23,48,32,49,41,96,97,100,59,66,75,81,83)]

colnames(hrt_ooph_binary)

dim(hrt_ooph_binary)

hrt_ooph_binary[is.na(hrt_ooph_binary)] <- 0

#making a list to reference from

binary_list_for_lin_regression = colnames(hrt_ooph_binary[c(12:19)])


dim(binary_list_for_lin_regression)

#performing linear regression on phenotypes relevant to objective questions of menopause status, HRT, oophorectomy

library(lmerTest)
library(lme4)

colnames(hrt_ooph_binary)

#linear regression for PH0000788 (Have you EVER taken Hormone Replacement Therapy (HRT)?)

output_PH0000788 <- apply(hrt_ooph_binary[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000788 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_ooph_binary, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000788_new = t(output_PH0000788)

colnames(output_PH0000788_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000788_new)

#no significant p.values for the metrics


#linear regression for PH0000789 (Are you currently taking HRT?)

output_PH0000789 <- apply(hrt_ooph_binary[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000789 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_ooph_binary, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000789_new = t(output_PH0000789)

colnames(output_PH0000789_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000789_new)

#no significant p.values for the metrics

#linear regression for PH0000792 (	Have you EVER taken hormone-based contraception such as contraceptive pills, injections, patches, vaginal rings, contraceptive implants)? )

output_PH0000792 <- apply(hrt_ooph_binary[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000792 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_ooph_binary, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000792_new = t(output_PH0000792)

colnames(output_PH0000792_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000792_new)

#linear regression for PH0000522 (Lifetime occurrence of oophorectomy operation AKA have you ever had an oophorectomy)

output_PH0000522  <- apply(hrt_ooph_binary[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000522  + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_ooph_binary, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000522_new = t(output_PH0000522)

colnames(output_PH0000522_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000522_new)

#linear regression for PH0000598 (What kind(s) of cancer have you been diagnosed with? \ Ovary)

output_PH0000598 <- apply(hrt_ooph_binary[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000598 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_ooph_binary, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000598_new = t(output_PH0000598)

colnames(output_PH0000598_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000598_new)

#FOUND SIGNIFICANT DATA IN CHAO1, FISHER_ALPHA AND OBSERVED OTUS
#now to check number of yesses and if thats above established threshold of 10

print(length(which(hrt_ooph_binary$PH0000598== 1)))

#its only 6 which is unfortately not enough 

#linear regression for PH0000767 (	Has a doctor ever told you that you have/had any of the following conditions? \ An ectopic pregnancy)

output_PH0000767 <- apply(hrt_ooph_binary[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000767 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_ooph_binary, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000767_new = t(output_PH0000767)

colnames(output_PH0000767_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000767_new)

#linear regression for PH0000773 (	Has a doctor ever told you that you have/had any of the following conditions? \ An ovarian cyst)

output_PH0000773 <- apply(hrt_ooph_binary[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000773 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_ooph_binary, REML=T, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000773_new = t(output_PH0000773)

colnames(output_PH0000773_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000773_new)

#I've NOTICED an issue/Warning message:

#In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#Model failed to converge with max|grad| = 0.0042433 (tol = 0.002, component 1)

#it seems to go away when I change REML to T
#the results dont differ much, I need to look at the tutorials and figure this out
#tutorials basically say not to worry about it

#Linear regression for PH0000775 (Do you have this condition at the moment? \ An ovarian cyst)

output_PH0000775 <- apply(hrt_ooph_binary[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000775 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_ooph_binary, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000775_new = t(output_PH0000775)

colnames(output_PH0000775_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000775_new)
#
#
#
#
#
#
#Now doing the same but with top 50 percent of beta diversity as well
#making data.frame to work from

hrt_ooph_binary_only = merged_CQ_alphaD_final[c(96,97,100,59,66,75,81,83)]

colnames(hrt_ooph_binary_only)

betaD_lin_regression = data.frame(details, over_50_cols, hrt_ooph_binary_only)

dim(betaD_lin_regression)

betaD_lin_regression$SEX.x

print(length(which(betaD_lin_regression$SEX.x == "F")))



betaD_lin_regression[is.na(betaD_lin_regression)] <- 0

#you're probably going to have to go back and get rid of the male subjects


colnames(betaD_lin_regression)

#linear regression for PH0000788 (Have you EVER taken Hormone Replacement Therapy (HRT)?)

taxa_output_PH0000788 <- apply(betaD_lin_regression[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000788 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = betaD_lin_regression, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000788_new = t(taxa_output_PH0000788)

colnames(taxa_output_PH0000788_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000788_new)


#linear regression for PH0000789 (Are you currently taking HRT?)

taxa_output_PH0000789 <- apply(betaD_lin_regression[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000789 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = betaD_lin_regression, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000789_new = t(taxa_output_PH0000789)

colnames(taxa_output_PH0000789_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000789_new)

#linear regression for PH0000792 (	Have you EVER taken hormone-based contraception such as contraceptive pills, injections, patches, vaginal rings, contraceptive implants)? )

taxa_output_PH0000792 <- apply(betaD_lin_regression[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000792 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = betaD_lin_regression, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000792_new = t(taxa_output_PH0000792)

colnames(taxa_output_PH0000792_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000792_new)


#linear regression for PH0000522 (Lifetime occurrence of oophorectomy operation AKA have you ever had an oophorectomy)

taxa_output_PH0000522 <- apply(betaD_lin_regression[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000522 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = betaD_lin_regression, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000522_new = t(taxa_output_PH0000522)

colnames(taxa_output_PH0000522_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000522_new)

print(length(which(betaD_lin_regression$PH0000522== 1)))

#n.b. = strange effect sizes

#linear regression for PH0000598 (What kind(s) of cancer have you been diagnosed with? \ Ovary)

taxa_output_PH0000598 <- apply(betaD_lin_regression[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000598 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = betaD_lin_regression, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000598_new = t(taxa_output_PH0000598)

colnames(taxa_output_PH0000598_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000598_new)



#linear regression for PH0000767 (	Has a doctor ever told you that you have/had any of the following conditions? \ An ectopic pregnancy)

taxa_output_PH0000767 <- apply(betaD_lin_regression[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000767 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = betaD_lin_regression, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000767_new = t(taxa_output_PH0000767)

colnames(taxa_output_PH0000767_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000767_new)


#linear regression for PH0000773 (	Has a doctor ever told you that you have/had any of the following conditions? \ An ovarian cyst)

taxa_output_PH0000773 <- apply(betaD_lin_regression[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000773 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = betaD_lin_regression, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000773_new = t(taxa_output_PH0000773)

colnames(taxa_output_PH0000773_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000773_new)



#Linear regression for PH0000775 (Do you have this condition at the moment? \ An ovarian cyst)

taxa_output_PH0000775 <- apply(betaD_lin_regression[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000775 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = betaD_lin_regression, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000775_new = t(taxa_output_PH0000775)

colnames(taxa_output_PH0000775_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000775_new)

#thats all the relevant binary values calculated, now to see which bacteria have a significant P.value for 
#the phenotypes

#Taxa relevant to PH0000788

taxa_PH0000788 = as.data.frame(taxa_output_PH0000788_new)

dim(taxa_PH0000788)

which(taxa_PH0000788$p.value <= 0.05)

p.value_microbes_PH0000788 = t(taxa_PH0000788[c(11,13,17,68),])

rownames(taxa_PH0000788[c(11,13,17,68),])

PH0000788_microbes = betaD_lin_regression[,which(colnames(betaD_lin_regression)%in%colnames(p.value_microbes_PH0000788))]
dim(PH0000788_microbes)

which(colnames(betaD_lin_regression)== "PH0000788")

PH0000788_statistically_relevant_taxa = data.frame(betaD_lin_regression[c(1,89)], PH0000788_microbes)

dim(PH0000788_statistically_relevant_taxa)

#Relevant taxa for 	PH0000789

taxa_PH0000789 = as.data.frame(taxa_output_PH0000789_new)

dim(taxa_PH0000789)

which(taxa_PH0000789$p.value <= 0.05)

p.value_microbes_PH0000789 = t(taxa_PH0000789[c(4,41),])

PH0000789_microbes = betaD_lin_regression[,which(colnames(betaD_lin_regression)%in%colnames(p.value_microbes_PH0000789))]

dim(p.value_microbes_PH0000789)

rownames(taxa_PH0000789[c(4,41),])
colnames(PH0000789_microbes)

which(colnames(betaD_lin_regression)== "PH0000789")

PH0000789_statistically_relevant_taxa = data.frame(betaD_lin_regression[c(1,90)],PH0000789_microbes)

dim(PH0000789_statistically_relevant_taxa)

length(which(PH0000789_statistically_relevant_taxa$PH0000789  == 1))

#Relevant taxa for PH0000792

taxa_PH0000792 = as.data.frame(taxa_output_PH0000792_new)

dim(taxa_PH0000792)

which(taxa_PH0000792$p.value <= 0.05)

p.value_microbes_PH0000792 = t(taxa_PH0000792[c(19,53,58,61,74),])

PH0000792_microbes = betaD_lin_regression[,which(colnames(betaD_lin_regression)%in%colnames(p.value_microbes_PH0000792))]

rownames(taxa_PH0000792[c(19,53,58,61,74),])
colnames(PH0000792_microbes)

which(colnames(betaD_lin_regression)== "PH0000792")

PH0000792_statistically_relevant_taxa = data.frame(betaD_lin_regression[c(1,91)],PH0000792_microbes)

dim(PH0000792_statistically_relevant_taxa)

length(which(PH0000792_statistically_relevant_taxa$PH0000792  == 1))


#statistically relevant taxa for PH0000522

taxa_PH0000522 = as.data.frame(taxa_output_PH0000522_new)

dim(taxa_PH0000522)

which(taxa_PH0000522$p.value <= 0.05)

p.value_microbes_PH0000522 = t(taxa_PH0000522[c(3,42,81),])

PH0000522_microbes = betaD_lin_regression[,which(colnames(betaD_lin_regression)%in%colnames(p.value_microbes_PH0000522))]


rownames(taxa_PH0000522[c(3,42,81),])
colnames(PH0000522_microbes)

which(colnames(betaD_lin_regression)== "PH0000522")

PH0000522_statistically_relevant_taxa = data.frame(betaD_lin_regression[c(1,92)],PH0000522_microbes)

dim(PH0000522_statistically_relevant_taxa)

length(which(PH0000522_statistically_relevant_taxa$PH0000522  == 1))

# statistically relevant taxa for PH0000598

taxa_PH0000598 = as.data.frame(taxa_output_PH0000598_new)

dim(taxa_PH0000598)

which(taxa_PH0000598$p.value <= 0.05)

p.value_microbes_PH0000598 = t(taxa_PH0000598[c(2,59,60),])

PH0000598_microbes = betaD_lin_regression[,which(colnames(betaD_lin_regression)%in%colnames(p.value_microbes_PH0000598))]


rownames(taxa_PH0000598[c(2,59,60),])
colnames(PH0000598_microbes)

which(colnames(betaD_lin_regression)== "PH0000598")

PH0000598_statistically_relevant_taxa = data.frame(betaD_lin_regression[c(1,93)],PH0000598_microbes)

dim(PH0000598_statistically_relevant_taxa)

length(which(PH0000598_statistically_relevant_taxa$PH0000598  == 1))

#ONLY 6 YES, so not relevant

# statistically relevant taxa for PH0000767

taxa_PH0000767 = as.data.frame(taxa_output_PH0000767_new)

dim(taxa_PH0000767)

which(taxa_PH0000767$p.value <= 0.05)

p.value_microbes_PH0000767 = t(taxa_PH0000767[c(12,29,62),])

PH0000767_microbes = betaD_lin_regression[,which(colnames(betaD_lin_regression)%in%colnames(p.value_microbes_PH0000767))]


rownames(taxa_PH0000767[c(12,29,62),])
colnames(PH0000767_microbes)

which(colnames(betaD_lin_regression)== "PH0000767")

PH0000767_statistically_relevant_taxa = data.frame(betaD_lin_regression[c(1,94)],PH0000767_microbes)

dim(PH0000767_statistically_relevant_taxa)

length(which(PH0000767_statistically_relevant_taxa$PH0000767  == 1))

## statistically relevant taxa for PH0000773

taxa_PH0000773 = as.data.frame(taxa_output_PH0000773_new)

dim(taxa_PH0000773)

which(taxa_PH0000773$p.value <= 0.05)

p.value_microbes_PH0000773 = t(taxa_PH0000773[c(9,24,27,28,43,47),])

PH0000773_microbes = betaD_lin_regression[,which(colnames(betaD_lin_regression)%in%colnames(p.value_microbes_PH0000773))]

rownames(taxa_PH0000773[c(9,24,27,28,43,47),])
colnames(PH0000773_microbes)

which(colnames(betaD_lin_regression)== "PH0000773")

PH0000773_statistically_relevant_taxa = data.frame(betaD_lin_regression[c(1,95)],PH0000773_microbes)

dim(PH0000773_statistically_relevant_taxa)

length(which(PH0000773_statistically_relevant_taxa$PH0000773  == 1))


## statistically relevant taxa for PH0000775

taxa_PH0000775 = as.data.frame(taxa_output_PH0000775_new)

dim(taxa_PH0000775)

which(taxa_PH0000775$p.value <= 0.05)

#there are zero below 0.05 therefore statistically unuseable

length(which(PH0000775_statistically_relevant_taxa$PH0000775  == 1))
#only 6 yesses, therefore not statistically relevant

dim(PH0000598_statistically_relevant_taxa)

length(which(PH0000522_statistically_relevant_taxa$PH0000522 == 1))


#N.B!! PH0000775 has less than 10 yesses and are therefore deemed statisitically insignificant

#Right so what I've done here is got all the phenotypes associated with HRT, and oophorectomies and made tables of the
#taxa that had a p.value of less than 0.05 in accourdance with BMI, SEX, Age, Family number and zygosity.
#Therefore, I have found association of certain taxa with certain binary Phenotypes

dominant_taxa = percentage_matrix[c(1,30,35,42,50,56,67,69,76,77,81,86,92,105,106,107,108,109,110,111,118,120,123,151,152,153,
                           154,155,156,238,249,252,255,259,265,273,299,300,303,304,306,307,309,311,314,319,320,321,324,325,328,329,
                           335,336,339,341,348,349,350,355,356,360,365,366,369,373,374,376,377,388,392,396,398,410,424,426,427,448,
                           452,453,472,497,511)]

dominant_taxa_df = as.data.frame(dominant_taxa)

dim(dominant_taxa_df)

rownames(dominant_taxa_df) = colnames(only_taxa[c(1,30,35,42,50,56,67,69,76,77,81,86,92,105,106,107,108,109,110,111,118,120,123,151,152,153,
                                               154,155,156,238,249,252,255,259,265,273,299,300,303,304,306,307,309,311,314,319,320,321,324,325,328,329,
                                               335,336,339,341,348,349,350,355,356,360,365,366,369,373,374,376,377,388,392,396,398,410,424,426,427,448,
                                               452,453,472,497,511)])



write.csv(dominant_taxa_df,"dominant_taxa.csv")

dominant_taxa_percentages = read.csv("dominant_taxa.csv")

dim(dominant_taxa_percentages)

colnames(dominant_taxa_percentages)

library(ggplot2)

ggplot(data=dominant_taxa_percentages, aes(x=Dominant_taxa, y=Percentage)) +
  geom_bar(stat="identity")


#dealing with non binary values to obtain results
#not all of the continuous data is the same. For example, some are the ages in which certain
#phenotypes became apparent e.g. age of 1st cancer diagnosis, menses etc. some are more catagorical
#such as menopause status: 1) pre menopausal, 2) going through menopause, 3) Post-menopausal
# As one of the main focuses for this project is menopause, breaking down this phenotype is the first stage
#of understanding the non-binary data

dim(non_binary)

print(length(which(non_binary$PH0000786 == 1)))
#9
print(length(which(non_binary$PH0000786 == 2)))
#23
print(length(which(non_binary$PH0000786 == 3)))
#832
sum(is.na(non_binary$PH0000786))
#62

#Question at this point is, is it possible to see a difference in microbiome composition between
#pre, post and current menopausal subjects. An issue at this point is that there are only 6 pre-menopausal
#subjects, Whilst I am curious to see if there is a microbial population difference between the two
#I am not sure if the limited amount will be deemed statistically significantf

dim(non_binary)

non_binary_t = t(non_binary[1,])     

dim(non_binary_t)

non_binary_pheno_code_df = read.csv("non_binary_pheno_codes.csv")

dim(non_binary_pheno_code_df)

non_binary_Qs = merge(non_binary_pheno_code_df, code_Q_names, by = "Phenotype.Code")

dim(non_binary_Qs)

dim(non_binary_Qs)

write.csv(non_binary_Qs, "Non_binary_Qs_and_codes.csv")

#seperating non-binary questions into catagories 

#removing rendunant collumns

#cleaning out phenotypes with 0 as answers

length(which(non_binary$PH0000798 > 0))

dim(non_binary$PH0000777)

colnames(non_binary[11])

non_binary_clean = non_binary[-c(6,9,11)]

colnames(non_binary_clean)

#age first operation

colnames(non_binary_clean)

non_binary_age_1st_op = non_binary_clean[c(1,2,3)]
View(non_binary_age_1st_op)

#age 1st diagnosis 

non_binary_age_1st_diag = non_binary_clean[c (4,5,6,7,8,9,10,11,12,13)]


colnames(non_binary_clean)

dim(non_binary_age_1st_diag)

#checking phenotype PH0000611 to see how many useable obervations there are

length(which(non_binary_age_1st_diag$PH0000611 > 0))


#menopausal status

non_binary_meno_status = non_binary_clean[15]

dim(non_binary_meno_status)

#1st and last period

non_binary_period_1st_last = non_binary_clean[c(14,16)]

colnames(non_binary_period_1st_last)
#Hormone based contraception and hormone replacement therapy

non_binary_hrt_contra = non_binary_clean[c(17,18,19,20)]

#Pregnancy 

non_binary_pregnancy = non_binary_clean[c(21,22)]

dim(non_binary_pregnancy)

#might be worth figuring out mean and standard deviation for these
#theres a chance they will line up with the previous matrix you did

#mean and sd for age 1st op

dim(non_binary_age_1st_op)

age_1st_op_mean_sd = matrix(nrow = 3, ncol = 3)

for(i in c(1:ncol(non_binary_age_1st_op)))
  {
  age_1st_op_mean_sd[i,1] = mean(non_binary_age_1st_op[,i], na.rm = T)
  age_1st_op_mean_sd[i,2] = sd(non_binary_age_1st_op[,i], na.rm = T)
  age_1st_op_mean_sd[i,3] = median(non_binary_age_1st_op[,i], na.rm = T)
}

rownames(age_1st_op_mean_sd) = colnames(non_binary_age_1st_op)

colnames(age_1st_op_mean_sd) = c("Mean", "SD","median")

View(age_1st_op_mean_sd)

#mean and sd for age 1st diagnosis

dim(non_binary_age_1st_diag)

age_1st_diag_mean_sd = matrix(nrow = 10, ncol = 3)

for(i in c(1:ncol(non_binary_age_1st_diag)))
{
  age_1st_diag_mean_sd[i,1] = mean(non_binary_age_1st_diag[,i], na.rm = T)
  age_1st_diag_mean_sd[i,2] = sd(non_binary_age_1st_diag[,i], na.rm = T)
  age_1st_diag_mean_sd[i,3] = median(non_binary_age_1st_diag[,i], na.rm = T)
}

rownames(age_1st_diag_mean_sd) = colnames(non_binary_age_1st_diag)

colnames(age_1st_diag_mean_sd) = c("Mean", "SD","Median")

dim(age_1st_diag_mean_sd)


#mean and sd for 1st and last period

dim(non_binary_period_1st_last)

period_1st_last_mean_sd = matrix(nrow = 2, ncol = 3)

for(i in c(1:ncol(non_binary_period_1st_last)))
{
  period_1st_last_mean_sd[i,1] = mean(non_binary_period_1st_last[,i], na.rm = T)
  period_1st_last_mean_sd[i,2] = sd(non_binary_period_1st_last[,i], na.rm = T)
  period_1st_last_mean_sd[i,3] = median(non_binary_period_1st_last[,i], na.rm = T)
  print(max(non_binary_period_1st_last[,i], na.rm = T))
  print(min(non_binary_period_1st_last[,i], na.rm = T))
}

rownames(period_1st_last_mean_sd) = colnames(non_binary_period_1st_last)

colnames(period_1st_last_mean_sd) = c("Mean", "Standard Deviation", "Median")

dim(period_1st_last_mean_sd)

min(non_binary_period_1st_last[,2], na.rm = T)

which(non_binary_period_1st_last$PH0000787 == 17)

dim(non_binary_period_1st_last$PH0000787[211])

write.csv(period_1st_last_mean_sd, "Menopause_Statistics.csv")

length(which(non_binary_period_1st_last > 45)

non_binary_period_1st_last_rm = non_binary_period_1st_last$PH0000787[-c(211)]

dim(non_binary_period_1st_last_rm)

min(non_binary_period_1st_last_rm, na.rm = T)

#in honesty, whilst age of menses is relevant, Im not entirely sure how relevant the menarch is to gut bacteria (Correction, of course
#its relevant, its a significant amount of hormonal influx in a short amount of time. How this can be measured, is the question)

summary(non_binary_period_1st_last$PH0000787)

#mean and sd for hormone replacement therapy and 

dim(non_binary_hrt_contra)

hrt_contra_mean_sd_matrix = matrix(nrow = 4, ncol = 4)

for(i in c(1:ncol(non_binary_hrt_contra)))
{
  hrt_contra_mean_sd_matrix[i,2] = mean(non_binary_hrt_contra[,i], na.rm = T)
  hrt_contra_mean_sd_matrix[i,3] = sd(non_binary_hrt_contra[,i], na.rm = T)
  hrt_contra_mean_sd_matrix[i,4] = median(non_binary_hrt_contra[,i], na.rm = T)
}

rownames(hrt_contra_mean_sd_matrix) = colnames(non_binary_hrt_contra)

colnames(hrt_contra_mean_sd_matrix) = c("Years/Months","Mean", "SD","Median")

hrt_contra_mean_sd = as.data.frame(hrt_contra_mean_sd_matrix)

hrt_contra_mean_sd[is.na(hrt_contra_mean_sd)] <- c("Y","M","Y","M")

dim(hrt_contra_mean_sd)

#Great, we've figured out means and SD for all these phenotypes. We are still yet to actually perform statistical analysis on them.
#it might be worth intially going the same route as the binary phenotypes. establishing p.values, 1st through T.tests, then through linear
#regression on both alpha and beta diversity data.
#in order to do permanova we're going to have to probably do that on both binary and non-binary data, follow the pipeline outlined in
#the statistics presentations in order to establish which tests to perform on which datasets. We have efficiently compartmentalized each.

library(lmerTest)
library(lme4)

#ok so the linear regression model works on the continous data sets

#menopause status will probably not work as most subjects are post menopausal and wont really allow for comparison

dim(meno_status)

meno_status_df = data.frame(details,Alpha_D_specific, non_binary_meno_status)

dim(meno_status_df)

colnames(meno_status_df)

meno_status_df[is.na(meno_status_df)] <- 0

output_meno_status <- apply(meno_status_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000786 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = meno_status_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_meno_status_new = t(output_meno_status)

colnames(output_meno_status_new)=c("effect_size", "std_error","p.value")

View(output_meno_status_new)

write.csv(output_meno_status_new, "meno_status_alpha_lin_reg_output.csv")

colnames(meno_status_df)

library(ggplot2)


#menopause status linear regression test with top 50% of taxonomy

meno_status_taxa_df = data.frame(details,over_50_cols, non_binary_meno_status)

colnames(meno_status_taxa_df)

meno_status_taxa_df[is.na(meno_status_taxa_df)] <- 0

taxa_output_meno_status <- apply(meno_status_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000786 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = meno_status_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_meno_status_new = t(taxa_output_meno_status)

colnames(taxa_output_meno_status_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_meno_status_new)

#determining significant microbe taxa in menopause status


taxa_output_meno_status_new_df = as.data.frame(taxa_output_meno_status_new)

rownames(taxa_output_meno_status_new_df)

which(taxa_output_meno_status_new_df$p.value <= 0.05)

p.value_microbes_menopause = t(taxa_output_meno_status_new_df[c(5,21,35,64),])

menopause_microbes = meno_status_taxa_df[,which(colnames(meno_status_taxa_df)%in%colnames(p.value_microbes_menopause))]

dim(menopause_microbes)

rownames(taxa_output_meno_status_new_df[c(5,21,35,64),])
colnames(menopause_microbes)
colnames(p.value_microbes_menopause)

which(colnames(meno_status_taxa_df)== "PH0000786")

meno_status_statistically_relevant_taxa = data.frame(meno_status_taxa_df[c(1,89)],menopause_microbes)

dim(meno_status_statistically_relevant_taxa)

length(which(meno_status_statistically_relevant_taxa$PH0000786  == 1))


#do volcano plot
#direction of volcano plot determines if its positively associated or negatively associated 

#linear regression for age of 1st operation PH0000521(Age at first hysterectomy)

age_1st_op_df = data.frame(details,Alpha_D_specific,non_binary_age_1st_op)

dim(age_1st_op_df)

age_1st_op_df[is.na(age_1st_op_df)] <- 0

colnames(age_1st_op_df)

output_PH0000521 <- apply(age_1st_op_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000521 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_op_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000521_new = t(output_PH0000521)

colnames(output_PH0000521_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000521_new)

#simpson and shannon have p.value below 0.05

#linear regression for age of 1st operation PH0000523 (Age at first oophorectomy)

dim(age_1st_op_df)

output_PH0000523 <- apply(age_1st_op_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000523 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_op_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000523_new = t(output_PH0000523)

colnames(output_PH0000523_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000523_new)

#no significant p.values

#linear regression for age of 1st operation PH0000525(Age at first caesarean section)

colnames(age_1st_op_df)

output_PH0000525 <- apply(age_1st_op_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000525 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_op_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})
h
output_PH0000525_new = t(output_PH0000525)

colnames(output_PH0000525_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000525_new)

#no significant p.values

#linear regression to see significant taxa
#For PH0000521

age_1st_op_taxa_df = data.frame(details,over_50_cols, non_binary_age_1st_op)

colnames(age_1st_op_taxa_df)

age_1st_op_taxa_df[is.na(age_1st_op_taxa_df)] <- 0

taxa_output_PH0000521 <- apply(age_1st_op_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000521 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_op_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000521_new = t(taxa_output_PH0000521)

colnames(taxa_output_PH0000521_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000521_new)

#For PH0000523
#

taxa_output_PH0000523 <- apply(age_1st_op_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000523 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_op_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000523_new = t(taxa_output_PH0000523)

colnames(taxa_output_PH0000523_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000523_new)

#For PH0000525
#

taxa_output_PH0000525 <- apply(age_1st_op_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000525 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_op_taxa_df, REML=, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000525_new = t(taxa_output_PH0000525)

colnames(taxa_output_PH0000525_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000525_new)

#calculating relevant taxa for phenoypes reffering to age of 1st operation
#For PH0000521
#

taxa_output_PH0000521_new_df = as.data.frame(taxa_output_PH0000521_new)

dim(taxa_output_PH0000521_new_df)


which(taxa_output_PH0000521_new_df$p.value <= 0.05)


p.value_microbes_PH0000521 = t(taxa_output_PH0000521_new_df[c(15,17,43,63,76),])

PH0000521_microbes = age_1st_op_taxa_df[,which(colnames(age_1st_op_taxa_df)%in%colnames(p.value_microbes_PH0000521))]

dim(p.value_microbes_PH0000521)

rownames(taxa_output_PH0000521_new[c(15,17,43,63,76),])

colnames(PH0000521_microbes)

which(colnames(age_1st_op_df)== "PH0000521")

PH0000521_statistically_relevant_taxa = data.frame(age_1st_op_df[c(1,12)],PH0000521_microbes)

dim(PH0000521_statistically_relevant_taxa)

length(which(PH0000521_statistically_relevant_taxa$PH0000521 > 0 ))


#calculating relevant taxa for phenoype #For PH0000523
#

taxa_output_PH0000523_new_df = as.data.frame(taxa_output_PH0000523_new)

dim(taxa_output_PH0000523_new_df)


which(taxa_output_PH0000523_new_df$p.value <= 0.05)


p.value_microbes_PH0000523 = t(taxa_output_PH0000523_new_df[c(3,81),])

PH0000523_microbes = age_1st_op_taxa_df[,which(colnames(age_1st_op_taxa_df)%in%colnames(p.value_microbes_PH0000523))]

dim(p.value_microbes_PH0000523)

rownames(taxa_output_PH0000523_new[c(3,81),])

colnames(PH0000523_microbes)

which(colnames(age_1st_op_df)== "PH0000523")

PH0000523_statistically_relevant_taxa = data.frame(age_1st_op_df[c(1,13)],PH0000523_microbes)

dim(PH0000523_statistically_relevant_taxa)

length(which(PH0000523_statistically_relevant_taxa$PH0000523 > 0 ))

#calculating relevant taxa for phenoype #For PH0000525
#

taxa_output_PH0000525_new_df = as.data.frame(taxa_output_PH0000525_new)

dim(taxa_output_PH0000525_new_df)


which(taxa_output_PH0000525_new_df$p.value <= 0.05)


p.value_microbes_PH0000525 = t(taxa_output_PH0000525_new_df[c(2,30,48),])

PH0000525_microbes = age_1st_op_taxa_df[,which(colnames(age_1st_op_taxa_df)%in%colnames(p.value_microbes_PH0000525))]

dim(p.value_microbes_PH0000525)

rownames(taxa_output_PH0000525_new[c(2,30,48),])

colnames(PH0000525_microbes)

which(colnames(age_1st_op_df)== "PH0000525")

PH0000525_statistically_relevant_taxa = data.frame(age_1st_op_df[c(1,14)],PH0000525_microbes)

dim(PH0000525_statistically_relevant_taxa)

length(which(PH0000525_statistically_relevant_taxa$PH0000525 > 0 ))


#linear regression 

colnames(age_1st_op_taxa_df)

#Alpha Diversity linear regression for age 1st diagnosis PH0000578 (Age at first diagnosis of cervix cancer)

age_1st_diag_df = data.frame(details,Alpha_D_specific,non_binary_age_1st_diag)

colnames(non_binary_age_1st_diag)

age_1st_diag_df[is.na(age_1st_diag_df)] <- 0

output_PH0000578 <- apply(age_1st_diag_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000578 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000578_new = t(output_PH0000578)

colnames(output_PH0000578_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000578_new)

colnames(age_1st_diag_df)


#Alpha Diversity linear regression for age 1st diagnosis PH0000599 (What kind(s) of cancer have you been diagnosed with? \ Age at 1st diagnosis: \ Ovary)

colnames(age_1st_diag_df)

output_PH0000599 <- apply(age_1st_diag_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000599 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

dim(age_1st_diag_df$PH0000599)

output_PH0000599_new = t(output_PH0000599)

colnames(output_PH0000599_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000599_new)

length(which(age_1st_diag_df$PH0000599 > 1 ))

dim(merged_CQ_alphaD_final)

#Alpha diversity linear regression for age 1st diagnosis PH0000611: What kind(s) of cancer have you been diagnosed with? \ Age at 1st diagnosis: \ Uterus



output_PH0000611 <- apply(age_1st_diag_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000611 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})


output_PH0000611_new = t(output_PH0000611)

colnames(output_PH0000611_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000611_new)

length(which(age_1st_diag_df$PH0000611 > 0))

#
colnames(age_1st_diag_df)

#Alpha diversity linear regression for age 1st diagnosis PH0000765 : 	Has a doctor ever told you that you have/had any of the following conditions? \ Age when it 1st happened: \ A miscarriage

output_PH0000765 <- apply(age_1st_diag_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000765 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})


output_PH0000765_new = t(output_PH0000765)

colnames(output_PH0000765_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000765_new)

#Alpha diversiy linear regression for age 1st diagnosis PH0000768: Has a doctor ever told you that you have/had any of the following conditions? \ Age when it 1st happened: \ An ectopic pregnancy

output_PH0000768 <- apply(age_1st_diag_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000768 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})


output_PH0000768_new = t(output_PH0000768)

colnames(output_PH0000768_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000768_new)

colnames(age_1st_diag_df)

#Alpha diversiy linear regression for age 1st diagnosis PH0000771: Has a doctor ever told you that you have/had any of the following conditions? \ Age when it 1st happened: \ Endometriosis

output_PH0000771 <- apply(age_1st_diag_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000771 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})


output_PH0000771_new = t(output_PH0000771)

colnames(output_PH0000771_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000771_new)

colnames(age_1st_diag_df)

#no statistically relevant p.values

#Alpha diversiy linear regression for age 1st diagnosis PH0000774: Has a doctor ever told you that you have/had any of the following conditions? \ Age when it 1st happened: \ An ovarian cyst

output_PH0000774 <- apply(age_1st_diag_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000774 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})


output_PH0000774_new = t(output_PH0000774)

colnames(output_PH0000774_new)=c("effect_size", "std_error","p.value")

View(output_PH0000774_new)

colnames(age_1st_diag_df)

#no statistically relvant p.values for alpha diversity metrics for PH0000774

length(which(age_1st_diag_df$PH0000774 > 0))

#Alpha diversiy linear regression for age 1st diagnosis PH0000777: 	Has a doctor ever told you that you have/had any of the following conditions? \ Age when it 1st happened: \ A uterine cyst or ?fibroid?

output_PH0000777 <- apply(age_1st_diag_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000777 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})


output_PH0000777_new = t(output_PH0000777)

colnames(output_PH0000777_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000777_new)


length(which(age_1st_diag_df$PH0000777 > 0))

colnames(age_1st_diag_df)
#no statistcally relevant p.values for PH0000777

#Alpha diversity metrics for PH0000780 Has a doctor ever told you that you have/had any of the following conditions? \ Age when it 1st happened: \ An abnormal cervical smear

output_PH0000780 <- apply(age_1st_diag_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000780 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})


output_PH0000780_new = t(output_PH0000780)

colnames(output_PH0000780_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000780_new)


length(which(age_1st_diag_df$PH0000780 > 0))

colnames(age_1st_diag_df)

#Alpha diversity metrics for PH0000783: 	Has a doctor ever told you that you have/had any of the following conditions? \ Age when it 1st happened: \ Polycystic ovary syndrome

output_PH0000783 <- apply(age_1st_diag_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000783 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})


output_PH0000783_new = t(output_PH0000783)

colnames(output_PH0000783_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000783_new)

#No statistically relevant P.valueS for PH0000783

#calculating taxa with a statistically relevant p.value for phenotypes related to age 1st diagnosis 
#for PH0000578

age_1st_diag_taxa_df = data.frame(details,over_50_cols, non_binary_age_1st_diag)

colnames(age_1st_diag_taxa_df)

age_1st_diag_taxa_df[is.na(age_1st_diag_taxa_df)] <- 0

taxa_output_PH0000578 <- apply(age_1st_diag_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000578 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000578_new = t(taxa_output_PH0000578)

colnames(taxa_output_PH0000578_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000578_new)

#for PH0000599

taxa_output_PH0000599 <- apply(age_1st_diag_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000599 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000599_new = t(taxa_output_PH0000599)

colnames(taxa_output_PH0000599_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000599_new)

#for PH0000611

taxa_output_PH0000611 <- apply(age_1st_diag_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000611 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000611_new = t(taxa_output_PH0000611)

colnames(taxa_output_PH0000611_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000611_new)

colnames(age_1st_diag_df)
#for PH0000765

taxa_output_PH0000765 <- apply(age_1st_diag_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000765 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000765_new = t(taxa_output_PH0000765)

colnames(taxa_output_PH0000765_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000765_new)

colnames(age_1st_diag_df)

#PH0000768

taxa_output_PH0000768 <- apply(age_1st_diag_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000768 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000768_new = t(taxa_output_PH0000768)

colnames(taxa_output_PH0000768_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000768_new)

colnames(age_1st_diag_df)

#PH0000711

taxa_output_PH0000771 <- apply(age_1st_diag_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000771 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000771_new = t(taxa_output_PH0000771)

colnames(taxa_output_PH0000771_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000771_new)

colnames(age_1st_diag_df)

#PH0000774

taxa_output_PH0000774 <- apply(age_1st_diag_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000774 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000774_new = t(taxa_output_PH0000774)

colnames(taxa_output_PH0000774_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000774_new)

colnames(age_1st_diag_df)

#PH0000777

library(lmer)

taxa_output_PH0000777 <- apply(age_1st_diag_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000777 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000777_new = t(taxa_output_PH0000777)

colnames(taxa_output_PH0000777_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000777_new)

colnames(age_1st_diag_df)

#for PH0000780

taxa_output_PH0000780 <- apply(age_1st_diag_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000780 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000780_new = t(taxa_output_PH0000780)

colnames(taxa_output_PH0000780_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000780_new)

colnames(age_1st_diag_df)

#for PH0000783

taxa_output_PH0000783 <- apply(age_1st_diag_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000783 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = age_1st_diag_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000783_new = t(taxa_output_PH0000783)

colnames(taxa_output_PH0000783_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000783_new)

colnames(age_1st_diag_df)


##establish relevant taxa to each phenotype in age 1st diagnosis


taxa_output_PH0000578_new_df = as.data.frame(taxa_output_PH0000578_new)

which(taxa_output_PH0000578_new_df$p.value <= 0.05)


p.value_microbes_PH0000578 = t(taxa_output_PH0000578_new_df[c(39,50,66),])

dim(taxa_output_PH0000578_new[c(39,50,66),])

#all positive effect sizes

PH0000578_microbes = age_1st_diag_taxa_df[,which(colnames(age_1st_diag_taxa_df)%in%colnames(p.value_microbes_PH0000578))]

dim(PH0000578_microbes)

rownames(taxa_output_PH0000578_new[c(39,50,66),])

colnames(PH0000578_microbes)

which(colnames(age_1st_diag_df)== "PH0000578")

PH0000578_statistically_relevant_taxa = data.frame(age_1st_diag_df[c(1,12)],PH0000578_microbes)

dim(PH0000578_statistically_relevant_taxa)

length(which(PH0000578_statistically_relevant_taxa$PH0000578 > 0 ))

colnames(age_1st_diag_df)

#
#for PH0000599

taxa_output_PH0000599_new_df = as.data.frame(taxa_output_PH0000599_new)

which(taxa_output_PH0000599_new_df$p.value <= 0.05)

p.value_microbes_PH0000599 = t(taxa_output_PH0000599_new_df[c(73),])

colnames(p.value_microbes_PH0000599)

#positive effect size

which( colnames(age_1st_diag_taxa_df) == "k__Bacteria.p__Firmicutes.c__Negativicutes.o__Selenomonadales.f__Veillonellaceae.g__Dialister.s__Dialister_invisus")


which(colnames(age_1st_diag_taxa_df)== "ParticipantID")

dim(age_1st_diag_taxa_df)

PH0000599_statistically_relevant_taxa = data.frame(age_1st_diag_taxa_df[c(1,90,79)])

dim(PH0000599_statistically_relevant_taxa)

length(which(PH0000599_statistically_relevant_taxa$PH0000599 > 0 ))

colnames(age_1st_diag_df)

#PH0000611

taxa_output_PH0000611_new_df = as.data.frame(taxa_output_PH0000611_new)

rownames(taxa_output_PH0000611_new_df)

which(taxa_output_PH0000611_new_df$p.value <= 0.05)

rownames(taxa_output_PH0000611_new_df[45,],)

which( colnames(age_1st_diag_taxa_df) == "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Lachnospiraceae.g__Blautia.s__Ruminococcus_gnavus")

#all positive effect sizes

which(colnames(age_1st_diag_taxa_df)== "PH0000611")

PH0000611_statistically_relevant_taxa = data.frame(age_1st_diag_taxa_df[c(1,91,51)])

dim(PH0000611_statistically_relevant_taxa)

length(which(PH0000611_statistically_relevant_taxa$PH0000611 > 0 ))

colnames(age_1st_diag_df)

#PH0000765

taxa_output_PH0000765_new_df = as.data.frame(taxa_output_PH0000765_new)

rownames(taxa_output_PH0000765_new_df)

which(taxa_output_PH0000765_new_df$p.value <= 0.05)

p.value_microbes_PH0000765 = t(taxa_output_PH0000765_new_df[c(4,9,26,64,76),])

dim(taxa_output_PH0000765_new[c(4,9,26,64,76),])

#3 positive, 2 negative, check above for exact numbers

PH0000765_microbes = age_1st_diag_taxa_df[,which(colnames(age_1st_diag_taxa_df)%in%colnames(p.value_microbes_PH0000765))]

dim(PH0000765_microbes)

rownames(taxa_output_PH0000765_new[c(4,9,26,64,76),])

colnames(PH0000765_microbes)

which(colnames(age_1st_diag_df)== "PH0000765")

PH0000765_statistically_relevant_taxa = data.frame(age_1st_diag_df[c(1,15)],PH0000765_microbes)

dim(PH0000765_statistically_relevant_taxa)

length(which(PH0000765_statistically_relevant_taxa$PH0000765 > 0 ))

colnames(age_1st_diag_df)

#PH0000768

taxa_output_PH0000768_new_df = as.data.frame(taxa_output_PH0000768_new)

rownames(taxa_output_PH0000768_new_df)

which(taxa_output_PH0000768_new_df$p.value <= 0.05)

p.value_microbes_PH0000768 = t(taxa_output_PH0000768_new_df[c(12,29,41,47,63),])

dim(taxa_output_PH0000768_new[c(12,29,41,47,63),])

#all positive effect size except: k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Lachnospiraceae.g__Blautia.s__Ruminococcus_torques

colnames(t(taxa_output_PH0000768_new_df[c(12,29,41,47,63),]))
colnames(p.value_microbes_PH0000768)

PH0000768_microbes = age_1st_diag_taxa_df[,which(colnames(age_1st_diag_taxa_df)%in%colnames(p.value_microbes_PH0000768))]

dim(PH0000768_microbes)

colnames(PH0000768_microbes)

which(colnames(age_1st_diag_df)== "PH0000768")

PH0000768_statistically_relevant_taxa = data.frame(age_1st_diag_df[c(1,16)],PH0000768_microbes)

dim(PH0000768_statistically_relevant_taxa)

length(which(PH0000768_statistically_relevant_taxa$PH0000768 > 0 ))

colnames(age_1st_diag_df)

#for PH0000771

taxa_output_PH0000771_new_df = as.data.frame(taxa_output_PH0000771_new)

which(taxa_output_PH0000771_new_df$p.value <= 0.05)

p.value_microbes_PH0000771 = t(taxa_output_PH0000771_new_df[c( 48,57,75),])

dim(taxa_output_PH0000771_new[c(48,57,75),])

#all negative effect size

colnames(t(taxa_output_PH0000771_new_df[c(48,57,75),]))
colnames(p.value_microbes_PH0000771)

PH0000771_microbes = age_1st_diag_taxa_df[,which(colnames(age_1st_diag_taxa_df)%in%colnames(p.value_microbes_PH0000771))]

dim(PH0000771_microbes)

colnames(PH0000771_microbes)

which(colnames(age_1st_diag_df)== "PH0000771")

PH0000771_statistically_relevant_taxa = data.frame(age_1st_diag_df[c(1,17)],PH0000771_microbes)

dim(PH0000771_statistically_relevant_taxa)

length(which(PH0000771_statistically_relevant_taxa$PH0000771 > 0 ))

colnames(age_1st_diag_df)

#PH0000774

taxa_output_PH0000774_new_df = as.data.frame(taxa_output_PH0000774_new)

which(taxa_output_PH0000774_new_df$p.value <= 0.05)

p.value_microbes_PH0000774 = t(taxa_output_PH0000774_new_df[c(9,47,65,79),])

dim(taxa_output_PH0000774_new[c(9,47,65,79),])

#all positve effect size except k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae.g__Bacteroides.s__Bacteroides_caccae	

colnames(t(taxa_output_PH0000774_new_df[c(9,47,65,79),]))
colnames(p.value_microbes_PH0000774)

PH0000774_microbes = age_1st_diag_taxa_df[,which(colnames(age_1st_diag_taxa_df)%in%colnames(p.value_microbes_PH0000774))]

dim(PH0000774_microbes)

colnames(PH0000774_microbes)

which(colnames(age_1st_diag_df)== "PH0000774")

PH0000774_statistically_relevant_taxa = data.frame(age_1st_diag_df[c(1,18)],PH0000774_microbes)

dim(PH0000774_statistically_relevant_taxa)

length(which(PH0000774_statistically_relevant_taxa$PH0000774 > 0 ))

colnames(age_1st_diag_df)

#for PH0000777


taxa_output_PH0000777_new_df = as.data.frame(taxa_output_PH0000777_new)

which(taxa_output_PH0000777_new_df$p.value <= 0.05)

p.value_microbes_PH0000777 = t(taxa_output_PH0000777_new_df[c( 12,43,74,75),])

dim(taxa_output_PH0000777_new[c(12,43,74,75),])

#all positive effect size except k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Eubacteriaceae.g__Eubacterium.s__Eubacterium_ventriosum

colnames(t(taxa_output_PH0000777_new_df[c(12,43,74,75),]))
colnames(p.value_microbes_PH0000777)

PH0000777_microbes = age_1st_diag_taxa_df[,which(colnames(age_1st_diag_taxa_df)%in%colnames(p.value_microbes_PH0000777))]

dim(PH0000777_microbes)

colnames(PH0000777_microbes)

which(colnames(age_1st_diag_df)== "PH0000777")

PH0000777_statistically_relevant_taxa = data.frame(age_1st_diag_df[c(1,19)],PH0000777_microbes)

dim(PH0000777_statistically_relevant_taxa)

length(which(PH0000777_statistically_relevant_taxa$PH0000777 > 0 ))

colnames(age_1st_diag_df)

#for PH0000780

taxa_output_PH0000780_new_df = as.data.frame(taxa_output_PH0000780_new)

which(taxa_output_PH0000780_new_df$p.value <= 0.05)

p.value_microbes_PH0000780 = t(taxa_output_PH0000780_new_df[c(18,22,24,59,67,69),])

dim(taxa_output_PH0000780_new[c(18,22,24,59,67,69),])

#two positive, four negative 

colnames(t(taxa_output_PH0000780_new_df[c(18,22,24,59,67,69),]))
colnames(p.value_microbes_PH0000780)

PH0000780_microbes = age_1st_diag_taxa_df[,which(colnames(age_1st_diag_taxa_df)%in%colnames(p.value_microbes_PH0000780))]

dim(PH0000780_microbes)

colnames(PH0000780_microbes)

which(colnames(age_1st_diag_df)== "PH0000780")

PH0000780_statistically_relevant_taxa = data.frame(age_1st_diag_df[c(1,20)],PH0000780_microbes)

dim(PH0000780_statistically_relevant_taxa)

length(which(PH0000780_statistically_relevant_taxa$PH0000780 > 0 ))

colnames(age_1st_diag_df)

#PH0000783

taxa_output_PH0000783_new_df = as.data.frame(taxa_output_PH0000783_new)

which(taxa_output_PH0000783_new_df$p.value <= 0.05)

p.value_microbes_PH0000783 = t(taxa_output_PH0000783_new_df[c(4,5,8,41),])

dim(taxa_output_PH0000783_new[c(4,5,8,41),])

#all positive 

colnames(t(taxa_output_PH0000783_new_df[c(4,5,8,41),]))
colnames(p.value_microbes_PH0000783)

PH0000783_microbes = age_1st_diag_taxa_df[,which(colnames(age_1st_diag_taxa_df)%in%colnames(p.value_microbes_PH0000783))]

dim(PH0000783_microbes)

colnames(PH0000783_microbes)

which(colnames(age_1st_diag_df)== "PH0000783")

PH0000783_statistically_relevant_taxa = data.frame(age_1st_diag_df[c(1,21)],PH0000783_microbes)

dim(PH0000783_statistically_relevant_taxa)

length(which(PH0000783_statistically_relevant_taxa$PH0000783 > 20 ))

colnames(age_1st_diag_df)


#all linear regression both alpha and beta completed on age first diagnosis relevant phenotypes and relevant taxa indentified withint
#the parameters of the top 50 percent of population taxa  

#Now HRT therapy and contraception phenotypes to be investigated

#Alpha diversity linear regression for phenotypes relevant to Hormone replacement therapy and contraception medication

#for PH0000790: Altogether, about how long have you taken/did you take HRT \ years

hrt_contra_df = data.frame(details,Alpha_D_specific,non_binary_hrt_contra)

colnames(non_binary_hrt_contra)

hrt_contra_df[is.na(hrt_contra_df)] <- 0

output_PH0000790 <- apply(hrt_contra_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000790 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_contra_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000790_new = t(output_PH0000790)

colnames(output_PH0000790_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000790_new)

colnames(hrt_contra_df)

#no statistically relevant alpha diversity phenotypes
# P0000791: Altogether, about how long have you taken/did you take HRT \ months

output_PH0000791 <- apply(hrt_contra_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000791 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_contra_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000791_new = t(output_PH0000791)

colnames(output_PH0000791_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000791_new)

colnames(hrt_contra_df)

#no statistically relevant alpha diversity phenotypes

#PH0000793 	Altogether, about how long have you taken/did you take hormone-based contraception? \ years

output_PH0000793 <- apply(hrt_contra_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000793 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_contra_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000793_new = t(output_PH0000793)

colnames(output_PH0000793_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000793_new)

colnames(hrt_contra_df)

#no statistically relevant alpha diversity phenotypes

#PH0000794: Altogether, about how long have you taken/did you take hormone-based contraception? \ months


output_PH0000794 <- apply(hrt_contra_df[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000794 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_contra_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000794_new = t(output_PH0000794)

colnames(output_PH0000794_new)=c("effect_size", "std_error","p.value")

dim(output_PH0000794_new)

colnames(hrt_contra_df)

#no statistically relevant alpha diversity phenotypes, however, observed otus has a p.value of 0.05529038

#Beta diversity linear regression next

hrt_contra_taxa_df = data.frame(details,over_50_cols, non_binary_hrt_contra)

colnames(hrt_contra_taxa_df[, 7:88],)

hrt_contra_taxa_df[is.na(hrt_contra_taxa_df)] <- 0

#for PH0000790

taxa_output_PH0000790 <- apply(hrt_contra_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000790 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_contra_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000790_new = t(taxa_output_PH0000790)

colnames(taxa_output_PH0000790_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000790_new)

#for PH0000791

taxa_output_PH0000791 <- apply(hrt_contra_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000791 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_contra_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000791_new = t(taxa_output_PH0000791)

colnames(taxa_output_PH0000791_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000791_new)

#for PH0000793


taxa_output_PH0000793 <- apply(hrt_contra_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000793 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_contra_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000793_new = t(taxa_output_PH0000793)

colnames(taxa_output_PH0000793_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000793_new)

#PH0000794

taxa_output_PH0000794 <- apply(hrt_contra_taxa_df[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000794 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = hrt_contra_taxa_df, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000794_new = t(taxa_output_PH0000794)

colnames(taxa_output_PH0000794_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000794_new)


#figuring out relevant taxa for hrt, contraception
#PH0000790

taxa_output_PH0000790_new_df = as.data.frame(taxa_output_PH0000790_new)

which(taxa_output_PH0000790_new_df$p.value <= 0.05)

p.value_microbes_PH0000790 = t(taxa_output_PH0000790_new_df[c(2,11,13,26,68,81),])

dim(taxa_output_PH0000790_new[c(2,11,13,26,68,81),])

#all positive positive except: k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Ruminococcaceae.g__Subdoligranulum.s__Subdoligranulum_unclassified	

colnames(t(taxa_output_PH0000790_new_df[c(2,11,13,26,68,81),]))
colnames(p.value_microbes_PH0000790)

PH0000790_microbes = hrt_contra_taxa_df[,which(colnames(hrt_contra_taxa_df)%in%colnames(p.value_microbes_PH0000790))]

dim(PH0000790_microbes)

colnames(PH0000790_microbes)

which(colnames(hrt_contra_taxa_df)== "PH0000790")

PH0000790_statistically_relevant_taxa = data.frame(hrt_contra_taxa_df[c(1,89)],PH0000790_microbes)

dim(PH0000790_statistically_relevant_taxa)

length(which(PH0000790_statistically_relevant_taxa$PH0000790 > 0 ))

colnames(hrt_contra_df)

#figuring out relevant taxa for hrt, contraception 
#PH0000791

taxa_output_PH0000791_new_df = as.data.frame(taxa_output_PH0000791_new)

which(taxa_output_PH0000791_new_df$p.value <= 0.05)

rownames(taxa_output_PH0000791_new_df[17,],)

which( colnames(hrt_contra_taxa_df) == "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae.g__Bacteroides.s__Bacteroides_vulgatus")

#all positive effect sizes

which(colnames(hrt_contra_taxa_df)== "PH0000791")

which(colnames(hrt_contra_taxa_df)== "ParticipantID")

PH0000791_statistically_relevant_taxa = data.frame(hrt_contra_taxa_df[c(1,90,23)])

dim(PH0000791_statistically_relevant_taxa)

length(which(PH0000791_statistically_relevant_taxa$PH0000791 > 0 ))

colnames(hrt_contra_taxa_df)

#PH0000793

taxa_output_PH0000793_new_df = as.data.frame(taxa_output_PH0000793_new)

which(taxa_output_PH0000793_new_df$p.value <= 0.05)
taxa_output_PH0000793_new_df = as.data.frame(taxa_output_PH0000793_new)

dim(taxa_output_PH0000793_new[c(11,60,64),])

p.value_microbes_PH0000793 = t(taxa_output_PH0000790_new_df[c(11,60,64),])

#all negative except: k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae.g__Bacteroides.s__Bacteroides_dorei	

colnames(t(taxa_output_PH0000793_new_df[c(11,60,64),]))
colnames(p.value_microbes_PH0000793)

PH0000793_microbes = hrt_contra_taxa_df[,which(colnames(hrt_contra_taxa_df)%in%colnames(p.value_microbes_PH0000793))]

dim(PH0000793_microbes)

colnames(PH0000793_microbes)

which(colnames(hrt_contra_taxa_df)== "PH0000793")

PH0000793_statistically_relevant_taxa = data.frame(hrt_contra_taxa_df[c(1,91)],PH0000793_microbes)

dim(PH0000793_statistically_relevant_taxa)

length(which(PH0000793_statistically_relevant_taxa$PH0000793 > 0 ))

colnames(hrt_contra_df)

#PH0000794

taxa_output_PH0000794_new_df = as.data.frame(taxa_output_PH0000794_new)

which(taxa_output_PH0000794_new_df$p.value <= 0.05)

p.value_microbes_PH0000794 = t(taxa_output_PH0000794_new_df[c(25,54,82),])

dim(taxa_output_PH0000794_new[c(25,54,82),])

#all positive positive except: k__Bacteria.p__Verrucomicrobia.c__Verrucomicrobiae.o__Verrucomicrobiales.f__Verrucomicrobiaceae.g__Akkermansia.s__Akkermansia_muciniphila

colnames(t(taxa_output_PH0000794_new_df[c(25,54,82),]))
colnames(p.value_microbes_PH0000794)

PH0000794_microbes = hrt_contra_taxa_df[,which(colnames(hrt_contra_taxa_df)%in%colnames(p.value_microbes_PH0000794))]

dim(PH0000794_microbes)

colnames(PH0000794_microbes)

which(colnames(hrt_contra_taxa_df)== "PH0000794")

PH0000794_statistically_relevant_taxa = data.frame(hrt_contra_taxa_df[c(1,92)],PH0000794_microbes)

dim(PH0000794_statistically_relevant_taxa)

length(which(PH0000794_statistically_relevant_taxa$PH0000794 > 0 ))

#Final stretch: tying up lose knots and doing any additional analysis you can fit in

#making dataframe to perform linear regression on phenotypes not yet explored with alpha diversity 

colnames(binary)

forgotten_binary_children_alphaD = data.frame(details,Alpha_D_specific, binary[c(4,7,9,11,15,25,26)])

dim(forgotten_binary_children_alphaD)

#AlphaD linear regression for PH0000577 (have you ever had cervix cancer)

output_PH0000577 <- apply(forgotten_binary_children_alphaD[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000577 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_alphaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000577_new = t(output_PH0000577)

colnames(output_PH0000577_new)=c("effect_size", "std_error","p.value")


#No relevant AlphaD p.values for  PH0000577

#AlphaD linear regression for PH0000610 (Have you ever had Uterus Cancer)

output_PH0000610 <- apply(forgotten_binary_children_alphaD[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000610 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_alphaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000610_new = t(output_PH0000610)

colnames(output_PH0000610_new)=c("effect_size", "std_error","p.value")

#No relevant AlphaD p.values for PH0000610

#AlphaD linear regression for PH0000764 (Have you ever had a miscarriage)

forgotten_binary_children_alphaD[is.na(forgotten_binary_children_alphaD)] <- 0

output_PH0000764 <- apply(forgotten_binary_children_alphaD[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000764 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_alphaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000764_new = t(output_PH0000764)

colnames(output_PH0000764_new)=c("effect_size", "std_error","p.value")

#no relevant p.values for PH0000764

#AlphaD linear regression for PH0000770 (Ever had endometrosis)

output_PH0000770 <- apply(forgotten_binary_children_alphaD[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000770 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_alphaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000770_new = t(output_PH0000770)

colnames(output_PH0000770_new)=c("effect_size", "std_error","p.value")

#No statistically relevant p.values

#AlphaD linear regression for PH0000776 (Ever had a uterine cyst or fibroid)

output_PH0000776 <- apply(forgotten_binary_children_alphaD[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000776 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_alphaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000776_new = t(output_PH0000776)

colnames(output_PH0000776_new)=c("effect_size", "std_error","p.value")

#No statistically relevant p.values

#AlphaD linear regression for PH0000796 (Ever become pregnant)

output_PH0000796 <- apply(forgotten_binary_children_alphaD[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000796 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_alphaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000796_new = t(output_PH0000796)

colnames(output_PH0000796_new)=c("effect_size", "std_error","p.value")

#No statistically relevant p.values

#AlphaD linear regression for PH0000799 (Ever had a caesarian section)

output_PH0000799 <- apply(forgotten_binary_children_alphaD[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000799 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_alphaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000799_new = t(output_PH0000799)

colnames(output_PH0000799_new)=c("effect_size", "std_error","p.value")

#No statistically relevant p.values

#

#making dataframe to perform linear regression on phenotypes not yet explored with beta diversity

forgotten_binary_children_betaD = data.frame(details,over_50_cols, binary[c(4,7,9,11,15,25,26)])

#linear regression for beta diversity of phenotypes not yet explored

#

#betaD linear regression for PH0000577 (have you ever had cervix cancer) and statistically relevant taxa calculation

taxa_output_PH0000577 <- apply(forgotten_binary_children_betaD[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000577 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_betaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000577_new = t(taxa_output_PH0000577)

colnames(taxa_output_PH0000577_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000577_new)

taxa_output_PH0000577_new_df = as.data.frame(taxa_output_PH0000577_new)

which(taxa_output_PH0000577_new_df$p.value <= 0.05)

p.value_microbes_PH0000577 = t(taxa_output_PH0000790_new_df[c(39,42,50,59),])

colnames(t(taxa_output_PH0000577_new_df[c(39,42,50,59),]))
colnames(p.value_microbes_PH0000577)

PH0000577_microbes = forgotten_binary_children_betaD[,which(colnames(forgotten_binary_children_betaD)%in%colnames(p.value_microbes_PH0000577))]

dim(PH0000577_microbes)

colnames(PH0000577_microbes)

which(colnames(forgotten_binary_children_betaD)== "ParticipantID")

PH0000577_statistically_relevant_taxa = data.frame(forgotten_binary_children_betaD[c(1,89)],PH0000577_microbes)

#betaD linear regression for PH0000610 (have you ever had uterus cancer) and statistically relevant taxa calculation

taxa_output_PH0000610 <- apply(forgotten_binary_children_betaD[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000610 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_betaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000610_new = t(taxa_output_PH0000610)

colnames(taxa_output_PH0000610_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000610_new)

taxa_output_PH0000610_new_df = as.data.frame(taxa_output_PH0000610_new)

which(taxa_output_PH0000610_new_df$p.value <= 0.05)

p.value_microbes_PH0000610 = t(taxa_output_PH0000790_new_df[c(5,34),])

colnames(t(taxa_output_PH0000610_new_df[c(5,34),]))
colnames(p.value_microbes_PH0000610)

PH0000610_microbes = forgotten_binary_children_betaD[,which(colnames(forgotten_binary_children_betaD)%in%colnames(p.value_microbes_PH0000610))]

dim(PH0000610_microbes)

colnames(PH0000610_microbes)

which(colnames(forgotten_binary_children_betaD)== "PH0000610")

PH0000610_statistically_relevant_taxa = data.frame(forgotten_binary_children_betaD[c(1,90)],PH0000610_microbes)

#betaD linear regression for PH0000764 (have you ever had a miscarriage) and statistically relevant taxa calculation

forgotten_binary_children_betaD[is.na(forgotten_binary_children_betaD)] <- 0

taxa_output_PH0000764 <- apply(forgotten_binary_children_betaD[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000764 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_betaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000764_new = t(taxa_output_PH0000764)

colnames(taxa_output_PH0000764_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000764_new)

taxa_output_PH0000764_new_df = as.data.frame(taxa_output_PH0000764_new)

which(taxa_output_PH0000764_new_df$p.value <= 0.05)

p.value_microbes_PH0000764 = t(taxa_output_PH0000790_new_df[c(4,9,21,26,76),])

colnames(t(taxa_output_PH0000764_new_df[c(4,9,21,26,76),]))
colnames(p.value_microbes_PH0000764)

PH0000764_microbes = forgotten_binary_children_betaD[,which(colnames(forgotten_binary_children_betaD)%in%colnames(p.value_microbes_PH0000764))]


colnames(PH0000764_microbes)

which(colnames(forgotten_binary_children_betaD)== "PH0000764")

PH0000764_statistically_relevant_taxa = data.frame(forgotten_binary_children_betaD[c(1,91)],PH0000764_microbes)

#betaD linear regression for PH0000770 (have you ever had endometrosis) and statistically relevant taxa calculation

taxa_output_PH0000770 <- apply(forgotten_binary_children_betaD[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000770 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_betaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000770_new = t(taxa_output_PH0000770)

colnames(taxa_output_PH0000770_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000770_new)

taxa_output_PH0000770_new_df = as.data.frame(taxa_output_PH0000770_new)

which(taxa_output_PH0000770_new_df$p.value <= 0.05)

p.value_microbes_PH0000770 = t(taxa_output_PH0000790_new_df[c(82),])

colnames(t(taxa_output_PH0000770_new_df[c(82),]))
dim(p.value_microbes_PH0000770)

PH0000770_microbes = forgotten_binary_children_betaD[,which(colnames(forgotten_binary_children_betaD)%in%colnames(p.value_microbes_PH0000770))]

head(PH0000770_microbes)

which(colnames(forgotten_binary_children_betaD)== "k__Bacteria.p__Verrucomicrobia.c__Verrucomicrobiae.o__Verrucomicrobiales.f__Verrucomicrobiaceae.g__Akkermansia.s__Akkermansia_muciniphila")

colnames(PH0000770_microbes)

which(colnames(forgotten_binary_children_betaD)== "PH0000770")

PH0000770_statistically_relevant_taxa = data.frame(forgotten_binary_children_betaD[c(1,92,88)])

dim(PH0000770_statistically_relevant_taxa)

#N.B. Negative effect size for this microbe.

#Linear regression for beta diversity of PH0000776, (ever had a uterine cyst or fibroid) and taxa calculation

taxa_output_PH0000776 <- apply(forgotten_binary_children_betaD[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000776 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_betaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000776_new = t(taxa_output_PH0000776)

colnames(taxa_output_PH0000776_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000776_new)

taxa_output_PH0000776_new_df = as.data.frame(taxa_output_PH0000776_new)

which(taxa_output_PH0000776_new_df$p.value <= 0.05)

p.value_microbes_PH0000776 = t(taxa_output_PH0000790_new_df[c(11,12,43,74),])

colnames(t(taxa_output_PH0000776_new_df[c(11,12,43,74),]))
colnames(p.value_microbes_PH0000776)

PH0000776_microbes = forgotten_binary_children_betaD[,which(colnames(forgotten_binary_children_betaD)%in%colnames(p.value_microbes_PH0000776))]


colnames(PH0000776_microbes)

which(colnames(forgotten_binary_children_betaD)== "PH0000776")

PH0000776_statistically_relevant_taxa = data.frame(forgotten_binary_children_betaD[c(1,93)],PH0000776_microbes)

#Linear regression for beta diversity of PH0000796, (ever tried to become pregnant) and taxa calculation

taxa_output_PH0000796 <- apply(forgotten_binary_children_betaD[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000796 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_betaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000796_new = t(taxa_output_PH0000796)

colnames(taxa_output_PH0000796_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000796_new)

taxa_output_PH0000796_new_df = as.data.frame(taxa_output_PH0000796_new)

which(taxa_output_PH0000796_new_df$p.value <= 0.05)

p.value_microbes_PH0000796 = t(taxa_output_PH0000790_new_df[c(20,23,65,67),])

colnames(t(taxa_output_PH0000796_new_df[c(20,23,65,67),]))
colnames(p.value_microbes_PH0000796)

PH0000796_microbes = forgotten_binary_children_betaD[,which(colnames(forgotten_binary_children_betaD)%in%colnames(p.value_microbes_PH0000796))]


colnames(PH0000796_microbes)

which(colnames(forgotten_binary_children_betaD)== "PH0000796")

PH0000796_statistically_relevant_taxa = data.frame(forgotten_binary_children_betaD[c(1,94)],PH0000796_microbes)

#Linear regression for beta diversity of PH0000796, (ever had a caesarian section) and taxa calculation

taxa_output_PH0000799 <- apply(forgotten_binary_children_betaD[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000799 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = forgotten_binary_children_betaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000799_new = t(taxa_output_PH0000799)

colnames(taxa_output_PH0000799_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000799_new)

taxa_output_PH0000799_new_df = as.data.frame(taxa_output_PH0000799_new)

which(taxa_output_PH0000799_new_df$p.value <= 0.05)

p.value_microbes_PH0000799 = t(taxa_output_PH0000790_new_df[c(30,35,48,49),])

colnames(t(taxa_output_PH0000799_new_df[c(30,35,48,49),]))
colnames(p.value_microbes_PH0000799)

PH0000799_microbes = forgotten_binary_children_betaD[,which(colnames(forgotten_binary_children_betaD)%in%colnames(p.value_microbes_PH0000799))]

colnames(PH0000799_microbes)

which(colnames(forgotten_binary_children_betaD)== "PH0000799")

PH0000799_statistically_relevant_taxa = data.frame(forgotten_binary_children_betaD[c(1,95)],PH0000799_microbes)

#making dataframe to perform linear regression the period phenotypes phenotypes not yet explored with alpha diversity 

#alpha diversity linear regression for age 1st menstrual period (PH0000785)

period_1st_last_alphaD = data.frame(details,Alpha_D_specific, non_binary_period_1st_last)

period_1st_last_alphaD[is.na(period_1st_last_alphaD)] <- 0

output_PH0000785 <- apply(period_1st_last_alphaD[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000785 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = period_1st_last_alphaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

output_PH0000785_new = t(output_PH0000785)

colnames(output_PH0000785_new)=c("effect_size", "std_error","p.value")
dim(output_PH0000785_new)

#no statistically significant p.values

#alpha diversity linear regression for How old were you when you became post-menopausal  (PH0000787)

output_PH0000787 <- apply(period_1st_last_alphaD[, 7:11], 2, function(i){
  fit <- lmer (i ~ PH0000787 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = period_1st_last_alphaD, REML=F, na.action=na.omit)
  null <- lmer (All$Response ~ (1|All$Fam) + (1|All$ZG2) + All$Shannon + All$Age2 + All$BMI2 ,REML = F)
  a <- anova(fit,null)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],a$"Pr(>Chisq)"[2],summary(fit)$devcomp$dims[1])
})

output_PH0000787_new = t(output_PH0000787)

colnames(output_PH0000787_new)=c("effect_size", "std_error","p.value","Chisq")

dim(output_PH0000787_new)

#no statistically significant p.values

#making dataframe to perform linear regression the period phenotypes phenotypes not yet explored with beta diversity 

period_1st_last_betaD = data.frame(details,over_50_cols, non_binary_period_1st_last)

period_1st_last_betaD[is.na(period_1st_last_betaD)] <- 0

#Beta diversity linear regression for age 1st menstrual period (PH0000785) and taxa calculation

taxa_output_PH0000785 <- apply(period_1st_last_betaD[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000785 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = period_1st_last_betaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000785_new = t(taxa_output_PH0000785)

colnames(taxa_output_PH0000785_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000785_new)

taxa_output_PH0000785_new_df = as.data.frame(taxa_output_PH0000785_new)

which(taxa_output_PH0000785_new_df$p.value <= 0.05)

p.value_microbes_PH0000785 = t(taxa_output_PH0000790_new_df[c(10,40,49),])

colnames(t(taxa_output_PH0000785_new_df[c(10,40,49),]))
colnames(p.value_microbes_PH0000785)

PH0000785_microbes = period_1st_last_betaD[,which(colnames(period_1st_last_betaD)%in%colnames(p.value_microbes_PH0000785))]

colnames(PH0000785_microbes)

which(colnames(period_1st_last_betaD)== "PH0000785")

PH0000785_statistically_relevant_taxa = data.frame(period_1st_last_betaD[c(1,89)],PH0000785_microbes)

#Beta diversity linear regression for How old were you when you became post-menopausal (PH0000787) and taxa calculation

taxa_output_PH0000787 <- apply(period_1st_last_betaD[, 7:88], 2, function(i){
  fit <- lmer (i ~ PH0000787 + BMI + SEX.x + age.16S + (1|Family_No.x) + (1|ZYGOSITY), data = period_1st_last_betaD, REML=F, na.action=na.omit)
  tbl<-summary(fit)$coef
  results <- c(tbl[2,1],tbl[2,2],tbl[2,5])
})

taxa_output_PH0000787_new = t(taxa_output_PH0000787)

colnames(taxa_output_PH0000787_new)=c("effect_size", "std_error","p.value")

dim(taxa_output_PH0000787_new)

taxa_output_PH0000787_new_df = as.data.frame(taxa_output_PH0000787_new)

which(taxa_output_PH0000787_new_df$p.value <= 0.05)

p.value_microbes_PH0000787 = t(taxa_output_PH0000790_new_df[c(2,12,24,48,68,81),])

colnames(t(taxa_output_PH0000787_new_df[c(2,12,24,48,68,81),]))
colnames(p.value_microbes_PH0000787)

PH0000787_microbes = period_1st_last_betaD[,which(colnames(period_1st_last_betaD)%in%colnames(p.value_microbes_PH0000787))]

colnames(PH0000787_microbes)

which(colnames(period_1st_last_betaD)== "PH0000787")

PH0000787_statistically_relevant_taxa = data.frame(period_1st_last_betaD[c(1,90)],PH0000787_microbes)

#calculating relative abundance of statistcally relevant taxa using yes as diseased and no as control in binary phenotypesj

#Calculating abundance of taxa identified as statisitcally significant for Ovary related disorders

#for those who answered yes for PH0000520

PH0000520_df = data.frame(binary$PH0000520,PH0000520_microbes)

PH0000520_YES <- subset(PH0000520_df, binary.PH0000520 == 1)

dim(PH0000520_YES)

colnames(PH0000520_YES)

only_taxa_520_yes = PH0000520_YES[-c(1)]

dim(only_taxa_520_yes)

PH0000520_matrix = matrix(nrow = 10, ncol = 2,)

rownames(PH0000520_matrix) = colnames(only_taxa_520_yes)

dim(PH0000520_matrix)

dim(only_taxa_520_yes)

head(colnames(only_taxa_520_yes))

for(i in c(1:ncol(only_taxa_520_yes))){
  amount = length(which(only_taxa_520_yes[i] > 0.000000000)) 
  PH0000520_matrix[i,1] = amount/221*100
}

PH0000520_matrix
#for those who answered no for PH0000520

PH0000520_NO <- subset(PH0000520_df, binary.PH0000520 == 0)

dim(PH0000520_NO)

colnames(PH0000520_NO)

only_taxa_520_NO = PH0000520_NO[-c(1)]

dim(only_taxa_520_NO)

dim(only_taxa_520_NO)

head(colnames(only_taxa_520_NO))

for(i in c(1:ncol(only_taxa_520_NO))){
  amount = length(which(only_taxa_520_NO[i] > 0.000000000)) 
  PH0000520_matrix[i,2] = amount/238*100
}

colnames(PH0000520_matrix) = c("Yes","No")

write.csv(PH0000520_matrix, "PH0000520_plot.csv")


#PH0000522, have you ever had an oophorectomy 

PH0000522_df = data.frame(binary$PH0000522,PH0000522_microbes)

PH0000522_YES <- subset(PH0000522_df, binary.PH0000522 == 1)

PH0000522_NO <- subset(PH0000522_df, binary.PH0000522 == 0)

only_taxa_522_yes = PH0000522_YES[-c(1)]

only_taxa_522_no = PH0000522_NO[-c(1)]

dim(only_taxa_522_yes)

PH0000522_matrix = matrix(nrow = 3, ncol = 2,)

rownames(PH0000522_matrix) = colnames(only_taxa_522_yes)

dim(only_taxa_522_yes)
dim(only_taxa_522_no)

for(i in c(1:ncol(only_taxa_522_yes))){
  amount_y = length(which(only_taxa_522_yes[i] > 0.000000000)) 
  PH0000522_matrix[i,1] = amount_y/93*100
}

for(i in c(1:ncol(only_taxa_522_no))){
  amount_n = length(which(only_taxa_522_no[i] > 0.000000000)) 
  PH0000522_matrix[i,2] = amount_n/313*100
}

colnames(PH0000522_matrix) = c("Yes","No")

dim(PH0000522_matrix)

write.csv(PH0000522_matrix, "PH0000522_plot.csv")

#For PH0000577

PH0000577_df = data.frame(binary$PH0000577,PH0000577_microbes)

PH0000577_YES <- subset(PH0000577_df, binary.PH0000577 == 1)

PH0000577_NO <- subset(PH0000577_df, binary.PH0000577 == 0)

only_taxa_577_yes = PH0000577_YES[-c(1)]

only_taxa_577_no = PH0000577_NO[-c(1)]

dim(only_taxa_577_yes)

dim(only_taxa_577_yes)

PH0000577_matrix = matrix(nrow = 4, ncol = 2,)

rownames(PH0000577_matrix) = colnames(only_taxa_577_yes)

dim(only_taxa_577_yes)
dim(only_taxa_577_no)

for(i in c(1:ncol(only_taxa_577_yes))){
  amount_y = length(which(only_taxa_577_yes[i] > 0.000000000)) 
  PH0000577_matrix[i,1] = amount_y/13*100
}

for(i in c(1:ncol(only_taxa_577_no))){
  amount_n = length(which(only_taxa_577_no[i] > 0.000000000)) 
  PH0000577_matrix[i,2] = amount_n/153*100
}

colnames(PH0000577_matrix) = c("Yes","No")

dim(PH0000577_matrix)

write.csv(PH0000577_matrix, "PH0000577_plot.csv")

#for PH0000610

PH0000610_df = data.frame(binary$PH0000610,PH0000610_microbes)

PH0000610_YES <- subset(PH0000610_df, binary.PH0000610 == 1)

PH0000610_NO <- subset(PH0000610_df, binary.PH0000610 == 0)

only_taxa_610_yes = PH0000610_YES[-c(1)]

only_taxa_610_no = PH0000610_NO[-c(1)]

dim(only_taxa_610_yes)

PH0000610_matrix = matrix(nrow = 2, ncol = 2,)

rownames(PH0000610_matrix) = colnames(only_taxa_610_yes)

dim(only_taxa_610_yes)
dim(only_taxa_610_no)

dim(only_taxa_610_yes)

for(i in c(1:ncol(only_taxa_610_yes))){
  amount_y = length(which(only_taxa_610_yes[i] > 0.000000000)) 
  PH0000610_matrix[i,1] = amount_y/10*100
}

for(i in c(1:ncol(only_taxa_610_no))){
  amount_n = length(which(only_taxa_610_no[i] > 0.000000000)) 
  PH0000610_matrix[i,2] = amount_n/154*100
}

colnames(PH0000610_matrix) = c("Yes","No")

dim(PH0000610_matrix)

write.csv(PH0000610_matrix, "PH0000610_plot.csv")

#For PH0000770

PH0000770_df = data.frame(binary$PH0000770,PH0000770_microbes)

PH0000770_YES <- subset(PH0000770_df, binary.PH0000770 == 1)

PH0000770_NO <- subset(PH0000770_df, binary.PH0000770 == 0)

only_taxa_770_yes = PH0000770_YES[-c(1)]

only_taxa_770_no = PH0000770_NO[-c(1)]

dim(only_taxa_770_yes)

PH0000770_matrix = matrix(nrow = 1, ncol = 2,)

rownames(PH0000770_matrix) = colnames(only_taxa_770_yes)

dim(only_taxa_770_yes)
dim(only_taxa_770_no)

for(i in c(1:ncol(only_taxa_770_yes))){
  amount_y = length(which(only_taxa_770_yes[i] > 0.000000000)) 
  PH0000770_matrix[i,1] = amount_y/49*100
}

for(i in c(1:ncol(only_taxa_770_no))){
  amount_n = length(which(only_taxa_770_no[i] > 0.000000000)) 
  PH0000770_matrix[i,2] = amount_n/312*100
}

colnames(PH0000770_matrix) = c("Yes","No")

dim(PH0000770_matrix)

write.csv(PH0000770_matrix, "PH0000770_plot.csv")

#For PH0000773

PH0000773_df = data.frame(binary$PH0000773,PH0000773_microbes)

PH0000773_YES <- subset(PH0000773_df, binary.PH0000773 == 1)

PH0000773_NO <- subset(PH0000773_df, binary.PH0000773 == 0)

only_taxa_773_yes = PH0000773_YES[-c(1)]

only_taxa_773_no = PH0000773_NO[-c(1)]

dim(only_taxa_773_yes)

PH0000773_matrix = matrix(nrow = 6, ncol = 2,)

rownames(PH0000773_matrix) = colnames(only_taxa_773_yes)

dim(only_taxa_773_yes)
dim(only_taxa_773_no)

for(i in c(1:ncol(only_taxa_773_yes))){
  amount_y = length(which(only_taxa_773_yes[i] > 0.000000000)) 
  PH0000773_matrix[i,1] = amount_y/95*100
}

for(i in c(1:ncol(only_taxa_773_no))){
  amount_n = length(which(only_taxa_773_no[i] > 0.000000000)) 
  PH0000773_matrix[i,2] = amount_n/284*100
}

colnames(PH0000773_matrix) = c("Yes","No")

dim(PH0000773_matrix)

write.csv(PH0000773_matrix, "PH0000773_plot.csv")

#For PH0000776


PH0000776_df = data.frame(binary$PH0000776,PH0000776_microbes)

PH0000776_YES <- subset(PH0000776_df, binary.PH0000776 == 1)

PH0000776_NO <- subset(PH0000776_df, binary.PH0000776 == 0)

only_taxa_776_yes = PH0000776_YES[-c(1)]

only_taxa_776_no = PH0000776_NO[-c(1)]

dim(only_taxa_776_yes)

dim(only_taxa_776_yes)

PH0000776_matrix = matrix(nrow = 4, ncol = 2,)

rownames(PH0000776_matrix) = colnames(only_taxa_776_yes)

dim(only_taxa_776_yes)
dim(only_taxa_776_no)

for(i in c(1:ncol(only_taxa_776_yes))){
  amount_y = length(which(only_taxa_776_yes[i] > 0.000000000)) 
  PH0000776_matrix[i,1] = amount_y/145*100
}

for(i in c(1:ncol(only_taxa_776_no))){
  amount_n = length(which(only_taxa_776_no[i] > 0.000000000)) 
  PH0000776_matrix[i,2] = amount_n/254*100
}

colnames(PH0000776_matrix) = c("Yes","No")

dim(PH0000776_matrix)

write.csv(PH0000776_matrix, "PH0000776_plot.csv")

#for PH0000779

PH0000779_df = data.frame(binary$PH0000779,PH0000779_microbes)

PH0000779_YES <- subset(PH0000779_df, binary.PH0000779 == 1)

PH0000779_NO <- subset(PH0000779_df, binary.PH0000779 == 0)

only_taxa_779_yes = PH0000779_YES[-c(1)]

only_taxa_779_no = PH0000779_NO[-c(1)]

dim(only_taxa_779_yes)

PH0000779_matrix = matrix(nrow = 14, ncol = 2,)

rownames(PH0000779_matrix) = colnames(only_taxa_779_yes)

dim(only_taxa_779_yes)
dim(only_taxa_779_no)

for(i in c(1:ncol(only_taxa_779_yes))){
  amount_y = length(which(only_taxa_779_yes[i] > 0.000000000)) 
  PH0000779_matrix[i,1] = amount_y/106*100
}

for(i in c(1:ncol(only_taxa_779_no))){
  amount_n = length(which(only_taxa_779_no[i] > 0.000000000)) 
  PH0000779_matrix[i,2] = amount_n/278*100
}

colnames(PH0000779_matrix) = c("Yes","No")

dim(PH0000779_matrix)

write.csv(PH0000779_matrix, "PH0000779_plot.csv")


#Calculating abundance of taxa identified as statisitcally significant for Menopause status

PH0000786_df = data.frame(non_binary$PH0000786,menopause_microbes)

Pre_meno <- subset(PH0000786_df, non_binary.PH0000786 == 1)

Current_meno <- subset(PH0000786_df, non_binary.PH0000786 == 2)

Post_meno <- subset(PH0000786_df, non_binary.PH0000786 == 3)

only_taxa_pre_meno = Pre_meno[-c(1)]

only_taxa_current_meno = Current_meno[-c(1)]

only_taxa_post_meno = Post_meno[-c(1)]

dim(only_taxa_post_meno)

menopause_matrix = matrix(nrow = 4, ncol = 3,)

rownames(menopause_matrix) = colnames(only_taxa_pre_meno)

dim(only_taxa_pre_meno)
dim(only_taxa_current_meno)
dim(only_taxa_post_meno)

for(i in c(1:ncol(only_taxa_pre_meno))){
  amount_1 = length(which(only_taxa_pre_meno[i] > 0.000000000)) 
  menopause_matrix[i,1] = amount_1/9*100
}

for(i in c(1:ncol(only_taxa_current_meno))){
  amount_2 = length(which(only_taxa_current_meno[i] > 0.000000000)) 
  menopause_matrix[i,2] = amount_2/23*100
}

for(i in c(1:ncol(only_taxa_post_meno))){
  amount_3 = length(which(only_taxa_post_meno[i] > 0.000000000)) 
  menopause_matrix[i,3] = amount_3/832*100
}

colnames(menopause_matrix) = c("Pre_Menopausal","Menopause","Post_Menopausal")

dim(menopause_matrix)

write.csv(menopause_matrix, "menopause_matrix_plot.csv")


#Now for hormone replacement therapy 

#for PH0000788

PH0000788_df = data.frame(binary$PH0000788,PH0000788_microbes)

PH0000788_YES <- subset(PH0000788_df, binary.PH0000788 == 1)

PH0000788_NO <- subset(PH0000788_df, binary.PH0000788 == 0)

only_taxa_788_yes = PH0000788_YES[-c(1)]

only_taxa_788_no = PH0000788_NO[-c(1)]

dim(only_taxa_788_yes)

PH0000788_matrix = matrix(nrow = 4, ncol = 2,)

rownames(PH0000788_matrix) = colnames(only_taxa_788_yes)

dim(only_taxa_788_yes)
dim(only_taxa_788_no)

for(i in c(1:ncol(only_taxa_788_yes))){
  amount_y = length(which(only_taxa_788_yes[i] > 0.000000000)) 
  PH0000788_matrix[i,1] = amount_y/397*100
}

for(i in c(1:ncol(only_taxa_788_no))){
  amount_n = length(which(only_taxa_788_no[i] > 0.000000000)) 
  PH0000788_matrix[i,2] = amount_n/489*100
}

colnames(PH0000788_matrix) = c("Yes","No")

dim(PH0000788_matrix)

write.csv(PH0000788_matrix, "PH0000788_plot.csv")

#PH0000789

PH0000789_df = data.frame(binary$PH0000789,PH0000789_microbes)

PH0000789_YES <- subset(PH0000789_df, binary.PH0000789 == 1)

PH0000789_NO <- subset(PH0000789_df, binary.PH0000789 == 0)

only_taxa_789_yes = PH0000789_YES[-c(1)]

only_taxa_789_no = PH0000789_NO[-c(1)]

dim(only_taxa_789_yes)

PH0000789_matrix = matrix(nrow = 2, ncol = 2,)

rownames(PH0000789_matrix) = colnames(only_taxa_789_yes)

dim(only_taxa_789_yes)
dim(only_taxa_789_no)

for(i in c(1:ncol(only_taxa_789_yes))){
  amount_y = length(which(only_taxa_789_yes[i] > 0.000000000)) 
  PH0000789_matrix[i,1] = amount_y/47*100
}

for(i in c(1:ncol(only_taxa_789_no))){
  amount_n = length(which(only_taxa_789_no[i] > 0.000000000)) 
  PH0000789_matrix[i,2] = amount_n/652*100
}

colnames(PH0000789_matrix) = c("Yes","No")

dim(PH0000789_matrix)

write.csv(PH0000789_matrix, "PH0000789_plot.csv")

#PH0000792

dim(PH0000792_df) = data.frame(binary$PH0000792,PH0000792_microbes)



PH0000792_YES <- subset(PH0000792_df, binary.PH0000792 == 1)

PH0000792_NO <- subset(PH0000792_df, binary.PH0000792 == 0)

only_taxa_792_yes = PH0000792_YES[-c(1)]

only_taxa_792_no = PH0000792_NO[-c(1)]

dim(only_taxa_792_yes)

PH0000792_matrix = matrix(nrow = 5, ncol = 2,)

rownames(PH0000792_matrix) = colnames(only_taxa_792_yes)

dim(PH0000792_microbes)

dim(only_taxa_792_yes)
dim(only_taxa_792_no)

for(i in c(1:ncol(only_taxa_792_yes))){
  amount_y = length(which(only_taxa_792_yes[i] > 0.000000000)) 
  PH0000792_matrix[i,1] = amount_y/613*100
}

for(i in c(1:ncol(only_taxa_792_no))){
  amount_n = length(which(only_taxa_792_no[i] > 0.000000000)) 
  PH0000792_matrix[i,2] = amount_n/276*100
}

colnames(PH0000792_matrix) = c("Yes","No")

dim(PH0000792_matrix)

write.csv(PH0000792_matrix, "PH0000792_plot.csv")


#Now for Phenotypes related to pregnancy

#for PH0000764

PH0000764_df = data.frame(binary$PH0000764,PH0000764_microbes)

PH0000764_YES <- subset(PH0000764_df, binary.PH0000764 == 1)

PH0000764_NO <- subset(PH0000764_df, binary.PH0000764 == 0)

only_taxa_764_yes = PH0000764_YES[-c(1)]

only_taxa_764_no = PH0000764_NO[-c(1)]

dim(only_taxa_764_yes)

PH0000764_matrix = matrix(nrow = 5, ncol = 2,)

rownames(PH0000764_matrix) = colnames(only_taxa_764_yes)

dim(only_taxa_764_yes)
dim(only_taxa_764_no)

for(i in c(1:ncol(only_taxa_764_yes))){
  amount_y = length(which(only_taxa_764_yes[i] > 0.000000000)) 
  PH0000764_matrix[i,1] = amount_y/198*100
}

for(i in c(1:ncol(only_taxa_764_no))){
  amount_n = length(which(only_taxa_764_no[i] > 0.000000000)) 
  PH0000764_matrix[i,2] = amount_n/217*100
}

colnames(PH0000764_matrix) = c("Yes","No")

dim(PH0000764_matrix)

write.csv(PH0000764_matrix, "PH0000764_plot.csv")

#For PH0000767

PH0000767_df = data.frame(binary$PH0000767,PH0000767_microbes)

PH0000767_YES <- subset(PH0000767_df, binary.PH0000767 == 1)

PH0000767_NO <- subset(PH0000767_df, binary.PH0000767 == 0)

only_taxa_767_yes = PH0000767_YES[-c(1)]

only_taxa_767_no = PH0000767_NO[-c(1)]

dim(only_taxa_767_yes)

PH0000767_matrix = matrix(nrow = 3, ncol = 2,)

rownames(PH0000767_matrix) = colnames(only_taxa_767_yes)

dim(only_taxa_767_yes)
dim(only_taxa_767_no)

for(i in c(1:ncol(only_taxa_767_yes))){
  amount_y = length(which(only_taxa_767_yes[i] > 0.000000000)) 
  PH0000767_matrix[i,1] = amount_y/15*100
  
}

for(i in c(1:ncol(only_taxa_767_no))){
  amount_n = length(which(only_taxa_767_no[i] > 0.000000000)) 
  PH0000767_matrix[i,2] = amount_n/334*100
}

colnames(PH0000767_matrix) = c("Yes","No")

dim(PH0000767_matrix)

write.csv(PH0000767_matrix, "PH0000767_plot.csv")

dim(LCQ_age)

dim(merged_CQ_alphaD_final)

(age16s$age.16S <= 64))

430+496

max(age16s$age.16S)

top_50_fam = 18.07
top_80_fam = 13.94



