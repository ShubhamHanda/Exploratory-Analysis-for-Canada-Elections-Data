# ctrl+shift+enter to run the whole script
# prefer using rstudio 
# install.packages("reshape2")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("corrplot")

library(corrplot)
library(reshape2)
library(tidyr)
library(dplyr)
library(tidyverse)

# Give your path over here

tableau12 <- read.csv("C:\\Users\\Dell\\Documents\\Conestoga\\mathematics for data anlytics\\Assignment 2\\table_tableau12.csv")

tableau11 <- read.csv("C:\\Users\\Dell\\Documents\\Conestoga\\mathematics for data anlytics\\Assignment 2\\table_tableau11.csv")

LPP2015 <- read.delim("C:\\Users\\Dell\\Documents\\Conestoga\\mathematics for data anlytics\\Assignment 2\\LPP2015.tab")




#QUESTION 1 , EXPLANATION IN WORD FILE

lpp2015s<-LPP2015%>%
  select(constituencynumber,yob,parties_1,parties_2,parties_3,parties_4,parties_5,leaders_1,leaders_2,leaders_3,leaders_4,leaders_5,nat_issue, nat_party,)%>%
  group_by(constituencynumber)%>% summarise(yob=mean(yob),parties_1=mean(parties_1),parties_2=mean(parties_2),parties_3=mean(parties_3),parties_4=mean(parties_4),parties_5=mean(parties_5),count=n(),leaders_1=mean(leaders_1),leaders_2=mean(leaders_2),leaders_3=mean(leaders_3),leaders_4=mean(leaders_4),leaders_5=mean(leaders_5), nat_issue_mean=mean(nat_issue),nat_issue_median=median(nat_issue), nat_party_mean=mean(nat_party))



tableau12 <- tableau12 %>%
  separate(Candidate.Candidat, c("Candidate.Name", "Candidate.Party"), sep = "/")

table_1 <- tableau12 %>%
  select(Electoral.District.Number.NumÃ.ro.de.circonscription, Candidate.Party, Votes.Obtained.Votes.obtenus)

colnames(table_1)[1] <- "District_Number"
colnames(table_1)[3] <- "Votes_Obtained"

table_1 <- reshape(table_1, idvar="District_Number", timevar="Candidate.Party", direction="wide")

colnames(table_1)[2] <- "Liberal"
colnames(table_1)[4] <- "NDP"
colnames(table_1)[5] <- "Conservative"
colnames(table_1)[6] <- "Green_Party"
colnames(table_1)[3] <- "No_Affiliation"
colnames(table_1)[7] <- "Forces_et_Démocratie"
colnames(table_1)[8] <- "Communist"

colnames(lpp2015s)[1] <- "District_Number"
colnames(tableau11)[3] <- "District_Number"

new_table <-merge(lpp2015s, tableau11, by=c("District_Number"))
head_table <-merge(new_table, table_1, by=c("District_Number"))

head_table <- head_table[c(1, 2, 3, 4, 5, 6, 7, 8, 9,10, 11, 12, 13, 14, 15, 16, 17, 18, 19,20, 21, 22, 23, 24, 25, 26, 27, 28,32, 29,30,31, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,53)]

head_table[, "maxVotesExceptCONSERVATIVE"] <- apply(head_table[, 30:53], 1, max, na.rm= TRUE)

head_table$CONSERVATIVEMargin = head_table$Conservative - head_table$maxVotesExceptCONSERVATIVE

head_table$CONSERVATIVEvictory <- 'Loss' 
head_table$CONSERVATIVEvictory[head_table$CONSERVATIVEMargin>0] <- 'Win'





#QUESTION 2 , EXPLANATION IN WORD FILE

head_table$yob_mean <- mean(head_table[['yob']])
head_table$yob_median <- median(head_table[['yob']])

colnames(head_table)[27] <- "Voter_Turnout"

head_table$Voter_Turnout_mean <- mean(head_table[['Voter_Turnout']])
head_table$Voter_Turnout_median <- median(head_table[['Voter_Turnout']])

head_table$n_mean <- mean(head_table[['count']])
head_table$n_median <- median(head_table[['count']])

head_table$parties_1_mean <- mean(head_table[['parties_1']])
head_table$parties_1_median <- median(head_table[['parties_1']])

head_table$parties_2_mean <- mean(head_table[['parties_2']])
head_table$parties_2_median <- median(head_table[['parties_2']])

head_table$parties_3_mean <- mean(head_table[['parties_3']])
head_table$parties_3_median <- median(head_table[['parties_3']])

head_table$parties_4_mean <- mean(head_table[['parties_4']])
head_table$parties_4_median <- median(head_table[['parties_4']])

head_table$parties_5_mean <- mean(head_table[['parties_5']])
head_table$parties_5_median <- median(head_table[['parties_5']])

head_table$leaders_1_mean <- mean(head_table[['leaders_1']])
head_table$leaders_1_median <- median(head_table[['leaders_1']])

head_table$leaders_2_mean <- mean(head_table[['leaders_2']])
head_table$leaders_2_median <- median(head_table[['leaders_2']])

head_table$leaders_3_mean <- mean(head_table[['leaders_3']])
head_table$leaders_3_median <- median(head_table[['leaders_3']])

head_table$leaders_4_mean <- mean(head_table[['leaders_4']])
head_table$leaders_4_median <- median(head_table[['leaders_4']])

head_table$leaders_5_mean <- mean(head_table[['leaders_5']])
head_table$leaders_5_median <- median(head_table[['leaders_5']])

head_table$yob_sd <- sd(head_table[['yob']])
head_table$yob_IQR <- IQR(head_table[['yob']])

head_table$Voter_Turnout_sd <- sd(head_table[['Voter_Turnout']])
head_table$Voter_Turnout_IQR <- IQR(head_table[['Voter_Turnout']])

head_table$n_sd <- sd(head_table[['count']])
head_table$n_IQR <- IQR(head_table[['count']])

head_table$parties_1_sd <- sd(head_table[['parties_1']])
head_table$parties_1_IQR <- IQR(head_table[['parties_1']])

head_table$parties_2_sd <- sd(head_table[['parties_2']])
head_table$parties_2_IQR <- IQR(head_table[['parties_2']])

head_table$parties_3_sd <- sd(head_table[['parties_3']])
head_table$parties_3_IQR <- IQR(head_table[['parties_3']])

head_table$parties_4_sd <- mean(head_table[['parties_4']])
head_table$parties_4_IQR <- IQR(head_table[['parties_4']])

head_table$parties_5_sd <- sd(head_table[['parties_5']])
head_table$parties_5_IQR <- IQR(head_table[['parties_5']])

head_table$leaders_1_sd <- sd(head_table[['leaders_1']])
head_table$leaders_1_IQR <- IQR(head_table[['leaders_1']])

head_table$leaders_2_sd <- sd(head_table[['leaders_2']])
head_table$leaders_2_IQR <- IQR(head_table[['leaders_2']])

head_table$leaders_3_sd <- sd(head_table[['leaders_3']])
head_table$leaders_3_IQR <- IQR(head_table[['leaders_3']])

head_table$leaders_4_sd <- sd(head_table[['leaders_4']])
head_table$leaders_4_IQR <- IQR(head_table[['leaders_4']])

head_table$leaders_5_sd <- sd(head_table[['leaders_5']])
head_table$leaders_5_IQR <- IQR(head_table[['leaders_5']])


colnames(head_table)[41] <- "Bloc_Quebecois"


sum_Liberal <- sum(head_table$Liberal)
sum_Convervative <- sum(head_table$Conservative)
sum_NDP <- sum(head_table$NDP)
sum_Bloc_Quebecois <- sum(head_table$Bloc_Quebecois, na.rm= TRUE)
sum_Green_Party <- sum(head_table$Green_Party, na.rm= TRUE)


vote_count <- c(sum_Liberal,sum_Convervative,sum_NDP,sum_Bloc_Quebecois,sum_Green_Party)
party_name <- c("Liberal", "Conservative", "NDP", "Bloc_Quebecois", "Green_Party")

barplot(vote_count,names.arg=party_name,xlab="Party Names",ylab=" Total No. of  Votes",col="Red",
        main="Party  Vs Total Votes")





#QUESTION 3 EXPLANATION GIVEN IN WORD FILE

total_votes = sum_Convervative + sum_Bloc_Quebecois + sum_Green_Party + sum_Liberal + sum_NDP

head_table[, "TotalVotes"] <- apply(head_table[, 29:53], 1, sum, na.rm= TRUE)

head_table$Next_Election <- 'No Chance'

head_table$Next_Election[((head_table$CONSERVATIVEMargin/head_table$TotalVotes)*100)>-12] <- 'In Play'

head_table$Next_Election[((head_table$CONSERVATIVEMargin/head_table$TotalVotes)*100)>12] <- 'Safe'

count_safe <- table(head_table$Next_Election)
count_names <-c("In Play", "No Chance", "Safe")

barplot(count_safe,names.arg=count_names,xlab="",ylab="Next Election",col="blue",
        main="CONSERVATIVE - Summary for Next elections")






#QUESTION 4 EXPLANATION GIVEN IN WORD FILE

head_table$Win_lose_per <- (head_table$CONSERVATIVEMargin/head_table$TotalVotes)*100

head_table$Percentage_votes <- (head_table$Conservative/head_table$TotalVotes)*100

correlation <- cor(head_table$Population, head_table$Voter_Turnout)
covariance <- cov(head_table$Population, head_table$Voter_Turnout)

plot(head_table$Population, head_table$Win_lose_per, main = "Population Performance Graph",
     xlab = "Population", ylab = "Performance",
     pch = 19, frame = FALSE)

plot(head_table$Population, head_table$Voter_Turnout, main = "Population Voter Turnout Graph",
     xlab = "Population", ylab = "Voter Turnout",
     pch = 19, frame = FALSE)
hist(head_table$Win_lose_per, main="how many districts won",col="red")
legend(1,95,legend=c("-ve vals= lose,+ve vals=win"))


plot(head_table$Win_lose_per, head_table$Voter_Turnout, main = "Performance Voter Turnout Graph",
     xlab = "Performance", ylab = "Voter Turnout",
     pch = 19, frame = FALSE)

co_relation_data <- head_table[, c(27,111)]
co_relation_matrix <- cor(co_relation_data)






#QUESTION 5 EXPLANATION GIVEN IN WORD FILE

correlation_liberal_party <- cor(head_table$parties_1, head_table$leaders_2)

correlation_conservative_party <- cor(head_table$parties_2, head_table$leaders_1)

correlation_ndp_party <- cor(head_table$parties_3, head_table$leaders_3)

correlation_quebecos_party <- cor(head_table$parties_4, head_table$leaders_4)

correlation_green_party <- cor(head_table$parties_5, head_table$leaders_5)

plot(head_table$parties_2, head_table$leaders_1, pch = 16, cex = 1.3, col = "Black", main = "Stephen harper VS PARTY", xlab = "Party_CONSERVATIVE_likeness", ylab = "Stephen harper_likeness") 
abline(lm(head_table$parties_2 ~ head_table$leaders_1 ))


# IN SUMMARY ALL THE GRAPHS AND DATA WAS ANALYZED  IN BOTH TABULAR AND GRAPHICAL FORM
# Thankyou Shubham Handa 8638369 Assignment 2 Mathematics for data analysis
