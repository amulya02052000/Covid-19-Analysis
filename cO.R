# COVID 19 ANALYSIS
#GOAL: Which countries have had the highest number of positive cases against the number of tests?


#INSTALL THE LIBRARIES REQUIRED
library(tidyverse)
library(readr)


#READ AND VIEW THE DATA
data<-read.csv("C:/Users/Amulya/Desktop/TSF-Data Analytics Internship Tasks/covid19.csv")
View(data)


#DIMENSIONS OF THE DATA
dim(data)


#DETERMINE THE COLUMN NAMES
vector_cols<-colnames(data)
vector_cols


#DISPLAY THE HEAD OF THE DATA
head(data)


#DISPLAY THE SUMMARY OF THE DATA
summary(data)


#Filter the rows related to "All States" from the Province_State column and remove the Province_State column from DATA dataframe.
covid_df_all_states<-data %>%
  filter(Province_State=="All States")%>%
  select(-Province_State)


#STORE ALL THE DATA INTO covid_df_all_states_daily
covid_df_all_states_daily<-select(covid_df_all_states,Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)  


#summarize the covid_df_all_states_daily dataframe by computing the sum of the number of tested, positive, active and hospitalized cases grouped by the Country_Region column.
covid_df_all_states_daily_sum<-covid_df_all_states_daily %>%
  group_by(Country_Region) %>%
  summarise(
    tested=sum(daily_tested),
    positive=sum(daily_positive),
    active=sum(active),
    hospitalized=sum(hospitalizedCurr),
  )%>%
  arrange(desc(tested))


#Extract the top ten rows from the covid_df_all_states_daily_sum
covid_top_10<-head(covid_df_all_states_daily_sum,10)
View(covid_top_10)


#Create the following vectors from the covid_top_10 dataframe.
countries<-covid_top_10$Country_Region
tested_cases<-covid_top_10$tested
positive_cases<-covid_top_10$positive
active_cases<-covid_top_10$active
hospitalized_cases<-covid_top_10$hospitalized
names(countries)<-countries
names(tested_cases)<-countries
names(positive_cases)<-countries
names(active_cases)<-countries
names(hospitalized_cases)<-countries


#Identify the top three positive against tested cases.
positive_test_under_top_3<-positive_cases/tested_cases
sorted_vect <- sort(positive_test_under_top_3, decreasing = T)
head(positive_test_under_top_3,3)


#Create the following vectors
united_states<-c(0.11, 1473672, 166909, 0, 0)
russia<-c(0.10, 17282363, 1877179, 0, 0)
italy<-c(0.08, 2031192, 163941, 2980960, 0)


#Create a matrix combining the vectors
covid_mat<-rbind(united_states,russia,italy)
colnames(covid_mat)<-c("Ratio", "tested", "positive", "active", "hospitalized")
View(covid_mat)


#Create a character variable named question that contains our question.
question <- "Which countries have had the highest number of positive cases against the number of tests?"


#Create a named vector that contains our answer with the following command:
answer <- c("Positive tested cases" = positive_test_under_top_3)


#Create a list that contains the data structures
daframes<-list(data, covid_df_all_states, covid_df_all_states_daily,covid_top_10)
vmatrices<-list(covid_mat)
vectors<-list(vector_cols,countries)
dataset<-list(daframes,vmatrices,vectors)
data_structure_list<-dataset


#Create a list that contains the following lists: question, answer, and data_structure_list
covid_analysis_list<-list(question,answer,data_structure_list)


#GOAL OF THE PROJECT ANSWER
covid_analysis_list[[2]]


