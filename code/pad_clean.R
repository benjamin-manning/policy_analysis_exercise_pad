library(tidyverse)
library(dplyr)
library(stringr)
library(svMisc)
library(ggplot2)
library(gtable)
library("ggpubr")

?progress

for (i in 0:length(log_data$logInfo)-1){
  progress(i,length(log_data$logInfo)-1)
  if (i==length(log_data$logInfo)-1) cat('DONE!\n')
}

#Ditribution of languages spoken
lang_spoken <- log_data %>%
  group_by(callerId, langId) %>%
  count()

ggplot(data=lang_spoken, aes(x=langId)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar()+
  labs(
    title = 'Distribution of Languages
    1 = Amharic, 2=Oromiffa, 3=Tigrigna, 4=Wolayitta, and 5=Sidamigna.',
    x = 'Language',
    y= 'count'
  )

#distribution of calls made

calls_made <- log_data %>%
  group_by(callerId, noCallsMade) %>%
  count()

summary(calls_made$noCallsMade)

ggplot(calls_made, aes(x=noCallsMade)) +
  geom_histogram(color="black", fill="blue") +
  xlim(0,350) +
  labs(
    title = 'Distribution of total Content listened to all Across all Calls (Outliers Excluded) \n
Min. 1st Qu.  Median  Mean   3rd Qu.    Max. 
    3.0    45.0    69.0     99.9    106.0    7110.0 ',
    x = 'Total number of calls'
  )


#Distribution of contenet listened to

content_listened <- log_data %>%
  group_by(callerId, noContentListened) %>%
  count()

summary(content_listened$noContentListened)

ggplot(content_listened, aes(x=noContentListened)) +
  geom_histogram(color="black", fill="green") +
  xlim(50, 250) +
  labs(
    title = 'Distribution of total Content listened to all Across all Calls (Outliers Excluded) \n
  Min. 1st Qu.  Median  Mean  3rd Qu.    Max. n\
  50.00   58.00   74.50   99.97  105.00   2367.00 ',
    x = 'Total Content Listen Across Calls'
  )

# Ratio of content per call
content_call_ratio_df <- inner_join(calls_made,content_listened,by="callerId")
content_call_ratio_df$ratio <- content_call_ratio_df$noContentListened/content_call_ratio_df$noCallsMade

summary(content_call_ratio_df$ratio)

ggplot(content_call_ratio_df, aes(x=ratio)) +
  geom_histogram(color="black", fill="red") +
  xlim(0, 5) +
  labs(
    title = 'Distribution of ration of content listened to Calls (Outliers Excluded) \n
    Min.         1st Qu.   Median     Mean    3rd Qu.      Max.
 0.05963    0.84598  1.18586  1.40733  1.69773   22.33333  ',
    x = 'Content to Call Ratio'
  )

#Filtering by top menu selection

top_menu <- log_data %>%
  filter(info_section == 'TOP MENU')

unique((top_menu$logInfo))

ggplot(top_menu, aes(logInfo)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar(aes(fill = langId))

tm_filter <- top_menu %>%
  group_by(callerId, logInfo, langId) %>%
  count()

# distribution of access at least once uniquely for each individual

frequencies <- c(62, 645, 132, 866, 416, 378, 185)
labels <- c('COVID', 'HHI OPTION', 'LIVESTOCK', 'RAIN', 'RESET PROFILE', 'INVALID KEY PRESSED', 'TOP MENU REPLAYED')

Amharic_1 <- c()
Oromiffa_2 <- c()
Tigrigna_3 <-c()
Wolayitta_4 <- c()
Sidamigna_5 <- c()

oringal_labels <- unique(tp_filter$logInfo)

oringal_labels
#Counting Language instances
unique(tp_filter$logInfo)

?append
for (j in 1:7){
  for (i in 1:5){
    counter <- tm_filter %>%
      filter(logInfo == oringal_labels[j]) %>%
      filter(langId == i)
    if (i == 1){
      Amharic_1 <- append(Amharic_1, length((counter$logInfo))) 
    }
    else if (i ==2){
      Oromiffa_2 <- append(Oromiffa_2,length((counter$logInfo))) 
    }
    else if (i ==3){
      Tigrigna_3 <- append(Tigrigna_3, length((counter$logInfo))) 
    }
    else if (i ==4){
      Wolayitta_4 <- append(Wolayitta_4, length((counter$logInfo))) 
    }
    else if (i ==5){
      Sidamigna_5  <- append(Sidamigna_5, length((counter$logInfo))) 
    }
    else {
      print('wtf?')
    }
  }
}
Amharic_1
Oromiffa_2 
Tigrigna_3
Wolayitta_4 
Sidamigna_5 


top_menu_choice <- data.frame(labels,frequencies, Amharic_1, Oromiffa_2, Tigrigna_3, Wolayitta_4, Sidamigna_5 )

top_menu_choice$sum_check <- top_menu_choice$Amharic_1 + top_menu_choice$Oromiffa_2 +top_menu_choice$Tigrigna_3+ top_menu_choice$Wolayitta_4+ top_menu_choice$Sidamigna_5

top_menu_choice$Amharic_perc <- round(top_menu_choice$Amharic_1/top_menu_choice$frequencies,3)
ggplot(top_menu_choice, aes(frequencies, y=labels)) +
  geom_bar()

gg_total <- ggplot(data=top_menu_choice, aes(x=labels, y=frequencies)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar(stat="identity", fill='black')+
  labs(
    title = 'Total Breakdown for Individuals 
    By Content Selected at least Once',
    x = 'Content Selected',
    y= 'count'
  )



  
g1<-ggplot(data=top_menu_choice, aes(x=labels, y=Amharic_1)) +
  theme(text = element_text(size=10),axis.text.x = element_text(angle = 90))+
  geom_bar(stat="identity", fill='red') +
  labs(
    title ='Amharic_1',
    x = 'Content Selected',
    y = 'Count'
  )
g2<-ggplot(data=top_menu_choice, aes(x=labels, y=Oromiffa_2 )) +
  theme(text = element_text(size=10),axis.text.x = element_text(angle = 90))+
  geom_bar(stat="identity", fill='green')+
  labs(
    title ='Oromiffa_2',
    x = 'Content Selected',
    y = 'Count'
  )
g3<-ggplot(data=top_menu_choice, aes(x=labels, y=Tigrigna_3)) +
  theme(text = element_text(size=10),axis.text.x = element_text(angle = 90))+
  geom_bar(stat="identity", fill='blue')+
  labs(
    title ='Tigrigna_3',
    x = 'Content Selected',
    y = 'Count'
  )
g4<-ggplot(data=top_menu_choice, aes(x=labels, y=Wolayitta_4 )) +
  theme(text = element_text(size=10),axis.text.x = element_text(angle = 90))+
  geom_bar(stat="identity", fill='grey')+
  labs(
    title ='Wolayitta_4',
    x = 'Content Selected',
    y = 'Count'
  )
g5<-ggplot(data=top_menu_choice, aes(x=labels, y=Sidamigna_5 )) +
  theme(text = element_text(size=10),axis.text.x = element_text(angle = 90))+
  geom_bar(stat="identity", fill='orange')+
  labs(
    title ='Sidamigna_5',
    x = 'Content Selected',
    y = 'Count'
  )


ggarrange(g1, g2, g3, g4, g5, gg_total)

#random filler
a <-10 
b<-2

#callerId: Id of caller/farmer
#langId: Language ID, 8028 IVR system provides agricultural information in 5 local languages; 1 = Amharic, 2=Oromiffa, 3=Tigrigna, 4=Wolayitta, and 5=Sidamigna.
#callTime: The time the caller called to 8028 for the first time 
#lastCallTime: The time the caller called the last time
#noCallsMade: Number of calls made so far
#callId: Unique identification of call
#eventTime: The timestamp of different events
#logInfo: events callers pass through in a call
#logInfoId: Numeric representation of logInfo
#inSurvey: dummy variable which is True when the caller was sampled in the recent survey.

#WHAT ARE THE LOGINFO ID numbers?

#loading data
#log_data <- read_csv("~/Desktop/PAE/github/data/logData.csv")

#MAKE IT ALL UPPER!!!

log_data$logInfo <- toupper(log_data$logInfo)

#creating extra log info columns to organize
log_data$info_del <- log_data$logInfo
log_data$info_section <- log_data$logInfo

#numeric Rpresentation
log_data$info_section_num <- as.numeric(log_data$logInfo)



#Creating Progress Bar
for (i in 1:(length(log_data$logInfo)-1)){
  progress(i,length(log_data$logInfo)-1)
  if (i==length(log_data$logInfo)-1) cat('DONE!\n')
  if (is.na(log_data$logInfo[i])){
    log_data$logInfo[i] = log_data$logInfo[i]
  }
  else if (str_detect(log_data$logInfo[i], "ALTITUDE MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "ALTITUDE MENU"
    log_data$info_section_num[i] = 1
  }
  else if (str_detect(log_data$logInfo[i], "APICULTURE MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "APICULTURE MENU"
    log_data$info_section_num[i] = 2
  }
  else if (str_detect(log_data$logInfo[i], "CONTENT PLAYED")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "CONTENT PLAYED"
    log_data$info_section_num[i] = 3
  }
  else if (str_detect(log_data$logInfo[i], "CONTENT MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "CONTENT MENU"
    log_data$info_section_num[i] = 4
  }
  else if (str_detect(log_data$logInfo[i], "COVID-19 MAIN MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "COVID-19 MAIN MENU"
    log_data$info_section_num[i] = 5
  }
  else if (str_detect(log_data$logInfo[i], "CROP MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "CROP MENU"
    log_data$info_section_num[i] = 6
  }
  else if (str_detect(log_data$logInfo[i], "HHICROP MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "HHICROP MENU"
    log_data$info_section_num[i] = 7
  }
  else if (str_detect(log_data$logInfo[i], "LANGUAGE MENU ")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "LANGUAGE MENU"
    log_data$info_section_num[i] = 8
  }
  else if (str_detect(log_data$logInfo[i], "LIVESTOCK MAIN MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "LIVESTOCK MAIN MENU"
    log_data$info_section_num[i] = 9
  }
  else if (str_detect(log_data$logInfo[i], "REGION MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "REGION MENU"
    log_data$info_section_num[i] = 10
  }
  else if (str_detect(log_data$logInfo[i], "RESET ATTRIBUTES MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "RESET ATTRIBUTES MENU"
    log_data$info_section_num[i] = 11
  }
  else if (str_detect(log_data$logInfo[i], "RESET MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "RESET MENU"
    log_data$info_section_num[i] = 12
  }
  else if (str_detect(log_data$logInfo[i], "TOP MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "TOP MENU"
    log_data$info_section_num[i] = 13
  }
  else if (str_detect(log_data$logInfo[i], "WOREDA MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "WOREDA MENU"
    log_data$info_section_num[i] = 14
  }
  else if (str_detect(log_data$logInfo[i], "ZONE MENU")){
    log_data$info_del[i] = 'DELETED'
    log_data$info_section[i] = "ZONE MENU"
    log_data$info_section_num[i] = 15
  }
  else{
    log_data$info_section[i] = 'TBD'
    log_data$info_section_num[i] = 16
  }
}
