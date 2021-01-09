library(tidyverse)
library(dplyr)
library(stringr)
library(svMisc)
library(ggplot2)
library(gtable)
library("ggpubr")

HHI_RAIN <- log_data




HHI_RAIN <- HHI_RAIN %>%
  filter(info_section=='HHI' | info_section=='RAIN')


# sorting through the HHI data and stream
HHI_df <- HHI_RAIN %>%
  filter(info_section=='HHI') %>%
  group_by(callerId, logInfo, langId) %>%
  count()

HHI_df$logInfo <- str_replace(HHI_df$logInfo, "HHI MAIN MENU - ", "")

labels <- unique(HHI_df$logInfo)

## FILTER THIS MORE!!!!
rain_df <- HHI_RAIN %>%
  filter(info_section=='RAIN')%>%
  group_by(callerId, logInfo, langId) %>%
  count()

table(rain_df$logInfo)

for (i in 1:(length(rain_df$logInfo)-1)){
  progress(i,length(rain_df$logInfo)-1)
  if (i==length(rain_df$logInfo)-1) cat('DONE!\n')
  if (is.na(rain_df$logInfo[i])){
    rain_df$logInfo[i] = rain_df$logInfo[i]
  }
  else if (str_detect(rain_df$logInfo[i], "LIVESTOCK MAIN MENU ")){
    rain_df$logInfo[i] = "NA"
  }
  else if (str_detect(rain_df$logInfo[i], "COVID-19 MAIN MENU")){
    rain_df$logInfo[i] = "NA"
  }
  else{
    rain_df$logInfo[i] = rain_df$logInfo[i]
  }
}


rain_df <- rain_df %>%
  filter(logInfo != 'NA')

rain_df$logInfo<- str_replace(rain_df$logInfo, "MAIN MENU - ", "")

rain_final<- rain_df %>%
  group_by(callerId, logInfo, langId) %>%
  count()

#plotting from rain menu (called main menu)
ggplot(data=rain_final, aes(x=logInfo,fill = langId)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar()+
  labs(
    title = 'Total Breakdown for Individuals 
    By Content Selected at least Once FROM RAIN MENU',
    x = 'Content Selected',
    y= 'count'
  )

table(rain_df$logInfo)
# 1- preplanting options, menu1
# 2-planting option menu2
table(HHI_df$logInfo)
# 1- preplanting options, HHImenu1
# 2-planting and transplanting option HHImenu2


MENU_1 <- filter(log_data, grepl("MENU 1",logInfo))
MENU_1 <- filter(MENU_1, !grepl("HHIMENU 1",logInfo))
MENU_1 <- filter(MENU_1, !grepl("APICULTURE MENU",logInfo))
MENU_1 <- filter(MENU_1, !grepl("APICULTURE SUB4 MENU",logInfo))
MENU_1 <- filter(MENU_1, !grepl("DAIRY MENU",logInfo))
MENU_1 <- filter(MENU_1, !grepl("SMALL-SCALE SUB5 MENU",logInfo))
MENU_1 <- filter(MENU_1, !grepl("DAIRY SUB2 MENU",logInfo))
MENU_1 <- filter(MENU_1, !grepl("FATTENING MANU",logInfo))

MENU_1 <- filter(MENU_1, !grepl("FATTENING SUB1 MENU",logInfo))
MENU_1 <- filter(MENU_1, !grepl("FATTENING SUB2 MENU",logInfo))
MENU_1 <- filter(MENU_1, !grepl("HOUSEHOLD MENU",logInfo))
MENU_1 <- filter(MENU_1, !grepl("HOUSEHOLD SUB1 MENU ",logInfo))


MENU_1 <- MENU_1 %>%
  group_by(callerId, logInfo, langId) %>%
  count()

table(MENU_1$logInfo)

MENU_1$logInfo<- str_replace(MENU_1$logInfo, "MENU 1 - ", "")

ggplot(data=MENU_1, aes(x=logInfo)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar()+
  labs(
    title = 'Total Breakdown for Individuals 
    Content Selected at least Once 
    FROM RAIN MENU/PRE PLANTING OPTION',
    x = 'Content Selected',
    y= 'count'
  )


#GETTING THE CROP MENU FROM MENU 1 (rain/preplanting/land preparation sub menu)
land_prep <- filter(log_data, grepl("CONTENT PLAYED - LAND PREPARATION - ",logInfo))

land_prep_final <- land_prep %>%
  group_by(callerId, logInfo, langId) %>%
  count()

land_prep_final$logInfo<- str_replace(land_prep_final$logInfo, "CONTENT PLAYED - LAND PREPARATION -", "")

ggplot(data=land_prep_final, aes(x=logInfo)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar()+
  labs(
    title = 'RAIN MENU/PRE PLANTING/LAND PREPARATION
    content actually played by language',
    x = 'Content Selected',
    y= 'count'
  )

#get rid of the language breakdown
land_prep_no_lang <- land_prep_final

land_prep_no_lang$logInfo<- str_replace(land_prep_no_lang$logInfo, "- AMHARIC", "")
land_prep_no_lang$logInfo<- str_replace(land_prep_no_lang$logInfo, "- OROMIFFA", "")
land_prep_no_lang$logInfo<- str_replace(land_prep_no_lang$logInfo, "- TIGRIGNA", "")
land_prep_no_lang$logInfo<- str_replace(land_prep_no_lang$logInfo, "- WOLAYITTA", "")
land_prep_no_lang$logInfo<- str_replace(land_prep_no_lang$logInfo, "- WOLAYTIGNA", "")
land_prep_no_lang$logInfo<- str_replace(land_prep_no_lang$logInfo, "- SIDAMIGNA", "")

table(land_prep_no_lang$logInfo)

ggplot(land_prep_no_lang, aes(x=logInfo)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar()+
  labs(
    title = 'RAIN MENU/PRE PLANTING/LAND PREPARATION
    content actually played',
    x = 'Content Selected',
    y= 'count'
  )

#GETTING THE CROP MENU FROM MENU 1 (rain/preplanting/seed variety sub menu)
seed_variety <- filter(log_data, grepl("CONTENT PLAYED - SEED VARITY - ",logInfo))

seed_variety_final <- seed_variety %>%
  group_by(callerId, logInfo, langId) %>%
  count()

seed_variety_final$logInfo<- str_replace(seed_variety_final$logInfo, "CONTENT PLAYED - SEED VARITY - ", "")

ggplot(data=seed_variety_final, aes(x=logInfo)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar()+
  labs(
    title = 'RAIN MENU/PRE PLANTING/SEED VARIETY
    content actually played by language',
    x = 'Content Selected',
    y= 'count'
  )

#get rid of the language breakdown
seed_variety_no_lang <- seed_variety_final

seed_variety_no_lang$logInfo<- str_replace(seed_variety_no_lang$logInfo, "- AMHARIC", "")
seed_variety_no_lang$logInfo<- str_replace(seed_variety_no_lang$logInfo, "- OROMIFFA", "")
seed_variety_no_lang$logInfo<- str_replace(seed_variety_no_lang$logInfo, "- TIGRIGNA", "")
seed_variety_no_lang$logInfo<- str_replace(seed_variety_no_lang$logInfo, "- WOLAYITTA", "")
seed_variety_no_lang$logInfo<- str_replace(seed_variety_no_lang$logInfo, "- WOLAYTIGNA", "")
seed_variety_no_lang$logInfo<- str_replace(seed_variety_no_lang$logInfo, "- SIDAMIGNA", "")

table(seed_variety_no_lang$logInfo)

ggplot(seed_variety_no_lang, aes(x=logInfo)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar()+
  labs(
    title = 'RAIN MENU/PRE PLANTING/seed variety
    content actually played',
    x = 'Content Selected',
    y= 'count'
  )



#PLANTING OPTION - second most accessed MENU 2
MENU_2 <- filter(log_data, grepl("MENU 2",logInfo))
MENU_2 <- filter(MENU_2, !grepl("HHIMENU 2",logInfo))
MENU_2 <- filter(MENU_2, !grepl("APICULTURE MENU",logInfo))
MENU_2 <- filter(MENU_2, !grepl("APICULTURE SUB2 MENU",logInfo))
MENU_2 <- filter(MENU_2, !grepl("DAIRY MENU",logInfo))
MENU_2 <- filter(MENU_2, !grepl("FATTENING MANU",logInfo))

MENU_2 <- MENU_2 %>%
  group_by(callerId, logInfo, langId) %>%
  count()

table(MENU_2$logInfo)

MENU_2$logInfo<- str_replace(MENU_2$logInfo, "MENU 2 - ", "")

ggplot(data=MENU_2, aes(x=logInfo)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar()+
  labs(
    title = 'Total Breakdown for Individuals 
    Content Selected at least Once 
    FROM RAIN MENU/PLANTING OPTION',
    x = 'Content Selected',
    y= 'count'
  )

#GETTING THE CROP MENU FROM MENU 2 (rain/planting/seed rate)
seed_rate <- filter(log_data, grepl("CONTENT PLAYED - SEED RATE",logInfo))

seed_rate_final <- seed_rate %>%
  group_by(callerId, logInfo, langId) %>%
  count()

seed_rate_final$logInfo<- str_replace(seed_rate_final$logInfo, "CONTENT PLAYED - SEED RATE -", "")

ggplot(data=seed_rate_final, aes(x=logInfo)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar()+
  labs(
    title = 'RAIN MENU/PLANTING/SEED RATE
    content actually played by language',
    x = 'Content Selected',
    y= 'count'
  )

#get rid of the language breakdown
seed_rate_no_lang <- seed_rate_final

seed_rate_no_lang$logInfo<- str_replace(seed_rate_no_lang$logInfo, "- AMHARIC", "")
seed_rate_no_lang$logInfo<- str_replace(seed_rate_no_lang$logInfo, "- OROMIFFA", "")
seed_rate_no_lang$logInfo<- str_replace(seed_rate_no_lang$logInfo, "- TIGRIGNA", "")
seed_rate_no_lang$logInfo<- str_replace(seed_rate_no_lang$logInfo, "- WOLAYITTA", "")
seed_rate_no_lang$logInfo<- str_replace(seed_rate_no_lang$logInfo, "- WOLAYTIGNA", "")
seed_rate_no_lang$logInfo<- str_replace(seed_rate_no_lang$logInfo, "- SIDAMIGNA", "")

table(seed_rate_no_lang$logInfo)

ggplot(seed_rate_no_lang, aes(x=logInfo)) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar()+
  labs(
    title = 'RAIN MENU/PLANTING/SEED RATE
    content actually played',
    x = 'Content Selected',
    y= 'count'
  )


#DO THEY SUBSCRIBE?
