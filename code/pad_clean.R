library(tidyverse)
library(dplyr)
library(stringr)


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

log_data <- read_csv("~/Desktop/PAE/data/logData.csv")

log_data$language <- log_data$logInfo

plot(log_data$langId, log_data$noCallsMade)
