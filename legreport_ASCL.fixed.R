
# Clean current work space
rm(list=ls())

# Suppress scientific notation
options(scipen = 9999)

# Load required package
library(tidyverse)
library(lubridate)
library(dbplyr)
library(tcltk)
library(rlang)
library(knitr)
library(stringr)
library(tinytex)


setwd("C:\\_R")

########################################################

#delete the first line of each entry with "submission" -- we don't need to count submission -> under review because this is time that we might not have been paid fees yet, so it really starts the clock when we start reviewing


########################################################


# Load input file(s)
ascl.in <- read_csv("legreport_ascl\\ASCL_Output_Completes_01232023.csv",  na = c("", " ", "NA"))
stat.pairs <- read_csv("legreport_ascl\\status_pairs_completes.csv",  na = c("", " ", "NA"))

ascl <- ascl.in

ascl <- ascl[ascl$`New Status` != "Submitted", ]

ids <- unique(ascl$`Document ID`)


########################################################

#add rename start status/date columns and add/populate end status/date columns


colnames(ascl) <- c("ID", "Start_Status", "Start_Date", "Changed_By")

ascl$End_Status <- NA
ascl$End_Date <- NA

ascl.new <- as_tibble(matrix(NA, nrow=1, ncol=ncol(ascl)))
colnames(ascl.new) <- colnames(ascl)

i <- 50
j <- 5


for (i in 1:length(ids)) {
  id.temp <- subset(ascl, ID == ids[i])
  
  for (j in 1:(nrow(id.temp)-1)) {
    
    id.temp$End_Status[j] <- id.temp$Start_Status[j+1]
    id.temp$End_Date[j] <- id.temp$Start_Date[j+1]
    
  }
  
  id.temp$End_Date <- as.Date.POSIXct(id.temp$End_Date)
  ascl.new <- bind_rows(ascl.new, id.temp)
  
}

ascl.new$Start_Date <- as.Date(ascl.new$Start_Date, format = "%m/%d/%Y")
ascl.new$End_Date <- as.Date(ascl.new$End_Date, format = "%m/%d/%Y")

head(ascl)
head(ascl.new)

ascl <- ascl.new

ids2 <- unique(ascl$ID)

#testprs <- cbind(ascl$Start_Status, ascl$End_Status)

#######################################################################

#calculate date difference

ascl$Start_Date <- ymd(ascl$Start_Date)
ascl$End_Date <- ymd(ascl$End_Date)
ascl$Length <- as.integer(ascl$End_Date - ascl$Start_Date)



#######################################################################

#join to status.pairs table to get dept vs appl time\
ascl.join <- right_join(stat.pairs, ascl, by = c("Start_Status", "End_Status"))
ascl.join <- ascl.join %>% 
  arrange(ID, Start_Date) %>%
  select(ID, Start_Status, Start_Date, End_Status, End_Date, Length, Whose_Time)
head(ascl.join)

ids3 <- unique(ascl.join$ID)

#summarise by permit
ascl.sum <- ascl.join %>%
  group_by(ID, Whose_Time) %>%
  summarise(n = n(), 
            Sum = sum(Length))
ascl.sum <- na.omit(ascl.sum)
ascl.sum

ascl.wide <- pivot_wider(ascl.sum, 
                         names_from = Whose_Time, 
                         values_from = c(n, Sum),
                         values_fill = 0)
ids4 <- unique(ascl.wide$ID)


#get dept vs appl averages
ascl.output <- ascl.sum %>%
  group_by(Whose_Time) %>%
  summarize(N_Permit = n(),
            N_Status_Changes = sum(n),
            Mean_Time = mean(Sum))
ascl.output <- na.omit(ascl.output)
ascl.output


#get dept vs appl averages
ascl.output2 <- ascl.wide %>%
  summarize(n_Permit = n(),
            n_statchange_appl = sum(n_applicant),
            mean_statchange_appl = mean(n_applicant),
            n_statchange_dept = sum(n_department),
            mean_statchange_dept = mean(n_department))


ascl.output2 <- na.omit(ascl.output2)
ascl.output2




length(unique(ascl$ID))
length(unique(ascl.wide$ID))
testlst <- (unique(ascl.wide$ID))

ids4[!(ids4 %in% ids)]
ids[!(ids %in% ids4)]


#############################

##ones without incompletes

ascl.slow <- ascl %>%  filter(Start_Status == "Correcting")
ascl.slow.id <- unique(ascl.slow$ID)
ascl.fast.id <- ids[!(ids %in% ascl.slow.id)]
ascl.fast <- ascl[ascl$ID %in% ascl.fast.id,]
  
ascl.fast.summ <- na.omit(ascl.fast) %>%
  group_by(ID) %>%
  summarize(total_time = sum(Length))

ascl.fast.summ.summ <- ascl.fast.summ %>%
  summarize(n_permits = n(),
            mean_time = mean(total_time))


############################

##length of time first under review to awarded

ascl2 <- ascl.in

colnames(ascl2) <- c("ID", "Start_Status", "Start_Date", "Changed_By")
ascl2 <- subset(ascl2, Start_Status %in% c("Submitted", "Awarded"))

ids2 <- unique(ascl2$ID)
  
  
ascl2$End_Status <- NA
ascl2$End_Date <- NA

ascl2.new <- as_tibble(matrix(NA, nrow=1, ncol=ncol(ascl2)))
colnames(ascl2.new) <- colnames(ascl2)

i <- 10
j <- 1


for (i in 1:length(ids2)) {
  id.temp <- subset(ascl2, ID == ids2[i])
  
  for (j in 1:(nrow(id.temp)-1)) {
    
    id.temp$End_Status[j] <- id.temp$Start_Status[j+1]
    id.temp$End_Date[j] <- id.temp$Start_Date[j+1]
    
  }
  
  #id.temp$End_Date <- as.Date.POSIXct(id.temp$End_Date)
  ascl2.new <- bind_rows(ascl2.new, id.temp)
  
}

ascl2.new$Start_Date <- as.Date(ascl2.new$Start_Date, format = "%m/%d/%Y")
ascl2.new$End_Date <- as.Date(ascl2.new$End_Date, format = "%m/%d/%Y")

head(ascl2)
head(ascl2.new)

ascl2 <- ascl2.new

###

idsdf <- as.data.frame(ids2)
idsdf$tot_len <- NA

for(i in 1:nrow(idsdf)) {
  temp <- subset(ascl2, ID == ids2[i])
  temp$Start_Date <- as.Date(temp$Start_Date, format = "%m/%d/%Y")
  temp$Start_Date <- ymd(temp$Start_Date)
  idsdf$tot_len[i] <- temp$Start_Date[2] - temp$Start_Date[1]

}






