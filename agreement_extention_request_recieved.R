## this looks through the PT action form, pulls agreements that have extension request received and agreement extended transactions, sees which is the most recent. if agreement is extended is most recent, marks it as a 0. if ext req rec is most recent, marks it as a 1 so we know it needs updating

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


dat <- read_csv("agreement_extension_request\\PT_Action_05302023.csv",  na = c("", " ", "NA"))
dat$ActionDesc2 <- dat$ActionDesc
dat$ActionDesc2 <- substr(dat$ActionDesc2, 1, 4)

#define and subset to focal statuses
aerr <- c("EXTR")
aext <- c("ERCV", "EXTN", "FELS", "FECL", "FECR")
aexp <- c("AEXP")
aall <- c(aerr,aext,aexp)

d <- dat
d$Update <- NA
dlst <- d$TrackingNumber[d$ActionDesc2 %in% aerr]

d$ActionDate <- as.Date(d$ActionDate, "%m/%d/%Y")

d <- d %>%
  arrange(desc((ActionDate))) %>% ###by tracking number - by group?
  filter(TrackingNumber %in% dlst) %>%
  filter(ActionDesc2 %in% aerr | ActionDesc2 %in% aext)



##loop
#make destination column
#subset for each tracking number
#for each chunk, IF last entry in ActionDesc is in aext category, enter 0 f0r all that category
#ELSE IF last entry is in aerr, enter 1
#alternatively can create a separate table with tracking number and a 0 or 1


i <- 15


for(i in 1:length(dlst)) {
  
  temp <- subset(d, TrackingNumber == dlst[i])
  temp <- arrange(temp, as.Date(ActionDate))
  if (temp$ActionDesc2[nrow(temp)] %in% aerr) {
    d$Update[d$TrackingNumber == dlst[i]] <- 1
  } else {
    d$Update[d$TrackingNumber == dlst[i]] <- 0
  }
  
}





##expiration dates

exp <- dat[dat$ActionDesc2 %in% aexp,]
exp$ActionDate <- as.Date(exp$ActionDate, "%m/%d/%Y")
#sort by most recent date to oldest date then delete the older if there are duplicates
exp <- exp %>%
  arrange(desc(ActionDate)) %>%
  distinct(TrackingNumber, .keep_all = TRUE) %>%
  mutate(ExpirationDate = ActionDate)

d <- left_join(x=d, y=exp[,c("TrackingNumber", "ExpirationDate")], by="TrackingNumber")



to_update_all <- d[d$Update == 1,]
to_update <- as.data.frame(unique(to_update_all$TrackingNumber))
write_csv(to_update, "to_update.csv", col_names = TRUE)

dont_update_all <- d[d$Update == 0,]
dont_update <- as.data.frame(unique(dont_update_all$TrackingNumber))
write_csv(dont_update, "dont_update.csv", col_names = TRUE)



write_csv(d, "agreementextreqreceived_updates.csv", col_names = TRUE)





##############################

##trying to figure out how to note which ones have aerr followed by aexp but no aext inbetween
##these should be ae but need to be reached out to regions to fix the transactions

dex <- dat
dex$ActionDate <- as.Date(dex$ActionDate, "%m/%d/%Y")
dex <- dex
