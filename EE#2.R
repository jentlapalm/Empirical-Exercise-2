library(readxl)
library(igraph)
library(data.table)
library(randtests)
library(stringr)
library(Matrix)
library(expm)
library(tidyverse)
library(stringr)

# home
setwd("C:\\Users\\Jent\\Documents\\College\\Emory\\Social Network Analytics\\EE#2")
# school
# setwd("C:\\Users\\jentl\\Documents\\Emory\\Fall 2021\\Social Network Analytics\\EE#2")

pf1="Funding_events_7.14.csv"
pf2="Funding_events_7.14_page2.xlsx"
pv="Venture_capital_firm_outcomes.csv"

f1 <- fread(pf1)
f2 <- read_excel(pf2)
v <-  fread(pv)

"
SETTING UP IGRAPH
tie=firms invested in same round (same row). ignore multiple instances of a tie

allow old ties to drop out of the network: a tie decays if it is not renewed within a time window greater than 90 percent of all renewal windows for all ties in the network.

remove decayed ties from the network 
"

# bind date, investor rows together
f1dt <- data.table(date=f1$`Deal Date`,investors=f1$Investors)
f2dt <- data.table(date=f2$`Deal Date`,investors=f2$Investors)

# converting to datetime
library(lubridate)

f1dt$date <- mdy(f1dt$date)
f2dt$date <- ymd(f2dt$date)

# binding
dt <- rbind(f1dt,f2dt)

# remove NA rows
dt <- na.omit(dt)

# separate investors into multiple columns

#checking for the max number of investors
ncols <- max(stringr::str_count(dt$investors, ",")) + 1 # 27 investors

# need to make list of columns to use this solution
cols=paste0('investor_',1:27)

dtwide <- tidyr::separate(data = dt, col = investors, sep = ",", into = cols, remove = FALSE)

# removing investors column
dtwide <- dtwide[,-'investors']

# only unique columns (same date doesn't count)
dtwide <- unique(dtwide)

"TRY TO USE DATA TABLE OPERATIONS TO DO THIS: CAN MATCH DPLYR'S FUNCTIONALITY BUT FASTER"
# Shu's to do list: loop to find investor relationships, trim white space,  remove duplicate rows, date diff fucntion (try data.table & dplyr), then find 90th percentile and remove rows

#test dataset 

test <- head(dtwide,1000)
test <- test[!is.na(test$investor_2)]

# creating for loop to get ties and dates
# need to remove rows where there is only one investor?

test_dt <- data.table(date=structure(numeric(0), class = "Date"), investor_1=character(), investor_2=character())

for (i in 1:nrow(test)){
  date <- test[i,1]
  mat <- as.matrix(test[i,-1])[1,]
  mat <- mat[!is.na(mat)]
  combs <- t(combn(mat,2))
  combs <- data.table(investor_1=combs[,1],investor_2=combs[,2])
  combs$date <- date
  test_dt <- rbind(test_dt,combs)
}


date <- test[2,1]
mat <- as.matrix(test[2,-1])[1,]
mat <- mat[!is.na(mat)]
combs <- t(combn(mat,2))
combs <- data.table(investor_1=combs[,1],investor_2=combs[,2])
combs$date <- date
test_dt <- rbind(test_dt,combs)

# test set worked! running on actual dt

dtwide <- dtwide[!is.na(dtwide$investor_2)]

network <- data.table(date=structure(numeric(0), class = "Date"), investor_1=character(), investor_2=character())

for (i in 1:nrow(dtwide)){
  date <- dtwide[i,1]
  mat <- as.matrix(dtwide[i,-1])[1,]
  mat <- mat[!is.na(mat)]
  combs <- t(combn(mat,2))
  combs <- data.table(investor_1=combs[,1],investor_2=combs[,2])
  combs$date <- date
  network <- rbind(network,combs)
}

# remove repeat rows
network <- unique(network)

#need to remove rows with Inc.
inc <- network[224296,2]
network <- network[!investor_1==inc]
network <- network[!investor_2==inc]

# removing whitespace
network$investor_1 <- trimws(network$investor_1, which = c("left"))
network$investor_2 <- trimws(network$investor_2, which = c("left"))

# need to write the decay function

# first, get all renewal windows for all ties in network
pairs <- unique(network[,2:3])

x <- network[investor_1==pairs[1,1] & investor_2==pairs[1,2]]
x[ , time := date-shift(date)]
x
window <- na.omit(x$time)
as.data.table(window)
