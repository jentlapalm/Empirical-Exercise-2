library(readxl)
library(igraph)
library(data.table)
library(randtests)
library(stringr)
library(Matrix)
library(expm)
library(tidyverse)
library(stringr)

# home wd
setwd("C:\\Users\\Jent\\Documents\\College\\Emory\\Social Network Analytics\\EE#2")
# school wd
# setwd("C:\\Users\\jentl\\Documents\\Emory\\Fall 2021\\Social Network Analytics\\EE#2")

f1 <- fread("Funding_events_7.14.csv")
f2 <- read_excel("Funding_events_7.14_page2.xlsx")
v <-  fread("Venture_capital_firm_outcomes.csv")

"SETTING UP NETWORK"

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

# list of columns
cols=paste0('investor_',1:27)

# separating investors into separate columns
dtwide <- tidyr::separate(data = dt, col = investors, sep = ",", into = cols, remove = FALSE)

# removing investors column
dtwide <- dtwide[,-'investors']

# only unique columns (same date doesn't count)
dtwide <- unique(dtwide)

# need to remove rows where there is only one investor
dtwide <- dtwide[!is.na(dtwide$investor_2)]

# creating for loop to get ties and dates

# empty data.table 
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

#need to remove rows with only Inc.
inc <- network[224296,2]
network <- network[!investor_1==inc]
network <- network[!investor_2==inc]

# removing whitespace
network$investor_1 <- trimws(network$investor_1, which = c("left"))
network$investor_2 <- trimws(network$investor_2, which = c("left"))

# decaying old ties
# first, get all renewal windows for all ties in network

# network[,renew := NULL] #to remove a column
 
# order network by date
network <- network[order(date)]

# getting renewal periods
network[,renew:=date-shift(date),by=.(investor_1,investor_2)]

# decaying ties > 90th percentile
network <- network[!renew>quantile(network$renew, .9 ,na.rm=TRUE)]

"QUESTION 1
Which firm is the center of the venture capital firm network as of July 2014? Consider the most central firm to be the firm with the highest closeness centrality, as in the Hollywood Actor example."

