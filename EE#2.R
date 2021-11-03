
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
#setwd("C:\\Users\\Jent\\Documents\\College\\Emory\\Social Network Analytics\\EE#2")
# school
setwd("C:\\Users\\jentl\\Documents\\Emory\\Fall 2021\\Social Network Analytics\\EE#2")


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
f1dt$date <- as.Date(f1dt$date,'%m/%d/%Y')
f2dt$date <- as.Date(f2dt$date,'%Y-%m-%d',tz='UTC')

# binding
dt <- rbind(f1dt,f2dt)

# remove NA rows
dt <- na.omit(dt)

# separate investors into multiple columns

#checking for the max number of investors
ncols <- max(stringr::str_count(dt$investors, ",")) + 1 # 27 investors


"TRY TO USE DATA TABLE OPERATIONS TO DO THIS: CAN MATCH DPLYR'S FUNCTIONALITY BUT FASTER"
# Shu's to do list: loop to find investor relationships, trim white space, date conversion, remove duplicate rows, date diff fucntion (try data.table & dplyr), then find 90th percentile and remove rows

# need to make list of columns to use this solution
cols=c()

for (i in 1:27){
  cols <- c(cols,paste('investor_',i))
}

dtwide <- tidyr::separate(data = dt, col = investors, sep = ",", into = cols, remove = FALSE)

#removing investors column
dtwide <- dtwide[,-2]

'LOAD EMPIRICAL EXERCISES #1 SOLUTIONS FOR DATA.TABLE CODE'