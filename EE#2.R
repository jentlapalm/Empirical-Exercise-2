library(data.table)
library(tidyverse)
library(readxl)

setwd("C:\\Users\\Jent\\Documents\\College\\Emory\\Social Network Analytics\\EE#2")

pf1="Funding_events_7.14.csv"
pf2="Funding_events_7.14_page2.xlsx"
pv="Venture_capital_firm_outcomes.csv"

f1 <- fread(pf1)
f2 <- read_excel(pf2)
v <-  fread(pv)

# drop unnecessary columns
# bind rows together
# convert to date-time format
# determine time-window algorithm
