library(readxl)
library(igraph)
library(data.table)
library(randtests)
library(stringr)
library(Matrix)
library(expm)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggthemes)

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
f1dt$date <- mdy(f1dt$date)
f2dt$date <- ymd(f2dt$date)

# binding
dt <- rbind(f1dt,f2dt)

# remove NA rows
dt <- na.omit(dt)

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

# removing whitespace
network$investor_1 <- trimws(network$investor_1, which = c("left"))
network$investor_2 <- trimws(network$investor_2, which = c("left"))

#need to remove rows with only Inc.
network <- network[!investor_1=='Inc.']
network <- network[!investor_2=='Inc.']

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

# unique ties before August 2014
bacon <- unique(network[date<"2014-08-01",2:3])

# creating igraph: my centrality values are in alphabetical order. this is strange
# how do I fix this?
bgraph <- graph_from_data_frame(bacon, directed=F)

most_central <- sort(closeness(bgraph), decreasing=T) 

most_central[1] # highest is Intel Capital with closeness of 7.946447e-07

"QUESTION 2
Allow the network to be up dated monthly for each month in the data, adding the new ties that occur through investments in the current month to be added to the existing network of ties that have occurred in previous months.

Plot the average k-core of each venture capital firm in the network over time. This can be computed using the igraph function coreness. On the x-axis should be time. On the y-axis should be the highest-degree k-core each venture capital firm belongs to, averaged over all firms in the network up to that month."

# x and y vectors

x <- lubridate::ymd()
y <- c()

# loop to calculate x and y

for (i in 1:375){
  # subset network
  subnet <- network[date %between% .(date[1],date[1]+months(i)),2:3]
  # add end-date to x
  x <- c(x,network$date[1]+months(i))
  # convert to igraph, calculate coreness
  core <- coreness(graph_from_data_frame(subnet))
  # max over average
  avg_k_core <- max(core)/mean(core)
  # save to y
  y <- c(y,avg_k_core)
}

#plotting with ggplot
ggdt <- data.table(x,y)

ggplot(ggdt,aes(x,y))+geom_line()+theme_wsj()+theme(axis.title = element_text())+xlab('Time')+ylab('Average k-core')+ggtitle('Investor Avg k-core Over Time')

"QUESTION #3 (A)
Next, we will look at the development of the venture capital firm co-investment network in
terms of its global core-periphery structure. 

Allow the network to be updated monthly, as in Question 2.

Calculate the co-investment network's concentration scores to determine if it tends towards a core-periphery structure across the data. 

Illustrate a figure, with one plot for one month from each calendar year in the data, 
that shows the range of concentration scores for each partition size p for the range of 
p in 1 to the number of nodes in the networks in the network for that month's cross-section. 

You can exclude the very early period of the data when all of the firms have the same eigenvector centrality."

# allie's advice:
"it is taking the centrality of each month of the data and sorting it then creating [1 ..0] matrices and computing the correlation with each of those matrices where you add a 1 each time"

# test case before loop
# first, choose a month. get subnet up to that month. get number of nodes/vertices (x axis). get range(concentration scores) for each partition size

avg_date <- mean(network$date)
test <- network[date<=avg_date,2:3]
testgraph <- graph_from_data_frame(test)
num_nodes <- length(V(testgraph))



"QUESTION #3(B)
Provide one other piece of descriptive evidence outside of the concentration scores to support your conclusion. You can see Slide 29 of Class 4 for some examples of evidence to use."