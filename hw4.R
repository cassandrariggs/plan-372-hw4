# installing packages I am using
install.packages('dplyr')
install.packages('tidyverse')
library(dplyr)
library(tidycensus)
library(readr)

# importing the airports csv
airport_pairs <- read_csv("airport_pairs.csv")

#filtering for only pairs with RDU as origin OR dest.
onlyRDU<- airport_pairs %>%
  filter(origin == 'RDU'| dest == 'RDU')

#filter to only include airport pairs with >10,000 passengers
onlyRDU<- onlyRDU %>%
  filter(passengers > 10000)

#creating a table in descending order according to passengers of airport pairs 
tableap<-onlyRDU[c("origin","dest","passengers")]
tableap[order(tableap$passengers, decreasing = TRUE),]  

#dowloading the census data for the 2010s
#census_api_key("apikey", overwrite = TRUE, install = TRUE)
# readRenviron("~/.Renviron")
#Sys.getenv("CENSUS_API_KEY")
census<-get_decennial(geography= "cbsa",variables = "P001001",year=(2010))

#creating separate dfs to merge origin pop and dest pop to the main df
origin<-census
dest<-census
origin$origin_cbsa<- census$GEOID
origin$origin_pop<- census$value
dest$dest_cbsa<- census$GEOID
dest$dest_pop<- census$value

#merging total pop with airport
merge1 = merge(x = airport_pairs, y = origin, by = "origin_cbsa")
totalpop = merge(x = merge1, y = dest, by = "dest_cbsa")
View(totalpop)

#removing micro data
totalpop<-totalpop[!grepl("Micro", totalpop$NAME.x),]
totalpop<-totalpop[!grepl("Micro", totalpop$NAME.y),]

#checking to make sure all the micro was taken out
unique(totalpop$NAME.x)
unique(totalpop$NAME.y)

#grouping by 
aggap <- totalpop %>% group_by(origin_cbsa,dest_cbsa) %>% 
  summarise(orgpopmean = mean(origin_pop), destpopmean = mean(dest_pop),totalpass=sum(passengers),meandist=mean(distancemiles))

#plotting
plot(x = aggap$orgpopmean,y = aggap$totalpass,
     xlab = "Origin Population",
     ylab = "Total Passengers",
     main = "Origin Population vs Total Passengers"
)

plot(x = aggap$destpopmean,y = aggap$totalpass,
     xlab = "Destination Population",
     ylab = "Total Passengers",
     main = "Destination Population vs Total Passengers"
)

plot(x = aggap$meandist,y = aggap$totalpass,
     xlab = "Flight Distance",
     ylab = "Total Passengers",
     main = "Flight Distance vs Total Passengers"
)



#3
#regression: total pass vs origin cbsa pop, dest cbsa pop, distance
lmpop = lm(totalpass~orgpopmean + destpopmean +meandist, data = aggap) #Create a linear regression with two variables
summary(lmpop)


#4
#creating a separate dataframe for the new pairs
org<-c('RDU','RDU','RDU','RDU')
originpop<-c(1130490,1130490,1130490,1130490)
destin <-c('PDX','ELP','TLH','SAN')
destinpop<-c(2226009,800647,367413,3095313)
distance<-c(2363,1606,496,2193)
newdata <- data.frame(org,originpop,destin,destinpop,distance)

#creating a calculated column to prerdiict the number of passengers based on the data
newdata$predictedpass<-7311 + ((0.02519 * newdata$originpop) + (0.02478 * newdata$destinpop) + (-26.98 * newdata$distance))


#making tables for the predicted passengers arranged decreasing to determnie which is most popular
table2<-newdata[c("org","destin","predictedpass")]
table2[order(table2$predictedpass, decreasing = TRUE),] 
 
