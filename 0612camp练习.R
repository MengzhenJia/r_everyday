#---0612camp
rm(list=ls())
library(tidyverse)
load(url('https://dssoc.github.io/datasets/congress.RData'))

#Create a new dataframe that includes only senators and the columns gender, birthyear, and party. Then use that new dataframe to compute the number of male and female democrats and republicans (the output should be five rows corresponding to female democrats, male democrats, male independents, female republicans, and male republicans).
new_df<-congress %>%
  filter(type=="sen") %>%
  count(gender,party)
#Identify the oldest and youngest male and female democrat senators using tidyverse functions.
moldest<-congress %>%
  filter(gender=="M")%>%
  arrange(birthyear)
moldest[1,1]
foldest<-congress %>%
  filter(gender=="F")%>%
  arrange(birthyear)
foldest[1,1]
#Using mutate, create a new variable called age which represents the approximate age of each member of congress. How many democratic senators are over 60 years old?
#Note: you can approximate age using the formula age = 2021-birthyear.

congress1 <- congress %>%
  mutate(age=2021-birthyear) %>%
  filter(age>60) 
nrow(congress1)

#Create a new column that indicates whether or not the member of congress is more than 55 years old, and create a single dataframe showing the number of male and female members of congress that are over and under 55.
#Note: the dataframe should have four rows: number of females over 55, number of males over 55, number of females under 55, number of males under 55.
congress2<-congress%>%
  mutate(age=2021-birthyear) %>%
  mutate(age55=age>55)%>%
  count(gender,age55)
###
#Using gather, create a new dataframe where each 
#row corresponds to a valid twitter, facebook, or 
#youtube social media account, then compute the total 
#number of accounts for each political party. 
#Then do the same with pivot_longer.
congress3<-congress %>%
  filter(!is.na(facebook)&!is.na(youtube)&!is.na(twitter))
congress4<-gather(congress3,key="socialmedia",value="value","facebook", "youtube","twitter")


travel_restrictions <- c("WA", "OR", "NV", "CA", "NM", "MN", "IL", "OH", "MI", "PA", "VA", "NY", "MA", "VH", "ME", "DE", "MD", "NJ")
require_masks <- c("HI", "WA", "OR", "NV", "CA", "MT", "CO", "NM", "KS", "TX", "MN", "AR", "LA", "WI", "IL", "AL", "MI", "IN", "OH", "KY", "WV", "NC", "VA", "DC", "DE", "PA", "NY", "VT", "NH", "MA", "RI", "CT", "ME")

as.data.frame(travel_restrictions)
as.data.frame(require_masks)

setdiff(travel_restrictions, require_masks)
#交集
s <- intersect(travel_restrictions, require_masks)
typeof(list(s))
#有旅行限制但是没有口罩要求的州???
which[travel_restrictions %in% list(s)]
m<- which(travel_restrictions%in%list(s) )
match(travel_restrictions,s,nomatch=1)


data <- data.frame(
    time = as.Date('2018-10-12') + 0:9,
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4)
  )
  library(tidyr)
  data2 <- gather(data, category, value, X, Y, Z)  
  