##-----0613:ggplotcamp
library(tidyverse)
load(url('https://dssoc.github.io/datasets/congress.RData'))
library(ggplot2)
library(dplyr)
#Create a bar plot to show the average ages of democrat and republican congress members. Now do the same for M and F genders (this second part should include members of all parties).
ggplot(congress%>%
         filter(party %in% c("Democrat","Republican")),
       aes(x=party[],fill=2021-birthyear))+
  geom_bar()

#Create two bar charts: one that shows the total number of social media accounts among democrats and republicans (Twitter, Facebook, YouTube), and one that shows the average number of accounts per-politician for each party.
#Which political party has more social media accounts? Which party has a higher per-politician average?
#congress1<-congress 
  #count(party, twitter,youtube,facebook) 
congress<- congress%>%
  filter(party %in% c("Democrat","Republican"))
congress1<- gather(congress,key="social_media",value="account",twitter,youtube,facebook)
p1<-ggplot(congress1,aes(x=party,color=social_media)) +
  geom_bar() 
p1
table(congress$type)
library(naniar)
naniar::miss_var_summary(congress1)
congress3<-congress%>%
  count(party)
d_n<- as.numeric(congress3[1,2]) 
r_n<- as.numeric(congress3[2,2])
congress2<-congress1%>%
  subset( !is.na(account))%>%
  count(party,social_media)%>%
  mutate(sum=ifelse(party=="Democrat",d_n,r_n))%>%
  mutate(pr=n/sum)
library(ggthemr)
library(patchwork)
p2<- ggplot(congress2,aes(x=party,y=pr,fill=social_media))+
  geom_col(position = "dodge")
ggthemr('pale', layout = 'scientific', spacing = 2, type = 'inner')
ggthemr_reset()
p1+p2 



