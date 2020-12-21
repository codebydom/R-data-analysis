library(dplyr)
library(ggplot2)
library(usmap)
library(tidyverse)

#data from csv file 
X <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"))

#only getting the most recent totals from the csv data by filtering on the max date
df <- X %>% group_by(location) %>% filter(date == max(date))
head(df)
#changing the column names in the data frame to be more easily understanable
colnames(df) <- c("Location","Date","Total_Vaccines_Distributed","Total_Vaccinations_Per_Hundred")

#global vaccinations total across the world by country and also the total distributed in the world (most up to date from data source)
ggplot(data=df, aes(x=reorder(Location, sort(Total_Vaccines_Distributed,TRUE)), y=Total_Vaccines_Distributed, fill=Location)) +
  geom_bar(stat="identity")
#global vaccinations by total vaccinations per hunred people in the country
ggplot(data=df, aes(x=reorder(Location, sort(Total_Vaccinations_Per_Hundred,TRUE)), y=Total_Vaccinations_Per_Hundred, fill=Location)) +
  geom_bar(stat="identity")

#read in the united states covid 19 vaccination data by state (aka Jurisdiction)
statesDf <- read.csv(url("https://data.cdc.gov/api/views/saz5-9hgg/rows.csv?accessType=DOWNLOAD"))
#must scarp out the special character *** in data
statesDf$Jurisdiction <- str_replace_all(statesDf$Jurisdiction, "[[:punct:]]", "")
#get rid of comma in doses column

statesDf$Total.Pfizer.Allocation..First.Dose..Shipments <- str_replace_all(statesDf$Total.Pfizer.Allocation..First.Dose..Shipments, "[[:punct:]]", "") %>% strtoi()
#change column names to perform a full join, population data is from 2015
colnames(statepop) <- c("fips","abr","Jurisdiction","pop_2015")

#inner join to include
final_df <- inner_join(statesDf,statepop,by="Jurisdiction")  %>% mutate(percent_vaccinated = (Total.Pfizer.Allocation..First.Dose..Shipments/pop_2015)*100)
final_df
#getting top ten most vaccinated states by percent vaccinated of population
topTenStatesVP <- head(arrange(final_df,desc(percent_vaccinated)), n = 10)

#getting top ten most vaccinated states by total number of vaccinations
topTenStatesP <- head(arrange(final_df,desc(Total.Pfizer.Allocation..First.Dose..Shipments)), n = 10)

#top ten vaccinated states by percentage of population

ggplot(data=topTenStatesVP, aes(x = reorder(Jurisdiction, sort(percent_vaccinated,TRUE)), y=percent_vaccinated, fill=Jurisdiction)) +
  geom_bar(stat="identity")
#top ten vaccinated states by total
ggplot(data=topTenStatesP, aes( x = reorder(Jurisdiction, sort(Total.Pfizer.Allocation..First.Dose..Shipments,TRUE)), y = Total.Pfizer.Allocation..First.Dose..Shipments, fill=Jurisdiction)) +
  geom_bar(stat="identity")

#us map of states with highest percentage of people vaccinated 
 plot_usmap(data = final_df, values = "percent_vaccinated", color = "black") + 
   scale_fill_continuous(
     low = "white", high = "green", name = "Percent Vaccinated", label = scales::comma
   ) + theme(legend.position = "right")
# total vaccines distributed per state
 plot_usmap(data = final_df, values = "Total.Pfizer.Allocation..First.Dose..Shipments", color = "black") + 
   scale_fill_continuous(
     low = "white", high = "blue", name = "Total Vaccinated", label = scales::comma
   ) + theme(legend.position = "right")
 
 # moderna vaccine data
 
modDf <- read.csv(url("https://data.cdc.gov/api/views/b7pe-5nws/rows.csv?accessType=DOWNLOAD"))
 
 
#must scarp out the special character *** in data
modDf$Jurisdiction <- str_replace_all(statesDf$Jurisdiction, "[[:punct:]]", "")
#get rid of comma in doses column
 head(modDf)
modDf$Doses.allocated.week.of.12.21 <- str_replace_all(modDf$Doses.allocated.week.of.12.21, "[[:punct:]]", "") %>% strtoi()

final_df2 <- inner_join(modDf,statepop,by="Jurisdiction")  %>% mutate(percent_vaccinated = (Doses.allocated.week.of.12.21/pop_2015)*100)
topTenStatesmVP <- head(arrange(final_df2,desc(percent_vaccinated)), n = 10)

#getting top ten most vaccinated states by total number of vaccinations
topTenStatesmP <- head(arrange(final_df2,desc(Doses.allocated.week.of.12.21)), n = 10)

#top ten vaccinated states by percentage of population

ggplot(data=topTenStatesVP, aes(x = reorder(Jurisdiction, sort(percent_vaccinated,TRUE)), y=percent_vaccinated, fill=Jurisdiction)) +
  geom_bar(stat="identity")
#top ten vaccinated states by total
ggplot(data=topTenStatesP, aes( x = Jurisdiction, y = Doses.allocated.week.of.12.21, fill=Jurisdiction)) +
  geom_bar(stat="identity")


plot_usmap(data = final_df2, values = "percent_vaccinated", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "orange", name = "Percent Vaccinated", label = scales::comma
  ) + theme(legend.position = "right")
# total vaccines distributed per state
plot_usmap(data = final_df2, values = "Doses.allocated.week.of.12.21", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "Total Vaccinated", label = scales::comma
  ) + theme(legend.position = "right")

head(final_df)
# total moderna and pzier vaccines 
finalDF3 <- inner_join(final_df,final_df2[1:3],by="Jurisdiction")  %>% mutate(total_vaccinated = Doses.allocated.week.of.12.21.y+Total.Pfizer.Allocation..First.Dose..Shipments,total_per = total_vaccinated/pop_2015*100)

plot_usmap(data = finalDF3, values = "total_vaccinated", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "black", name = "Total Vaccinations distributed (Pfizer+Moderna)", label = scales::comma
  ) + theme(legend.position = "right")

plot_usmap(data = finalDF3, values = "total_per", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "purple", name = "Percent of Vaccines covering Population (Pfizer+Moderna)", label = scales::comma
  ) + theme(legend.position = "right")

 