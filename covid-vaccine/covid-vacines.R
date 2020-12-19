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
statesDf$First.Doses <- str_replace_all(statesDf$First.Doses, "[[:punct:]]", "") %>% strtoi()
#change column names to perform a full join, population data is from 2015
colnames(statepop) <- c("fips","abr","Jurisdiction","pop_2015")

#inner join to include
final_df <- inner_join(statesDf,statepop,by="Jurisdiction")  %>% mutate(percent_vaccinated = (First.Doses/pop_2015)*100)

#getting top ten most vaccinated states by percent vaccinated of population
topTenStatesVP <- head(arrange(final_df,desc(percent_vaccinated)), n = 10)

#getting top ten most vaccinated states by total number of vaccinations
topTenStatesP <- head(arrange(final_df,desc(First.Doses)), n = 10)

#top ten vaccinated states by percentage of population
ggplot(data=topTenStatesVP, aes(x = reorder(Jurisdiction, sort(percent_vaccinated,TRUE)), y=percent_vaccinated, fill=Jurisdiction)) +
  geom_bar(stat="identity")
#top ten vaccinated states by total
ggplot(data=topTenStatesP, aes( x = reorder(Jurisdiction, sort(First.Doses,TRUE)), y = First.Doses, fill=Jurisdiction)) +
  geom_bar(stat="identity")

#us map of states with highest percentage of people vaccinated 
 plot_usmap(data = final_df, values = "percent_vaccinated", color = "black") + 
   scale_fill_continuous(
     low = "white", high = "green", name = "Percent Vaccinated", label = scales::comma
   ) + theme(legend.position = "right")
# total vaccines distributed per state
 plot_usmap(data = final_df, values = "First.Doses", color = "black") + 
   scale_fill_continuous(
     low = "white", high = "blue", name = "Total Vaccinated", label = scales::comma
   ) + theme(legend.position = "right")
 