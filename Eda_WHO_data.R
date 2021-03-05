#WHO dataset
library(tidyverse)
library(dplyr)
library(plotly)
library(lubridate)
library(data.table)

covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")

View(covid)
names(covid)
#date range "2020-03-18" "2021-02-22"

covid = subset(covid, between(covid$Date_reported,"2020-03-18", "2021-02-22"))


USA = subset(covid, Country_code=="US")
#USA$Date_reported = format(USA$Date_reported, "%Y/%b")
head(USA)
View(USA)

# #num new cases and day USA, need to filter some dates???
# ggplot(USA)+
#   geom_bar(aes(x=Date_reported, y = New_cases), stat = "identity")+xlab("Date") +
#   ylab("Number of New Cases")+ theme_bw()
# #case fatality
# USA$case_fatality_per_tenk = (USA$Cumulative_deaths/USA$Cumulative_cases) *10000
# USA$case_fatality_per_tenk[which(is.na(USA$case_fatality_per_tenk))] = 0
# #again maybe filter
# ggplot(USA)+
#   geom_bar(aes(x=Date_reported, y = case_fatality_per_tenk), stat = "identity", position = "dodge")+xlab("Date") +
#   ylab("Case Fatalities per 10k")+ theme_bw()

plot_new_case = function(place_abbr){
  data_for_country = subset(covid, Country_code == place_abbr)
  
  ggplot(data_for_country)+
    geom_bar(aes(x=Date_reported, y = New_cases), stat = "identity")+xlab("Date") +
    ylab("Number of New Cases")+ggtitle(data_for_country$Country)+ theme_bw()
  
}

plot_new_case("AL")


plot_cf_for_some_country = function(place_abbr){
  data_for_country = subset(covid, Country_code == place_abbr)
  data_for_country$case_fatality_per_tenk = (data_for_country$Cumulative_deaths/data_for_country$Cumulative_cases) *10000

  ggplot(data_for_country)+
    geom_bar(aes(x=Date_reported, y = case_fatality_per_tenk), stat = "identity",position = "dodge")+xlab("Date") +
    ylab("Case Fatalities per 10k")+ggtitle(data_for_country$Country)+ theme_bw()

 #return
  
}

plot_cf_for_some_country("GB")
ggplotly(plot_cf_for_some_country("GB"))

######

#Global info on covid

sum(covid$Cumulative_cases)
