#WHO dataset
library(tidyverse)
library(dplyr)
library(plotly)
library(lubridate)
library(data.table)
library(zoo)

covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")

#date range "2020-03-18" "2021-02-22"
covid = subset(covid, between(covid$Date_reported,"2020-03-18", "2021-02-22"))
View(covid)

USA = subset(covid, Country_code=="US")
#USA$Date_reported = format(USA$Date_reported, "%Y/%b")
head(USA)
View(USA)

#per day
plot_new_case_pr_day = function(place_abbr){
  data_for_country = subset(covid, Country_code == place_abbr)
  
  ggplot(data_for_country)+
    geom_bar(aes(x=Date_reported, y = New_cases), stat = "identity")+xlab("Date") +
    ylab("Number of New Cases")+ggtitle(data_for_country$Country)+ theme_bw()
  
}

plot_new_case("AL")


monthly_new_cases = function(place_abbr){
  
  data_for_country = subset(covid, Country_code == place_abbr)
  
  #split into month and year
  data_for_country$mo <- strftime(data_for_country$Date_reported, "%m")
  data_for_country$yr <- strftime(data_for_country$Date_reported, "%Y")
  
  #aggregate by month
  newd = aggregate(New_cases~mo +yr, data = data_for_country, sum)
  newd$date = as.yearmon(paste(newd$yr, newd$mo), "%Y %m")
  newd$date = as.Date(newd$date)
  
  ggplot(newd)+
    geom_bar(aes(x=date, y = New_cases), stat = "identity")+xlab("Date") +
    ylab("Number of New Cases")+ggtitle(paste0("Monthly Number of New Cases for ", data_for_country$Country))+
    theme(plot.title = element_text(hjust = 0.5))+theme_bw()+
    scale_x_date(date_labels = "%y %b",date_breaks = "1 month")+
    #scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
    theme(axis.text.x = element_text(angle = 90))

}

monthly_new_cases("IQ")
monthly_new_cases("ES")
#use this function for county data
plot_cf_for_some_country = function(place_abbr){
  data_for_country = subset(covid, Country_code == place_abbr)
  data_for_country$case_fatality_per_tenk = (data_for_country$Cumulative_deaths/data_for_country$Cumulative_cases) *10000

  ggplot(data_for_country)+
    geom_bar(aes(x=Date_reported, y = case_fatality_per_tenk), stat = "identity",position = "dodge")+xlab("Date") +
    ylab("Case Fatalities per 10k")+ggtitle(data_for_country$Country)+ theme_bw()

}

##############################

#Global info on covid

sum(covid$Cumulative_cases)

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

