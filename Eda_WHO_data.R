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
####################################### per day
plot_new_case_pr_day = function(place_abbr){
  data_for_country = subset(covid, Country_code == place_abbr)
  
  newd = aggregate(New_cases~mo +yr, data = data_for_country, sum)
  
  
  newd$date = as.yearmon(paste(newd$yr, newd$mo), "%Y %m")
  newd$date = as.Date(newd$date)
  
  ggplot(data_for_country)+
    geom_bar(aes(x=Date_reported, y = New_cases), stat = "identity")+xlab("Date") +
    ylab("Number of New Cases")+ggtitle(data_for_country$Country)+ theme_bw()
  
}

plot_new_case("AL")

########################################################
library(gridExtra)
monthly_new_cases = function(place_abbr){
  
  data_for_country = subset(covid, Country_code == place_abbr)
  
  #split into month and year
  data_for_country$mo <- strftime(data_for_country$Date_reported, "%m")
  data_for_country$yr <- strftime(data_for_country$Date_reported, "%Y")
  
  #aggregate by month
  new.case = aggregate(New_cases~mo +yr, data = data_for_country, sum)
  new.case$date = as.yearmon(paste(new.case$yr, new.case$mo), "%Y %m")
  new.case$date = as.Date(new.case$date)
  
 ggplot(new.case)+
    geom_bar(aes(x=date, y = New_cases), stat = "identity")+xlab("Date") +
    ylab("Number of New Cases")+ggtitle(paste0("Monthly Number of New Cases for ", data_for_country$Country))+
    theme(plot.title = element_text(hjust = 0.5))+theme_bw()+
    scale_x_date(date_labels = "%y %b",date_breaks = "1 month")+
    #scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
    theme(axis.text.x = element_text(angle = 90))
}

gCIQ =monthly_new_cases("IQ")
gCES = monthly_new_cases("ES")


monthly_num_dead = function(place_abbr){
  
  data_for_country = subset(covid, Country_code == place_abbr)

  #split into month and year
  data_for_country$mo <- strftime(data_for_country$Date_reported, "%m")
  data_for_country$yr <- strftime(data_for_country$Date_reported, "%Y")

  #aggregate by month
  newd.dead = aggregate(New_deaths~mo +yr, data = data_for_country, sum)
  newd.dead$date = as.yearmon(paste(newd.dead$yr, newd.dead$mo), "%Y %m")
  newd.dead$date = as.Date(newd.dead$date)
  
  ggplot(newd.dead)+
    geom_bar(aes(x=date, y = New_deaths), stat = "identity")+xlab("Date") +
    ylab("Mortalities")+ggtitle(paste0("Monthly Mortalities for ", data_for_country$Country))+
    theme(plot.title = element_text(hjust = 0.5))+theme_bw()+
    scale_x_date(date_labels = "%y %b",date_breaks = "1 month")+
    #scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
    theme(axis.text.x = element_text(angle = 90))
}

gIQ = monthly_num_dead("IQ")
gES = monthly_num_dead("ES")

grid.arrange(gCIQ, gCES, gIQ, gES)
#####################################################

cases_mortality_plot = function(place_abbr){
  
  data_for_country = subset(covid, Country_code == place_abbr)
  
  #split into month and year
  data_for_country$mo <- strftime(data_for_country$Date_reported, "%m")
  data_for_country$yr <- strftime(data_for_country$Date_reported, "%Y")
  
  #aggregate by month
  new.case = aggregate(New_cases~mo +yr, data = data_for_country, sum)
  new.case$date = as.yearmon(paste(new.case$yr, new.case$mo), "%Y %m")
  new.case$date = as.Date(new.case$date)
  
  g1 = ggplot(new.case)+
    geom_bar(aes(x=date, y = New_cases), stat = "identity")+xlab("Date") +
    ylab("Number of New Cases")+ggtitle(paste0("Monthly Number of New Cases for ", data_for_country$Country))+
    theme(plot.title = element_text(hjust = 0.5))+theme_bw()+
    scale_x_date(date_labels = "%y %b",date_breaks = "1 month")+
    #scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
    theme(axis.text.x = element_text(angle = 90))
  
  #aggregate by month
  newd.dead = aggregate(New_deaths~mo +yr, data = data_for_country, sum)
  newd.dead$date = as.yearmon(paste(newd.dead$yr, newd.dead$mo), "%Y %m")
  newd.dead$date = as.Date(newd.dead$date)
  
  g2 = ggplot(newd.dead)+
    geom_bar(aes(x=date, y = New_deaths), stat = "identity")+xlab("Date") +
    ylab("Mortalities")+ggtitle(paste0("Monthly Mortalities for ", data_for_country$Country))+
    theme(plot.title = element_text(hjust = 0.5))+theme_bw()+
    scale_x_date(date_labels = "%y %b",date_breaks = "1 month")+
    #scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
    theme(axis.text.x = element_text(angle = 90))
  
  a = grid.arrange(g1,g2, nrow = 2)
  return(a)
}


gE = cases_mortality_plot("ES")
GS = cases_mortality_plot("IQ")



########################################################

ourData = read.csv('Data/final.csv')
View(ourData)

ourData$caseFatality = ourData$totalcountdeaths/ourData$totalcountconfirmed *10000
#move case column 
ourData = ourData[,c(1:4,12,5:11)]

overall.cf = sum(ourData$totalcountdeaths, na.rm= TRUE) / sum(ourData$totalcountconfirmed,na.rm= TRUE) *10000
overall.cf

data = subset(ourData, county == "Alameda")
c = sum(data$totalcountdeaths, na.rm= TRUE) 

get.percent.dead = function(location, dataset){
  specific.data = subset(dataset, county == location)
  num_dead = sum(specific.data$totalcountdeaths, na.rm= TRUE)
  overall_dead = sum(dataset$totalcountdeaths, na.rm = TRUE)
  percent = (num_dead/overall_dead) * 100
  return(percent)
}
get.percent.dead("Alameda", ourData)

a = aggregate(totalcountdeaths~county, ourData, sum)
head(a)

ggplot(a)+
  geom_bar(aes(x=county,y= totalcountdeaths ))

plot_by_calif_county = function(place){

  county.specific.data = subset(ourData, county == place)
  #split into month and year
  county.specific.data$mo = strftime(county.specific.data$date, "%m")
  county.specific.data$yr <- strftime(county.specific.data$date, "%Y")
  #aggregate by month
  cases  = aggregate(totalcountconfirmed~mo +yr, county.specific.data, sum)
  dead = aggregate(totalcountdeaths~mo +yr, county.specific.data, sum)
  newd = cbind(cases, dead[,3])
  names(newd) = c("month", "yr", "cases", "dead")
  
  newd$date = as.yearmon(paste(newd$yr, newd$mo), "%Y %m")
  newd$date = as.Date(newd$date)
  newd$case_fatality_per_tenk = (newd$dead/newd$cases) *10000
  
  
  ggplot(newd)+
    geom_bar(aes(x=date, y = case_fatality_per_tenk), stat = "identity")+xlab("Date") +
    ylab("Case Fatality Rate per 10k")+ggtitle(paste0("Monthly Case Fatality Rate for ", county.specific.data$county))+
    theme(plot.title = element_text(hjust = 0.5))+theme_bw()+
    #scale_x_date(date_labels = "%y %b",date_breaks = "1 month")+
    #scale_x_date(date_breaks  ="1 month")+
    scale_x_date(date_labels = "%Y %b",date_breaks = "1 month")+
    theme(axis.text.x = element_text(angle = 90))
  
}

g1 = plot_by_calif_county("San Francisco")
g2 = plot_by_calif_county("Inyo")
plot_by_calif_county("Alpine")
grid.arrange(g1,g2,nrow = 2, ncol = 2)
######################################################################################

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

