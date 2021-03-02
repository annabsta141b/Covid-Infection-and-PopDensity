#case fatality = #dead from covid /#covid cases
library(dplyr)
data = read.csv('Data/final.csv')
View(data)

data$caseFatality = data$totalcountdeaths/data$totalcountconfirmed *10000
#move case column
data = data[,c(1:4,12,5:11)]


library(ggplot2)
ggplot(data %>% filter(county == c("Riverside","San Francisco", "Modoc" )))+
  geom_histogram(aes(x =caseFatality, fill = county), position = 'dodge')
 

caseFatalRate = function(x){
  x$totalcountdeaths/x$totalcountconfirmed *10000
}

caseFatalRate(data)

overall_caseFatality = sapply(data, caseFatalRate)

data$county =="Alameda"

caseFatality_per_specific_county = function(x){
sum(data$totalcountdeaths[data$county==x])/sum(data$totalcountconfirmed[data$county == x])
}
caseFatality_per_specific_county("Alameda")

total_dead_per_county = aggregate(totalcountdeaths~county, data = data, sum) 
cases_per_county = aggregate(totalcountconfirmed~county, data = data, sum) 
case_fatality_per_county = cbind(total_dead_per_county,cases_per_county)
case_fatality_per_county[,3] = NULL

View(case_fatality_per_county)

case_fatality_per_county$cf_rate = caseFatalRate(case_fatality_per_county)

ggplot(case_fatality_per_county)+
  geom_bar(aes(x=county, y = cf_rate), stat = "identity")

