#load datasets

library(tidyverse)
library(tigris)

data(fips_codes)

#gives us the lat and long
counties <- read.table('/Users/maryfrancis/Downloads/2020_Gaz_counties_national.txt', sep = "\t", header = TRUE)
counties_ca <- counties[which(counties$USPS == "CA"),]
#write.csv(counties_ca, '/Users/maryfrancis/Documents/GitHub/Project-3/Data/counties_ca.csv')
#data cleaning manual (add spaces in county names)
counties_ca <- read.csv('/Users/maryfrancis/Documents/GitHub/Project-3/Data/counties_ca.csv', header = TRUE)



#gives us the GEOID (to access lat and long)
counties_geoid <- counties("California")
counties_geoid_name <- as.data.frame(counties_geoid)

covid <- read.table("/Users/maryfrancis/Documents/GitHub/Project-3/Data/statewide_cases.csv", header = TRUE, sep = ",")
density <- read.table("/Users/maryfrancis/Documents/GitHub/Project-3/Data/Average_Household_Size_and_Population_Density_-_County.csv", header = TRUE, sep = ",")
demographics <-read.csv("/Users/maryfrancis/Documents/GitHub/Project-3/Data/demographic information.csv",header = T)
ages <- read.csv("/Users/maryfrancis/Documents/GitHub/Project-3/Data/age_urban_counties.csv")


#select relevant cols in ages
ages=ages[ages$State=="CA",c("State","Area_Name","POP_ESTIMATE_2018","Rural.urban_Continuum.Code_2013","Urban_Influence_Code_2013","Total_age65plus","Density.per.square.mile.of.land.area...Population","Density.per.square.mile.of.land.area...Housing.units","ICU.Beds")]

colnames(ages)=c("State","NAME","Pop_Estimate","Rural_urban_Continuum_Code_2013","Urban_Influence_Code_2013","Total_age65plus","PopDensity_per_square_mile_of_land_area","HouseDensity.per.square.mile.of.land.area","ICU_Beds")




#select relevant cols in demographics
demographics=(demographics[,c("County.FIPS.Code","Postal.Code","Name","Poverty.Estimate..All.Ages","Poverty.Percent..All.Ages","Median.Household.Income")])
demographics=demographics[demographics$Postal.Code=="CA",]

#merge sets by counties in CA
colnames(density)[6] <- "NAME"
colnames(demographics)[3]<-"NAME"
colnames(covid)[1]<-"NAME"
merged <- merge(covid,density[density$State == "California",],by="NAME")
merged <- merge(merged, demographics,by='NAME')

#Select predictors and response for models
merged=merged[,c("NAME", "COUNTYNS" ,"totalcountconfirmed" ,"totalcountdeaths","newcountconfirmed"  ,"newcountdeaths","date","B01001_calc_PopDensity",    "Poverty.Estimate..All.Ages","Poverty.Percent..All.Ages","Median.Household.Income")]
#adjust names
colnames(merged)=c("NAME", "fips" ,"totalcountconfirmed" ,"totalcountdeaths","newcountconfirmed"  ,"newcountdeaths","date","PopDensity",    "Poverty_Estimate","Poverty_Percent","Median_Household_Income")

#merge ages with merged
merged <- merge(merged, ages,by='NAME')

#merge GEOID with merge
merged <- merge(merged, counties_geoid_name,by='NAME')
merged=merged[,c("NAME", "fips" ,"totalcountconfirmed" ,"totalcountdeaths","newcountconfirmed"  ,"newcountdeaths","date","PopDensity",    "Poverty_Estimate","Poverty_Percent","Median_Household_Income","GEOID")]

#figure out lat and long with GEOID

merged <- merge(merged, counties_ca,by='NAME')
merged=merged[,c("NAME", "fips" ,"totalcountconfirmed" ,"totalcountdeaths","newcountconfirmed"  ,"newcountdeaths","date","PopDensity",    "Poverty_Estimate","Poverty_Percent","Median_Household_Income","INTPTLAT","INTPTLONG")]

              #process values which should NOT be factors
#sapply(merged, class)

#numeric -> factor
merged$Rural_urban_Continuum_Code_2013=as.factor(merged$Rural_urban_Continuum_Code_2013)

merged$Urban_Influence_Code_2013=as.factor(merged$Urban_Influence_Code_2013)

#factor->numeric
merged$Poverty_Estimate=as.numeric(as.character(
  gsub(",","",merged$Poverty_Estimate)))



merged$Poverty_Percent=as.numeric(as.character(merged$Poverty_Percent))

merged$Median_Household_Income=as.numeric(as.character(gsub(",","",merged$Median_Household_Income)))
merged$elder_ratio=merged$Total_age65plus/merged$Pop_Estimate
merged$new_casesrate=merged$newcountconfirmed/merged$Pop_Estimate*10000 #per10k

#code in our popdensity above or below median, below or equal  med = 0
medp=median(merged$PopDensity_per_square_mile_of_land_area)
merged$PopCode <- factor(ifelse(merged$PopDensity_per_square_mile_of_land_area <= medp, "0", "1"))


#export working dataset
final=merged
write.csv(final,'/Users/maryfrancis/Documents/GitHub/Project-3/Data/final_mapping.csv')


