#load datasets
covid <- read.table("Data/statewide_cases.csv", header = TRUE, sep = ",")
density <- read.table("Data/Average_Household_Size_and_Population_Density_-_County.csv", header = TRUE, sep = ",")
demographics <-read.csv("Data/demographic information.csv",header = T)
ages <- read.csv("Data/age_urban_counties.csv")


#select relevant cols in ages
ages=ages[ages$State=="CA",c("State","Area_Name","POP_ESTIMATE_2018","Rural.urban_Continuum.Code_2013","Urban_Influence_Code_2013","Total_age65plus","Density.per.square.mile.of.land.area...Population","Density.per.square.mile.of.land.area...Housing.units","ICU.Beds")]

colnames(ages)=c("State","county","Pop_Estimate","Rural_urban_Continuum_Code_2013","Urban_Influence_Code_2013","Total_age65plus","PopDensity_per_square_mile_of_land_area","HouseDensity.per.square.mile.of.land.area","ICU_Beds")




#select relevant cols in demographics
demographics=(demographics[,c("County.FIPS.Code","Postal.Code","Name","Poverty.Estimate..All.Ages","Poverty.Percent..All.Ages","Median.Household.Income")])
demographics=demographics[demographics$Postal.Code=="CA",]

#merge sets by counties in CA
colnames(density)[6] <- "county"
colnames(demographics)[3]<-"county"
merged <- merge(covid,density[density$State == "California",],by="county")
merged <- merge(merged, demographics,by='county')

#Select predictors and response for models
merged=merged[,c("county" ,"totalcountconfirmed" ,"totalcountdeaths","newcountconfirmed"  ,"newcountdeaths","date","B01001_calc_PopDensity",    "Poverty.Estimate..All.Ages","Poverty.Percent..All.Ages","Median.Household.Income")]
#adjust names
colnames(merged)=c("county" ,"totalcountconfirmed" ,"totalcountdeaths","newcountconfirmed"  ,"newcountdeaths","date","PopDensity",    "Poverty_Estimate","Poverty_Percent","Median_Household_Income")

#merge ages with merged
merged <- merge(merged, ages,by='county')


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


#export working dataset
final=merged
write.csv(final,'Data/final.csv')
View(final)

