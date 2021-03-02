#load datasets
covid <- read.table("Data/statewide_cases.csv", header = TRUE, sep = ",")
density <- read.table("Data/Average_Household_Size_and_Population_Density_-_County.csv", header = TRUE, sep = ",")
demographics <-read.csv("Data/demographic information.csv",header = T)

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

final=merged
#export working dataset
write.csv(final,'Data/final.csv')