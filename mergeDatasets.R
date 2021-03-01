covid <- read.table("Data/statewide_cases.csv", header = TRUE, sep = ",")
density <- read.table("Data/Average_Household_Size_and_Population_Density_-_County.csv", header = TRUE, sep = ",")
colnames(density)[6] <- "county"
merged <- merge(covid,density[density$State == "California",],by="county")
summary(merged)
