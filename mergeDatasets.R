covid <- read.table("statewide_cases.csv", header = TRUE, sep = ",")
density <- read.table("Average_Household_Size_and_Population_Density_-_County.csv", header = TRUE, sep = ",")
colnames(density)[6] <- "county"
merged <- merge(covid,density,by="county")
