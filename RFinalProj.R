library(dplyr)

# Initializing data -------------------------------------------------------

  #we only plan to look at data from before Dec 11, 2020, when the vaccine was released
  case_data <- read.csv("C:\\Users\\Jacob\\Desktop\\us-counties.csv", header = TRUE)
  maryland_case_data <- subset(case_data, state == "Maryland")
  maryland_case_data <- subset(maryland_case_data, date < "2020-12-11")
  
  county_census_data <- read.csv("C:\\Users\\Jacob\\Documents\\Maryland county populations.csv", header = TRUE)
  county_sizes <- read.csv("C:\\Users\\Jacob\\Documents\\Maryland county sizes.csv", header = TRUE)
  
  counties <- unique(county_census_data$County)

# Building graph ----------------------------------------------------------
avgs <- c()
densities <- c()

for(cnty in counties){
  cnty_data <- filter(maryland_case_data, maryland_case_data$county == cnty)
  
  #get population/landmass of county to make density
  cnty_pop <- subset(county_census_data, County == cnty)
  cnty_pop <- as.numeric(gsub(",", "", cnty_pop$X2020.census))

  cnty_size <- subset(county_sizes, COUNTY == cnty) 
  
  #gsub strips commas from the 2020 census data to make the numbers usable
  density <- cnty_pop/as.numeric(cnty_size$Land.Area.)
  
  avgs <- append(avgs, mean(cnty_data$cases)/cnty_pop)
  densities <- append(densities, density)
}

# Each county has a number corresponding to its place (left to right) on the following graphs:
# 1: Allegany
# 2: Anne Arundel
# 3: Baltimore
# 4: Calvert
# 5: Caroline
# 6: Carroll
# 7: Cecil
# 8: Charles
# 9: Dorchester
# 10: Frederick
# 11: Garrett
# 12: Harford
# 13: Howard
# 14: Kent
# 15: Montgomery
# 16: Prince George's
# 17: Queen Anne's
# 18: St. Mary's
# 19: Somerset
# 20: Talbot
# 21: Washington
# 22: Wicomico
# 23: Worcester

barplot(avgs, xlab = "County", ylab = "Average proportion of cases")
barplot(densities, xlab = "County", ylab = "Population density")

#Correlation coefficient of the proportion of infected people in each county
print(cor(avgs, densities))
