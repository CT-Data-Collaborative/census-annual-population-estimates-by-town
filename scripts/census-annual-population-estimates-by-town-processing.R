library(plyr)
library(dplyr)
library(datapkg)
library(tidyr)

##################################################################
#
# Processing Script for Census Annual Population Estimates by Town
# Created by Jenna Daly
# On 05/25/2018
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", raw_location))
data_location <- grep("data$", sub_folders, value=T)
path_to_data <- (paste0(getwd(), "/", data_location))
pop_df <- dir(path_to_raw_data, recursive=T, pattern = ".csv")

pop_est <- read.csv(paste0(path_to_raw_data, "/", pop_df), stringsAsFactors = FALSE, header=T, check.names = F) 
pop_est_CT <- pop_est %>% filter(STNAME == "Connecticut") %>% select(-c(1:8, 10:12)) 

#Remove rows we don't need
pop_est_CT_towns <- pop_est_CT %>% 
  filter(!grepl("County|Balance|balance| borough", NAME) )

#Clean up town names (remove duplicates)
pop_est_CT_towns$NAME <- gsub(" town", "", pop_est_CT_towns$NAME)
pop_est_CT_towns$NAME <- gsub(" city", "", pop_est_CT_towns$NAME)
pop_est_CT_towns <- unique(pop_est_CT_towns)

#Roll up towns (combine Groton town and Groton City)
pop_est_CT_towns <- ddply(pop_est_CT_towns, "NAME", function(x) colSums(x[c("POPESTIMATE2010",
                                                                            "POPESTIMATE2011", 
                                                                            "POPESTIMATE2012",
                                                                            "POPESTIMATE2013",
                                                                            "POPESTIMATE2014",
                                                                            "POPESTIMATE2015",
                                                                            "POPESTIMATE2016",
                                                                            "POPESTIMATE2017")]))

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

pop_est_CT_towns <- merge(pop_est_CT_towns, fips, by.x = "NAME", by.y = "Town", all.y = T)

#Convert wide to long
pop_est_CT_towns_long <- gather(pop_est_CT_towns, Year, Value, 2:9)

#Clean up year column
pop_est_CT_towns_long$Year <- gsub("POPESTIMATE", "", pop_est_CT_towns_long$Year)

#Assign MT and Variable columns
pop_est_CT_towns_long$`Measure Type` <- "Number"
pop_est_CT_towns_long$Variable <- "Estimated Population"

#Rename and sort columns
pop_est_CT_towns_long <- pop_est_CT_towns_long %>% 
  select(NAME, FIPS, Year, `Measure Type`, Variable, Value) %>% 
  arrange(NAME) %>% 
  dplyr::rename(Town = NAME)

# Write to File
write.table(
  pop_est_CT_towns_long,
  file.path(path_to_data, "census-population-by-town.csv"),
  sep = ",",
  row.names = F
)
