library(tidyverse)
library(dplyr)
library(sf)
library(stringr)
library(viridis)
library(readxl)
library(tidyr)
library(leaflet)


## read in all the data and pivot to wide format based on specific columns for each dataset
CHVI_df <- read.csv("/Users/ewanwhittaker-walker/github_farm/data/selectedCHVIdata.csv")
# print(head(CHVI_df))
# print(colnames(CHVI_df))


CHVI_df$pivot_col = paste0(CHVI_df$Definition, "_", CHVI_df$Race, "_", CHVI_df$Year)
CHVI_df$pivot_col <- gsub(" ", "_", CHVI_df$pivot_col)
CHVI_df$pivot_col <- gsub("-", "_", CHVI_df$pivot_col)
CHVI_df_for_piv <- CHVI_df[,c("County","FIPS", "Region", "Mean", "pivot_col")]
print(CHVI_df_for_piv)

piv_CHVI <- CHVI_df_for_piv  %>% pivot_wider(names_from = pivot_col, values_from = Mean)
piv_CHVI <- piv_CHVI %>% mutate_at(3:206, as.character)
piv_CHVI <- piv_CHVI %>% mutate_at(3:206, unlist)
piv_CHVI <- piv_CHVI %>% mutate_at(3:206, as.numeric)


CES_df <- read_excel("/Users/ewanwhittaker-walker/github_farm/data/calenviroscreen40resultsdatadictionary_F_2021.xlsx")
# print(head(CES_df))
colnames(CES_df) <- gsub(" ", "_", colnames(CES_df)) 
CES_df <- CES_df %>% mutate_at(5:58, as.numeric)
# print(colnames(CES_df))
CES_df$GEOID <- paste0("0" , as.character(CES_df$Census_Tract))


ROI_df <- read_excel("/Users/ewanwhittaker-walker/github_farm/data//ROI_data.xlsx")
colnames(ROI_df) <- gsub(" ", "_", colnames(ROI_df)) 
ROI_df <- ROI_df %>% mutate_at(6:143, as.numeric)
# print(head(ROI_df))
# print(colnames(ROI_df))
ROI_df$GEOID <- paste0("0" , as.character(ROI_df$Census_Tract))

                       

HPI_df <- read_excel("/Users/ewanwhittaker-walker/github_farm/data/hpi_3_complete_file.xlsx")
colnames(HPI_df) <- gsub(" ", "_", colnames(HPI_df)) 
HPI_df <- HPI_df %>% mutate_at(7:84, as.numeric)
# print(head(HPI_df))
# print(colnames(HPI_df))
HPI_df$GEOID <- paste0("0" , as.character(HPI_df$GEO_ID))


map_df <- st_read("/Users/ewanwhittaker-walker/github_farm/data/tl_2019_06_tract/tl_2019_06_tract.shp", quiet = TRUE)
head(map)


map_df$FIPS<- sapply(map_df$GEOID, function(id) str_sub(id,2,5))
#print(map_df$FIPS)

df_merge <- merge(map_df,piv_CHVI,by="FIPS")
df_merge <- merge(df_merge,CES_df,by="GEOID")
df_merge <- merge(df_merge,HPI_df,by="GEOID")
df_merge <- merge(df_merge, ROI_df,by="GEOID")

## clean up column names
colnames(df_merge) <- str_replace(colnames(df_merge), "Maximum_Ozone_Concentration", "Max_Ozone_Conc")
print(colnames(df_merge))
colnames(df_merge) <- str_replace(colnames(df_merge), "Projected_number_of_", "")
colnames(df_merge) <- str_replace(colnames(df_merge), "_Total_2040_2060", "")
colnames(df_merge) <- str_replace(colnames(df_merge), "_Total_2080_2099", "")
colnames(df_merge) <- str_replace(colnames(df_merge), "Average", "Avg")
colnames(df_merge) <- str_replace(colnames(df_merge), "Health/Environment", "Health/Environ")


save(df_merge, file = "/Users/ewanwhittaker-walker/github_farm/data/merged_map.rda")
print("saved")


## dropping geometry column for querying pop-up data
st_geometry(map_df) <- NULL
map_df_dropped <- map_df
print(colnames(map_df_dropped))
print(head(map_df_dropped))

df_merge_pop <- merge(map_df_dropped,piv_CHVI,by="FIPS")
df_merge_pop <- merge(df_merge_pop,CES_df,by="GEOID")
df_merge_pop <- merge(df_merge_pop,HPI_df,by="GEOID")
df_merge_pop <- merge(df_merge_pop, ROI_df,by="GEOID")

add_county_df <- df_merge_pop[,c("GEOID", "County")]

## clean up column names
colnames(df_merge_pop) <- str_replace(colnames(df_merge_pop), "Maximum_Ozone_Concentration", "Max_Ozone_Conc")
colnames(df_merge_pop) <- str_replace(colnames(df_merge_pop), "Projected_number_of_", "")
colnames(df_merge_pop) <- str_replace(colnames(df_merge_pop), "_Total_2040_2060", "")
colnames(df_merge_pop) <- str_replace(colnames(df_merge_pop), "_Total_2080_2099", "")
colnames(df_merge_pop) <- str_replace(colnames(df_merge_pop), "Average", "Avg")
colnames(df_merge_pop) <- str_replace(colnames(df_merge_pop), "Health/Environment", "Health/Environ")

save(df_merge_pop, file = "/Users/ewanwhittaker-walker/github_farm/data/merged_map_pop.rda")
print("saved pop")

## pivoting data down for data tab
print(colnames(df_merge_pop))

df_merge_pop <- df_merge_pop[,c(1, (16:503))]
df_merge_pop <- df_merge_pop %>% mutate_at(2:489, as.numeric)

df_merge_long <- df_merge_pop %>%
  pivot_longer(!GEOID, names_to = "indication", values_to = "Value")

## add bac in county data 
df_merge_long <- merge(df_merge_long, add_county_df, by.x = "GEOID", by.y = "GEOID")
df_merge_long <- df_merge_long[,c("GEOID", "indication", "Value", "County.x")]
names(df_merge_long)[names(df_merge_long) == 'County.x'] <- 'County'

save(df_merge_long , file = "/Users/ewanwhittaker-walker/github_farm/data/merged_map_long.rda")
print("saved long")




