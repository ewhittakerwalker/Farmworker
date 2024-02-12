library(tidyverse)
library(dplyr)
library(sf)
library(stringr)
library(viridis)
library(readxl)
library(tidyr)
library(leaflet)


## read in all the data and pivot to wide format based on specific columns for each dataset
CHVI_df <- read.csv("/Users/ewanwhittaker-walker/Rshiny/selectedCHVIdata.csv")
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


CES_df <- read_excel("/Users/ewanwhittaker-walker/Rshiny/calenviroscreen40resultsdatadictionary_F_2021.xlsx")
# print(head(CES_df))
colnames(CES_df) <- gsub(" ", "_", colnames(CES_df)) 
CES_df <- CES_df %>% mutate_at(5:58, as.numeric)
# print(colnames(CES_df))
CES_df$GEOID <- paste0("0" , as.character(CES_df$Census_Tract))


ROI_df <- read_excel("/Users/ewanwhittaker-walker/Rshiny/ROI_data.xlsx")
colnames(ROI_df) <- gsub(" ", "_", colnames(ROI_df)) 
ROI_df <- ROI_df %>% mutate_at(6:143, as.numeric)
# print(head(ROI_df))
# print(colnames(ROI_df))
ROI_df$GEOID <- paste0("0" , as.character(ROI_df$Census_Tract))

                       

HPI_df <- read_excel("/Users/ewanwhittaker-walker/Rshiny/hpi_3_complete_file.xlsx")
colnames(HPI_df) <- gsub(" ", "_", colnames(HPI_df)) 
HPI_df <- HPI_df %>% mutate_at(7:84, as.numeric)
# print(head(HPI_df))
# print(colnames(HPI_df))
HPI_df$GEOID <- paste0("0" , as.character(HPI_df$GEO_ID))


map_df <- st_read("/Users/ewanwhittaker-walker/Rshiny/tl_2019_06_tract/tl_2019_06_tract.shp", quiet = TRUE)
head(map)


map_df$FIPS<- sapply(map_df$GEOID, function(id) str_sub(id,2,5))
#print(map_df$FIPS)

df_merge <- merge(map_df,piv_CHVI,by="FIPS")
df_merge <- merge(df_merge,CES_df,by="GEOID")
df_merge <- merge(df_merge,HPI_df,by="GEOID")
df_merge <- merge(df_merge, ROI_df,by="GEOID")


save(df_merge, file = "/Users/ewanwhittaker-walker/Rshiny/merged_map.rda")
print("saved")
