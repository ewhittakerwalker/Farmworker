library(tidyverse)
library(dplyr)
library(sf)
library(stringr)
library(viridis)
library(readxl)
library(tidyr)
library(leaflet)
library(deeplr)
library(polyglotr)
#library(cld2)

dire <- getwd()
dire <- paste0(dire, "/Desktop/Farmworker")

## read in all the data and pivot to wide format based on specific columns for each dataset
CHVI_df <- read.csv(paste0(dire, "/data/selectedCHVIdata.csv"))
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


CES_df <- read_excel(paste0(dire, "/data/calenviroscreen40resultsdatadictionary_F_2021.xlsx"))
# print(head(CES_df))
colnames(CES_df) <- gsub(" ", "_", colnames(CES_df)) 
CES_df <- CES_df %>% mutate_at(5:58, as.numeric)
# print(colnames(CES_df))
CES_df$GEOID <- paste0("0" , as.character(CES_df$Census_Tract))


ROI_df <- read_excel(paste0(dire, "/data//ROI_data.xlsx"))
colnames(ROI_df) <- gsub(" ", "_", colnames(ROI_df)) 
ROI_df <- ROI_df %>% mutate_at(6:143, as.numeric)
# print(head(ROI_df))
# print(colnames(ROI_df))
ROI_df$GEOID <- paste0("0" , as.character(ROI_df$Census_Tract))

                       

HPI_df <- read_excel(paste0(dire, "/data/hpi_3_complete_file.xlsx"))
colnames(HPI_df) <- gsub(" ", "_", colnames(HPI_df)) 
HPI_df <- HPI_df %>% mutate_at(7:84, as.numeric)
# print(head(HPI_df))
# print(colnames(HPI_df))
HPI_df$GEOID <- paste0("0" , as.character(HPI_df$GEO_ID))


map_df <- st_read(paste0(dire, "/data/tl_2019_06_tract/tl_2019_06_tract.shp"), quiet = TRUE)
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


## spanish translation
# print("doing translation")
# ## authentication key ff1a8324-b541-4e57-8e29-c40778bf7c8e:fx
# 
# df_merge_spanish <- df_merge
# colnames(df_merge_spanish) <- lapply(colnames(df_merge_spanish), function(x) {google_translate(x, target_language = 'es')})
# names(df_merge_spanish)[names(df_merge_spanish) == "geometría"] <- "geometry"
# names(df_merge_spanish)[names(df_merge_spanish) == "GEOIDE"] <- "GEOID"
# print(df_merge)
# df_merge_spanish <- df_merge_spanish %>% st_set_geometry(NULL)

# ## addid in spansih versions of names
# df_merge <- merge(df_merge, df_merge_spanish, by="GEOID")
# ## removing duplicate names 
# #df_merge <- df_merge %>% names() %>% stringr::str_remove(pattern = "\\.x")
# df_merge <- df_merge[grep(".y", colnames(df_merge), invert = TRUE)]
# 
# 
# print(colnames(df_merge))

## saving R objects
save(df_merge, file = paste0(dire, "/data/merged_map.rda"))
print("saved")

indicator_choices <- colnames(df_merge)
print(indicator_choices)
indicator_choices <- indicator_choices[
  grepl("_heat_|Water|Air|Lead|Ozone|Pesticides|Environment|tree|Housing", 
        indicator_choices)]

df_merge_columns_table <- df_merge
df_merge_columns_table <- df_merge_columns_table[,indicator_choices]
cols <- colnames(df_merge_columns_table)

cols_spanish <- lapply(cols, function(x) {google_translate(x, target_language = 'es')})
cols_spanish <- unlist(cols_spanish)

col_to_save <- data.frame(indicator = cols, 
                          spanish_translation = cols_spanish)

write.csv(col_to_save, paste0(dire, "/data/df_merge_columns.csv"))

cols_df_merge <- colnames(df_merge)

col_to_save_df_merge <- data.frame(indicator = cols_df_merge)

write.csv(col_to_save_df_merge, paste0(dire, "/data/df_merge_all_columns.csv"))

# save(df_merge_spanish, file = paste0(dire, "/data/merged_map_spanish.rda"))

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


## translation for pop-ups
# df_merge_pop_spanish <- df_merge_pop
# colnames(df_merge_pop_spanish) <- lapply(colnames(df_merge_pop_spanish), function(x) {google_translate(x, target_language = 'es')})
# names(df_merge_pop_spanish)[names(df_merge_pop_spanish) == "GEOIDE"] <- "GEOID"

print(df_merge)

## saving 
save(df_merge_pop, file = paste0(dire, "/data/merged_map_pop.rda"))
print("saved pop")

# save(df_merge_pop_spanish, file = paste0(dire, "/data/merged_map_pop_spanish.rda"))
# print("saved pop")

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

save(df_merge_long , file = paste0(dire, "/data/merged_map_long.rda"))
print("saved long")

# ## translation
# df_merge_long_spanish <- df_merge_long
# df_merge_long_spanish["indication"] <- lapply(df_merge_long_spanish["indication"], 
#                                               function(x) {google_translate(x, target_language = 'es')})
# 
# print(df_merge_long_spanish["indication"])
# save(df_merge_long_spanish, file = paste0(dire, "/data/merged_map_long_spanish.rda")) 
# 

