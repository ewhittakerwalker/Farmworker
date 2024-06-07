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
library(ggmap)
library(RODBC)
library(tigris)

dire <- getwd()
dire <- paste0(dire, "/Desktop/Farmworker")


##crowdsourcing data 
## register_google(key = "AIzaSyDt01LDXBHuoBn9xp9-jygsFZlSy91cH-4")
# gsheet_df <- read_csv(paste0(dire, "/data/Farmworker Form Sheet.csv"))
# print(gsheet_df)
# print(colnames(gsheet_df))
# 
# names(gsheet_df)[names(gsheet_df) == "Address - Address to Report"] <- "Address"
# names(gsheet_df)[names(gsheet_df) == "City - Address to Report"] <- "City"
# names(gsheet_df)[names(gsheet_df) == "State - Address to Report"] <- "State"
# names(gsheet_df)[names(gsheet_df) == "Zip Code - Address to Report"] <- "Zip"
# 
# ggmap::register_google(key = "AIzaSyDt01LDXBHuoBn9xp9-jygsFZlSy91cH-4")
# 
# gsheet_df$full_address <- paste0(gsheet_df$Address, ", ",
#                                 gsheet_df$City,", ",
#                                 gsheet_df$State,", ",
#                                 gsheet_df$Zip
#                                 )
# print(gsheet_df$full_address)
# latlong <- geocode(gsheet_df$full_address)
# print(latlong)
# gsheet_df$lat <- latlong$lat 
# gsheet_df$long <- latlong$lon
# 
# save(gsheet_df, file = paste0(dire, "/data/crowdsourced_data.rda"))


## Waterboard ILRP data 
df_ilrp <- read.delim(paste0(dire, "/data/gama_data/gama_wb_ilrp_statewide_v2.txt"), header = TRUE, sep = "\t", dec = ".")
print(head(df_ilrp))
df_ilrp <- df_ilrp[, c("GM_CHEMICAL_NAME", "GM_RESULT", "GM_LATITUDE", "GM_LONGITUDE", "SRC_LATITUDE" ,"SRC_LONGITUDE","SRC_WELL_CATEGORY", "GM_SAMP_COLLECTION_DATE")] 
# write.csv(df_ilrp, paste0(dire, "/data/ILRP_filtered.csv"))
# err
df_ilrp$year <- lapply(df_ilrp$GM_SAMP_COLLECTION_DATE, function(x) substr(x, 7, 10))
print(df_ilrp$year)
print(all(df_ilrp$GM_LATITUDE == df_ilrp$SRC_LATITUDE))
print(all(df_ilrp$GM_LONGITUDE == df_ilrp$SRC_LONGITUDE))
df_ilrp <- df_ilrp[df_ilrp$year > '2018',]
df_ilrp <- df_ilrp[df_ilrp$SRC_WELL_CATEGORY == 'Agriculture/irrigation well',]
print(df_ilrp$year)
print(unique(df_ilrp$GM_CHEMICAL_NAME))
df_unique_chemical_names <- unique(df_ilrp$GM_CHEMICAL_NAME)
#df_ilrp <- unlist(df_ilrp)
write.csv(df_unique_chemical_names, paste0(dire, "/data/Unique_ILRP_Chemicals.csv"))
# write.csv(df_ilrp, paste0(dire, "/data/ILRP_filtered.csv"))

polygons <- st_read(paste0(dire, "/data/tl_2019_06_tract/tl_2019_06_tract.shp"))
points_sf <- st_as_sf(df_ilrp, coords = c("SRC_LONGITUDE","SRC_LATITUDE"))
st_crs(points_sf) <- st_crs(polygons)
points_sf_joined <- st_join(points_sf, polygons, join = st_within)


points_sf_joined <- points_sf_joined[, c("GM_CHEMICAL_NAME", "GM_RESULT",
                                         "GEOID")]

st_geometry(points_sf_joined) <- NULL


ilrp_final_df <- data.frame("GM_CHEMICAL_NAME" = c(), 
                            "GM_RESULT" = c(), 
                            "GEOID" = c())

## take the mean of all measurements and geoids with more than one measurement for census tract and append them 

for (measurement in unique(points_sf_joined$GM_CHEMICAL_NAME)) {
  fil_measure <- points_sf_joined[points_sf_joined$GM_CHEMICAL_NAME == measurement,]
  print(measurement) 
  print(fil_measure)
  for (geoid in unique(fil_measure$GEOID)) {
    #print(colnames(fil_measure))'
    print(geoid)
    fil_measure_geo <- fil_measure[fil_measure$GEOID == geoid,]
    print(fil_measure_geo)
    if (nrow(fil_measure_geo) > 1) {
      #print(fil_measure_geo$GM_RESULT)
      df_to_add <- data.frame("GM_CHEMICAL_NAME" = c(measurement), 
                              "GM_RESULT" = c(mean(fil_measure_geo$GM_RESULT)), 
                              "GEOID" = c(geoid))
      ilrp_final_df <- rbind(ilrp_final_df, df_to_add)
    } else {
      ilrp_final_df <- rbind(ilrp_final_df, fil_measure_geo)
    }
  }
}  

ilrp_final_df <- ilrp_final_df %>%
  pivot_wider(
    names_from = GM_CHEMICAL_NAME,
    values_from = GM_RESULT
  )


### shapefile
#CA <- sf::read_sf(paste0(dire, "/data/tl_2019_06_tract/tl_2019_06_tract.shp"))
# #sf::st_set_crs(CA, 4326)
# df_ilrp_sf <- sf::st_as_sf(df_ilrp, coords = c("GM_LATITUDE", "GM_LONGITUDE"), crs = 4326 )
# df_ilrp_sf$tract <- as.numeric(st_within(df_ilrp_sf, CA))

# ### your data
# print(df_ilrp_sf)
# #print(CA)
# err

#df_ilrp$census_code <- apply(df_ilrp, 1, function(row) call_geolocator_latlon(row["GM_LATITUDE"], row["GM_LONGITUDE"]))




## read in all the data and pivot to wide format based on specific columns for each dataset

## immigrant data portal CIDP 
cidp_services_df <- read_xlsx(paste0(dire, "/data/cidp_accessibility__servces_20220713_map.xlsx"))
cidp_services_df$FIPS <- lapply(cidp_services_df$geo_code_long, function(x) substr(x, 14, 18))
cidp_services_df_for_merge  <- cidp_services_df[,c("FIPS","imm_org_score")]

print(cidp_services_df)


cidp_hatecrimes_df <- read_xlsx(paste0(dire, "/data/cidp_hate_crimes_20220718_map.xlsx"))
cidp_hatecrimes_df$FIPS <- lapply(cidp_hatecrimes_df$geo_code_long, function(x) substr(x, 14, 18))
cidp_hatecrimes_df_for_merge  <- cidp_hatecrimes_df[,c("FIPS","total_incidents_per_100k")]


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
df_merge <- merge(df_merge, cidp_services_df_for_merge,by="FIPS", all = TRUE)
df_merge <- merge(df_merge, cidp_hatecrimes_df_for_merge,by="FIPS", all = TRUE)
df_merge <- merge(df_merge,CES_df,by="GEOID")
df_merge <- merge(df_merge,HPI_df,by="GEOID")
df_merge <- merge(df_merge, ROI_df,by="GEOID")
df_merge <- merge(df_merge, ilrp_final_df,by="GEOID", all = TRUE)


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

## adding percentile columns to df_merge 
df_indicator_choices <- read.csv(paste0(dire, "/data/Indicators_Farmworker_WebApplication.csv"))
print(df_indicator_choices)

save_geo <- df_merge$geometry
st_geometry(df_merge) <- NULL

for (column_name in df_indicator_choices$indicator) {
  #print(df_merge[column_name])
  if (!grepl("percentile", column_name)) {
    print(column_name)
    num_col <- sapply(df_merge[column_name], as.numeric)
    #print(unlist(df_merge_percentile_table[colname_i]))
    df_merge[paste0(column_name, "_percentile")] <- percent_rank(num_col)
  }
}

st_geometry(df_merge) <- save_geo

df_merge_for_app <- df_merge[, df_indicator_choices$indicator]

 ## saving R objects
save(df_merge, file = paste0(dire, "/data/merged_map.rda"))
print("saved")

indicator_choices <- colnames(df_merge)
print(indicator_choices)
indicator_choices <- indicator_choices[
  grepl("_heat_|Water|water|Air|air|Lead|lead|Ozone|ozone|Pesticides|pesticides|Environment|environment|tree|Tree|Housing|housing", 
        indicator_choices)]
print(indicator_choices)

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
df_merge_pop <- merge(df_merge_pop, cidp_services_df_for_merge,by="FIPS", all = TRUE)
df_merge_pop <- merge(df_merge_pop, cidp_hatecrimes_df_for_merge,by="FIPS", all = TRUE)
df_merge_pop <- merge(df_merge_pop,CES_df,by="GEOID")
df_merge_pop <- merge(df_merge_pop,HPI_df,by="GEOID")
df_merge_pop <- merge(df_merge_pop, ROI_df,by="GEOID")
df_merge_pop <- merge(df_merge_pop, ilrp_final_df,by="GEOID", all = TRUE)

add_county_df <- df_merge_pop[,c("GEOID", "County")]

## clean up column names
colnames(df_merge_pop) <- str_replace(colnames(df_merge_pop), "Maximum_Ozone_Concentration", "Max_Ozone_Conc")
colnames(df_merge_pop) <- str_replace(colnames(df_merge_pop), "Projected_number_of_", "")
colnames(df_merge_pop) <- str_replace(colnames(df_merge_pop), "_Total_2040_2060", "")
colnames(df_merge_pop) <- str_replace(colnames(df_merge_pop), "_Total_2080_2099", "")
colnames(df_merge_pop) <- str_replace(colnames(df_merge_pop), "Average", "Avg")
colnames(df_merge_pop) <- str_replace(colnames(df_merge_pop), "Health/Environment", "Health/Environ")

for (column_name in df_indicator_choices$indicator) {
  #print(df_merge_pop [column_name])
  if (!grepl("percentile", column_name)) {
    num_col <- sapply(df_merge_pop[column_name], as.numeric)
    #print(unlist(df_merge_percentile_table[colname_i]))
    df_merge_pop[paste0(column_name, "_percentile")] <- percent_rank(num_col)
  }
}

## translation for pop-ups
# df_merge_pop_spanish <- df_merge_pop
# colnames(df_merge_pop_spanish) <- lapply(colnames(df_merge_pop_spanish), function(x) {google_translate(x, target_language = 'es')})
# names(df_merge_pop_spanish)[names(df_merge_pop_spanish) == "GEOIDE"] <- "GEOID"

## saving 
save(df_merge_pop, file = paste0(dire, "/data/merged_map_pop.rda"))
print("saved pop")


# save(df_merge_pop_spanish, file = paste0(dire, "/data/merged_map_pop_spanish.rda"))
# print("saved pop")

## pivoting data down for data tab
print(colnames(df_merge_pop))

df_merge_pop <- df_merge_pop[,c(1, (16:561))]
df_merge_pop <- df_merge_pop %>% mutate_at(2:503, as.numeric)

df_merge_long <- df_merge_pop %>%
  pivot_longer(!GEOID, names_to = "indication", values_to = "Value")

## add bac in county data 
df_merge_long <- merge(df_merge_long, add_county_df, by.x = "GEOID", by.y = "GEOID")
print(colnames(df_merge_long))
df_merge_long <- df_merge_long[,c("GEOID", "indication", "Value", "County")]
#names(df_merge_long)[names(df_merge_long) == 'County.x'] <- 'County'

save(df_merge_long , file = paste0(dire, "/data/merged_map_long.rda"))
print("saved long")


## calculate percentile for all indicators of interest 
df_indicator_choices <- read.csv(paste0(dire, "/data/Indicators_Farmworker_WebApplication.csv"))

indicator_choices <- df_indicator_choices$indicator

df_merge_percentile_table <- df_merge_pop
df_merge_percentile_table <- df_merge_percentile_table[c("GEOID", indicator_choices)]


for (colname_i in colnames(df_merge_percentile_table)) {
  print(colname_i)
  if (colname_i != "GEOID") {
    num_col <- sapply(df_merge_percentile_table[colname_i], as.numeric)
    #print(unlist(df_merge_percentile_table[colname_i]))
    print(percent_rank(num_col))
    df_merge_percentile_table[paste0(colname_i, "_percentile")] <- percent_rank(num_col)
    
  }
  
}

above_80th_percentile_df = data.frame(GEOID = c("GEOID"), 
                        indicator = c("indicator"),
                        value = c("value"), 
                        percentile = c("percentile"))

ls_high_percentile_indics <- c("extreme_heat_days_2040_2060_percentile", 
                               "Ozone_percentile",
                               "Drinking_Water_percentile", 
                               "Lead_percentile",
                               "Pesticides_percentile", 
                               "Air_Quality_percentile", 
                               "total_incidents_per_100k_percentile")

ls_low_percentile_indics <- c("treecanopy_percentile",
                              "Housing_Cost_Burden_percentile", 
                              "Housing_Affordability_percentile", 
                              "imm_org_score_percentile")
j = 0
for (i in 1:nrow(df_merge_percentile_table)) {
  for (column in colnames(df_merge_percentile_table)) {
    if (grepl("_percentile", column, fixed = TRUE)) {
      tract <- df_merge_percentile_table[i,"GEOID"]
      if (!(grepl("_Pctl", column, fixed = TRUE)) & !(grepl("_pctile", column, fixed = TRUE)))  {
        #print(column)
        percentile <- df_merge_percentile_table[i,column]
        value <- df_merge_percentile_table[i,gsub("_percentile", "", column)]
        if (!is.na(percentile)) {
          if ((percentile > .80) && (column %in% ls_high_percentile_indics)) {
            vec <- c(tract, gsub("_percentile", "", column), value, percentile)
            #rint(vec)
            above_80th_percentile_df = rbind(above_80th_percentile_df , vec)
            j = j + 1
          } else if ((percentile < .20) && (column %in% ls_low_percentile_indics)) {
            vec <- c(tract, gsub("_percentile", "", column), value, percentile)
            #rint(vec)
            above_80th_percentile_df = rbind(above_80th_percentile_df , vec)
            j = j + 1
        }
        }
      }
    }
  }
}
above_80th_percentile_df  = above_80th_percentile_df[-1,]
above_80th_percentile_df$value <- as.numeric(above_80th_percentile_df$value)
above_80th_percentile_df$percentile <- as.numeric(above_80th_percentile_df$percentile)

above_80th_percentile_df <- above_80th_percentile_df  %>% mutate_at(vars(value, percentile), list(~ round(., 3)))
above_80th_percentile_df <- unique(above_80th_percentile_df[,c("GEOID","indicator","value","percentile")])
print(above_80th_percentile_df)

save(above_80th_percentile_df , file = paste0(dire, "/data/above_80th_percentile_df.rda"))

# ## translation
# df_merge_long_spanish <- df_merge_long
# df_merge_long_spanish["indication"] <- lapply(df_merge_long_spanish["indication"], 
#                                               function(x) {google_translate(x, target_language = 'es')})
# 
# print(df_merge_long_spanish["indication"])
# save(df_merge_long_spanish, file = paste0(dire, "/data/merged_map_long_spanish.rda")) 
# 

