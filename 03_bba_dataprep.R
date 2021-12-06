# 20201002
## updated 20210730
## updated 20211204
# when updated ebird data are available use this to standardize the file and
# add pertinent columns.

library(auk)
library(here)
library(dplyr)
library(stringr)
library(lubridate)
library(sf)
library(suncalc)

# Import data -----------------------------------------------------------------
# update month and year to the most recently downloaded date
## eg a download from Sept 15 would be data up to Aug 31, use "aug" as month
month <- "oct"
year <- 2021

# pull in files with additional pertinent info
## eBird's internal and external breeding codes
codes <- read.csv(here("data", "ebird", "0_metadata",
                       "ebird_internal_codes.csv"))

## Block names and numbers
blocks <- read.csv(here("data", "bba3", "block_metadata.csv"))

## Species list
species <- read.csv(here("data", "ebird", "0_metadata",
                         "eBird_Taxonomy_v2021.csv")) %>%
  select(taxonomic_order = ï..TAXON_ORDER, 
         common_name = PRIMARY_COM_NAME, 
         scientific_name = SCI_NAME)

## eBird protocol codes and types
protocols <- read.csv(here("data", "ebird", "0_metadata",
                           "ebird_protocol_codes.csv"))

## Atlaser names and contact info
# read the most current list of bba3 atlasers
# all atlaser files are saved paste0("atlasers_", MONTH), so retrieve all 
# files and visually check to make sure it's correct.
(atlaser_files <- list.files(here("data", "bba3"), pattern = "atlasers_"))

# retrieve the most recent file, print and visually check to make sure it's
# correct.
(file <- atlaser_files[which.max(file.mtime(here("data", "bba3", 
                                                 atlaser_files)))])
# read the selected file
atlasers <- read.csv(here("data", "bba3", file))

# ebd downloads are named based on the date range of the data request
# convert the month object to a number and make sure it is two digits (ie
# 10, 11, 12, or has a zero in front of it).
num <- as.integer(sapply(month, 
                         function(x) grep(paste0("(?i)", x), month.abb))) + 1

num <- ifelse(nchar(num) == 1, paste0(0, num), num)

# read ebd files using the auk package, so that subspecies are lumped, 
# non-taxa are filtered out (rollup = TRUE), and duplicate group checklists 
# are not removed (unique = FALSE; if unique = TRUE an additional column, 
# checklist_id, will be created that does not exist otherwise).
## Maryland data
ebdmd <- read_ebd(here("data", "ebird", "1_raw", 
                       paste0("ebd_US-MD_202001_2021", num, "_rel", 
                              str_to_title(month), "-", year, ".txt")),
                  rollup = TRUE, unique = FALSE)

## DC data
ebddc <- read_ebd(here("data", "ebird", "1_raw", 
                       paste0("ebd_US-DC_202001_2021", num, "_rel", 
                              str_to_title(month), "-", year, ".txt")),
                  rollup = TRUE, unique = FALSE)

## check column names against each other
if(any(setequal(colnames(ebdmd), colnames(ebddc)) == FALSE)) {
  print(c(setdiff(colnames(ebddc), colnames(ebdmd)),
          setdiff(colnames(ebdmd), colnames(ebddc)))) &
    stop("Column names do not match")
}

# pull in special downloads
## Sensitive species
sens <- read_ebd(here("data", "ebird", "1_raw",
                      "ebd_sensitive_relJul-2021_MDDC_BBA.txt"),
                 rollup = TRUE, unique = FALSE)

# check sens column names against the ebd column names
if(any(setequal(colnames(sens), colnames(ebdmd)) == FALSE)) {
  print(c(setdiff(colnames(sens), colnames(ebdmd)),
          setdiff(colnames(ebdmd), colnames(sens)))) &
    stop("Column names do not match")
}

sens <- mutate(sens, data_origin = "sensitive_sp")

## eBird monthly automated special downloads
# get the pertinent files and their metadata
details <- file.info(list.files(here("data", "ebird", "1_raw"), 
           pattern = c("Maryland_DC BBA", "tsv"), full.names = TRUE))

# sort by most recently created and keep the most recent five, since eBird
# only sends five downloads each month.
details <- head(details[order(details$ctime, decreasing = TRUE),], n = 5)

# get friendly names from the files
spcl <- row.names(details) %>%
  str_extract("(?<=Maryland_DC BBA )(.+)(?=-2021)") %>%
  str_to_lower() %>%
  str_replace_all(c(" " = "_", "-" = "_"))

# read in the data
for(i in 1:nrow(details)) {
  assign(spcl[i],
  read.delim(row.names(details)[i], quote = ""))
}

# standardize the files
## check the column names to make sure they're what is expected
checkcols <- read.csv(here("data", "bba3", 
                           "ebird_autodownloads_column_names_dec2021.csv"))

# get the column names from the imported dataframes
name_list <- lapply(list(effort = effort,
                         summary = summary,
                         user_hidden_records = user_hidden_records,
                         zero_count_records = zero_count_records,
                         zero_species_checklists = zero_species_checklists), 
                    colnames)

# check them against the names on file
for (x in unique(checkcols$dataset)) {
  if(any(setequal(name_list[[x]],
                  checkcols$column_name[checkcols$dataset == x])) == FALSE) {
    print(c(setdiff(name_list[[x]],
                    checkcols$column_name[checkcols$dataset == x]))) &
      stop("Unexpected column names")
  }
}

# get a missing county for any set of observations with lat/lon
## df = dataframe of coordinates; must be at least two columns of x and y
## map_sf = a simple feature object applicable to the area covered by df
## map_dsn = (if no map_sf) data source of shapefile applicable to the area 
## covered by df.
## map_layer = (if no map_sf) shapefile layer
## map_col = column in shapefile that contains county name
## crs = spatial projection; default is wgs84
## lon = name of the column in df containing x (longitude)
## lat = name of the column in df containing y (latitude)
find_county <- function(df, map_dsn, map_layer, map_sf = NULL, 
                        map_col = "NAME", crs = 4326,
                        lon = "longitude", lat = "latitude") {
  if(any(class(map_sf) %in% c("sf", "sfc", "sfg"))) {
    census_county <- sf::st_transform(map_sf, crs = crs)
  } else {
    census_county <- sf::st_read(map_dsn, map_layer) %>%
      sf::st_transform(crs = crs)
  }
  points <- sf::st_as_sf(df, coords = c(lon, lat), crs = crs) 
  
  counties_points_in <- sf::st_intersects(points, census_county)
  
  county <- NA
  
  for(i in seq_along(df[, 1])) {
    county[i] <- data.frame(census_county)[counties_points_in[[i]], map_col]
  }
  return(county)
} 

census_county <- st_read(here("data", "mapping", "census_county_boundaries"),
                         "census_county_boundaries_mddc")

# get a single observation count from a max/min range
get_abundance <- function(df, min, max) {
  for(i in seq_along(df[, min])) {
    if(df[i, min] ==
       df[i, max]) {
      x[i] <- df[i, min]
    } else {
      x[i] <- NA_integer_
    }
  }
  x
}

# format the datetime column
format_datetime <- function(x) {
  as.character(parse_date_time(x, c("%y%m%d %H%M%S", "%m%d%y %H%M%S"), 
                               tz = "EST", truncated = 3))
}

## these files contain commas in any numbers >999
effort <- effort %>% 
  left_join(., select(blocks, 
                      atlas_block, block_name, dnr_block_name, region)) %>%
  rename(project_code = proj_period_id,
         nocturnal_hrs = nocturnal_hours,
         total_hrs = total_hours,
         diurnal_hrs = diurnal_hours,
         block_county = region) %>%
  mutate(project_code = str_replace(project_code,
                                    "EBIRD_ATL_MD_DC_202[0-4]", 
                                    "EBIRD_ATL_MD_DC"),
         across(ends_with("hrs"), function(x) {
           as.numeric(str_replace(x, ",", ""))
         }))

if(any(!unique(effort$project_code) %in% "EBIRD_ATL_MD_DC")) {
  print(unique(effort$project_code)) &
          stop("Unexpected project IDs")
}

summary <- summary %>%
  left_join(., select(blocks, 
                      atlas_block, block_name, dnr_block_name, region)) %>%
  rename(breeding_category = category_code) %>%
  mutate(project_code = "EBIRD_ATL_MD_DC")

user_hidden_records <- user_hidden_records %>%
  left_join(., species) %>%
  left_join(., protocols) %>%
  rename(project_code = proj_period_id,
         category = taxon_category,
         approved_checklist = sub_reviewstatus,
         aux_code = breeding_code,
         aux_behav = behavior_code,
         datetime = to_char,
         last_edited_date = last_edited_dt,
         locality_type = loc_type) %>%
  mutate(global_unique_identifier = 
           paste0("URN:CornellLabOfOrnithology:EBIRD:", 
                  global_unique_identifier),
         county = find_county(., map_sf = census_county),
         observation_count = get_abundance(., "how_many_atleast", 
                                           "how_many_atmost"),
         atlaser_name = paste(first_name, last_name),
         observer_id = str_replace_all(observer_id, "USER", "obsr"),
         duration_minutes = duration_hrs*60,
         across(contains("date"), format_datetime),
         time_observations_started = 
           ifelse(obs_time_valid == 1,
                  as.character(hms::as_hms(as_datetime(datetime))), 
                  NA_POSIXct_),
         observation_date = as_date(datetime),
         data_origin = "hidden_obs") %>%
  left_join(., codes, by = c("aux_code" = "internal")) %>%
  rename(breeding_code = public) %>%
  left_join(., codes, by = c("aux_behav" = "internal")) %>%
  rename(behavior_code = public) %>%
  select(!c(orig_species_code,
            how_many_atleast,
            how_many_atmost,
            aux_code,
            aux_behav,
            first_name,
            last_name,
            checklist_id))

zero_count_records <- zero_count_records %>%
  left_join(., species) %>%
  left_join(., protocols) %>%
  rename(project_code = proj_period_id,
         category = taxon_category,
         aux_code = breeding_code,
         aux_behav = behavior_code,
         datetime = obs_dt,
         locality_type = loc_type) %>%
  mutate(global_unique_identifier = 
           paste0("URN:CornellLabOfOrnithology:EBIRD:", 
                  global_unique_identifier),
         observation_count = get_abundance(., "how_many_atleast", 
                                           "how_many_atmost"),
         atlaser_name = paste(first_name, last_name),
         duration_minutes = duration_hrs*60,
         across(contains("date"), format_datetime),
         observer_id = str_replace_all(observer_id, "USER", "obsr"),
         time_observations_started = 
           ifelse(obs_time_valid == 1,
                  as.character(hms::as_hms(as_datetime(datetime))),
                  NA_POSIXct_),
         observation_date = as_date(datetime),
         data_origin = "zero_count") %>%
  left_join(., codes, by = c("aux_code" = "internal")) %>% 
  rename(breeding_code = public) %>%
  left_join(., codes, by = c("aux_behav" = "internal")) %>%
  rename(behavior_code = public) %>%
  select(!c(how_many_atleast,
            how_many_atmost,
            aux_code,
            aux_behav,
            first_name,
            last_name))

zero_species_checklists <- zero_species_checklists %>%
  left_join(., protocols) %>%
  rename(datetime = observation_date,
         is_nocturnal = nocturnal,
         number_observers = num_observers) %>%
  mutate(across(contains("date"), format_datetime),
         time_observations_started = as.character(
           hms::as_hms(as_datetime(datetime))
         ),
         observation_date = as_date(datetime),
         duration_minutes = duration_hrs*60,
         is_nocturnal = ifelse(is_nocturnal == "t", TRUE, FALSE),
         data_origin = "zero_species")

rm(num, atlaser_files, file, details, spcl)

# Join raw data ---------------------------------------------------------------

# join md, dc, sens, hidden, and zero files together
ebd <- ebdmd %>%
  full_join(., ebddc) %>%
  mutate(data_origin = "ebd") %>%
  full_join(., sens) %>%
  full_join(., user_hidden_records) %>%
  full_join(., zero_species_checklists) %>%
  full_join(., zero_count_records) %>%
  mutate(project_code = str_replace(project_code,
                                    "EBIRD_ATL_MD_DC_202[0-4]", 
                                    "EBIRD_ATL_MD_DC"))
# 4,688,015 observations

if(any(!unique(ebd$project_code) %in% "EBIRD_ATL_MD_DC")) {
  print(unique(ebd$project_code)) &
    stop("Unexpected project IDs")
}

rm(ebdmd, ebddc, sens, user_hidden_records, zero_species_checklists, 
   zero_count_records)

# Standardize dataset ---------------------------------------------------------
# change NAs in breeding_category column to C1
# add columns to ebd dataset for
## block name
## block county
## whether the location was submitted at the block level
## atlaser name
## checklist link
## julian date
## sunset/sunrise/nautical_dawn/nautical_dusk
## ebird sunset/sunrise (sunset +20 min, sunrise -40 min)
## whether checklist is defined as nocturnal or diurnal
## effort in hours
## effort at the block level
## nocturnal effort at the block level

# o Standardize breeding_category ---------------------------------------------
# change breeding_category NAs to C1 because all of those observations are 
# uncoded and therefore "Observed" (or C1) observations, but eBird only applies
# C1 to observations with code F.
ebd$breeding_category[is.na(ebd$breeding_category)] <- "C1"

# MD-DC atlas interprets code PE as Probable, not Confirmed breeding evidence,
# but eBird interprets PE as Confirmed, so update ebd to match MD-DC project.
ebd$breeding_category[which(ebd$breeding_code == "PE")] <- "C3"

# o Add block info ------------------------------------------------------------
# add block names to atlas_block ID
# add block county (county is the county the observation was in, not the county
# the block is considered to be in, which is based off the block centroid).
ebd <- ebd %>%
  left_join(., blocks[,c("atlas_block", 
                         "block_name",
                         "region")], 
            by = "atlas_block") %>%
  rename(block_county = region) 

# check if location is at the block level and mark it as such
ebd <- ebd %>%
  mutate(is_block_level = ifelse(ebd$locality_id %in% blocks$locality_id, 
                                 TRUE, FALSE))

# o Add atlaser info ----------------------------------------------------------
# add observer names to observer_id 
atlasers$name <- paste(atlasers$first_name, atlasers$last_name)
atlasers$user_id <- gsub("USER", "obsr", atlasers$user_id)

ebd$atlaser_name <- unlist(lapply(strsplit(ebd$observer_id, ","),
                                  function(x) {
                                    paste0(
                                      atlasers[which(atlasers$user_id %in% x), 
                                               "name"],
                                      collapse = ","
                                    )
                                  }))

# o Add a checklist link ------------------------------------------------------
# o Add Julian date -----------------------------------------------------------
ebd <- ebd %>%
  mutate(link = paste0("https://ebird.org/atlasmddc/checklist/",
                       sub("\\,.*", "", ebd$sampling_event_identifier)),
         jdate = yday(observation_date))

# o Add daylight info ---------------------------------------------------------
# label nocturnal checklists
# ebird nocturnal = 20 min after sunset, 40 min before sunrise

ebd <- getSunlightTimes(data = data.frame(date = as_date(ebd$observation_date),
                                          lat = ebd$latitude,
                                          lon = ebd$longitude),
                        keep = c("sunrise", 
                                 "nauticalDawn",
                                 "sunset", 
                                 "nauticalDusk"),
                        tz = "EST") %>%
  rename(nautical_dawn = nauticalDawn,
         nautical_dusk = nauticalDusk) %>%
  select(sunrise:nautical_dusk) %>%
  bind_cols(ebd, .) %>%
  # if there is NA time, only the date will be kept in the column
  mutate(datetime = as_datetime(paste(observation_date, 
                                      time_observations_started),
                                tz = "EST"))

# getSunlightTimes() doesn't account for daylight savings time
# it uses the date and location to find the (non-DST) sunrise, etc time, so it 
# isn't informed by the time on the checklist.
dst <- list(
  as_datetime("2020-03-08 02:00:00", tz = "EST") %--%
    as_datetime("2020-11-01 02:00:00", tz = "EST"),
  
  as_datetime("2021-03-14 02:00:00", tz = "EST") %--%
    as_datetime("2021-11-07 02:00:00", tz = "EST"),
  
  as_datetime("2022-03-13 02:00:00", tz = "EST") %--%
    as_datetime("2022-11-06 02:00:00", tz = "EST"),
  
  as_datetime("2023-03-12 02:00:00", tz = "EST") %--%
    as_datetime("2023-11-05 02:00:00", tz = "EST"),
  
  as_datetime("2024-03-10 02:00:00", tz = "EST") %--%
    as_datetime("2024-11-03 02:00:00", tz = "EST")
)

# an NA time will be coerced to 1 AM if it isn't excluded
indx <- which(ebd$datetime %within% dst &
                !is.na(ebd$time_observations_started))

cols <- c("sunrise", "nautical_dawn", "sunset", "nautical_dusk")

ebd[indx, cols] <- lapply(ebd[indx, cols], function(x) x + hours(1))

rm(indx, cols)

# add columns for what ebird considers nocturnal
ebd <- ebd %>%
  mutate(ebird_dawn = sunrise - hms::as_hms(40*60),
         ebird_dusk = sunset + hms::as_hms(20*60),
         is_nocturnal = ifelse(datetime <= ebird_dawn |
                                 datetime >= ebird_dusk,
                               TRUE, FALSE))

# datetime without time is assumed to be 0:00, which would be interpreted as
# nocturnal. Change these to NAs.
ebd$is_nocturnal[is.na(ebd$time_observations_started)] <- NA

# check there are the same number of NA values in each column
sum(is.na(ebd$time_observations_started)) == sum(is.na(ebd$is_nocturnal))

# change the checklist's designation to diurnal if it extends past dawn, since
# the dawn chorus is so active that this will skew any exploration of 
# nocturnal effort.
notna <- which(!is.na(ebd$duration_minutes) &
                 !is.na(ebd$time_observations_started))

ebd[which(ebd$is_nocturnal[notna] == TRUE &
            ms(paste(ebd$duration_minutes[notna], 0)) > 
            abs(as_datetime(ebd$ebird_dawn[notna]) -
                  as_datetime(ebd$datetime[notna]))), "is_nocturnal"] <- FALSE

rm(notna, dst)

# o Add duration hours --------------------------------------------------------
ebd$duration_hrs <- ebd$duration_minutes/60

# o Calculate block effort ----------------------------------------------------
# objects for filtering the dataset
breeding <- c("C2", "C3", "C4")
# these protocols make up most complete checklist submissions, but doesn't
# consider Historical or Nocturnal Flight Count checklists
protocol <- c("Traveling", "Stationary", "Area", "Random")

# filter out non-portal observations to have a bba3 dataset
bba3 <- filter(ebd, project_code == "EBIRD_ATL_MD_DC")
# 1,586,242 observations

# filter out non-coded observations 
bba3_di <- bba3 %>% filter(breeding_category %in% breeding)

# get unique coded checklists
bba3_di <- bba3_di[! duplicated(bba3_di$link), ]

# remove incomplete and nocturnal checklists
bba3_di <- bba3_di %>% filter(all_species_reported == TRUE &
                                is_nocturnal == FALSE &
                                protocol_type %in% protocol)

# sum effort by block
bba3_di <- bba3_di %>% 
  group_by(block_name) %>%
  mutate(block_effort = sum(duration_hrs, na.rm = TRUE)) %>%
  ungroup() %>%
  select(block_name, block_effort)

bba3_di <- bba3_di[! duplicated(bba3_di$block_name), ]

# calculate nocturnal effort
# remove incomplete and diurnal checklists
bba3_noc <- bba3[! duplicated(bba3$link), ] %>% 
  filter(is_nocturnal == TRUE &
           all_species_reported == TRUE &
           protocol_type %in% protocol) %>%
  group_by(block_name) %>%
  mutate(noc_eff = sum(duration_hrs, na.rm = TRUE)) %>%
  ungroup() %>%
  select(block_name, noc_eff)
  
bba3_noc <- bba3_noc[! duplicated(bba3_noc$block_name), ]

# join the effort columns to the main dataset
bba3 <- bba3 %>%
  left_join(., bba3_di) %>%
  left_join(., bba3_noc)

rm(bba3_di, bba3_noc)

# Filter dataset --------------------------------------------------------------
# remove NFC counts
# remove unneeded columns
bba3 <- bba3 %>%
  filter(protocol_type != "Nocturnal Flight Call Count") %>%
  select(!c(category,
            scientific_name,
            age_sex,
            country,
            country_code,
            state_code,
            county_code,
            iba_code,
            bcr_code,
            usfws_code,
            atlas_block,
            locality_id,
            protocol_code,
            approved))

# filter out uncoded checklists from portal dataset
ebdbba3 <- bba3 %>% group_by(checklist_id) %>%
  filter(any(breeding_category %in% breeding) | 
           is_nocturnal == TRUE) 
# 734956 observations

# filter out portal observations, non-breeding code observations from 
# statewide dataset so that there isn't a list of all breeding codes that 
# didn't make into the Atlas dataset.
# don't include code F since it isn't truly a breeding code
ebd_code <- ebd %>%
  filter(project_code != "EBIRD_ATL_MD_DC" & 
           ! is.na(breeding_code) & breeding_code != "F")

# Save datasets ---------------------------------------------------------------
# export the standardized effort dataset
write.csv(effort, file = here("data", "ebird", "2_standardized",
                              paste0("mddcbba3_effort_", month, year,
                                     "_standardized.csv")),
          row.names = FALSE)

# export the standardized summary dataset
write.csv(summary, file = here("data", "ebird", "2_standardized",
                               paste0("mddcbba3_summary_", month, year,
                                      "_standardized.csv")),
          row.names = FALSE)

# export the file of bba3 results
write.table(bba3, file = here("data", "ebird", "2_standardized",
                              paste0("mddcbba3_", month, year,
                                     "_standardized.txt")),
            sep = "\t", quote = FALSE, row.names = FALSE)

# export file of bba3 coded checklists
write.table(ebdbba3, file = here("data", "ebird", "3_filtered",
                                 paste0("mddcbba3_coded_checklists_",
                                        month, year, ".txt")),
            sep = "\t", quote = FALSE, row.names = FALSE)

# export file of all MD-DC ebird data
write.table(ebd, file = here("data", "ebird", "2_standardized",
                              paste0("ebd_", month, year,
                                     "_standardized.txt")),
            sep = "\t", quote = FALSE, row.names = FALSE)

# save file of all non-portal observations with breeding codes
write.table(ebd_code,
            file = here("data", "ebird", "2_standardized", 
                        paste0("ebd_non-portal_codes_",
                               month, year, 
                               "_standardized.txt")),
            sep = "\t", quote = FALSE, row.names = FALSE)
