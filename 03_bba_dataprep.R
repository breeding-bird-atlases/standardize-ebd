# 20201002
## updated 20210730
# when updated ebird data are available use this to standardize the file and
# add pertinent columns.

library(auk)
library(here)
library(dplyr)
library(stringr)
library(lubridate)
library(suncalc)

# Import data -----------------------------------------------------------------
# update month and year to the most recently downloaded date
## eg a download from Sept 15 would be data up to Aug 31, use "aug" as month
month <- "jun"
year <- 2021

# ebd downloads are named based on the date range of the data request
# convert the month object to a number and make sure it is two digits (ie
# 10, 11, 12, or has a zero in front of it)
num <- as.integer(sapply(month, 
                         function(x) grep(paste0("(?i)", x), month.abb))) + 1

num <- ifelse(nchar(num) == 1, paste0(0, num), num)

# read in a file with internal and external breeding codes
codes <- read.csv(here("data", "ebird", "0_metadata",
                       "ebird_internal_codes.csv"))

# read ebd files using the auk package, so that subspecies are lumped, 
# non-taxa are filtered out, and group checklists are put together.
## Maryland data
ebdmd <- read_ebd(here("data", "ebird", "1_raw", 
                       paste0("ebd_US-MD_202001_2021", num, "_rel", 
                              str_to_title(month), "-", year, ".txt")))

## DC data
ebddc <- read_ebd(here("data", "ebird", "1_raw", 
                       paste0("ebd_US-DC_202001_2021", num, "_rel", 
                              str_to_title(month), "-", year, ".txt")))

# pull in special downloads
## Sensitive species
sens <- read_ebd(here("data", "ebird", "1_raw",
                      "ebd_sensitive_relSep-2020_MDDC_BBA.txt"))

# pull in files with additional pertinent info
## Block names and numbers
blocks <- read.csv(here("data", "bba3", "block_metadata.csv"))

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

## eBird monthly automated special downloads
# get the pertinent files and their metadata
details <- file.info(list.files(here("data", "ebird", "1_raw"), 
           pattern = c("Maryland_DC BBA", "tsv"), full.names = TRUE))

# sort by most recently created and keept the most recent five, since eBird
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

# standardize the file names
effort <- effort %>% 
  rename(atlas_block = block_code,
         project_code = proj_period_id,
         nocturnal_hrs = nocturnal_hours,
         total_hrs = total_hours,
         diurnal_hrs = diurnal_hours) %>%
  mutate(project_code = str_replace(project_code, 
                                    "EBIRD_ATL_MD_DC_2020", 
                                    "EBIRD_ATL_MD_DC"),
         total_hrs = as.numeric(str_replace(total_hrs, ",", "")),
         diurnal_hrs = as.numeric(str_replace(diurnal_hrs, ",", "")))

summary <- summary %>%
  rename(atlas_block = region_code,
         breeding_category = category_code)

user_hidden_records <- user_hidden_records %>%
  rename(global_unique_identifier = obs_id,
         sampling_event_identifier = sub_id,
         common_name = primary_com_name,
         approved_checklist = sub_reviewstatus,
         approved = obs_reviewstatus,
         reason = reason_code,
         species_comments = obs_comments,
         group_identifier = group_id,
         protocol_code = protocol_id,
         locality_id = loc_id,
         atlas_block = region_code,
         state_code = subnational1_code,
         observer_id = user_id,
         all_species_reported = all_obs_reported,
         trip_comments = sub_comments,
         datetime = to_char,
         number_observers = num_observers) %>%
  mutate(global_unique_identifier = 
           paste0("URN:CornellLabOfOrnithology:EBIRD:", 
                  global_unique_identifier),
         observation_count = str_replace(how_many_atmost, 
                                         "999,999,999", "X"),
         atlaser_name = paste(first_name, last_name),
         duration_minutes = duration_hrs*60,
         observation_time = ifelse(obs_time_valid == 1,
                                   hms::as_hms(as_datetime(datetime)),
                                   NA_POSIXct_),
         observation_date = as_date(datetime)) %>%
  left_join(., codes, by = c("aux_code" = "internal")) %>%
  rename(breeding_code = public) %>%
  select(!c(how_many_atleast,
            how_many_atmost,
            aux_code,
            value,
            first_name,
            last_name))

zero_count_records <- zero_count_records %>%
  rename(global_unique_identifier = obs_id,
         sampling_event_identifier = sub_id,
         common_name = primary_com_name,
         approved = valid,
         species_comments = obs_comments,
         group_identifier = group_id,
         protocol_code = protocol_id,
         locality_id = loc_id,
         atlas_block = region_code,
         state_code = subnational1_code,
         observer_id = user_id,
         all_species_reported = all_obs_reported,
         trip_comments = sub_comments,
         datetime = to_char,
         number_observers = num_observers) %>%
  mutate(global_unique_identifier = 
           paste0("URN:CornellLabOfOrnithology:EBIRD:", 
                  global_unique_identifier),
         observation_count = str_replace(how_many_atmost, 
                                         "999,999,999", "X"),
         atlaser_name = paste(first_name, last_name),
         duration_minutes = duration_hrs*60,
         observation_time = ifelse(obs_time_valid == 1,
                                   hms::as_hms(as_datetime(datetime)),
                                   NA_POSIXct_),
         observation_date = as_date(datetime)) %>%
  left_join(., codes, by = c("aux_code" = "internal")) %>%
  rename(breeding_code = public) %>%
  select(!c(how_many_atleast,
            how_many_atmost,
            aux_code,
            value,
            first_name,
            last_name))

zero_species_checklists <- zero_species_checklists %>%
  rename(project_code = proj_id,
         sampling_event_identifier = sub_id,
         group_identifier = group_id,
         atlas_block = block) %>%
  mutate(observation_date = as_date(to_char, format = "%m/%d/%y"))

rm(num, atlaser_files, file, details, spcl)

# Join raw data ---------------------------------------------------------------

# join md, dc, sens, hidden, and zero files together
ebd <- ebdmd %>%
  full_join(., ebddc) %>%
  full_join(., sens) %>%
  full_join(., user_hidden_records) %>%
  full_join(., zero_species_checklists) %>%
  full_join(., zero_count_records)
# 4,688,015 observations

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
ebd <- ebd %>%
  mutate(link = paste0("https://ebird.org/atlasmddc/checklist/",
                       sub("\\,.*", "", ebd$sampling_event_identifier)))

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

indx <- which(ebd$is_nocturnal[notna] == TRUE &
                ms(paste(ebd$duration_minutes[notna], 0)) > 
                abs(as_datetime(ebd$ebird_dawn[notna]) -
                      as_datetime(ebd$datetime[notna])))

ebd[indx, "is_nocturnal"] <- FALSE

rm(indx, notna, dst)

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
