library(auk)
library(here)
library(dplyr)
library(stringr)
library(sf)
library(lubridate)

# If you want to use Gabe's weird import system, uncomment this (Ctrl+Shift+C)

# # Import data -----------------------------------------------------------------
# # update month and year to the most recently downloaded date
# ## eg a download from Sept 15 would be data up to Aug 31, use "aug" as month
# month <- "sep"
# year <- 2021
# 
# # ebd downloads are named based on the date range of the data request
# # convert the month object to a number and make sure it is two digits (ie
# # 10, 11, 12, or has a zero in front of it)
# num <- as.integer(sapply(month, 
#                          function(x) grep(paste0("(?i)", x), month.abb))) + 1
# 
# num <- ifelse(nchar(num) == 1, paste0(0, num), num)
# 

# 
# # read ebd files using the auk package, so that subspecies are lumped, 
# # non-taxa are filtered out, and group checklists are put together.
# ## Maryland data
# ebdmd <- read_ebd(here("data", "ebird", "1_raw", 
#                        paste0("ebd_US-MD_202001_2021", num, "_rel", 
#                               str_to_title(month), "-", year, ".txt")),
#                   unique = FALSE)
# 
# ## DC data
# ebddc <- read_ebd(here("data", "ebird", "1_raw", 
#                        paste0("ebd_US-DC_202001_2021", num, "_rel", 
#                               str_to_title(month), "-", year, ".txt")),
#                   unique = FALSE)
# pull in files with additional pertinent info
## eBird's internal and external breeding codes
# these can be downloaded from github
## https://github.com/breeding-bird-atlases/standardize-ebd/blob/main/ebird_internal_codes.csv
codes <- read.csv(here("ebird_internal_codes.csv"))

## Species list
## https://github.com/breeding-bird-atlases/standardize-ebd/blob/main/eBird_Taxonomy_v2021.csv
species <- read.csv(here("eBird_Taxonomy_v2021.csv")) %>%
  select(taxonomic_order = ï..TAXON_ORDER, 
         common_name = PRIMARY_COM_NAME, 
         scientific_name = SCI_NAME)

## eBird protocol codes and types
## https://github.com/breeding-bird-atlases/standardize-ebd/blob/main/ebird_protocol_codes.csv
protocols <- read.csv(here("ebird_protocol_codes.csv"))

## Block names and numbers (this is not on github, and is mainly used if
## eBird block names don't match up with state block names)
# blocks <- read.csv("filepathhere")

# read in ebd data ------------------------------------------------------------
ebd <- read_ebd("filepathhere")

# pull in special downloads
## Sensitive species
sens <- read_ebd("filepathhere")

# check sens column names against the ebd column names
if(any(setequal(colnames(sens), colnames(ebd)) == FALSE)) {
  print(c(setdiff(colnames(sens), colnames(ebd)),
          setdiff(colnames(ebd), colnames(sens)))) &
    stop("Column names do not match")
}

sens <- mutate(sens, data_origin = "sensitive_sp")

# ## Atlaser names and contact info
atlasers <- read.csv("filepathhere")
# # read the most current list of bba3 atlasers
# # all atlaser files are saved paste0("atlasers_", MONTH), so retrieve all 
# # files and visually check to make sure it's correct.
# (atlaser_files <- list.files(here("data", "bba3"), pattern = "atlasers_"))
# 
# # retrieve the most recent file, print and visually check to make sure it's
# # correct.
# (file <- atlaser_files[which.max(file.mtime(here("data", "bba3", 
#                                                  atlaser_files)))])
# # read the selected file
# atlasers <- read.csv(here("data", "bba3", file))

## eBird monthly automated special downloads
# get the pertinent files and their metadata


# For this, substitute the filepath to where your monthly downloads are, but 
# don't include the actual files' names.
# the pattern should be a general identifier of the monthly downloads.
details <- file.info(list.files("D:/gabri/R/mddcbba3/data/ebird/1_raw", 
                                pattern = c("Wisconsin BBA", "tsv"), 
                                full.names = TRUE))

# sort by most recently created and keept the most recent five, since eBird
# only sends five downloads each month.
details <- head(details[order(details$ctime, decreasing = TRUE), ], n = 5)

# get friendly names from the files (if "Wisconsin BBA" doesn't work, just sub 
# in the appropriate wording)
spcl <- row.names(details) %>%
  str_extract("(?<=Wisconsin BBA )(.+)(?=-2021)") %>%
  str_to_lower() %>%
  str_replace_all(c(" " = "_", "-" = "_"))

# read in the data
for(i in 1:nrow(details)) {
  assign(spcl[i],
         read.delim(row.names(details)[i], quote = ""))
}

# standardize the file names
## check if any column names don't match the expected column names
# can get this off the github
## https://github.com/breeding-bird-atlases/standardize-ebd/blob/main/ebird_autodownloads_column_names_dec2021.csv
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
  # you probably don't have to join the blocks object, since your names 
  # probably match.
  # left_join(., select(blocks, 
  #                     atlas_block, block_name, dnr_block_name, region)) %>%
  rename(project_code = proj_period_id,
         nocturnal_hrs = nocturnal_hours,
         total_hrs = total_hours,
         diurnal_hrs = diurnal_hours,
         block_county = region) %>%
  mutate(project_code = str_replace(project_code,
                                    "EBIRD_ATL_WI_202[0-4]", 
                                    "EBIRD_ATL_WI"),
         across(ends_with("hrs"), function(x) {
           as.numeric(str_replace(x, ",", ""))
         }))

if(any(!unique(effort$project_code) %in% "EBIRD_ATL_WI")) {
  print(unique(effort$project_code)) &
    stop("Unexpected project IDs")
}

summary <- summary %>%
  # you probably don't have to join the blocks object, since your names 
  # probably match.
  # left_join(., select(blocks, 
  #                     atlas_block, block_name, dnr_block_name, region)) %>%
  rename(breeding_category = category_code) %>%
  mutate(project_code = "EBIRD_ATL_WI")

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

# Join raw data ---------------------------------------------------------------

# join md, dc, sens, hidden, and zero files together
## if there are datasets you don't want to join to ebd, comment them out
ebd <- ebd %>%
  mutate(data_origin = "ebd") %>%
  full_join(., sens) %>%
  full_join(., user_hidden_records) %>%
  full_join(., zero_species_checklists) %>%
  full_join(., zero_count_records)

if(any(!unique(ebd$project_code) %in% "EBIRD_ATL_WI")) {
  print(unique(ebd$project_code)) &
    stop("Unexpected project IDs")
}
