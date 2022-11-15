# 20221027
# WI BBA2 Block List Screening

library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(sf)

# atlas data
bba2 <- read.delim(here("ebird_data_sample_wbbaii.txt"), quote = "") 

# phenology data
dates <- read.csv(here("acceptable_dates2015.csv")) %>%
  rename(B_safe_start_date = Beginning.B.date,
         B_safe_end_date = Ending.B.date,
         E_recorded_breeding_start_date = Beginning.E.date,
         E_recorded_breeding_end_date = Ending.E.date)

# block names
blocks <- st_read(dsn = here(), layer = "WbbaBlocks2015_v0_2") %>%
  mutate(block_name = paste(QUAD_NAME, BLOCK_POS)) %>%
  select(block_id = BLOCK_ID,
         block_name,
         block_county = COUNTY)

# format column names to snake case
colnames(bba2) <- str_replace_all(str_to_lower(colnames(bba2)), "\\.", "_")

# convert date to date format and add julian date column
bba2 <- bba2 %>%
  mutate(observation_date = as_date(observation_date),
         jdate = yday(observation_date),
         atlas_block = ifelse(atlas_block == "", NA_character_, atlas_block)) 

# find any missing blocks
find_block <- function(df, shapefile, 
                        map_col = "block_id", crs = 4326,
                        lon = "longitude", lat = "latitude") {
  shapefile <- shapefile %>%
    sf::st_transform(crs = crs)
  
  points <- sf::st_as_sf(df, coords = c(lon, lat), crs = crs) 
  
  blocks_points_in <- sf::st_intersects(points, shapefile)
  
  for(i in seq_along(df[, 1])) {
    if(is.na(df$atlas_block[i])) {
      df$atlas_block[i] <- data.frame(shapefile)[blocks_points_in[[i]], map_col]
    } else next
  }
  return(df)
}

bba2 <- find_block(bba2, blocks)

# join the block names
bba2 <- left_join(bba2, blocks, by = c("atlas_block" = "block_id"))

x <- length(dates)
y <- length(dates) - 3

dates[, y:x] <- lapply(dates[, y:x], as_date, format = "%m/%d/%Y")

rm(x, y)

# convert dates to julian dates
dates <- dates %>%
  mutate(
    breeding_start = yday(B_safe_start_date),
    
    breeding_end = ifelse(yday(B_safe_end_date) == 365, 366,
                          yday(B_safe_end_date)),
    
    prebreeding_start = ifelse(yday(E_recorded_breeding_start_date) >=
                                 breeding_start, NA,
                               yday(E_recorded_breeding_start_date)),
    
    prebreeding_end = ifelse(is.na(prebreeding_start), NA,
                             (breeding_start - 1)),
    
    earlyseason_start = ifelse(is.na(prebreeding_start) &
                                 breeding_start != 1, 1, 
                               ifelse(breeding_start == 1 | 
                                        prebreeding_start == 1, NA, 1)),
    
    earlyseason_end = ifelse(is.na(earlyseason_start), NA,
                             ifelse(is.na(prebreeding_start),
                                    (breeding_start - 1),
                                    (prebreeding_start - 1))),
    
    postbreeding_end = ifelse(yday(E_recorded_breeding_end_date) <= 
                                breeding_end, NA,
                              ifelse(yday(E_recorded_breeding_end_date) == 365,
                                     366, yday(E_recorded_breeding_end_date))),
    
    postbreeding_start = ifelse(is.na(postbreeding_end), NA,
                                (breeding_end + 1)),
    
    lateseason_end = ifelse(is.na(postbreeding_end) &
                              breeding_end != 366, 366,
                            ifelse(postbreeding_end == 366 |
                                     breeding_end == 366, NA, 366)),
    
    lateseason_start = ifelse(is.na(lateseason_end), NA, 
                              ifelse(is.na(postbreeding_end),
                                     (breeding_end + 1),
                                     (postbreeding_end + 1)))
  ) %>%
  select(1:last_col(10), 
         earlyseason_start,
         earlyseason_end,
         prebreeding_start,
         prebreeding_end,
         breeding_start,
         breeding_end,
         postbreeding_start,
         postbreeding_end,
         lateseason_start,
         lateseason_end)

seq_na <- function(x, y) {
  if(is.na(x) | is.na(y)) NA else seq(x, y)
}

# list all the days that are in each season
dates$season_len <- lapply(1:nrow(dates), function(x) {
  list(
    earlyseason = ifelse(dates$breeding_start[x] < dates$breeding_end[x],
                         map2(dates$earlyseason_start[x],
                              dates$earlyseason_end[x], seq_na), list(NA)),
    
    prebreeding = ifelse(dates$breeding_start[x] < dates$breeding_end[x],
                         map2(dates$prebreeding_start[x],
                              dates$prebreeding_end[x], seq_na), list(NA)),
    
    breeding = ifelse(dates$breeding_start[x] < dates$breeding_end[x],
                      map2(dates$breeding_start[x],
                           dates$breeding_end[x], seq_na),
                      list(c(seq(from = 1, to = dates$breeding_end[x]),
                             seq(from = dates$breeding_start[x], to = 366)))),
    
    postbreeding = ifelse(dates$breeding_start[x] < dates$breeding_end[x],
                          map2(dates$postbreeding_start[x],
                               dates$postbreeding_end[x], seq_na),
                          ifelse(is.na(dates$postbreeding_end[x]),
                                 list(NA),
                                 list(seq(
                                   from = dates$postbreeding_start[x],
                                   to = min(c(
                                     (dates$breeding_start[x] - 1),
                                     (dates$lateseason_start[x] - 1))
                                   ))))),
    
    lateseason = ifelse(dates$breeding_start[x] < dates$breeding_end[x],
                        map2(dates$lateseason_start[x],
                             dates$lateseason_end[x], seq_na),
                        ifelse(is.na(dates$postbreeding_end[x]),
                               list(seq(from = (dates$breeding_end[x] + 1), 
                                        to = (dates$breeding_start[x] - 1))),
                               list(seq(from = (dates$postbreeding_end[x] + 1),
                                        to = (dates$breeding_end[x] - 1)))))
  )
})

# provide the species name as the list name
names(dates$season_len) <- dates$common_name

breeding_sp <- unlist(dates$Species)

# check that the new dates column look as expected
if(any(names(which(lapply(dates$season_len, every, is.na) == TRUE)) 
       %in% breeding_sp)){
  warning("Julian dates were not created as expected")
}

# every species should have each julian date represented exactly once
if (any(c(any(map(map(dates$season_len, discard, is.na), transpose) %>%
              flatten() %>%
              map(., reduce, sum) %>%
              map(., sum) %>%
              unlist() != sum(1:366)),
          any(names(which(lapply(dates$season_len, every, is.na) == TRUE)) 
              %in% breeding_sp)))){
  warning("Julian dates were not created as expected")
}

# join dates to atlas data
bba2 <- left_join(bba2, select(dates, Species, season_len), 
                  by = c("common_name" = "Species")) 


# assign the season the observation was recorded in
bba2$observation_season <- sapply(1:nrow(bba2), function(x) {
  if (bba2$jdate[x] %in% bba2$season_len[[x]]$earlyseason[[1]]) {
    "earlyseason"
  } else if (bba2$jdate[x] %in% bba2$season_len[[x]]$prebreeding[[1]]) {
    "prebreeding"
  } else if (bba2$jdate[x] %in% bba2$season_len[[x]]$breeding[[1]]) {
    "breeding"
  } else if (bba2$jdate[x] %in% bba2$season_len[[x]]$postbreeding[[1]]) {
    "postbreeding"
  } else if (bba2$jdate[x] %in% bba2$season_len[[x]]$lateseason[[1]]) {
    "lateseason"
  } else {
    NA_character_
  }
})

# check that species were evaluated as expected
table(bba2$observation_season)

if (any(unique(bba2[which(is.na(bba2$observation_season)), "common_name"]) 
        %in% breeding_sp)){
  warning("Seasons have not been evaluated as expected")
}

## Screen 1 -- Identify uncoded, eligible species -----------------------------

# identify the coded species
coded_spp <- bba2 %>%
  group_by(block_name) %>%
  filter(breeding_category %in% c("C2", "C3", "C4")) %>%
  distinct(common_name) %>%
  ungroup()

# filter observations to only species that haven't been coded in that block
## detailed output
uncoded <- anti_join(bba2, coded_spp) %>%
  group_by(block_name, common_name, observation_season) %>%
  mutate(n_seasonal_occurrences = n() - sum(is.na(observation_season))) %>%
  group_by(block_name, common_name) %>%
  mutate(n_occurrences = n()) %>%
  ungroup() %>%
  mutate(link = paste0("https://ebird.org/atlaswi/checklist/", 
                       sampling_event_identifier)) %>%
  select(taxonomic_order,
         common_name,
         n_occurrences,
         n_seasonal_occurrences,
         observation_season,
         observation_date,
         locality,
         block_name,
         block_county,
         county,
         observation_count,
         duration_minutes,
         effort_distance_km,
         project_code,
         species_comments,
         sampling_event_identifier,
         link)

# filter observations to only species that haven't been coded in that block
## summarized output
uncoded_summary <- uncoded %>%
  pivot_wider(names_from = observation_season,
              values_from = n_seasonal_occurrences) %>%
group_by(common_name, block_name) %>%
  mutate(across(c(earlyseason, 
                prebreeding,
                breeding,
                postbreeding,
                lateseason), sum, na.rm = TRUE)) %>%
  distinct(common_name,
           n_occurrences,
           earlyseason,
           prebreeding,
           breeding,
           postbreeding,
           lateseason,
           block_name,
           block_county)  %>% 
ungroup()

block_eval <- unique(uncoded$block_name)

for(i in block_eval) {
  county_name <- unlist(unique(uncoded[which(uncoded$block_name %in% i), 
                                       "block_county"]))
  
  screen1_detailed <- uncoded %>%
    filter(block_name %in% i) %>%
    select(-block_county) 
  
  screen1_summary <- uncoded_summary %>%
    filter(block_name %in% i) %>%
    select(-block_name, -block_county)%>%
    arrange(-breeding, -n_occurrences) %>%
    rename(BREEDING = breeding)
  
  if(!dir.exists(here("wibba2 screen", county_name))) {
    dir.create(here("wibba2 screen", county_name))
  }
  
  write.csv(screen1_detailed, here("wibba2 screen", county_name, 
                                   paste0("wibba2_screen1_details_", 
                                          i, ".csv")),
            row.names = FALSE)
  
  write.csv(screen1_summary, here("wibba2 screen", county_name, 
                                  paste0("wibba2_screen1_summary_", 
                                         i, ".csv")),
            row.names = FALSE)
}

# Screen 2 -- Identify potentially ineligible coded species -------------------

coded <- bba2 %>%
  filter(project_code == "EBIRD_ATL_WI" &
           breeding_category %in% c("C2", "C3", "C4")) %>%
  mutate(group_identifier = ifelse(group_identifier == "", 
                                   NA_character_, group_identifier)) %>%
  group_by(common_name) %>%
  filter(!duplicated(group_identifier, incomparables = NA)) %>%
  group_by(block_name, common_name) %>%
  mutate(n_codes = n(),
         year = year(observation_date),
         jdate = ifelse(year == 2016, jdate + 365, jdate),
         first_code = format(as_date(min(jdate - 1), 
                                     origin = "2015-01-01"), "%b %d"),
         last_code = format(as_date(max(jdate - 1), 
                                    origin = "2015-01-01"), "%b %d"),
         mean_code = format(as_date(mean(jdate - 1), 
                                    origin = "2015-01-01"), "%b %d"),
         days_present = (max(jdate) + 1) - min(jdate),
         in_breeding_season = ifelse(any(observation_season == "breeding"), 
                                         TRUE, FALSE),
         below_three_codes = ifelse(n_codes < 3, TRUE, FALSE)) %>%
  ungroup() %>%
  mutate(link = paste0("https://ebird.org/atlaswi/checklist/", 
                       sampling_event_identifier)) %>%
  select(taxonomic_order,
         common_name,
         n_codes,
         first_code,
         last_code,
         mean_code,
         days_present,
         in_breeding_season,
         below_three_codes,
         observation_season,
         observation_date,
         locality,
         block_name,
         block_county,
         county,
         observation_count,
         duration_minutes,
         effort_distance_km,
         project_code,
         species_comments,
         sampling_event_identifier,
         link)

coded_summary <- coded %>%
  distinct(block_name, common_name, .keep_all = TRUE)

block_eval <- unique(coded$block_name)

# write files
for(i in block_eval) {
  county_name <- unlist(unique(coded[which(coded$block_name %in% i), 
                                     "block_county"]))
  
  screen2_detailed <- coded %>%
    filter(block_name %in% i &
             (in_breeding_season == FALSE |
             below_three_codes == TRUE)) %>%
    select(-block_county)
  
  screen2_summary <- coded_summary %>%
    filter(block_name %in% i) %>%
    select(-block_name, -block_county)
  
  if(!dir.exists(here("wibba2 screen", county_name))) {
    dir.create(here("wibba2 screen", county_name))
  }
  
  write.csv(screen2_detailed, here("wibba2 screen", county_name, 
                                   paste0("wibba2_screen2_details_", 
                                          i, ".csv")),
            row.names = FALSE)
  
  write.csv(screen2_summary, here("wibba2 screen", county_name, 
                                  paste0("wibba2_screen2_summary_", 
                                         i, ".csv")),
            row.names = FALSE)
}
