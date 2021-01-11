
library(tidyverse)
library(stringr)
library(odbc)
library(readr)
library(readxl)
library(janitor)

source("\\\\SRVshares\\Heinz\\OPENDATA\\R_Setup\\db_connect.R")

### ------- Get schedule data from public timetable.
# set current pick
currentPick <- "2011"

# read timetable
timetableAll <- read.csv(paste0("\\\\Srvgiro2016\\giro\\hastus2016_wr\\hastus2016\\users\\NEWMAEC\\inter\\",
                                currentPick,
                                "_Public_Timetable_With_TripID.csv"),
                      stringsAsFactors = FALSE) %>%
  mutate(route = str_remove(trimws(trp_route), "^0+"),
         cleverid = as.character(clever_id),
         ttp_passing_time = trimws(ttp_passing_time)) %>%
  filter(route != "",
         variant != "IZ") %>%
  select(-c(trp_route, runs.on.weekday, runs.on.Saturday, runs.on.Sunday, variant, clever_id))

# summarise timetable for sorting stops 
timetableSeq <- timetableAll %>%
  group_by(route, cleverid, direction) %>%
  summarize(ttp_sequence = max(ttp_sequence)) %>%
  select(route, cleverid, direction, ttp_sequence)

# create vector of unique timepoint stops for filtering Ridecheck load data. 
timepoints <- as.vector(unique(timetableSeq$cleverid))



### ------- get Ridecheck Plus load data from function

raw <- getRidecheckPlusData(startDate = Sys.Date()-18, 
                            endDate = Sys.Date()-4) %>%
  filter(clever_id %in% timepoints) 
  

# get min and max dates from ridecheck
dates <- raw %>%
  mutate(date = as.Date(survey_date, format = "%m/%d/%Y %H:%M:%S")) %>%
  summarise(start_date = min(date),
            end_date = max(date))


### --- Process raw ridecheck data, join in timetable and stop arrival times.

df <- raw %>%
  mutate(date = as.Date(survey_date, format = "%m/%d/%Y %H:%M:%S"),
         veh_capacity = case_when( # set current passenger capacity based on vehicle ID
             vehicle_number >= 1700 & vehicle_number < 1800  ~ "10",
             vehicle_number >= 3000 & vehicle_number < 4000  ~ "25",
             TRUE ~ "15" ),
         direction_name = str_to_sentence(direction_name)) %>%
  group_by(route = route_name, direction_name, weekday = service_period, block = block_key, trip_start_time, 
                    tripid = trip_key, clever_id, latitude, longitude) %>%
  summarise(avg = round(mean(load)),
            max = max(load),
            med = round(median(load)),
            n = n(),
            veh_capacity =names(which.max(table(veh_capacity))),
            avg_crowding = as.numeric(veh_capacity) - avg) %>%
            add_column(dates) 


# Now join df to timetableAll. Add fancy route names. Remove extra columns,
# then join the timetable Seq with the max stop sequence for sorting in Tableau. 


df_timetable <- df %>%
  inner_join(timetableAll, by = c("tripid" = "internal.trip.id",
                                  "route" = "route",
                                  "clever_id" = "cleverid",
                                  "direction_name" = "direction"))%>%
  left_join(getRoutes(), by = c("route" = "Route_Code")) %>%
  select(-c(Internal_UTA, Friendly_Name, SB_Ridership_DB, Articulated, Mode, ttp_sequence)) %>%
  left_join(timetableSeq, by = c("route" = "route",
                                 "clever_id" = "cleverid",
                                 "direction_name" = "direction"))


write.csv(df_timetable, "\\\\SRVshares\\Heinz\\OPENDATA\\R_Projects\\ridership\\ridecheck_2week_loads.csv", row.names = FALSE)



# not needed- for 2003 emergency pick data nightmares.
### --- getting correct trip IDs.
### pull in 2003 emerg. inner join with original 2003. convert to vector for filtering df later. 

#trips_2003emerg <- read.csv("2003_e_lPublic_Timetable_With_TripID.csv")%>%
#  filter(ttp_sequence == '1') %>%
#  select(emerg.trip.id = internal.trip.id, trip.number) %>%
#  distinct()

#trips_2003 <- read.csv("Public_Timetable_IID.csv") %>%
#  filter(ttp_sequence == '1') %>%
#  select(internal.trip.id, trip.number) %>%
#  distinct() %>%
#  inner_join(trips_2003emerg, by = "trip.number")

#sched <- trips_2003[,1]