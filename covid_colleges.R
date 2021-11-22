# COVID and start of colleges - Fall 2021
library(readxl)
library(zipcodeR)
library(janitor)
library(sf)
library(GISTools)
library(leaflet)
library(tidyverse)

today <- '2021-11-05'

##----------Data Load---------
zips <- read_csv("data/ZIP-COUNTY-FIPS_2017-06.csv") %>% 
  mutate(ZIP = as.character(str_pad(ZIP, 5, pad = "0")))

cases <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv")

colleges_with_fips <- read_xlsx("data/colleges.xlsx") %>% 
  mutate(ZIP = substr(ZIP, 1, 5)) %>% 
  mutate(ZIP = as.character(str_pad(ZIP, 5, pad = "0"))) %>% 
  left_join(zips, by = "ZIP") 

populations <- read.csv("https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.csv?v=8356.7",
                        colClasses=c("FIPStxt"="character")) %>%
  clean_names() %>%
  mutate(fip_stxt = str_pad(fip_stxt, 5, pad = "0")) %>% 
  filter(attribute == "Population 2020") %>% 
  rename(population = value) %>% 
  mutate(population = as.integer(population)) %>% 
  select(-rural_urban_continuum_code_2013, -attribute)
# changed to 2020 pops on 10/12/21

colleges_cases <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/colleges/colleges.csv")
# cases in this dataset is cumulative cases since beginning of pandemic

single_start_dates <- read_csv("data/single_college_counties.csv") %>% 
  select(-X24) %>% 
  mutate(stcountyfp = as.character(str_pad(stcountyfp, 5, pad = "0")))

national_cases <- cases %>% 
  group_by(date) %>%
  summarize(national_cases = sum(cases)) %>% 
  ungroup() %>% 
  filter('2021-07-01' <= date & date <= '2021-11-05') %>% 
  mutate(nat_cases_per100k = (national_cases/331002651) *100000)

all_colleges <- read_csv("data/all-schools-info-USdeptedu.csv") %>% 
  mutate(ZIP = substr(ZIP, 1, 5)) %>% 
  mutate(ZIP = as.character(str_pad(ZIP, 5, pad = "0"))) %>% 
  left_join(zips, by = 'ZIP') %>% 
  mutate(UNITID = as.character(UNITID))

##----------cases in week after school started-----------
colleges_and_cases <- colleges_with_fips %>% 
  left_join(cases, by = c("STCOUNTYFP" = "fips")) %>% 
  clean_names() %>%
  group_by(institution_name) %>% 
  filter('2021-08-11' <= date & date <= today) %>%
  # filter(start_date <= date & date <= (as.Date(start_date)+7))
  left_join(populations, by = c("countyname" = "area_name", "state_x" = "state")) %>% 
  select(unitid, opeid, opeid6, institution_name, city, state_x, state_y, countyname,
         fip_stxt, zip, latitude, longitude, accreditor, url, main, preddeg, start_date,
         date, cases, deaths, pop_estimate_2019) %>% 
  mutate(cases_per100k = (cases/pop_estimate_2019)*100000)

colleges_and_cases %>% 
  filter(countyname != "Norfolk County") %>% 
    # select Suffolk county for BU 
  filter(countyname != "Solano County") %>% 
    # select Yolo county for BU
  filter(state_x != "CA") %>% 
  #filter(institution_name %in% c("The University of Texas at Austin", 
                                # "University of Georgia",
                                 #"University of Alabama at Birmingham")) %>%
  ggplot(aes(x = date, y = cases_per100k, color = institution_name)) +
    geom_line() +
    geom_point() +
    geom_vline(aes(xintercept = as.Date(start_date), color = institution_name),
               linetype="dashed") +
  labs(x = '',
       y = 'Institution County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester',
       color = 'Institution Name') +
  theme_minimal()

##----------start dates - single college counties-----------

# line up all colleges on the same start date - and see if there 
# is an increase overall on start date and the 2 weeks after 
startdates_and_cases <- single_start_dates %>% 
  left_join(cases, by = c("stcountyfp" = "fips")) %>% 
  clean_names() %>%
  group_by(instnm) %>% 
  filter('2021-07-01' <= date & date <= today) %>%
  drop_na(start_date) %>%
  left_join(populations, by = c("countyname" = "area_name", "state_x" = "state"))%>% 
  select(unitid, start_date, instnm, city, county, state_x, state_y,
         fip_stxt, zip, latitude, longitude, main, preddeg,
         date, cases, deaths, population) %>% 
  mutate(cases_per100k = (cases/population)*100000) %>% 
  mutate(start_date = as.Date(start_date, "%m/%d/%y")) %>% 
  mutate(date = as.Date(date))

dates_around_start <- startdates_and_cases %>% 
  filter(date >= (as.Date(start_date)-9) & date <= (as.Date(start_date)+21)) %>% 
  group_by(county, zip) %>% 
  mutate(date_aroundstart = seq.int(-9, 21)) %>% 
  mutate(colors = case_when(date_aroundstart == 0 ~ "Start of Semester",
                            date_aroundstart > 0 ~ "After Start",
                            date_aroundstart < 0 ~ "Before Start"))

ggplot(dates_around_start, 
       aes(x = date_aroundstart, y = cases_per100k, group = date_aroundstart,
           fill = colors)) +
  geom_boxplot() + 
  labs(x = 'Days around Start of Semester (Start = Day 0)',
       y = 'Institution County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester',
       fill = " ") +
  theme_minimal() 
  
# arrange all schools around actual dates and divide into
# late starting colleges and early starting colleges 
# (before or after 8/23 - not perfect but good start)
late_vs_early <- startdates_and_cases %>% 
  mutate(late_early =  case_when(start_date >= '2021-08-23' ~ "Late",
                                  start_date < '2021-08-23' ~ "Early")) %>%
  filter('2021-07-01' <= date & date <= '2021-10-20')

late_vs_early %>% 
  ggplot(aes(x = date, y = cases_per100k, fill = late_early)) +
  geom_boxplot(aes(group = interaction(date, late_early))) + 
  # adding national covid cases 
  geom_line(data = national_cases, 
            aes(x = date, y = nat_cases_per100k, group = 1), color = 'blue',
            inherit.aes = FALSE) +
  labs(x = '',
       y = 'Institution County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester - Early (before 8/23) vs Late (after 8/23) Starts - All States',
       fill = 'Semester Start Date:') +
  theme_minimal()

# averages instead of boxplots
late_vs_early %>% 
  group_by(date, late_early) %>% 
  mutate(cases_per100k = mean(cases_per100k)) %>%
  ggplot(aes(x = date, y = cases_per100k, color = late_early)) +
  geom_line() + 
  # adding national covid cases 
  geom_line(data = national_cases, 
            aes(x = date, y = nat_cases_per100k, group = 1), color = 'black',
            inherit.aes = FALSE) +
  labs(x = '',
       y = 'Institution County Cases per 100k',
       title = 'Mean County COVID Cases Around College Start of Semester - Early (before 8/23) vs\nLate (after 8/23) Starts - All States',
       color = 'Semester Start Date:') +
  ylim(8000, 20000) +
  annotate("text", x = as.Date('2021-09-26'), y = 11500, 
           label = "National COVID Cases per 100k") +
  theme_minimal()

# same as above but only states that contain both early and late starts
late_vs_early_states <- startdates_and_cases %>% 
  mutate(late_early =  case_when(start_date >= '2021-08-23' ~ "Late",
                                 start_date < '2021-08-23' ~ "Early")) %>%
  filter('2021-07-01' <= date & date <= '2021-10-10') %>% 
  group_by(state_y) %>% 
  distinct(late_early) %>% 
  count(state_y) %>% 
  filter(n > 1)

late_vs_early_states %>% 
  left_join(late_vs_early, by = "state_y") %>% 
  ggplot(aes(x = date, y = cases_per100k, fill = late_early)) +
  geom_boxplot(aes(group = interaction(date, late_early))) + 
  # adding national covid cases 
  geom_line(data = national_cases, 
            aes(x = date, y = nat_cases_per100k, group = 1), color = 'blue',
            inherit.aes = FALSE) +
  labs(x = '',
       y = 'Institution County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester - Early (before 8/23) vs Late (after 8/23) Starts - Only States with Both Late and Early Start Colleges',
       fill = 'Semester Start Date:') +
  theme_minimal()

# averages instead of boxplots
late_vs_early_states %>% 
  left_join(late_vs_early, by = "state_y") %>% 
  group_by(date, late_early) %>% 
  mutate(cases_per100k = mean(cases_per100k)) %>%
  ggplot(aes(x = date, y = cases_per100k, color = late_early)) +
  geom_line() + 
  # adding national covid cases 
  geom_line(data = national_cases, 
            aes(x = date, y = nat_cases_per100k, group = 1), color = 'black',
            inherit.aes = FALSE) +
  labs(x = '',
       y = 'Institution County Cases per 100k',
       title = 'Mean County COVID Cases Around College Start of Semester - Early (before 8/23) vs\nLate (after 8/23) Starts - Only States with Both Late and Early Start Colleges',
       color = 'Semester Start Date:') +
  ylim(8000, 20000) +
  annotate("text", x = as.Date('2021-09-26'), y = 11500, 
           label = "National COVID Cases per 100k") +
  theme_minimal()

  


##----------counties with only one school------------
single_college_counties <- all_colleges %>% 
  count(STCOUNTYFP) %>% 
  filter(n == 1)

single_college_counties <- as.list(single_college_counties$STCOUNTYFP)

single_college_county <- all_colleges %>% 
  filter(STCOUNTYFP %in% single_college_counties) %>% 
  clean_names() %>% 
  filter(state != 'PR')
# 735 lines, 579 unique colleges (excluding PR)

write_csv(single_college_county, "single_college_counties.csv")

single_college_county_cases <- single_college_county %>% 
  left_join(cases, by = c("stcountyfp" = "fips")) %>% 
  clean_names() %>%
  group_by(instnm) %>% 
  filter('2021-07-01' <= date & date <= today) %>%
  left_join(populations, by = c("countyname" = "area_name", "state_x" = "state")) %>% 
  select(unitid, opeid, opeid6, instnm, city, state_x, state_y, countyname,
         fip_stxt, zip, latitude, longitude, insturl, main, preddeg,
         date, cases, deaths, population) %>% 
  mutate(cases_per100k = (cases/population)*100000) 

# all schools 
single_college_county_cases %>% 
  group_by(instnm) %>%
  arrange(countyname) %>% 
  # selects only the first county for all coleges with multiple counties
  top_n(n = 1, wt=countyname) %>% 
  # gets rid of one weirdly high outlier
  filter(cases_per100k < 30000) %>% 
  # plot:
  ggplot(aes(x = date, y = cases_per100k, color = instnm)) +
  geom_line() +
  geom_point() +
  labs(x = '',
       y = 'Institution County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester - Single College Counties',
       color = 'Institution Name') +
  guides(color = FALSE) +
  theme_minimal()

# box plot 
single_college_county_cases %>% 
  group_by(instnm) %>%
  arrange(countyname) %>% 
  # selects only the first county for all colleges with multiple counties
  top_n(n = 1, wt=countyname) %>% 
  # gets rid of one weirdly high outlier
  filter(cases_per100k < 30000) %>% 
  # plot:
  ggplot(aes(x = date, y = cases_per100k, group = date)) +
  geom_boxplot() + 
  # adding national covid cases 
  geom_line(data = national_cases, 
            aes(x = date, y = nat_cases_per100k, group = 1), color = 'blue') +
  labs(x = '',
       y = 'Institution County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester - Single College Counties',
       color = 'National COVID Cases per 100k') +
  guides(color = FALSE) +
  theme_minimal()

# means
single_college_county_cases %>% 
  group_by(instnm) %>%
  arrange(countyname) %>% 
  # selects only the first county for all colleges with multiple counties
  top_n(n = 1, wt=countyname) %>% 
  # gets rid of one weirdly high outlier
  filter(cases_per100k < 30000) %>% 
  ungroup() %>%
  group_by(date) %>%
  mutate(mean_cases_per100k = mean(cases_per100k)) %>%

  # plot:
  ggplot(aes(x = date, y = mean_cases_per100k)) +
  geom_line() + 
  # adding national covid cases 
  geom_line(data = national_cases, 
            aes(x = date, y = nat_cases_per100k, group = 1), color = 'blue') +
  labs(x = '',
       y = 'Institution County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester - Single College Counties',
       color = 'National COVID Cases per 100k') +
  guides(color = FALSE) +
  theme_minimal()

##----------counties with no schools------------

college_counties <- all_colleges %>% distinct(STCOUNTYFP) %>% drop_na()
college_counties <- as.list(college_counties$STCOUNTYFP)

`%!in%` <- Negate(`%in%`)

no_colleges_counties_list <- cases %>%
  filter(fips %!in% college_counties) %>% 
  drop_na() %>%
  distinct(fips)

no_colleges_counties_list <- as.list(no_colleges_counties_list$fips)

no_colleges_counties <- cases %>%
  filter(fips %!in% college_counties) %>% 
  drop_na() %>%
# 1268 counties with no colleges (and that have a listed fips code)
  left_join(populations, by = c("fips" = "fip_stxt")) %>% 
  mutate(cases_per100k = cases/population *100000) %>% 
  filter('2021-07-01' <= date & date <= today) %>%
  # gets rid of two weirdly high outlier counties
  filter(cases_per100k < 30000) %>% 
  group_by(date) %>%
  mutate(mean_cases_per100k = mean(cases_per100k))
  
# box plot of no collge counties 
no_colleges_counties %>% 
  # plot:
  ggplot(aes(x = date, y = cases_per100k, group = date)) +
  geom_boxplot() + 
  geom_line(data = national_cases, 
            aes(x = date, y = nat_cases_per100k, group = 1), color = 'blue') +
  labs(x = '',
       y = 'County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester - No College Counties',
       color = 'National COVID Cases per 100k') +
  guides(color = FALSE) +
  theme_minimal()


##----------counties with multiple schools------------
all_colleges <- read_csv("data/all-schools-info-USdeptedu.csv") %>% 
  mutate(ZIP = substr(ZIP, 1, 5)) %>% 
  mutate(ZIP = as.character(str_pad(ZIP, 5, pad = "0"))) %>% 
  left_join(zips, by = 'ZIP')

multi_college_counties <- all_colleges %>% 
  count(STCOUNTYFP) %>% 
  filter(n > 1)

multi_college_counties <- as.list(multi_college_counties$STCOUNTYFP)
# 1176 multi-college counties

multi_college_county <- all_colleges %>% 
  filter(STCOUNTYFP %in% multi_college_counties) %>% 
  clean_names() %>% 
  filter(state != 'PR')

multi_college_county_cases <- multi_college_county %>% 
  left_join(cases, by = c("stcountyfp" = "fips")) %>% 
  clean_names() %>%
  group_by(instnm) %>% 
  filter('2021-08-30' <= date & date <= today) %>%
  left_join(populations, by = c("countyname" = "area_name", "state_x" = "state")) %>% 
  select(unitid, opeid, opeid6, instnm, city, state_x, state_y, countyname,
         fip_stxt, zip, latitude, longitude, insturl, main, preddeg,
         date, cases, deaths, pop_estimate_2019) %>% 
  mutate(cases_per100k = (cases/pop_estimate_2019)*100000) 

multi_college_county_cases %>% 
  group_by(instnm) %>%
  arrange(countyname) %>% 
  # selects only the first county for all colleges with multiple counties
  top_n(n = 1, wt=countyname) %>% 
  # gets rid of one weirdly high outlier
  filter(cases_per100k < 30000) %>% 
  # plot:
  ggplot(aes(x = date, y = cases_per100k, group = date)) +
  geom_boxplot() + 
  # adding national covid cases 
  geom_line(data = national_cases, 
            aes(x = date, y = nat_cases_per100k, group = 1), color = 'blue') +
  labs(x = '',
       y = 'Institution County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester - Multi-College Counties',
       color = 'National COVID Cases per 100k') +
  guides(color = FALSE) +
  theme_minimal()


##----------combined visualizations------------

# COMBINED box plot - no colleges plus one college
single_college_county_cases %>% 
  group_by(instnm) %>%
  arrange(countyname) %>% 
  # selects only the first county for all colleges with multiple counties
  top_n(n = 1, wt=countyname) %>% 
  # gets rid of one weirdly high outlier
  filter(cases_per100k < 30000) %>% 
  # plot:
  ggplot(aes(x = date, y = cases_per100k, group = date)) +
  geom_boxplot(color="purple", fill="blue", alpha=0.2) + 
  geom_boxplot(data = no_colleges_counties, aes(x = date, y = cases_per100k, group = date), 
               color="red", fill="orange", alpha=0.2) + 
  # adding national covid cases 
  geom_line(data = national_cases, 
            aes(x = date, y = nat_cases_per100k, group = 1), color = 'black') +
  labs(x = '',
       y = 'County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester - Single (purple) vs No (red) College Counties',
       color = 'National COVID Cases per 100k') +
  guides(color = FALSE) +
  theme_minimal() 
# 3 rows that are weirdly high!! - currently dropping them 


# COMBINED box plot - no colleges plus multi college
multi_college_county_cases %>% 
  group_by(instnm) %>%
  arrange(countyname) %>% 
  # selects only the first county for all colleges with multiple counties
  top_n(n = 1, wt=countyname) %>% 
  # gets rid of one weirdly high outlier
  filter(cases_per100k < 30000) %>% 
  # plot:
  ggplot(aes(x = date, y = cases_per100k, group = date)) +
  geom_boxplot(color="purple", fill="blue", alpha=0.2) + 
  geom_boxplot(data = no_colleges_counties, aes(x = date, y = cases_per100k, group = date), 
               color="red", fill="orange", alpha=0.2) + 
  # adding national covid cases 
  geom_line(data = national_cases, 
            aes(x = date, y = nat_cases_per100k, group = 1), color = 'black') +
  labs(x = '',
       y = 'County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester - Multi (purple) vs No (red) College Counties',
       color = 'National COVID Cases per 100k') +
  guides(color = FALSE) +
  theme_minimal()

# COMBINED box plot - ALL 3 
multi_college_county_cases %>% 
  group_by(instnm) %>%
  arrange(countyname) %>% 
  # selects only the first county for all colleges with multiple counties
  top_n(n = 1, wt=countyname) %>% 
  # gets rid of one weirdly high outlier
  filter(cases_per100k < 30000) %>% 
  # plot:
  ggplot(aes(x = date, y = cases_per100k, group = date)) +
  geom_boxplot(color="purple", fill="blue", alpha=0.2) + 
  geom_boxplot(data = no_colleges_counties, aes(x = date, y = cases_per100k, group = date), 
               color="red", fill="orange", alpha=0.2) + 
  geom_boxplot(data = single_college_county_cases, aes(x = date, y = cases_per100k, group = date), 
               color="green", fill="green", alpha=0.2) + 
  # adding national covid cases 
  geom_line(data = national_cases, 
            aes(x = date, y = nat_cases_per100k, group = 1), color = 'black') +
  labs(x = '',
       y = 'County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester - Single (Green), No (red), Multi (purple)',
       color = 'National COVID Cases per 100k') +
  guides(color = FALSE) +
  ylim(0,30000) +
  theme_minimal()

# plot means
county_breakdown <- cases %>% 
  mutate(type = case_when(fips %in% single_college_counties ~ "single college county",
                          fips %in% multi_college_counties ~ "multi college county",
                          fips %in% no_colleges_counties_list ~ "no college county")) %>% 
  select(county, state, fips, type) %>% 
  distinct() %>% 
  filter(county != "Unknown")

# combine county breakdown, cases and populations
all_info <- all_colleges %>% 
  left_join(county_breakdown, by = c("STCOUNTYFP" = "fips")) %>% 
  clean_names() %>%
  drop_na() %>%
  left_join(cases, by = c("stcountyfp" = "fips")) %>% 
  clean_names() %>%
  group_by(instnm) %>% 
  filter('2021-07-01' <= date & date <= today) %>%
  left_join(populations, by = c("countyname" = "area_name", "state_x" = "state")) %>% 
  select(unitid, opeid, opeid6, instnm, city, state_x, state_y, countyname,
         fip_stxt, type, zip, latitude, longitude, insturl, main, preddeg,
         date, cases, deaths, population) %>% 
  mutate(cases_per100k = (cases/population)*100000) 

# plot the different 
all_info %>% 
  group_by(instnm) %>%
  arrange(countyname) %>% 
  # selects only the first county for all colleges with multiple counties
  top_n(n = 1, wt=countyname) %>% 
  # gets rid of one weirdly high outlier
  filter(cases_per100k < 30000) %>% 
  ungroup() %>%
  group_by(date, type) %>%
  mutate(mean_cases_per100k = mean(cases_per100k)) %>%
  
  # plot:
  ggplot() +
  geom_line(aes(x = date, y = mean_cases_per100k, color = type)) + 
  geom_line(data = no_colleges_counties, 
            aes(x = date, y = mean_cases_per100k, group = 1, color = 'no college county')) +
  # adding national covid cases 
  geom_line(data = national_cases, 
            aes(x = date, y = nat_cases_per100k, group = 1, color = 'national covid cases'),
            linetype="dashed") +
  labs(x = '',
       y = 'Mean Institution County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester In Different Counties',
       color = ' ') +
  guides(linetype=FALSE) +
  theme_minimal()


##----------map visualizations------------

# where are the multi, single, and no college counties
county_breakdown <- cases %>% 
  mutate(type = case_when(fips %in% single_college_counties ~ "single college county",
                          fips %in% multi_college_counties ~ "multi college county",
                          fips %in% no_colleges_counties_list ~ "no college county")) %>% 
  select(county, state, fips, type) %>% 
  distinct() %>% 
  filter(county != "Unknown")

US <- st_read("data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp") %>% 
  filter(STATEFP != 02) %>% 
  filter(STATEFP != 15) %>% 
  left_join(county_breakdown, by = c("GEOID" = "fips")) %>% 
  drop_na() 

pal <- colorFactor(palette = c("blue", "red", "green"), US$type)

leaflet(US) %>%
  setView(lng = -98.583, lat = 39.833, zoom = 4.4) %>%
  addTiles() %>%
  addPolygons(weight = 1,
              color = "#555555",
              opacity = 1,
              fillOpacity = 0.8,
              smoothFactor = 0.3,
              fillColor = pal(US$type),
              label = ~paste0(county, ": ", US$type)) %>%
  addLegend("bottomright", pal = pal, values = ~type, title = "County Type",
            opacity = 1)

# where are the early vs late 
late_vs_early

US_le <- st_read("data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp") %>% 
  filter(STATEFP != 02) %>% 
  filter(STATEFP != 15) %>% 
  left_join(late_vs_early, by = c("GEOID" = "fip_stxt")) %>% 
  drop_na() 

pal <- colorFactor(palette = c("blue", "red"), US_le$late_early)

leaflet(US_le) %>%
  setView(lng = -98.583, lat = 39.833, zoom = 4.4) %>%
  addTiles() %>%
  addPolygons(weight = 1,
              color = "#555555",
              opacity = 1,
              fillOpacity = 0.8,
              smoothFactor = 0.3,
              fillColor = pal(US_le$late_early),
              label = ~paste0(county, ": ", US_le$late_early)) %>%
  addLegend("bottomright", pal = pal, values = ~late_early, title = "Semester Start",
            opacity = 1)

##----------new ideas------------

# plot the no, single, and multi college DIFFERENCE between mean increase and national
# cases increase on each day 
  # more of a lift


# geographic division of colleges with most/least cumulative cases 
colleges_cases_loc <- colleges_cases %>% 
  left_join(all_colleges, by = c("ipeds_id" = "UNITID")) %>% 
  select(date, state, STATE, county, STCOUNTYFP, city, ZIP, college,
         cases, cases_2021, LATITUDE, LONGITUDE) %>% 
  mutate(cases_ranking = case_when(cases <= quantile(colleges_cases$cases)[2] ~ "bottom quarter",
                   cases <= quantile(colleges_cases$cases)[3] & cases > quantile(colleges_cases$cases)[2] ~ "lower middle quarter",
                   cases <= quantile(colleges_cases$cases)[4] & cases > quantile(colleges_cases$cases)[3] ~ "upper middle quarter",
                   cases > quantile(colleges_cases$cases)[4] ~ "top quarter"))

# something weird going on with this palette....
pal <- colorFactor(palette = c("green", "yellow", "red", "orange"), 
                   domain = c("bottom quarter", "lower middle quarter", "upper middle quarter", "top quarter"))

leaflet(data = colleges_cases_loc) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    ~LONGITUDE, ~LATITUDE,
    color = ~pal(colleges_cases_loc$cases_ranking),
    stroke = FALSE, 
    fillOpacity = 0.5,
    label = ~paste0(college, ": \n", (cases_ranking))
  )


# new idea - what is the best predictor for number of cases? urban/rural, undergrad population, etc? 

