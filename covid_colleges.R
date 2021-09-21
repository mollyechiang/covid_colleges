# COVID and start of colleges - Fall 2021
library(readxl)
library(zipcodeR)
library(janitor)
library(tidyverse)

today <- '2021-09-20'

##----------Data Load---------
x <- read_xlsx("data/colleges.xlsx") %>% 
  mutate(ZIP = substr(ZIP, 1, 5)) %>% 
  mutate(ZIP = as.character(str_pad(ZIP, 5, pad = "0")))

zips <- read_csv("data/ZIP-COUNTY-FIPS_2017-06.csv") %>% 
  mutate(ZIP = as.character(str_pad(ZIP, 5, pad = "0")))

cases <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv")

colleges_with_fips <- x %>% 
  left_join(zips, by = "ZIP") 

populations <- read.csv("https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.csv?v=8356.7",
                        colClasses=c("FIPStxt"="character")) %>%
  clean_names() %>%
  mutate(fip_stxt = str_pad(fip_stxt, 5, pad = "0")) %>% 
  select(fip_stxt, state, area_name, pop_estimate_2019) %>%
  mutate(pop_estimate_2019 = gsub(",", "", pop_estimate_2019)) %>%
  mutate(pop_estimate_2019 = as.integer(pop_estimate_2019))

colleges_cases <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/colleges/colleges.csv")
# cases in this dataset is cumulative cases since beginning of pandemic

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

##----------counties with only one school------------
all_colleges <- read_csv("data/all-schools-info-USdeptedu.csv") %>% 
  mutate(ZIP = substr(ZIP, 1, 5)) %>% 
  mutate(ZIP = as.character(str_pad(ZIP, 5, pad = "0"))) %>% 
  left_join(zips, by = 'ZIP')

single_college_counties <- all_colleges %>% 
  count(STCOUNTYFP) %>% 
  filter(n == 1)

single_college_counties <- as.list(single_college_counties$STCOUNTYFP)

single_college_county <- all_colleges %>% 
  filter(STCOUNTYFP %in% single_college_counties) %>% 
  clean_names() %>% 
  filter(state != 'PR')
# 735 lines, 579 unique colleges (excluding PR)

single_college_county_cases <- single_college_county %>% 
  left_join(cases, by = c("stcountyfp" = "fips")) %>% 
  clean_names() %>%
  group_by(instnm) %>% 
  filter('2021-08-30' <= date & date <= today) %>%
  left_join(populations, by = c("countyname" = "area_name", "state_x" = "state")) %>% 
  select(unitid, opeid, opeid6, instnm, city, state_x, state_y, countyname,
         fip_stxt, zip, latitude, longitude, insturl, main, preddeg,
         date, cases, deaths, pop_estimate_2019) %>% 
  mutate(cases_per100k = (cases/pop_estimate_2019)*100000) 

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
       title = 'COVID Cases Around College Start of Semester',
       color = 'Institution Name') +
  guides(color = FALSE) +
  theme_minimal()

national_cases <- cases %>% 
  group_by(date) %>%
  summarize(national_cases = sum(cases)) %>% 
  ungroup() %>% 
  filter('2021-08-30' <= date & date <= today) %>% 
  mutate(nat_cases_per100k = (national_cases/331002651) *100000)

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
       title = 'COVID Cases Around College Start of Semester',
       color = 'National COVID Cases per 100k') +
  guides(color = FALSE) +
  theme_minimal()

##----------counties with no schools------------

college_counties <- all_colleges %>% distinct(STCOUNTYFP) %>% drop_na()
college_counties <- as.list(college_counties$STCOUNTYFP)

`%!in%` <- Negate(`%in%`)

no_colleges_counties <- cases %>%
  filter(fips %!in% college_counties) %>% 
  drop_na() %>%
# 1268 counties with no colleges (and that have a listed fips code)
  left_join(populations, by = c("fips" = "fip_stxt")) %>% 
  mutate(cases_per100k = cases/pop_estimate_2019 *100000) %>% 
  filter('2021-08-30' <= date & date <= today) %>%
  # gets rid of two weirdly high outlier counties
  filter(cases_per100k < 30000)
  
# box plot of no collge counties 
no_colleges_counties %>% 
  # plot:
  ggplot(aes(x = date, y = cases_per100k, group = date)) +
  geom_boxplot() + 
  labs(x = '',
       y = 'County Cases per 100k',
       title = 'COVID Cases Around College Start of Semester',
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


##----------combined visualziations------------

# COMBINED box plot - no colleges plus one college

colors <- c("Sepal Width" = "blue", "Petal Length" = "red", "Petal Width" = "orange")

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
       title = 'COVID Cases Around College Start of Semester',
       color = 'National COVID Cases per 100k') +
  guides(color = FALSE) +
  theme_minimal() +
  scale_color_manual(values = "red", label = "We are red points")

# 3 rows that are weirdly high!! - currently dropping them 


  