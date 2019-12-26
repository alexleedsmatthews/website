
# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(tidyverse)

# download and unzip data
download.file("https://storage.googleapis.com/transparencyreport/google-political-ads-transparency-bundle.zip", destfile = "google.zip")
unzip("google.zip", overwrite = TRUE)
file.remove("google.zip")

#targeting data?
#showing change in content and ad buys over time?
#loop gif change over time--retrospetive 2016 election? trump something? events in the last few years (kavanaugh hearings, impeachment inquiry, mueller report, testimony, etc.)
# heat map of ad dollars at these crucial moments
# something about disinformation? not sure how to analyze this given text is in images

#load data
advertiser_data <- read_csv("google-political-ads-transparency-bundle/google-political-ads-advertiser-stats.csv")

#load targeting data 
#how do I parse dates correctly and also deal with funky age characters in R?
targeting_data <- read_csv("google-political-ads-transparency-bundle/google-political-ads-campaign-targeting.csv")

#load ad stats
ad_data <- read_csv("google-political-ads-transparency-bundle/google-political-ads-creative-stats.csv")

#looking at biggest spenders
# biggest_spenders <- advertiser_data %>%
#   select(Advertiser_Name, Advertiser_ID, Total_Creatives, Spend_USD, Regions) %>%
#   filter(Regions == "US") %>%
#   group_by(Advertiser_Name) %>%
#   arrange(desc(Spend_USD)) %>%
#   head(20) %>%
#   mutate(total_spending = Spend_USD)
# 
#load time data
weekly_spending <- read_csv("google-political-ads-transparency-bundle/google-political-ads-advertiser-weekly-spend.csv")

# #making the dataframes simpler for the join
# weekly_spending <- weekly_spending %>%
#   select(Advertiser_ID, Advertiser_Name, Week_Start_Date, Spend_USD)
# 
# biggest_spenders <- biggest_spenders %>%
#   select(Advertiser_Name, Advertiser_ID, Total_Creatives, total_spending)
# 
# big_spenders_weekly <- inner_join(biggest_spenders, weekly_spending)

# load translation table
advertiser_categories <- read_csv("category_table.csv")

# # sum spend for week by category for the 20 biggest spenders overall
# big_spenders_weekly <- left_join(big_spenders_weekly, advertiser_categories, by = "Advertiser_Name") 
# 
# big_spenders_weekly_sum <- big_spenders_weekly %>%
#   group_by(Category, Week_Start_Date) %>%
# #   summarize(Spend_USD = sum(Spend_USD, na.rm = TRUE)/10^6)
# 
# # color palette for categories
# pal <- c("#90d65a","#244C96", "#ffe12f", "#df3a3a")

# # line chart of spending by categories
# ggplot(big_spenders_weekly_sum, aes(x = Week_Start_Date, y = Spend_USD)) + 
#   theme_minimal(base_size = 14, base_family = "Georgia") + 
#   geom_line(aes(color = Category, Week_Start_Date)) +
#   xlab("Date") +
#   ylab("Spending ($ millions)") +
#   scale_color_manual(values = pal,
#                      breaks = c("Republican",
#                                 "Democratic",
#                                 "Issue",
#                                 "Clickbait")) +
#   geom_hline(yintercept = 0, size = 0.3)

# # bar chart of spending by categories in the buildup to midterms
# midterms_only <- big_spenders_weekly %>%
#   select(Advertiser_Name, Advertiser_ID, Total_Creatives, total_spending, Week_Start_Date, Spend_USD, Category, Campaign) %>%
#   filter(Week_Start_Date > "2018-06-01" & Week_Start_Date < "2018-11-07") %>%
#   group_by(Category) %>%
#   summarize(group_spending = sum(Spend_USD, na.rm = TRUE)/10^6)
# 
# ggplot(midterms_only, aes(x = reorder(Category,group_spending), y = group_spending)) +
#   theme_minimal(base_size = 14, base_family = "Georgia") + 
#   geom_col(aes(fill=Category)) +
#   xlab("") +
#   ylab("Spending ($ millions)") +
#   scale_fill_manual(values = pal, guide = FALSE) +
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank()) +
#   coord_flip()
# 
# # that spending over time 
# ggplot(big_spenders_weekly_sum, aes(x = Week_Start_Date, y = Spend_USD)) + 
#   theme_minimal(base_size = 14, base_family = "Georgia") + 
#   geom_line(aes(color = Category, Week_Start_Date)) +
#   xlab("Date") +
#   ylab("Spending ($ millions)") +
#   scale_x_date(limits = c(as.Date("2018-06-01"),as.Date("2018-11-07"))) +
#   scale_color_manual(values = pal,
#                      breaks = c("Republican",
#                                 "Democratic",
#                                 "Issue",
#                                 "Clickbait")) +
#   geom_hline(yintercept = 0, size = 0.3)
# 
# ## ^ having already made this chart higher, can just set limits on the x axis to display a ttime period
# 
# # midterms over time grouped by campaign
# midterms_by_campaign_type <- big_spenders_weekly %>%
#   select(Advertiser_Name, Advertiser_ID, Total_Creatives, total_spending, Week_Start_Date, Spend_USD, Category, Campaign) %>%
#   filter(Week_Start_Date > "2018-06-01" & Week_Start_Date < "2018-11-07") %>%
#   group_by(Week_Start_Date, Campaign) %>%
#   summarize(group_spending = sum(Spend_USD, na.rm = TRUE)/10^6)
# 
# ggplot(midterms_by_campaign_type, aes(x = Week_Start_Date, y = group_spending)) + 
#   geom_line(aes(color=Campaign)) + 
#   theme_minimal(base_size = 14, base_family = "Georgia") +
#   xlab("Date") +
#   ylab("Spending ($ millions)") +
#   geom_hline(yintercept = 0, size = 0.3)
#   
# load location data
geo_ad_data <- read_csv("google-political-ads-transparency-bundle/google-political-ads-geo-spend.csv")

us_adbuys_bystate <- geo_ad_data %>%
  select(Country, Country_Subdivision_Primary, Country_Subdivision_Secondary, Spend_USD) %>%
  filter(Country=="US") %>%
  group_by(Country_Subdivision_Primary) %>%
  summarize(state_spending = sum(Spend_USD, na.rm = TRUE))

adspending_bydistrict <- geo_ad_data %>%
  select(Country, Country_Subdivision_Primary, Country_Subdivision_Secondary, Spend_USD) %>%
  filter(Country=="US") %>%
  group_by(Country_Subdivision_Secondary) %>%
  summarize(district_spending = sum(Spend_USD, na.rm = TRUE))

###########
# get census data

library(tidycensus)
library(tigris)
library(sf)
library(stringr)

v17 <- load_variables(2017, "acs5", cache = TRUE)

# get estimated population by state
pop_by_state <- get_acs(geography = "state",
                        variables = c(population = "B00001_001"),
                        year = 2017,
                        geometry = TRUE,
                        cb = FALSE,
                        shift_geo = TRUE) # don't include last line if exporting to Mapbox


us_adbuys_bystate_map <- us_adbuys_bystate %>%
  separate(Country_Subdivision_Primary, into=c("country","state_abb"), sep = "-")

# process data for join
states <- tibble(state_abb = state.abb, state = state.name) 
dc <- tibble(state_abb = "DC", state = "District of Columbia")
states <- bind_rows(states,dc)

# joins
us_adbuys_bystate_map <- inner_join(us_adbuys_bystate_map,states, by = "state_abb") 
us_adbuys_bystate_map <- left_join(pop_by_state,us_adbuys_bystate_map, by = c("NAME" = "state"))

# calculate spend per person
us_adbuys_bystate_map <- us_adbuys_bystate_map %>%
  mutate(per_person_spend = round(state_spending/estimate,2))

# map
ggplot(us_adbuys_bystate_map) +
  geom_sf(color = "white", size = 0.2, aes(fill = per_person_spend)) +
  scale_fill_distiller(palette = "Blues", 
                       label = dollar, 
                       direction = 1,
                       name = "Spend per person") +
  theme_void()

# write out for mapbox
st_write(us_adbuys_bystate_map, "us_adbuys_bystate.map.geojson")

st_write(us_adbuys_bystate_map, "us_adbuys_bystate_mapbox.map.geojson")

#make map interactive


# make a geojson that doesn't have geoshift true in order to use the geojson in mapbox

# # get estimated population by congressional district
# pop_by_congressional_district <- get_acs(geography = "congressional district",
#                                          variables = c(population = "B00001_001"),
#                                          year = 2017)
# 
# congressional_map <- congressional_districts(cb = FALSE, year = 2017, class = "sf")
# 
# # process data for join
# states <- tibble(state_abb = state.abb, state = state.name)
# 
# pop_by_congressional_district <- pop_by_congressional_district %>%
#   separate(NAME, into = c("district", "state"), sep = ",") %>%
#   mutate(state = str_trim(state),
#          district = str_extract(district, "[0-9]+"),
#          district = gsub("115","AT LARGE", district)) %>%
#   inner_join(states) %>%
#   mutate(Country_Subdivision_Secondary = paste0(state_abb,"-",district))
# 
# # join and calculate ad spending per capita
# adspending_bydistrict <- inner_join(adspending_bydistrict,pop_by_congressional_district) %>%
#   mutate(spending_percap = district_spending/estimate)
# 
# # join to map
# adspending_bydistrict <- inner_join(congressional_map,adspending_bydistrict)
# 
# # map of spending by district (this is taking too long to plot)
# # ggplot(adspending_bydistrict, aes(fill = spending_percap)) +
# #   geom_sf(color = NA) + 
# #   coord_sf(crs = 5070) + # Albers projection
# #   scale_fill_distiller(palette = "Blues", 
# #                        direction = 1, 
# #                        na.value = "white",
# #                        name = "Ad spending per person",
# #                        labels = dollar) +
# #   theme_void()
# 
# # save as shapefile 
# st_write(adspending_bydistrict, "adspending_bydistrict/adspending_bydistrict.shp", delete_layer = TRUE)

#### haven't gone beyond here
# got rid of redundant lines of code and keyword data (working in a separate script)
