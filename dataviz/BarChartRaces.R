# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(tidyverse)
library(gganimate)
library(lubridate)
library(gifski)
library(png)
library(stringr)
library(RColorBrewer)
library(base)
library(chron)

# download and unzip data
download.file("https://storage.googleapis.com/transparencyreport/google-political-ads-transparency-bundle.zip", destfile = "google.zip")
unzip("google.zip", overwrite = TRUE)
file.remove("google.zip")

#load data
advertiser_data <- read_csv("google-political-ads-transparency-bundle/google-political-ads-advertiser-stats.csv")

#create dataframe for bar chart race
advertisers <- advertiser_data %>%
  select(Advertiser_ID, Advertiser_Name, Total_Creatives, Spend_USD, Regions, Public_IDs_List) %>%
  filter(Regions == "US") %>%
  group_by(Advertiser_ID) %>%
  arrange(desc(Spend_USD))


# maybe just fixing the names first then filtering out NAs and false positives is a better start.
presidential_candidates <- advertisers %>%
  mutate(candidate = case_when(grepl("TRUMP",Advertiser_Name, ignore.case = TRUE) ~ "Trump",
                               grepl("Bennet",Advertiser_Name, ignore.case = TRUE) ~ "Bennet",
                               grepl("BIDEN",Advertiser_Name, ignore.case = TRUE) ~ "Biden",
                               grepl("Bloomberg",Advertiser_Name, ignore.case = TRUE) ~ "Bloomberg",
                               grepl("Booker",Advertiser_Name, ignore.case = TRUE) ~ "Booker",
                               grepl("Bullock",Advertiser_Name, ignore.case = TRUE) ~ "Bullock",
                               grepl("Buttigieg",Advertiser_Name, ignore.case = TRUE) ~ "Buttigieg",
                               grepl("Castro",Advertiser_Name, ignore.case = TRUE) ~ "Castro",
                               grepl("Delaney",Advertiser_Name, ignore.case = TRUE) ~ "Delaney",
                               grepl("Kamala",Advertiser_Name, ignore.case = TRUE) ~ "Harris",
                               grepl("Harris",Advertiser_Name, ignore.case = TRUE) ~ "Harris",
                               grepl("Gabbard",Advertiser_Name, ignore.case = TRUE) ~ "Gabbard",
                               grepl("Tulsi",Advertiser_Name, ignore.case = TRUE) ~ "Gabbard",
                               grepl("Klobuchar",Advertiser_Name, ignore.case = TRUE) ~ "Klobuchar",
                               grepl("Messam",Advertiser_Name, ignore.case = TRUE) ~ "Messam",
                               grepl("Beto",Advertiser_Name, ignore.case = TRUE) ~ "O'Rourke",
                               grepl("O'Rourke",Advertiser_Name, ignore.case = TRUE) ~ "O'Rourke",
                               grepl("Deval",Advertiser_Name, ignore.case = TRUE) ~ "Patrick",
                               grepl("Patrick",Advertiser_Name, ignore.case = TRUE) ~ "Patrick",
                               grepl("Sanders",Advertiser_Name, ignore.case = TRUE) ~ "Sanders",
                               grepl("Bernie",Advertiser_Name, ignore.case = TRUE) ~ "Sanders",
                               grepl("Sestak",Advertiser_Name, ignore.case = TRUE) ~ "Sestak",
                               grepl("Steyer",Advertiser_Name, ignore.case = TRUE) ~ "Steyer",
                               grepl("Warren",Advertiser_Name, ignore.case = TRUE) ~ "Warren",
                               grepl("Williamson",Advertiser_Name, ignore.case = TRUE) ~ "Williamson",
                               grepl("Yang",Advertiser_Name, ignore.case = TRUE) ~ "Yang",
                               grepl("Weld",Advertiser_Name, ignore.case = TRUE) ~ "Weld",
                               grepl("Walsh",Advertiser_Name, ignore.case = TRUE) ~ "Walsh",
                               grepl("Ojeda",Advertiser_Name, ignore.case = TRUE) ~ "Ojeda",
                               grepl("Swalwell",Advertiser_Name, ignore.case = TRUE) ~ "Swalwell",
                               grepl("Gravel",Advertiser_Name, ignore.case = TRUE) ~ "Gravel",
                               grepl("Hickenlooper",Advertiser_Name, ignore.case = TRUE) ~ "Hickenlooper",
                               grepl("Inslee",Advertiser_Name, ignore.case = TRUE) ~ "Inslee",
                               grepl("Moulton",Advertiser_Name, ignore.case = TRUE) ~ "Moulton",
                               grepl("de Blasio",Advertiser_Name, ignore.case = TRUE) ~ "de Blasio",
                               grepl("Tim Ryan",Advertiser_Name, ignore.case = TRUE) ~ "Ryan",))

                              

# filter out false positives and non candidates, rename spending
presidential_candidates <- presidential_candidates %>%
  filter(!Advertiser_ID %in% c("AR413965028346036224",
                               "AR77652733833773056",
                               "AR405669762870280192",
                               "AR542349503563300864",
                               "AR264932274514952192",
                               "AR564078052351410176",
                               "AR425191042065629184",
                               "AR104374439801520128",
                               "AR328428521263202304",
                               "AR492809907662225408",
                               "AR76226942130454528",
                               "AR148284536168382464",
                               "AR389162245046665216",
                               "AR1128098930098176",
                               "AR220615908601626624",
                               "AR150429820793126912",
                               "AR468571723583717376",
                               "AR109214489986990080",
                               "AR463311797395390464",
                               "AR41964922858373120",
                               "AR34365098487185408",
                               "AR409596531209928704",
                               "AR562645732297801728",
                               "AR368870002041815040",
                               "AR27562076448751616",
                               "AR128771846907625472",
                               "AR193330771644121088",
                               "AR317552495838625792",
                               "AR95367240546779136",
                               "AR372368922919305216",
                               "AR430105103127543808",
                               "AR507374725878513664",
                               "AR509365735277985792",
                               "AR164940831659130880",
                               "AR364406122271997952",
                               "AR254394761232777216",
                               "AR77897993646243840",
                               "AR106892802465464320",
                               "AR280887219026132992",
                               "AR308824984854200320",
                               "AR232258568508669952",
                               "AR546237307959640064",
                               "AR254394761232777216")) %>%
  filter(!is.na(candidate)) %>%
  rename(total_spending = Spend_USD)
  

#load weekly data
weekly_data <- read_csv("google-political-ads-transparency-bundle/google-political-ads-advertiser-weekly-spend.csv") 

#tidy weekly data
weekly_data <- weekly_data %>%
  select(Advertiser_ID, Advertiser_Name, Week_Start_Date, Spend_USD) %>%
  rename(weekly_spending = Spend_USD)

#join presidential and weekly data
weekly_presidential_advertising <- left_join(presidential_candidates, weekly_data, by = c("Advertiser_ID" = "Advertiser_ID", "Advertiser_Name" = "Advertiser_Name"))

#see if there are NA values any week and replace w zeros
weekly_presidential_advertising <- weekly_presidential_advertising %>%
  spread(Week_Start_Date, weekly_spending)


weekly_presidential_advertising[is.na(weekly_presidential_advertising)] = 0

#make sure the number of columns is correct for gathering
weekly_presidential_advertising <- weekly_presidential_advertising %>%
  gather(Week_Start_Date, weekly_spending, 8:length(weekly_presidential_advertising))

glimpse(weekly_presidential_advertising)

weekly_presidential_advertising <- weekly_presidential_advertising %>%
  mutate(Week_Start_Date = (as.Date(Week_Start_Date, format = "%Y-%m-%d")))


#combine spending for duplicate campaigns & do cumulative spending column
weekly_presidential_advertising <- weekly_presidential_advertising %>%
  group_by(Week_Start_Date, candidate) %>%
  summarize(weekly_spending = sum(weekly_spending, na.rm = TRUE))

weekly_presidential_advertising <- weekly_presidential_advertising %>%
  group_by(candidate) %>%
  mutate(cumulative_spending = cumsum(weekly_spending))


# assign ranks by cumulative spending each week 
weekly_presidential_advertising <- weekly_presidential_advertising %>%
  group_by(Week_Start_Date) %>%
  arrange(-cumulative_spending) %>%
  mutate(rank = row_number()) %>%
  filter(!is.na(Week_Start_Date)) %>%
  mutate(formatted_date = format(as.character(Week_Start_Date)))
glimpse(weekly_presidential_advertising)


# combined parties and assigned colors
dems_and_reps <- weekly_presidential_advertising %>%
  mutate(party = (recode(candidate, Trump = "R", Weld = "R", Walsh = "R", .default = "D")))

#to do this I had to install this new package
#which changes colors for every geometry you add after
#and then add dems and reps as separate geometry. 
install.packages("ggnewscale")

library(ggnewscale)

dems_and_reps_chart <- ggplot(dems_and_reps, aes(x = -rank, group = candidate)) +
  geom_tile(data = filter(dems_and_reps, party == "D"),
            aes(y = cumulative_spending/2, 
                height = cumulative_spending,
                fill = cumulative_spending,
                width = 0.9)) +
  geom_text(aes(label = candidate, y = -100000), 
            hjust = "right",
            nudge_y = -100000,
            check_overlap = TRUE) +
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       na.value = "white",
                       name = "cumulative_spending") +
  new_scale_fill() +
  geom_tile(data = filter(dems_and_reps, party == "R"),
            aes(y = cumulative_spending/2, 
                height = cumulative_spending,
                fill = cumulative_spending,
                width = 0.9)) +
  geom_text(aes(label = candidate, y = -100000), 
            hjust = "right",
            nudge_y = -100000,
            check_overlap = TRUE) +
  geom_text(aes(y = cumulative_spending,
                label = dollar(cumulative_spending)),
            hjust = "left",
            nudge_y = 100000) +
  scale_fill_distiller(palette = "Reds",
                       direction = 1,
                       na.value = "white",
                       name = "cumulative_spending") +
  coord_flip() +
  scale_x_discrete("") +
  scale_y_continuous("", labels = dollar) +
  theme_void() +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 15)) +
  transition_time(Week_Start_Date) +
  ease_aes("linear") +
  labs(subtitle = "Week of {format(frame_time, '%b %d, %Y')}",
       caption='Source: Google Ad Transparency Database')

animate(dems_and_reps_chart,nframes = 750, width = 700, height = 400, end_pause = 30)
anim_save("pres_bar_chart_thumb.gif")

animate(dems_and_reps_chart, nframes = 750, fps = 25, width = 1500, height = 900, end_pause = 30)
anim_save("pres_bar_chart_race.gif")
# nframes = 750, fps = 25,

# make a static ggplot chart
pres_bar_race <- ggplot(weekly_presidential_advertising, aes(x = -rank, group = candidate)) +
  geom_tile(aes(y = cumulative_spending/2, 
                height = cumulative_spending,
                fill = cumulative_spending,
                width = 0.9)) +
  geom_text(aes(label = candidate, y = -100000), 
            hjust = "right",
            nudge_y = -100000,
            check_overlap = TRUE) +
  geom_text(aes(y = cumulative_spending,
                label = dollar(cumulative_spending)),
            hjust = "left",
            nudge_y = 100000) +
  scale_fill_distiller(palette = "RdBu",
                       direction = -1,
                       na.value = "white",
                       name = "cumulative_spending") +
  coord_flip() +
  scale_x_discrete("") +
  scale_y_continuous("", labels = dollar) +
  theme_void() +
  theme(legend.position = "none") +
  transition_time(Week_Start_Date) +
  ease_aes("linear") +
  labs(title='Google Ad Spending in the 2020 Presidential Race',
       subtitle='Week of: {formatted_date}',
       caption='Source: Google Ad Transparency Database')


animate(pres_bar_race, width = 1200, height = 900, end_pause = 30)
anim_save("pres_bar_chart_race.gif")

#make version just for dems
dem_weekly_spending <- weekly_presidential_advertising %>%
  filter(!candidate %in% c("Walsh","Trump","Weld")) %>%
  group_by(Week_Start_Date) %>%
  arrange(-cumulative_spending) %>%
  mutate(rank = row_number())

dem_weekly_spending$cumulative_spending <- as.numeric(dem_weekly_spending$cumulative_spending)
glimpse(dem_weekly_spending)


dem_primary_race <- ggplot(dem_weekly_spending, aes(x = -rank, group = candidate)) +
  geom_tile(aes(y = cumulative_spending/2, 
                height = cumulative_spending,
                fill = cumulative_spending,
                width = 0.9)) +
  geom_text(aes(label = candidate, y = -100000), 
            hjust = "right",
            nudge_y = -100000,
            check_overlap = TRUE) +
  geom_text(aes(y = cumulative_spending,
                label = dollar(cumulative_spending)),
            hjust = "left",
            nudge_y = 100000) +
  scale_fill_distiller(palette = "Blues", 
                       direction = 1,
                       na.value = "white",
                       name = "cumulative_spending") +
  coord_flip() +
  scale_x_discrete("") +
  scale_y_continuous("", labels = dollar) +
  theme_void() +
  theme(legend.position = "none") +
  transition_time(Week_Start_Date) +
  ease_aes("linear") +
  labs(subtitle="Week of {format(frame_time, '%b %d, %Y')}",
       caption='Source: Google Ad Transparency Database')

animate(dem_primary_race, nframes = 750, fps = 25, width = 1400, height = 900, end_pause = 30)
anim_save("dem_primary_race.gif")


#keyword chart
# load keyword data
keywords <- read_csv("google-political-ads-transparency-bundle/google-political-ads-top-keywords-history.csv")

# create data frames for each keyword ranking
keyword_1 <- keywords %>%
  select(Report_Date, Keyword_1, Spend_USD_1)

keyword_1 <- keyword_1 %>%
  rename(keyword = Keyword_1,
         spending = Spend_USD_1) 

keyword_2 <- keywords %>%
  select(Report_Date, Keyword_2, Spend_USD_2) %>%
  rename(keyword = Keyword_2) %>%
  rename(spending = Spend_USD_2)

keyword_3 <- keywords %>%
  select(Report_Date, Keyword_3, Spend_USD_3) %>%
  rename(keyword = Keyword_3) %>%
  rename(spending = Spend_USD_3)

keyword_4 <- keywords %>%
  select(Report_Date, Keyword_4, Spend_USD_4) %>%
  rename(keyword = Keyword_4) %>%
  rename(spending = Spend_USD_4)

keyword_5 <- keywords %>%
  select(Report_Date, Keyword_5, Spend_USD_5) %>%
  rename(keyword = Keyword_5) %>%
  rename(spending = Spend_USD_5)

keyword_6 <- keywords %>%
  select(Report_Date, Keyword_6, Spend_USD_6) %>%
  rename(keyword = Keyword_6) %>%
  rename(spending = Spend_USD_6)

# combine in one long format  data frame by appending
keywords <- bind_rows(keyword_1,keyword_2,keyword_3,keyword_4,keyword_5,keyword_6)

# resolve multiple versions of beto
keywords <- keywords %>%
  mutate(keyword = case_when(grepl("beto",keyword) ~ "beto o'rourke",
                             TRUE ~ keyword)) %>%
  group_by(Report_Date,keyword) %>%
  summarize(spending = sum(spending, na.rm = TRUE))

#spread to wide format on report date, introdices NAs, replace them with zeros
keywords <- keywords %>%
  spread(Report_Date,spending)
keywords[is.na(keywords)] = 0

# back to long format with gather
keywords <- keywords %>%
  gather(Report_Date,spending, -keyword,2:length(keywords))

# calculate cumulative spending
keywords <- keywords %>%
  group_by(keyword) %>%
  mutate(cumulative_spending = cumsum(spending))

# make sure report dates are dates
keywords <- keywords %>%
  mutate(Report_Date = ymd(Report_Date),
         date = format(Report_Date,"%b %d, %Y"))

# assign ranks by cumulative spending each week
keywords <- keywords %>%
  group_by(Report_Date) %>%
  arrange(-cumulative_spending) %>%
  mutate(rank = row_number(),
         keyword = str_to_title(keyword),
         keyword = gsub("Beto O'rourke","Beto O'Rourke", keyword),
         keyword = gsub("Aclu","ACLU", keyword))

glimpse(keywords)

bar_chart_spending <- ggplot(keywords, aes(x = -rank, group = keyword)) +
  geom_tile(aes(y = cumulative_spending/2, 
                height = cumulative_spending, 
                fill = cumulative_spending,
                width = 0.9)) +
  geom_text(aes(label = keyword, y = -100000), 
            hjust = "right",
            nudge_y = -400000,
            check_overlap = TRUE) +
  geom_text(aes(y = cumulative_spending,
                label = dollar(cumulative_spending)),
            hjust = "left",
            nudge_y = 100000) +
  coord_flip() +
  scale_fill_distiller(palette = "Greens",
                       direction = 1,
                       na.value = "white",
                       name = "cumulative_spending") +
  scale_x_discrete("") +
  scale_y_continuous("", labels = dollar) +
  theme_void(base_size = 16, base_family = "Arial") +
  theme(legend.position = "none") +
  transition_time(Report_Date) +
  ease_aes("linear") +
  labs(subtitle = "Week of {format(frame_time, '%b %d, %Y')}",
       caption='Source: Google Transparency Report')


animate(bar_chart_spending, nframes = 750, fps = 25, width = 1800, height = 900, end_pause = 30)
anim_save("keyword_bar_chart_race.gif")



