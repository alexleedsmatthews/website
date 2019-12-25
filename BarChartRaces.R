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

#filter for presidential candidates (can I use grepl to return a match in another dataframe? like if i had a csv of presidentila candidates?)
# Included unusual first names to capture advertisers like 'Beto For America' and candidates who dropped out

#but this grepl filter didn't work it was too long. so I did a few names at a time
# y <- advertisers %>%
#   filter(grepl("Bennet|Beto|Bloomberg|Biden|Booker|Bullock|Buttigieg|Castro|Delaney|Gabbard|Gillibrand|Tulsi||Harris|Kamala|Klobuchar|Messam|Patrick|Deval|Sanders|Sestak|Steyer|Warren|Williamson|Yang|Sanford|Trump|Walsh|Weld", Advertiser_Name, ignore.case = TRUE))

# presidential_advertisers <- advertisers %>%
#   filter(grepl("Bennet|Beto|Bloomberg|Biden|Booker|Bullock|Buttigieg|Castro|Delaney",Advertiser_Name,ignore.case = TRUE))
# 
# presidential_advertisers2 <- advertisers %>%
#   filter(grepl("Bennet|Beto|Bloomberg|Biden|Booker|Bullock|Buttigieg|Castro|Delaney",Advertiser_Name,ignore.case = TRUE))
# test fuzzy join to get presidential candidates

# presidential_candidates <- read_csv("pres-candidates.csv")
# 
# 
# x <- advertisers %>%
#   fuzzy_left_join(presidential_candidates, by = c("Advertiser_Name", "last"), match_fun = str_detect)
# 
# y <- advertisers %>%
#   regex_left_join(presidential_candidates, by = c(Advertiser_Name = "last"))
# 
# z <- left_join(advertiser_data, presidential_candidates, by = str_detect(advertiser_data$Advertiser_Name, presidential_candidates$last))
# 
# 
# # x %>% regex_inner_join(y, by = c(string = "seed"))
# # x %>% fuzzy_inner_join(y, by = c("string" = "seed"), match_fun = str_detect)
# # inner_join(x, y, by=stringr::str_detect(x$string, y$seed))

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
                               "AR468571723583717376")) %>%
  filter(!is.na(candidate)) %>%
  rename(total_spending = Spend_USD)
  
  
  # summarize(spending = sum(total_spending, na.rm = TRUE))


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

# #see which advertisers are false positive presidential
# pres_advertisers <- weekly_presidential_advertising %>%
#   group_by(Advertiser_Name, Advertiser_ID) %>%
#   tally()

# # resolve multiple versions of trump
# test <- weekly_presidential_advertising %>%
#   mutate(Advertiser_Name = case_when(grepl("trump",Advertiser_Name) ~ "TRUMP",
#                             TRUE ~ Advertiser_Name)) %>%
#   group_by(Week_Start_Date,Advertiser_Name) %>%

#combine spending for duplicate campaigns & do cumulative spending column
weekly_presidential_advertising <- weekly_presidential_advertising %>%
  group_by(Week_Start_Date, candidate) %>%
  summarize(weekly_spending = sum(weekly_spending, na.rm = TRUE))

weekly_presidential_advertising <- weekly_presidential_advertising %>%
  group_by(candidate) %>%
  mutate(cumulative_spending = cumsum(weekly_spending))

#count number of weeks
# test <- weekly_presidential_advertising %>%
#   group_by(candidate) %>%
#   tally()
# nframes = 78*5

# # calculate cumulative spending, filter out false positives, rename remaining for candidates
# weekly_presidential_advertising <- weekly_presidential_advertising %>%
#   group_by(Advertiser_Name) %>%
#   mutate(cumulative_spending = cumsum(weekly_spending))

# assign ranks by cumulative spending each week 
weekly_presidential_advertising <- weekly_presidential_advertising %>%
  group_by(Week_Start_Date) %>%
  arrange(-cumulative_spending) %>%
  mutate(rank = row_number()) %>%
  filter(!is.na(Week_Start_Date)) %>%
  mutate(formatted_date = format(as.character(Week_Start_Date)))
glimpse(weekly_presidential_advertising)

# test <- weekly_presidential_advertising %>%
#   group_by(Week_Start_Date) %>%
#   arrange(-cumulative_spending) %>%
#   mutate(rank = row_number()) %>%
#   filter(!is.na(Week_Start_Date)) %>%
#   mutate(formatted_date = format(as.character(Week_Start_Date)))
# glimpse(test)
# test <- test %>%
#   mutate(new_date = chron(formatted_date, format = "Y-m-d", out.format = "mmm d, y"))
# warnings()

# combined parties and assigned colors
dems_and_reps <- weekly_presidential_advertising %>%
  mutate(party = (recode(candidate, Trump = "R", Weld = "R", Walsh = "R", .default = "D")))

#to do this I had to install this new package
#which changes colors for every geometry you add after
#and then add dems and reps as separate geometry. 
#I'm sure there's a more elegant way to do it
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
  theme(legend.position = "none") +
  transition_time(Week_Start_Date) +
  ease_aes("linear") +
  labs(subtitle = "Week of {format(frame_time, '%b %d, %Y')}",
       caption='Source: Google Ad Transparency Database')

# library(extrafont)
# font_import()
animate(dems_and_reps_chart, width = 1200, height = 900, end_pause = 30)


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


# pres_bar_race <- ggplot(weekly_presidential_advertising, aes(rank)) +
#   geom_tile(aes(y = cumulative_spending / 2,
#     height = cumulative_spending,
#     width = 0.9,
#     fill = Advertiser_Name
#   ), alpha = 0.8, color = NA) +
#   geom_text(aes(y = 0, label = paste(Advertiser_Name, " ")), size = 12, vjust = 0.2, hjust = 1) +
#   geom_text(aes(y = cumulative_spending, label = Value_lbl, hjust = 0)) +
#   coord_flip(clip = "off", expand = FALSE) +
#   scale_x_discrete("") +
#   scale_y_continuous("",labels = dollar) +
#   transition_time(Week_Start_Date) + 
#   ease_aes("linear")

animate(pres_bar_race, width = 1200, height = 900, end_pause = 30)
anim_save("pres_bar_chart_race.gif")
# nframes = 750, fps = 25,
#can I make nframes a calculated number (like 3 frames per week?), 
# so like nframes = (tally(weekly_presidential_advertising$Week_Start_Date) * 3?


#make version just for d's
dem_weekly_spending <- weekly_presidential_advertising %>%
  filter(!candidate %in% c("Walsh","Trump","Weld")) %>%
  group_by(Week_Start_Date) %>%
  arrange(-cumulative_spending) %>%
  mutate(rank = row_number())

dem_weekly_spending$cumulative_spending <- as.numeric(dem_weekly_spending$cumulative_spending)
glimpse(dem_weekly_spending)
# dem primary chart

# nb.cols <- 24
# demblues <- colorRampPalette(brewer.pal(24, "Blues"))(nb.cols)
                                                    
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
  # scale_fill_brewer(type = "seq", palette = "Blues", direction = 1,
  #                   aesthetics = "fill") +
  # scale_colour_manual(values=rep(brewer.pal(9,"Blues"),times=3))
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


#create dataframe for flourish and reformat dates
flourish_df <- weekly_presidential_advertising %>%
  select(Week_Start_Date, candidate, weekly_spending) %>%
  arrange(Week_Start_Date)%>%
  spread(Week_Start_Date, weekly_spending)

flourish_df_2 <- weekly_presidential_advertising %>%
  select(Week_Start_Date, candidate, cumulative_spending) %>%
  arrange(Week_Start_Date) %>%
  spread(Week_Start_Date, cumulative_spending)
  
write.csv(flourish_df_2, "flourish_presidential.csv")




