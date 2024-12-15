################################################################################
##########     INITIAL ANALYSIS THESIS 400     #################################
################################################################################

# Author: Alex Broening

###
install.packages("tidyverse")
install.packages("haven")
install.packages("ggplot2")
install.packages("sf")
install.packages("tidycensus")
install.packages("lwgeom")
install.packages("readxl")
install.packages("paletteer")
install.packages("ggrepel")
install.packages("Cairo")
install.packages("scales")

library (tidyverse)
library(haven)
library(ggplot2)
library(sf)
library(tidycensus)
library(lwgeom)
library(readxl)
library(ggrepel)
library(paletteer)
library(Cairo)
library(scales)
###SETUP

options(scipen = 999)
census_api_key("753915d84691b3657721d57a715b97feafbbf81a", install = T)
sf_use_s2(FALSE)

##FIX PATH FUNCTION FOR MAC/WINDOWS
fix_path <- function(fullpath) {
  #if windows path on mac
  if (startsWith(fullpath, "//hemisphere") & 
      Sys.info()[['sysname']] == "Darwin") {
    return(gsub("//hemisphere", "/Volumes", fullpath))
    #if mac path on pc
  } else if (startsWith(fullpath, "/Volumes") & Sys.info()[['sysname']] == "Windows") {
    return(gsub("/Volumes", "//hemisphere", fullpath))
  } else {
    return(fullpath)
  }
}

### CCES DATA
cces_2022 <- read.csv("H:/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/dataverse_files/CCES22_Common_OUTPUT_vv_topost.csv")

cces_all <- read_stata(fix_path("/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/cumulative_2006-2023.dta"))
#mac
cces_all <- read_stata(file.choose())


### GEOGRAPHY DATA
ru_urb <- read_sf("C:/Users/ab5nm/Desktop/EDGE_Locale21_US/edge_locale21_nces_all_us.shp")

###UAC DATA - URBAN AREAS, DONT ALIGN WITH COUNTIES
uac20 <- read_sf("C:/Users/ab5nm/Desktop/tl_2024_us_uac20/tl_2024_us_uac20.shp")

### RURAL-URBAN CONTINUUM CODES
continuumcodes <- read_excel("C:/Users/ab5nm/Desktop/Ruralurbancontinuumcodes2023.xlsx")

### API Call Function
## Calls the US Census American Community Survey API, with a number of user defined variables, to output a dataframe in wide format
acs_call <- function(yr = 2022, varlist = var_list, states = state_list, level = "tract", surv = "acs5") {
  #   Arguments:
  #     yr - accepts integers 2010-2022 (updated 07.04.2024), defines the end-year of multi-year surveys
  #     varlist - named vector of strings, recalling variables from the ACS
  #     states - accepts list of states or states fips codes. Defaults to list defined above with get_state_list()
  #     level - accepts string, defining level of geography the ACS estimates should evaluate -
  #         Options (small to large): "block group", "tract" (default), "county", "state", "us", "cbsa"
  #     surv - accepts string. Defines which dataset to pull from. 
  #         Options: "acs1", "acs3", "acs5" (default), for 1, 3, and 5 year estimates
  
  #   Loop through ACS query for each state given - allows for single pull of block group and tract estimates across states
  df <- map_dfr(states, ~{
    get_acs(
      geography = level,
      variables = varlist,
      survey = surv,
      year = yr,
      state = .x,
      output = "wide",
      geometry = TRUE
    )
  }, .id = "state"
  )
  
  #   Remove "state" placeholder column
  df$state = NULL
  return(df)
}

### DEFINE STATE LIST FUNCTION
get_state_list <- function(states = "contig") {
  #Loads all state fips codes
  all <- c("01","02","04","05","06","08","09","10","11","12","13","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53","54","55","56","60","66","69","72","74","78")
  if (states == "contig") {
    #removes non states, and hawaii and alaska
    return(all[-which(all %in% c(alaska = "02", 
                                 hawaii = "15",
                                 dc = "11",
                                 puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all") {
    #removes nothing
    return(all)  
  } else if (states == "all_states") {
    #removes nonstates
    return(all[-which(all %in% c(dc = "11",
                                 puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all_pr") {
    #removes non states, leaving Puerto Rico
    return(all[-which(all %in% c(dc = "11",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all_dc") {
    #removes non states, leaving District of Columbia
    return(all[-which(all %in% c(puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all_dc_pr") {
    #removes non states, leaving Puerto Rico and District of Columbia
    return(all[-which(all %in% c(guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "contig_dc") {
    #removes non states, removes Hawaii and Alaska, leaving District of Columbia
    return(all[-which(all %in% c(alaska = "02", 
                                 hawaii = "15",
                                 puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
    
  } 
}

### GET ALL STATES
state_list <- get_state_list("all_states")

#tracts <- acs_call(yr = 2021, varlist = c("pop" = "B01003_001"), level = "county")
tracts <- get_acs(year = 2021, variables = c("pop" = "B01003_001"), survey = "acs5", geography = "county", output = "wide", geometry = TRUE)
tracts <- tracts %>% 
  mutate(
    county_FIPS = substr(GEOID, 1, 7)
  )

###Spatial Join

tracts_ru <- tracts %>% 
  st_join(., ru_urb %>% select(LOCALE, geometry), join = st_intersects, largest = T)



### Mutate COUNTY FIPS
cces_all %>% 
  mutate(FIPS = as.numeric(paste0(st, county_fips))) %>% 
  select(year, case_id, FIPS)





### EXTRACT CODES FROM VALUES: 
as.data.frame(val_labels(cces_all$faminc))

### PARTIES CODES

#Democrat                                                 1
#Republican                                               2
#Other / Someone Else                                     3
#Did not Vote                                             4
#Not Sure / Don't Recall                                  5
#Other                                                    6

##URBANITY METRIC?

## group by urbanity, year
## produce summary - percetage dem vs rep
#ggplot


### DESCRIPTIVE
##NOTE LIMITIATION : NOT ALL SURVEY ROWS INCLUDE HAVING TAKEN THE POST SURVEY
cces_r_faminc <- cces_all %>% 
  # Filter to Election years
  filter(year %% 2 == 0) %>%
  # UNHELPFUL FAMINC LEVELS REMOVE
  filter(between(faminc, 1, 12)) %>% 
  #Group By Year
  group_by(year, faminc) %>% 
  # Get Cols for number of votes and number of republican votes
  summarize(
    gov_votes = sum(!is.na(voted_gov_party)),
    rep_votes = sum(!is.na(voted_rep_party)),
    sen_votes = sum(!is.na(voted_sen_party)),
    pres_votes = sum(!is.na(voted_pres_party)),
    gov_r_votes = sum(voted_gov_party == 2, na.rm = T),
    rep_r_votes = sum(voted_rep_party == 2, na.rm = T),
    sen_r_votes = sum(voted_sen_party == 2,na.rm = T),
    pres_r_votes = sum(voted_pres_party == 2, na.rm = T)
  ) %>% 
  ungroup() %>% 
  # Calculate Percentages, except Pres in non-pres elections
  mutate(
    gov_r_pct = gov_r_votes / gov_votes,
    rep_r_pct = rep_r_votes / rep_votes,
    sen_r_pct = sen_r_votes / sen_votes,
    pres_r_pct = ifelse(year %% 4 != 0, NA, pres_r_votes / pres_votes)
  ) %>% 
  # Calculate Averages
  mutate(
    r_avg = rowMeans(select(.,gov_r_pct, rep_r_pct, sen_r_pct, pres_r_pct), na.rm = T),
    r_avg_nogov = rowMeans(select(., rep_r_pct, sen_r_pct, pres_r_pct), na.rm = T)
  )

cces_r_faminc %>% 
  mutate(
    faminc = as.factor(faminc)
  ) %>% 
  mutate(faminc_labs = case_when(
    faminc == 1 ~ "< 10k",
    faminc == 2 ~ "10k - 20k",
    faminc == 3 ~ "20k - 30k",
    faminc == 4 ~ "30k - 40k",
    faminc == 5 ~ "40k - 50k",
    faminc == 6 ~ "50k  -60k",
    faminc == 7 ~ "60k - 70k",
    faminc == 8 ~ "70k - 80k",
    faminc == 9 ~ "80k - 100k",
    faminc == 10 ~ "100k - 120k",
    faminc == 11 ~ "120k - 150k",
    faminc == 12 ~ "> 150k",
  )) %>% 
  #pivot_longer(cols = c("r_avg", "r_avg_nogov"), values_to = "avg_val", names_to = "avg_type") %>% 
  ggplot(aes(x = year, y = r_avg, color = faminc_labs)) +
  geom_line() +
  scale_color_viridis_d()+
  #geom_smooth(formula = y~x, method = "lm",  se = FALSE)+
  labs(title = "Republican Voting Average by Family income Bracket Over Time", 
       y = "Republican aggregated voting average", 
       x = "Year", 
       subtitle = "Data from CCES")+
  facet_wrap(~fct_inorder(faminc_labs))+
  geom_smooth(linetype = "dashed", method = "lm", se = F, color = "red", linewidth = .5)+
  guides(color = "none") +
  theme(plot.subtitle = element_text(size = 10),
        plot.title = element_text(face = "bold"))

cces_r_faminc %>% 
  mutate(
    faminc = as.factor(faminc),
  ) %>% 
  #rename factors
  mutate(faminc_labs = case_when(
    faminc == 1 ~ "< 10k",
    faminc == 2 ~ "10k - 20k",
    faminc == 3 ~ "20k - 30k",
    faminc == 4 ~ "30k - 40k",
    faminc == 5 ~ "40k - 50k",
    faminc == 6 ~ "50k  -60k",
    faminc == 7 ~ "60k - 70k",
    faminc == 8 ~ "70k - 80k",
    faminc == 9 ~ "80k - 100k",
    faminc == 10 ~ "100k - 120k",
    faminc == 11 ~ "120k - 150k",
    faminc == 12 ~ "> 150k",
  )) %>% 
  #convert to sensical percent
  mutate(r_avg = 100 * r_avg) %>% 
  #plot it
  ggplot(aes(x = fct_inorder(faminc_labs), y = r_avg, group = year)) +
  geom_line() +
  facet_wrap(~year, nrow = 1)+
  scale_color_viridis_c() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_smooth(formula = y ~ x, se = F, linewidth = 0.5, linetype = "dashed", color = "red")+
  labs(x = "Income Group", 
       y = "Republican Voting Percentage", title = "Republican Base Income Curve Over Time")
  

### ON ONE PLOT
cces_r_faminc_refact <- cces_r_faminc |> 
  mutate(faminc_labs = case_when(
    faminc == 1 ~ "< 10k",
    faminc == 2 ~ "10k - 20k",
    faminc == 3 ~ "20k - 30k",
    faminc == 4 ~ "30k - 40k",
    faminc == 5 ~ "40k - 50k",
    faminc == 6 ~ "50k  -60k",
    faminc == 7 ~ "60k - 70k",
    faminc == 8 ~ "70k - 80k",
    faminc == 9 ~ "80k - 100k",
    faminc == 10 ~ "100k - 120k",
    faminc == 11 ~ "120k - 150k",
    faminc == 12 ~ "> 150k",
  )) |> 
  mutate(year = as.factor(year)) |> 
  mutate(r_avg = 100 * r_avg)
  
  

cces_r_faminc %>% 
  mutate(
    faminc = as.factor(faminc),
  ) %>% 
  filter(year %in% c(2008, 2012, 2016, 2020)) |> 
  mutate(year = as.factor(year)) %>% 
  #rename factors
  mutate(faminc_labs = case_when(
    faminc == 1 ~ "< 10k",
    faminc == 2 ~ "10k - 20k",
    faminc == 3 ~ "20k - 30k",
    faminc == 4 ~ "30k - 40k",
    faminc == 5 ~ "40k - 50k",
    faminc == 6 ~ "50k  - 60k",
    faminc == 7 ~ "60k - 70k",
    faminc == 8 ~ "70k - 80k",
    faminc == 9 ~ "80k - 100k",
    faminc == 10 ~ "100k - 120k",
    faminc == 11 ~ "120k - 150k",
    faminc == 12 ~ "> 150k",
  )) %>%
  #convert to sensical percent
  mutate(r_avg = 100 * r_avg) %>% 
  #plot
  ggplot(aes(x = fct_inorder(faminc_labs), y = r_avg, color = year, group = year)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.85, hjust = 1)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = F, linewidth = 0.5, linetype = "dashed", color = "red")+
  labs(x = "Income Group",
       y = "Republican Voting Percentage", 
       title = "Republican Base Income Curve Over Time",
       subtitle = "Data from CCES") +
  geom_text_repel(data = cces_r_faminc_refact |> filter(faminc == 12, year %in% c(2008, 2012, 2016, 2020)), 
                  aes(x = fct_inorder(faminc_labs), 
                      y = r_avg, 
                      label = year), 
                  max.overlaps = 50,
                  position = position_nudge_repel(x = .5, y = 0))+
  theme(axis.title.y = element_text(margin = margin(r = 10)),
        plot.subtitle = element_text(size = 10))+
  guides(color = "none")+
  scale_colour_brewer(palette = "Set1")
ggsave("singleplotincomecurves.png", width = 3000, height = 1800, units = "px", type = "cairo-png")



### BY REPORTED GEOGRAPHY
cces_r_faminc <- cces_all %>% 
  # Filter to Election years
  filter(year %% 2 == 0) %>%
  # UNHELPFUL FAMINC LEVELS REMOVE

  #Group By Year
  group_by(year, faminc) %>% 
  # Get Cols for number of votes and number of republican votes
  summarize(
    gov_votes = sum(!is.na(voted_gov_party)),
    rep_votes = sum(!is.na(voted_rep_party)),
    sen_votes = sum(!is.na(voted_sen_party)),
    pres_votes = sum(!is.na(voted_pres_party)),
    gov_r_votes = sum(voted_gov_party == 2, na.rm = T),
    rep_r_votes = sum(voted_rep_party == 2, na.rm = T),
    sen_r_votes = sum(voted_sen_party == 2,na.rm = T),
    pres_r_votes = sum(voted_pres_party == 2, na.rm = T)
  ) %>% 
  ungroup() %>% 
  # Calculate Percentages, except Pres in non-pres elections
  mutate(
    gov_r_pct = gov_r_votes / gov_votes,
    rep_r_pct = rep_r_votes / rep_votes,
    sen_r_pct = sen_r_votes / sen_votes,
    pres_r_pct = ifelse(year %% 4 != 0, NA, pres_r_votes / pres_votes)
  ) %>% 
  # Calculate Averages
  mutate(
    r_avg = rowMeans(select(.,gov_r_pct, rep_r_pct, sen_r_pct, pres_r_pct), na.rm = T),
    r_avg_nogov = rowMeans(select(., rep_r_pct, sen_r_pct, pres_r_pct), na.rm = T)
  )

testing <- cces_all %>% 
  # FILTER TO ELECTION YEARS
  filter(year == 2020) %>% 
  left_join(continuumcodes %>% select(FIPS, RUCC_2023, Description), by = c("county_fips" = "FIPS")) %>% 
  #filter unclassified counties
  filter(!is.na(RUCC_2023)) %>% 
  group_by(RUCC_2023) %>% 
  summarize(
    gov_votes = sum(!is.na(voted_gov_party)),
    rep_votes = sum(!is.na(voted_rep_party)),
    sen_votes = sum(!is.na(voted_sen_party)),
    pres_votes = sum(!is.na(voted_pres_party)),
    gov_r_votes = sum(voted_gov_party == 2, na.rm = T),
    rep_r_votes = sum(voted_rep_party == 2, na.rm = T),
    sen_r_votes = sum(voted_sen_party == 2,na.rm = T),
    pres_r_votes = sum(voted_pres_party == 2, na.rm = T)
  ) %>% 
  ungroup() %>% 
  # Calculate Percentages, except Pres in non-pres elections
  mutate(
    gov_r_pct = gov_r_votes / gov_votes,
    rep_r_pct = rep_r_votes / rep_votes,
    sen_r_pct = sen_r_votes / sen_votes,
    pres_r_pct = pres_r_votes / pres_votes
  ) %>% 
  # Calculate Averages
  mutate(
    r_avg = rowMeans(select(.,gov_r_pct, rep_r_pct, sen_r_pct, pres_r_pct), na.rm = T),
    r_avg_nogov = rowMeans(select(., rep_r_pct, sen_r_pct, pres_r_pct), na.rm = T)
  )
  




filepath <- choose.files()
anes <- read.csv(filepath)
anes <- read.csv(fix_path("/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/anes_timeseries_cdf_csv_20220916/anes_timeseries_cdf_csv_20220916.csv"))

anes %>% 
  filter()



anes_urbanity <- anes %>% 
  filter(VCF0004 <= 2000) %>% 
  select(VCF0004, VCF0006, VCF0111, VCF0301, VCF0114) %>% 
  mutate(
    rep = ifelse(VCF0301 %in% c(5,6,7), 1, 0),
    dem = ifelse(VCF0301 %in% c(1,2,3), 1, 0),
    ind = ifelse(VCF0301 %in% c(4), 1, 0)
  ) %>% 
  filter(!is.na(VCF0111)) %>% 
  group_by(VCF0004, VCF0111) %>% 
  summarize(
    total = n(),
    rep_pct = sum(rep) / total
  )


anes_urbanity %>% 
  filter(VCF0004 > 1980) %>% 
  filter(VCF0111 != 0) %>% 
  mutate(urbanity = case_when(
    VCF0111 == 1 ~ "Urban",
    VCF0111 == 2 ~ "Suburban",
    VCF0111 == 3 ~ "Rural"
  )) |> 
  ggplot(aes(x = as.factor(urbanity), y = rep_pct, color = as.factor(VCF0004), group = VCF0004))+
  geom_point(linewidth = 2) +
  labs(x = "Urbanity (Urban > Sub > Rural)",
       color = "Election Year",
       y = "Percent Republican Respondents",
       title = "Republican Voting Percentage by Urbanity Over Time 1982 - 2000",
       subtitle = "Data from ANES")+
  scale_color_viridis_d()+
  geom_smooth(method = "loess", se = F)+
  scale_colour_paletteer_d("MoMAColors::Exter")+
  theme(plot.subtitle = element_text(size = 10))



anes_income_byurban <- anes %>% 
  filter(VCF0004 <= 2000) %>% 
  select(VCF0004, VCF0006, VCF0111, VCF0301, VCF0114) %>% 
  mutate(
    rep = ifelse(VCF0301 %in% c(5,6,7), 1, 0),
    dem = ifelse(VCF0301 %in% c(1,2,3), 1, 0),
    ind = ifelse(VCF0301 %in% c(4), 1, 0)
  ) %>% 
  filter(!is.na(VCF0111)) %>% 
  group_by(VCF0004, VCF0114, VCF0111) %>% 
  summarize(
    total = n(),
    rep_pct = sum(rep) / total
  )

anes_income_byurban %>% 
  filter(VCF0004 > 1990) %>% 
  filter(VCF0111 != 0) %>% 
  ggplot()+
  geom_line(aes(x = as.factor(VCF0114), y = rep_pct, color = as.factor(VCF0004), group = VCF0004), linewidth = 2) +
  labs(x = "Income Group (Poor > Rich)", color = "year", y = "Percent Republican Respondents", title = "Republican Income Curves Over Time, by Urbanity", subtitle = "1 = Urban, 2 = Suburban, 3 = Rural")+
  scale_color_viridis_d()+
  facet_grid(VCF0004 ~ VCF0111)


counties_class <- read_csv(fix_path("/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_censusbureaucountyurbanity_11.10.2024.csv")) |> 
  mutate(
    fips = (paste0(STATE, COUNTY)),
    ...32 = NULL,
    ...33 = NULL
  ) |> 
  filter(!is.na(STATE))

cces_all_urb_pop <- cces_all %>% 
  left_join(counties_class, by = c("county_fips" = "fips")) %>% 
  mutate(
    rur_pct = as.numeric(substr(POPPCT_RUR, 1, nchar(POPPCT_RUR) -1)),
  )

test <- cces_all_urb_pop |> 
  group_by(county_fips, year) %>%
  summarize(
    rur_pct = first(rur_pct),
    faminc_classavg = mean(faminc),
    gov_votes = sum(!is.na(voted_gov_party)),
    rep_votes = sum(!is.na(voted_rep_party)),
    sen_votes = sum(!is.na(voted_sen_party)),
    pres_votes = sum(!is.na(voted_pres_party)),
    gov_r_votes = sum(voted_gov_party == 2, na.rm = T),
    rep_r_votes = sum(voted_rep_party == 2, na.rm = T),
    sen_r_votes = sum(voted_sen_party == 2,na.rm = T),
    pres_r_votes = sum(voted_pres_party == 2, na.rm = T),
    ct = n()
  ) %>%
  mutate(
    gov_r_pct = gov_r_votes / gov_votes,
    rep_r_pct = rep_r_votes / rep_votes,
    sen_r_pct = sen_r_votes / sen_votes,
    pres_r_pct = ifelse(year %% 4 != 0, NA, pres_r_votes / pres_votes)
  ) 
    

test |> 
  filter(year %% 2 == 0) |> 
  ggplot(aes(x = rur_pct, y = rep_r_pct, color = as.factor(year), group = year)) +
  geom_smooth(se = F) +
  labs(title = "Smoothed Republican House Vote Share By Percent Rural Population Over Time",
       subtitle = "Data from CCES and U.S. Census Bureau",
       x = "Percent Rural Population",
       y = "House Election Republican Vote Share",
       color = "Election Year")+
  theme(plot.subtitle = element_text(size = 10),
        panel.background = element_rect(fill = "#F5F5F5"))+
  paletteer::scale_color_paletteer_d("trekcolors::enara", direction = -1) +
  scale_y_continuous(labels = scales::percent_format())+
  theme(axis.title.y = element_text(margin = margin(r = 10)))
ggsave(filename = "graph_voteshareruraltime_12.12.2024.png", width = 3000, height = 1800, units = "px", type = "cairo-png")


### CAUTION: LOW RSQAURED
test |> 
  mutate(
    cut_rur = cut(rur_pct, c(0, 20, 40, 60, 80, 100), labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"))
  ) |> 
  filter(!is.na(cut_rur)) |> 
  filter(year > 2000) |> 
  filter(year %% 4 == 0) %>% 
  mutate(
    year = as.factor(year)
  ) %>% 
  ggplot(aes(x = faminc_classavg, y = rep_r_pct, group = cut_rur, color = year))+
  geom_smooth(aes(group = year), se = F, method = "loess") +
  geom_point()+
  facet_wrap(~cut_rur, nrow = 1)+
  scale_color_viridis_d()+
  labs(title = "Republican House Voting Percent vs average Family Income class by County, Split by Rural Population %",y = "Republican House Voting Percent of County", x = "Average Family Income Class of County")

### House Vote by Income over time, wrapped by urbanity

test





county_results <- read.csv(fix_path("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/mit_countyreturns_2000-2020/countypres_2000-2020.csv")) |> 
  mutate(county_fips = as.character(county_fips),
         county_fips = ifelse(nchar(county_fips) < 5, paste0("0", county_fips), county_fips)
  ) |> 
  filter(party %in% c("DEMOCRAT", "REPUBLICAN"))

county_mhi <- get_acs(geography = "county", output = "wide", year = 2020, variables = c("mhi" = "B19013_001"), survey = "acs5")

testing <- county_results |> 
  select(-mode, -version) |> 
  group_by(county_fips, candidate, year) |> 
  summarize(
    party = first(party),
    votes = sum(candidatevotes), 
    total_votes = mean(totalvotes)
  ) |> 
  ungroup()  %>% 
  select(-candidate) |> 
  pivot_wider(names_from = party, values_from = votes) |> 
  left_join(county_mhi |> select(GEOID, mhiE), by = c("county_fips" = "GEOID")) |> 
  left_join(counties_class |> select(fips, POP_COU, POP_URB, POP_RUR), by = c("county_fips" = "fips")) |> 
  filter(!is.na(POP_URB)) |> 
  mutate(
    rural_pct = (POP_RUR / POP_COU) * 100,
    cut_rur = cut(rural_pct, c(0, 20, 40, 60, 80, 100), 
                  labels = c("0-20% Rural", 
                             "20-40% Rural", 
                             "40-60% Rural",
                             "60-80% Rural",
                             "80-100% Rural"), include.lowest = TRUE),
    rep_pct = (REPUBLICAN / total_votes) * 100 
  ) 
  
testing |> 
  as.data.frame() |> 
  ggplot(aes(x = mhiE, y = rep_pct, group = cut_rur))+
  geom_smooth() +
  geom_point(alpha = 0.1)+
  facet_wrap(~ cut_rur, nrow = 1)+
  labs(title = "2020 Republican Pres. Voting Percent vs average Family Income class by County, Split by Rural Population %",
       subtitle = "Data from MIT Election Data, American Community Survey, U.S. Census Bureau Urbanity Estimates",
       y = "Republican Pres. Voting Percent of County",
       x = "Median Family Income of County")+
  theme(plot.subtitle = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

testing %>% 
  as.data.frame() %>% 
  mutate(
    year = as.factor(year),
    mhi_k = mhiE / 1000
  ) %>% 
  ggplot(aes(x = mhi_k, y = rep_pct, color = year, group = cut_rur)) +
  geom_smooth(aes(group = year), se = F, linewidth = .8) +
  facet_wrap(~ cut_rur, nrow = 1)+
  scale_x_continuous(
    limits = c(-1, 150),
    breaks = c(0, 50, 100, 150)
  )+
  paletteer::scale_color_paletteer_d("rcartocolor::BurgYl", direction = 1)+
  labs(
    title = "Percent of Republican Votes by County Income over Time, Split by Rural Portion of Population",
    subtitle = "Data from MIT Election Data, American Community Survey, U.S. Census Bureau Urbanity Estimates",
    x = "County Median Household Income in 2022 (Thousands of Dollars)",
    y = "Percent of Ballots Cast for Republican Candidates",
    color = "Election Year"
  ) +
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
ggsave("mhivsrepublicanvotingcounty_splitbyurbanity.png", height = 1800, width = 3000, units = "px", type = "cairo-png")




  
  filter(VCF0004 %in% c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)) |> 
  select(VCF0004, VCF0006, VCF0111, VCF0301, VCF0114) %>% 
  mutate(
    rep = ifelse(VCF0301 %in% c(5,6,7), 1, 0),
    dem = ifelse(VCF0301 %in% c(1,2,3), 1, 0),
    ind = ifelse(VCF0301 %in% c(4), 1, 0)
  )  
  
  
###### RURAL/INCOME/VOTING
  
anes_income <- anes %>% 
  filter(VCF0004 >= 1980) %>% 
  select(VCF0004, VCF0006, VCF0111, VCF0301, VCF0114) %>% 
  mutate(
    rep = ifelse(VCF0301 %in% c(5,6,7), 1, 0),
    dem = ifelse(VCF0301 %in% c(1,2,3), 1, 0),
    ind = ifelse(VCF0301 %in% c(4), 1, 0)
  ) %>% 
  group_by(VCF0004, VCF0114) %>% 
  summarize(
    total = n(),
    rep_pct = sum(rep) / total
  )
  
anes_income %>%  
  filter(VCF0114 != 0) %>% 
  mutate(
    income = case_when(
      VCF0114 == 1 ~ "0 - 16",
      VCF0114 == 2 ~ "17 - 33",
      VCF0114 == 3 ~ "34 - 67",
      VCF0114 == 4 ~ "68 - 95",
      VCF0114 == 5 ~ "95 - 100"
      ),
    rep_pct = 100 * rep_pct
  ) %>% 
  rename(year = VCF0004) %>% 
  filter(year %% 4 == 0) %>% 
  ggplot(aes(x = as.factor(income), y = rep_pct, color = as.factor(year), group = year))+
  geom_line(show.legend = F, linewidth = 1.1)+
  geom_smooth(method = "lm", color = "red", se = F, size = .5, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.65),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.subtitle = element_text(size = 10))+
  scale_y_continuous(
    limits = c(19, 81),
    breaks = c(20, 40, 60, 80)
  )+
  scale_color_viridis_d() +
  facet_wrap(~ year) +
  labs(
    x = "Income Percentile",
    y = "Percentage of Voters Voting for Republican Candidate",
    title = "Income Curves of Republican Support Over Time, 1980-2020",
    subtitle = "Data from ANES"
  )
ggsave("republicanbyincome_anes_wrapped.png", width = 3000, height = 1800, units = "px", type = "cairo-png")

##########
# MORE REP INCOME BY URBANITY 

################################################################################
##########    PRECINCT ANALYSIS     ############################################
################################################################################

#read precinct results 2020
precinct <- st_read("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_vestdatapull_11.22.2024.shp") %>% 
  filter(!duplicated(.)) %>% 
  st_transform(5070)
precinct_locale <- st_read("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_precinctlocale_12.11.2024.shp") %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  filter(!duplicated(.))

p_all <- precinct %>% 
  left_join(precinct_locale, by = "GEOID20")


p_all <- p_all %>% 
  mutate(LOCALE = as.factor(LOCALE)) %>%
  mutate(pct_rep = arv_20 / (arv_20 + adv_20))

test <- p_all %>%
  group_by(LOCALE) %>%
  summarize(med_rpct_20 = median(pct_rep, na.rm = T))


test2 <- test %>% 
  filter(!is.na(LOCALE)) %>% 
  mutate (
    locale_name = case_when(
      LOCALE == "11" ~ "Large Urban",
      LOCALE == "12" ~ "Midsize Urban",
      LOCALE == "13" ~ "Small Urban",
      LOCALE == "21" ~ "Large Suburban",
      LOCALE == "22" ~ "Midsize Suburban",
      LOCALE == "23" ~ "Small Suburban",
      LOCALE == "31" ~ "Fringe Town",
      LOCALE == "32" ~ "Distant Town",
      LOCALE == "33" ~ "Remote Town",
      LOCALE == "41" ~ "Fringe Rural",
      LOCALE == "42" ~ "Distant Rural",
      LOCALE == "43" ~ "Remote Rural",
    )
  ) 

test2$locale_name <- factor(test2$locale_name, levels = c("Large Urban", "Midsize Urban", "Small Urban", "Large Suburban", "Midsize Suburban", "Small Suburban", "Fringe Town", "Distant Town", "Remote Town", "Fringe Rural", "Distant Rural", "Remote Rural"))
test2 %>%   
  ggplot(aes(x = locale_name, y = med_rpct_20, group = 1))+
  geom_line()+
  geom_point()+
  geom_smooth(aes(group = NULL))+
  theme(
    axis.text.x = element_text(angle = 45, margin = margin(t = 3), hjust = 1),
    plot.subtitle = element_text(size = 10),
    axis.title.y = element_text(margin = margin(r = 10))
  )+
  scale_y_continuous(labels = scales::percent_format())+
  labs(
    title = "Republican Vote Share of Precincts by Urbanity Classification",
    subtitle = "Data from NCES EDGE Locale Classification, 2020 VEST Election Returns.",
    x = "Locale Classification",
    y = "Republican Vote Share"
  )
ggsave("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/Final Paper Graphics/graph_repsharerurclass_12.13.2024.png",
       width = 3000, 
       height = 1800, 
       units = "px",
       type = "cairo-png")



####
#12.13.2024
###
## DFS FROM EC ANALYSIS
ct_locale <- bg_locale %>% 
  left_join(bg_pops, by = c("bg_fips" = "GEOID")) %>% 
  as.data.frame() %>% 
  select(-geometry, -NAME.x, -NAME.y, -popM) %>% 
  mutate(
    ct_fips = substr(bg_fips, 1, 5)
  ) %>% 
  group_by(ct_fips, LOCALE) %>% 
  summarize(
    pop = sum(popE, na.rm = T)
  ) %>% 
  pivot_wider(names_from = LOCALE, values_from = pop, names_prefix = "pop_", values_fill = 0) %>% 
  mutate(
    pop_edge = sum(pop_11, pop_12, pop_13, pop_21, pop_22, pop_23, pop_31, pop_32, pop_33, pop_41, pop_42, pop_43),
    pop_urb = sum(pop_11, pop_12, pop_13, na.rm = T),
    pop_sub = sum(pop_21, pop_22, pop_23, na.rm = T),
    pop_twn = sum(pop_31, pop_32, pop_33, na.rm = T),
    pop_rur = sum(pop_41, pop_42, pop_43, na.rm = T),
    pct_11 = pop_11 / pop_edge,
    pct_12 = pop_12 / pop_edge,
    pct_13 = pop_13 / pop_edge,
    pct_21 = pop_21 / pop_edge,
    pct_22 = pop_12 / pop_edge,
    pct_23 = pop_13 / pop_edge,
    pct_31 = pop_31 / pop_edge,
    pct_32 = pop_32 / pop_edge,
    pct_33 = pop_33 / pop_edge,
    pct_41 = pop_41 / pop_edge,
    pct_42 = pop_42 / pop_edge,
    pct_43 = pop_43 / pop_edge,
    pct_urb = pop_urb / pop_edge,
    pct_sub = pop_sub / pop_edge,
    pct_twn = pop_twn / pop_edge,
    pct_rur = pop_rur / pop_edge)

county_rural_graph <- ct_locale %>% 
  inner_join(ct_pres, by = c("ct_fips" = "fips")) %>% 
  inner_join(ct_demo, by = c("ct_fips" = "GEOID")) %>% 
  mutate(
    locale_grp = case_when(
      pct_urb >= .5  ~ "URBAN",
      pct_sub >= .5 ~ "SUBURBAN",
      pct_twn >= .5 ~ "TOWN",
      pct_rur >= 0.5 ~ "RURAL"
    )
  ) 


text_x <- c(22000, 30000, 35000, 40000)
text_y <- c(.37, .29, .21, .33)
text_val <- c("Rural", "Town", "Suburban", "Urban")
text_labs <- data.frame(text_x, text_y, text_val)

county_rural_graph %>% 
  filter(!is.na(locale_grp)) %>% 
  ggplot(aes(x = mhiE, y = rep_pct, color = locale_grp)) +
#  geom_point() +
  geom_smooth(se = F, show.legend = F)+
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(label = comma)+
  theme(
    plot.subtitle = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )+
  labs(
    title = "Smoothed County Republican Vote Share Income Distribution by Urbanity",
    y = "County Republican Vote Share",
    x = "County Median Household Income (2020 USD)",
    color = "",
    subtitle = "Counties classified into urbanity categories by selecting units with more than 50% of the population in one category. \nData from MIT 2020 Election Returns, American Community Survey 2016-2020, and EDGE Locale Classification",
  )+
  scale_colour_paletteer_d("futurevisions::mars", direction = 1)+
  geom_text(data = text_labs, aes(y = text_y, x = text_x, color = NULL, label = text_val), show.legend = F)
ggsave("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/Final Paper Graphics/graph_voteshareincomeurbanity_12.13.2024.png", 
       width = 3000,
       height = 1800,
       units = "px", 
       type = "cairo-png")

county_rural_graph %>%
  mutate(
    locale_grp = case_when(
      pct_urb >= .5 ~ "URBAN",
      pct_sub >= .5 ~ "SUBURBAN",
      pct_twn >= .5 ~ "TOWN",
      pct_rur >= 0.5 ~ "RURAL"
    )
  ) %>% 
  filter(locale_grp == "SUBURBAN") %>% 
  ggplot(aes(x = mhiE, y = rep_pct, color = locale_grp)) +
  geom_smooth()
