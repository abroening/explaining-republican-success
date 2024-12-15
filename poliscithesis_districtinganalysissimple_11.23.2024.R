################################################################################
##########     SETUP     #######################################################
################################################################################
install.packages("redist")
install.packages(c("tidyverse", "sf", "ggplot2"))
install.packages("tidycensus")
install.packages("broom")
install.packages("redistmetrics")
install.packages("alarmdata")
install.packages("labelled")
install.packages("cowplot")
install.packages("paletteer")
install.packages("colorspace")

library(alarmdata)
library(broom)
library(redist)
library(tidyverse)
library(sf)
library(ggplot2)
library(tidycensus)
library(redistmetrics)
library(utils)
library(labelled)
library(cowplot)
library(paletteer)
library(colorspace)
options(scipen = 999)

################################################################################
##########     HELPER FUNCTIONS     ############################################
################################################################################

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

################################################################################
##########     IMPORTING DATA     ##############################################
################################################################################

# Import StateList with FIPS and Abbreviations
state_list <- read.csv(fix_path("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/state_fips.csv")) %>% 
  mutate(state_fips = as.character(state_fips),
         state_fips = ifelse(nchar(state_fips) < 2, paste0("0", state_fips), state_fips))
  
# Import from ALARM DATASETS
# TAKES ~ 30 MIN IN TESTING
precinct_returns <- data.frame()
for (x in state_list$abb) {
    temp <- alarm_census_vest(x, geometry = T, epsg = alarm_epsg(x)) %>% 
      select(GEOID20, state, county, vtd, pop:vap_two, arv_16:ndv, geometry) %>% 
      filter(!is.na(ndv)) %>% 
      st_transform(5070)
    precinct_returns <- rbind(precinct_returns, temp)
    rm(temp)
    rm(x)
}

### SAVE DATA IF DESIRED
st_write(precinct_returns, "//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_vestdatapull_11.22.2024.shp")

# Import Districts and Geometries
districts <- st_read(fix_path("/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/Census Congressional Districts Shapefile 2020 (20M)_20241111/geo_export_ce0411ea-f826-4fcf-a933-96bbd903933c.shp")) %>% 
  st_transform(5070) %>% 
  select(geoid, geometry) %>% 
  # Using numerical fips for redistmetrics package, requiring numeric vector
  mutate(num_fips = as.numeric(geoid))

### Import Block Group Locale Data

block_group_locale <- st_read("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/block_group_data/poliscithesis_blockgrouplocale_11.23.2024.shp")

################################################################################
##########     JOIN TO DISTRICT ASSIGNMENTS     ################################
################################################################################

### ASSIGN PRECINCTS TO THEIR DISTRICTS
precinct_returns_d <- precinct_returns %>% 
  st_join(districts, largest = TRUE)

# SAVE DATA IF DESIRED
st_write(precinct_returns_d, "//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_vestdatapull-bydist_11.22.2024.shp")

#READ DATA IN IF SAVED TO SKIP JOIN
precinct_returns_d <- st_read(fix_path("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_vestdatapull-bydist_11.22.2024.shp"))

################################################################################
##########     DISTRICT AGGREGATION     ########################################
################################################################################

### GET DISTRICT LEVEL RESULTS
district_returns <- precinct_returns_d %>% 
  rename(precinct_fips = GEOID20,
         district_fips = geoid) %>% 
  st_drop_geometry() %>% 
  group_by(district_fips) %>% 
  summarize(
    state = first(state),
    county = first(county),
    across(pop:ndv, ~ sum(.x, na.rm = T))
  ) %>%
  # Removing Extraneous NA GEOID Summary
  filter(!is.na(district_fips))

################################################################################
##########     METRICS CALCULATIONS     ########################################
################################################################################

###### PREP FOR METRICS #####

district_measures <- district_returns %>% 
  left_join(districts, by = c("district_fips" = "geoid")) %>% 
  st_as_sf()

##### EFFICIENCY GAP #####

# Note: JUST USING 2020 DATA HERE
district_measures <- district_measures %>% 
  mutate(
    total_20 = adv_20 + arv_20,
    threshold = ceiling(0.5 * total_20) + 1,
    d_wasted_lose = ifelse(adv_20 < arv_20, adv_20, 0),
    d_wasted_win = ifelse(adv_20 > arv_20, adv_20 - threshold, 0),
    r_wasted_lose = ifelse(arv_20 < adv_20, arv_20, 0),
    r_wasted_win = ifelse(arv_20 > adv_20, arv_20 - threshold, 0), 
    d_wasted = d_wasted_lose + d_wasted_win,
    r_wasted = r_wasted_lose + r_wasted_win
  ) %>% 
  mutate(
    eg = (d_wasted - r_wasted) / total_20
  ) %>% 
  set_variable_labels(
    eg = "Dem Advantage When Positive"
  )

##### COMPACTNESS #####

### MUTATE MEASURES
district_measures <- district_measures %>% 
  mutate(
    polsby = comp_polsby(plans = district_measures$num_fips, shp = district_measures),
    reock = comp_reock(plans = district_measures$num_fips, shp = district_measures),
    convex_hull = comp_ch(plans = district_measures$num_fips, shp = district_measures)
  ) %>% 
  set_variable_labels(
    polsby = "0-1, 1 is most compact",
    reock = "0-1, 1 is most compact",
    convex_hull = "0-1, 1 is most compact"
  )

##### DECLINATION #####
##### CRASHES R - DONT USE #####
test <- district_measures

test %>% 
  mutate(
    declination = part_decl(plans = test$num_fips, shp = test, dvote = adv_20, rvote = arv_20)
  )

###### BIAS MEASURES #####

# New Competeitiveness measures
### Assuming no 3rd Parties
#get mean-med diff
district_measures <- district_measures %>% 
  mutate(
    d_pct_20 = adv_20 / total_20,
    r_pct_20 = arv_20 / total_20, 
  ) %>% 
  group_by(state) %>% 
  mutate(
    st_mean = mean(d_pct_20),
    st_median = median(d_pct_20),
    st_meanmed_diff = st_mean - st_median
  ) %>%
  ungroup() %>% 
  set_variable_labels(
    st_meanmed_diff = "Positive is Rep Advantage"
  ) %>% 
  mutate(
    mean_dist = abs(d_pct_20 - st_mean)
  ) %>% 
  set_variable_labels(
    mean_dist = "Distance from mean"
  ) %>% 
  group_by(state) %>% 
  mutate(
    avg_mean_dist = mean(mean_dist)
  ) %>% 
  ungroup() %>% 
  mutate(
    compet_score = (mean_dist / avg_mean_dist) * abs(st_meanmed_diff),
    #handling Single District States
    compet_score = ifelse(is.nan(compet_score), 0, compet_score)
  ) 


#### SPLITS ATTEMPT IF TIME

################################################################################
##########     JOINING URBAN/RURAL POPS     ####################################
################################################################################
# Import By-County Urban/Rural Info
counties_class <- read_csv(fix_path("/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_censusbureaucountyurbanity_11.10.2024.csv"))
# Import By-BG/By-District Urban/Rural Info
bg_assign <- st_read(fix_path("/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_bgassignment_11.23.2024.shp")) %>% 
  st_drop_geometry()

block_group_join <- block_group_locale %>% 
  left_join(bg_assign %>% select(bg_fips, dist_fips), by = c("GEOID" = "bg_fips")) %>% 
  st_drop_geometry()

demo_district <- block_group_join %>% 
  mutate(
    locale_vague = substr(LOCALE, 1, 1),
    locale_specific = substr(LOCALE, 2,2)
  ) %>% 
  group_by(dist_fips, locale_vague) %>% 
  select(-popM, -yhtM, -asianM, -latinM, -blkM, -mhiM) %>% 
  summarize(
    across(popE:latinE, ~ sum(.x, na.rm = T)),
    mhi = mean(mhiE, na.rm = T)
    ) %>% 
  mutate(
    total = sum(popE, na.rm = T)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(dist_fips)) %>% 
  mutate(
    mhi = ifelse(is.nan(mhi), NA, mhi)
  ) %>% 
  pivot_wider(names_from = locale_vague, values_from = c(popE:mhi)) %>% 
  mutate(
    urban_pct_pop = popE_1 / total,
    suburb_pct_pop = popE_2 / total,
    town_pct_pop = popE_3 / total,
    rur_pct_pop = popE_4 / total
  )
  #### AT SOME POINT SHOULD DO THIS FOR RACE AND INCOME AS WELL, FOR NOW, SELECTING OUT
### MAPPING COUNTY TO DISTRICT
county_dist_x <- block_group_join %>% 
  mutate(
    county_fips = substr(GEOID, 1, 5),
    locale_vague = substr(LOCALE, 1, 1),
    locale_specific = substr(LOCALE, 2,2)
  ) %>% 
  filter(!is.na(locale_vague)) %>% 
  group_by(dist_fips, county_fips) %>%
  summarize(
    total = sum(popE, na.rm = T)
  ) %>% 
  arrange(county_fips, desc(total)) %>% 
  ungroup() %>% 
  group_by(county_fips) %>% 
  slice_head(n = 1) %>% 
  select(-total)

demo_county <- block_group_join %>% 
  mutate(
    county_fips = substr(GEOID, 1, 5),
    locale_vague = substr(LOCALE, 1, 1),
    locale_specific = substr(LOCALE, 2,2)
  ) %>% 
  filter(!is.na(locale_vague)) %>% 
  group_by(county_fips, locale_vague) %>%
  summarize(
    across(popE:latinE, ~ sum(.x, na.rm = T)),
    mhi = mean(mhiE, na.rm = T)
  ) %>% 
  mutate(
    total = sum(popE, na.rm = T)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(county_fips)) %>% 
  mutate(
    mhi = ifelse(is.nan(mhi), NA, mhi)
  ) %>% 
  pivot_wider(names_from = locale_vague, values_from = c(popE:mhi)) %>% 
  mutate(
    urban_pct_pop = popE_1 / total,
    suburb_pct_pop = popE_2 / total,
    town_pct_pop = popE_3 / total,
    rur_pct_pop = popE_4 / total
  ) %>% 
  inner_join(county_dist_x, by = "county_fips")
#### AT SOME POINT SHOULD DO THIS FOR RACE AND INCOME AS WELL, FOR NOW, SELECTING OUT

################################################################################
##########     JOIN IT ALL UP     ##############################################
################################################################################

dist_join <- demo_district %>% 
  inner_join(district_measures, by = c("dist_fips" = "district_fips"))

county_join <- demo_county %>% 
  inner_join(district_measures, by = c("dist_fips" = "district_fips"))


################################################################################
##########     PLOT IT ALL...     ##############################################
################################################################################
### SAVING FOR EASE

write.csv(county_join,"//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_countydistrictjoin_11.24.2024.csv")
write.csv(dist_join,"//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_districtjoin_11.24.2024.csv")

test <- read.csv(fix_path("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_countyjoin_11.24.2024.csv"))
### Measures Against Each Other:

self_1 <- dist_join %>% 
  ggplot(aes(x = compet_score, y = reock)) +
  geom_point()
self_2 <- dist_join %>% 
  ggplot(aes(x = eg, y = compet_score)) +
  geom_point()
self_3 <- dist_join %>% 
  ggplot(aes(x = eg, y = reock)) +
  geom_point()
self_4 <- dist_join %>% 
  ggplot(aes(x = polsby, y = reock)) +
  geom_point()


plot_grid(self_1, self_2, self_3, self_4, labels = c("a", "b", "c", "d"), ncol = 2, nrow =2) 
  
  
### Mueasures Grid
  
dist_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100
  ) %>% 
  ggplot(aes(y = eg, x = rur_pct_pop)) +
  geom_point(alpha = 0.7, size =2, color = "#09283C") +
  labs(title = "Efficiency Gap against Rural Population by House District",
       subtitle = "Positive Efficiency Gap Indicates Dem. Advantage.  \nData from NCES EDGE Locale Data, 2020 VEST Election Returns.",
       y = "Efficiency Gap",
       x = "Rural Population Share")+
  scale_x_continuous(labels = scales::percent_format(scale =1), limits = c(0, 101))+
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.subtitle = element_text(size = 10)
  )
ggsave("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/Final Paper Graphics/graph_egscatter_12.13.2024.png",
       width = 3000, 
       height = 3000, 
       units = "px",
       type = "cairo-png")


reock_plot <- dist_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100
  ) %>% 
  ggplot(aes(y = reock, x = rur_pct_pop)) +
  geom_point(alpha = .7) +
  labs(title = "Reock Score against Rural Population by House District",
       subtitle = "Higher Reock Score indicates more gerrymandering. \nData from NCES EDGE Locale Data, 2020 VEST Election Returns.",
       y = "Reock Score",
       x = "Rural Population Share")+
  scale_x_continuous(labels = scales::percent_format(scale =1), limits = c(0, 101))+
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.subtitle = element_text(size = 10)
  )
polsby_plot <- dist_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100
  ) %>% 
  ggplot(aes(y = polsby, x = rur_pct_pop)) +
  geom_point(alpha = .7) +
  labs(title = "Efficiency Gap against Rural Population by House District",
       subtitle = "Higher Polsby-Popper Score indicates more gerrymandering. \nData from NCES EDGE Locale Data, 2020 VEST Election Returns.",
       y = "Polsby-Popper Measure",
       x = "Rural Population Share")+
  scale_x_continuous(labels = scales::percent_format(scale =1), limits = c(0, 101))+
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.subtitle = element_text(size = 10)
  )
hull_plot <- dist_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100
  ) %>% 
  ggplot(aes(y = convex_hull, x = rur_pct_pop)) +
  geom_point(alpha = .7) +
  labs(title = "Convex Hull Score against Rural Population by House District",
       subtitle = "Higher Convex Hull Score indicates more gerrymandering. \nData from NCES EDGE Locale Data, 2020 VEST Election Returns.",
       y = "Convex Hull",
       x = "Rural Population Share")+
  scale_x_continuous(labels = scales::percent_format(scale =1), limits = c(0, 101))+
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.subtitle = element_text(size = 10)
  )
compet_plot <- dist_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100
  ) %>% 
  ggplot(aes(y = compet_score, x = rur_pct_pop)) +
  geom_point(alpha = .7) +
  labs(title = "Competitiveness Score against Rural Population by House District",
       subtitle = "Higher Competetiveness Score indicates more gerrymandering. \nData from NCES EDGE Locale Data, 2020 VEST Election Returns.",
       y = "Competitiveness Score",
       x = "Rural Population Share")+
  scale_x_continuous(labels = scales::percent_format(scale =1), limits = c(0, 101))+
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.subtitle = element_text(size = 10)
  )

plot_grid(hull_plot, polsby_plot, compet_plot, reock_plot, ncol = 2, nrow =2)
ggsave("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/Final Paper Graphics/graph_gerryscatters_12.13.2024.png",
       width = 4000, 
       height = 3000, 
       units = "px",
       type = "cairo-png")


### COUNTY MEASURES GRID
eg_plot_c <- county_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100
  ) %>% 
  ggplot(aes(x = eg, y = rur_pct_pop)) +
  geom_point() +
  labs(title = "Efficiency Gap against Rural Population by County",
       subtitle = "Positive Efficiency Gap Indicates Dem. Advantage",
       x = "Efficiency Gap",
       y = "Percent Rural Population")
reock_plot_c <- county_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100
  ) %>% 
  ggplot(aes(x = reock, y = rur_pct_pop)) +
  geom_point() +
  labs(title = "Reock Score against Rural Population by County",
       subtitle = "Higher Reock Score inidcates more gerrymandering",
       x = "Reock Score",
       y = "Percent Rural Population")
polsby_plot_c <- county_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100
  ) %>% 
  ggplot(aes(x = polsby, y = rur_pct_pop)) +
  geom_point() +
  labs(title = "Efficiency Gap against Rural Population by County",
       subtitle = "Higher Polsby-Popper Score inidcates more gerrymandering",
       x = "Polsby-Popper Measure",
       y = "Percent Rural Population")
compet_plot_c <- county_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100
  ) %>% 
  ggplot(aes(x = compet_score, y = rur_pct_pop)) +
  geom_point() +
  labs(title = "Competitiveness Score against Rural Population by County",
       subtitle = "Higher Competetiveness Score inidcates more gerrymandering",
       x = "Competitiveness Score",
       y = "Percent Rural Population")
plot_grid(eg_plot_c, polsby_plot_c, compet_plot_c, reock_plot_c, ncol = 2, nrow =2)

### EG-BLACK %
dist_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100,
    blck_pct = (vap_black / vap) * 100
  ) %>% 
  ggplot(aes(x = eg, y = blck_pct)) +
  geom_point() +
  labs(title = "Efficiency Gap against Rural Population by House District",
       subtitle = "Higher Reock Score inidcates more gerrymandering",
       x = "Efficiency Gap",
       y = "Percent Rural Population")

egb_plot <- dist_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100,
    blck_pct = (vap_black / vap) * 100
  ) %>% 
  ggplot(aes(x = eg, y = blck_pct)) +
  geom_point() +
  labs(title = "Efficiency Gap against Black % Population by House District",
       subtitle = "Positive Efficiency Gap Indicates Dem. Advantage",
       x = "Efficiency Gap",
       y = "Percent Black Population")
reockb_plot <- dist_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100,
    blck_pct = (vap_black / vap) * 100
  ) %>% 
  ggplot(aes(x = reock, y = blck_pct)) +
  geom_point() +
  labs(title = "Reock Score against Black % Population by House District",
       subtitle = "Higher Reock Score inidcates more gerrymandering",
       x = "Reock Score",
       y = "Percent Black Population")
polsbyb_plot <- dist_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100,
    blck_pct = (vap_black / vap) * 100
  ) %>% 
  ggplot(aes(x = polsby, y = blck_pct)) +
  geom_point() +
  labs(title = "Efficiency Gap against Black % Population by House District",
       subtitle = "Higher Polsby-Popper Score inidcates more gerrymandering",
       x = "Polsby-Popper Measure",
       y = "Percent Black Population")
competb_plot <- dist_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100,
    blck_pct = (vap_black / vap) * 100
  ) %>% 
  ggplot(aes(x = compet_score, y = blck_pct)) +
  geom_point() +
  labs(title = "Competitiveness Score against Black % Population by House District",
       subtitle = "Higher Competetiveness Score inidcates more gerrymandering",
       x = "Competitiveness Score",
       y = "Percent Black Population")
plot_grid(egb_plot, polsbyb_plot, competb_plot, reockb_plot, ncol = 2, nrow =2)

#### CHECK GERRYMANDERING MEASURES
eg_reock <- dist_join %>% 
  ggplot(aes(x = eg, y = reock)) +
  geom_point() +
  labs(title = "Efficiency Gap against Reock Score",
       subtitle = "Extremes of EG, higher Reock: more gerrymandering",
       x = "Efficiency Gap",
       y = "Reock Score")
eg_compet <- dist_join %>% 
  ggplot(aes(x = eg, y = compet_score)) +
  geom_point() +
  labs(title = "Efficiency Gap against Competitiveness Score",
       subtitle = "Extremes of EG, higher competitiveness: more gerrymandering",
       x = "Efficiency Gap",
       y = "Competitiveness Score")
reock_compet <- dist_join %>% 
  ggplot(aes(x = reock, y = compet_score)) +
  geom_point() +
  labs(title = "Reock Score against Competitiveness Score",
       subtitle = "Higher Reock, higher competitiveness: more gerrymandering",
       x = "Reock Score",
       y = "Competitiveness Score")
popper_reock <- dist_join %>% 
  ggplot(aes(x = polsby, y = reock)) +
  geom_point() +
  labs(title = "Polsby-Popper Measure against Reock Score",
       subtitle = "Higher Polsby-Popper, higher Reock: more gerrymandering",
       x = "Polsby-Popper Measure",
       y = "Reock Score")+
  geom_smooth(se = F)
plot_grid(eg_reock, eg_compet, reock_compet, popper_reock, ncol = 2, nrow =2)




### MAPS

district_measures %>% 
  filter(!state %in% c("HI", "AK")) %>% 
  ggplot() +
  geom_sf(aes(fill = eg), color = NA)+
  scale_fill_continuous_diverging(rev = T)+
  theme_minimal()+
  labs(
    title = "Map of Efficiency Gap in U.S. Congressional Districts",
    subtitle = "Data from 2020 VEST Election Returns. Positive EG indicates Democratic Efficiency Advantage ",
    fill = "Efficiency Gap"
  ) +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        plot.subtitle = element_text(size =10))
ggsave("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/Final Paper Graphics/graph_efficiencymap_12.13.2024.png",
       width = 3000, 
       height = 1800, 
       units = "px",
       type = "cairo-png")


dist_join %>% 
  ggplot() +
  geom_point(aes(x = eg, y = rur_pct_pop))

eg_plot <- dist_join %>% 
  mutate(
    rur_pct_pop = rur_pct_pop * 100
  ) %>% 
  ggplot(aes(x = eg, y = rur_pct_pop)) +
  geom_point() +
  labs(title = "Efficiency Gap agains Rural Population by House District",
       subtitle = "Positive Efficiency Gap Indicates Dem. Advantage",
       x = "Efficiency Gap",
       y = "Percent Rural Population")+
  geom_smooth(se = F)

dist_join %>% 
  ggplot() +
  geom_point(aes(x = reock, y = rur_pct_pop))


dist_join %>% 
  ggplot() +
  geom_point(aes(x = polsby, y = rur_pct_pop))

dist_join %>% 
  ggplot() +
  geom_point(aes(x = convex_hull, y = rur_pct_pop))

dist_join %>% 
  ggplot() +
  geom_point(aes(x = compet_score, y = rur_pct_pop))





  gmutate(  
    fips = (paste0(STATE, COUNTY)),
    ...32 = NULL,
    ...33 = NULL
  ) |> 
  filter(!is.na(STATE))

join <- cty_dist_returns_geo_sd_gap %>% 
  left_join(counties_class, by = "fips") %>% 
  mutate(
    POPPCT_RUR = as.numeric(substr(POPPCT_RUR, 1, nchar(POPPCT_RUR) -1)),
    POPPCT_URB = as.numeric(substr(POPPCT_URB, 1, nchar(POPPCT_URB) -1))
  )

test <- join %>% 
  group_by(geoid) %>% 
  mutate(
    total_urb = sum(POP_URB), 
    total_rur = sum(POP_RUR),
    pct_rur = total_rur / sum(total_urb, total_rur)
  )






# Mean-Median
test <- district_measures %>% 
  st_drop_geometry()

test2 <- test %>% 
  mutate(
    mean_median = part_mean_median(plans = test$num_fips, shp = test, rvote = test$arv_20, dvote = test$adv_20) 
  )

district_measures <- district_measures %>% 
  mutate(
    mean_median = part_mean_median(plans = district_measures$num_fips, shp = district_measures, dvote = district_measures$adv_20, rvote = district_measures$arv_20) 
  )




test <- district_measures
test %>% 
  mutate(compet2 = compet_talisman(plans = test$num_fips, shp = test, rvote = nrv, dvote = ndv))


### CALC COMPACTNESS
# USE dist_2020

### CALC MEAN-MED









#fileslist on pc
fileslist <- list.files(
  choose.dir(), 
  full.names = T
)
# init df for returns
returns <- data.frame()

#fileslist on mac
fileslist <- c(
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ak_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/al_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ar_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/az_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ca_2020_block.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/co_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ct_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/dc_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/de_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/fl_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ga_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/hi_2020_block.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ia_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/id_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/il_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/in_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ks_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ky_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/la_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/log_time.txt",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ma_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/md_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/me_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/mi_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/mn_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/mo_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ms_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/mt_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/nc_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/nd_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ne_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/nh_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/nj_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/nm_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/nv_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ny_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/oh_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ok_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/or_2020_block.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/pa_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/README.md",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ri_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/sc_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/sd_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/tn_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/tx_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/ut_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/va_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/vt_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/wa_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/wi_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/wv_2020_vtd.csv",
  "/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/census-2020-main/census-2020-main/census-vest-2020/wy_2020_vtd.csv"
)
#sublists for differing columns
precinct_list <- fileslist[!(endsWith(fileslist, "dc_2020_vtd.csv") | endsWith(fileslist, ".txt") | endsWith(fileslist, ".md") |endsWith(fileslist, "hi_2020_block.csv")| endsWith(fileslist, "ca_2020_block.csv")| endsWith(fileslist, "or_2020_block.csv"))]
other_list <- fileslist[(endsWith(fileslist, "hi_2020_block.csv")| endsWith(fileslist, "ca_2020_block.csv")| endsWith(fileslist, "or_2020_block.csv"))]

for (x in precinct_list) {
  temp <- read.csv(x) %>% 
    select(GEOID20, state, county, pop:vap_two, arv_16:ndv) %>% 
    filter(!is.na(ndv))
  returns <- rbind(returns, temp)
  rm(temp)
  rm(x)
}
for (x in other_list) {
  temp <- read.csv(x) %>% 
    select(GEOID20, state, county, pop:vap_two, arv_16:ndv) %>% 
    filter(!is.na(ndv))
  returns <- rbind(returns, temp)
  rm(temp)
  rm(x)
}

### FIXING IMPORT GEOID

returns <- returns %>% 
  mutate(
    GEOID20 = ifelse(startsWith(GEOID20, "8"), paste0("0", GEOID20), GEOID20),
    GEOID20 = ifelse(startsWith(GEOID20, "6"), paste0("0", GEOID20), GEOID20),
    GEOID20 = ifelse(GEOID20 == "4.5003e+10", 45003000001, GEOID20),
    GEOID20 = ifelse(GEOID20 == "4.5007e+10", 45007000054, GEOID20)
  )

### Getting County and State Separate

returns <- returns %>% 
  mutate(
    st_fips = substr(GEOID20, 1, 2),
    cty_fips = substr(GEOID20, 3, 5)
  )

### DISTRICT DATA
dist_2020 <- st_read(fix_path("/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/Census Congressional Districts Shapefile 2020 (20M)_20241111/geo_export_ce0411ea-f826-4fcf-a933-96bbd903933c.shp")) %>% 
  st_transform(5070)

### PRECINCT SHAPES
prec_2020 <- st_read(fix_path("/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/national_2020_prec_bounds/national_2020_prec_bounds.shp")) %>% 
  st_transform(5070)




cty_returns <- returns %>% 
  group_by(st_fips, cty_fips) %>% 
  summarize(
    fips_raw = first(GEOID20),
    pop = sum(pop),
    white = sum(pop_white),
    black = sum(pop_black),
    hisp = sum(pop_hisp),
    vap = sum(vap),
    wvap = sum(vap_white),
    bvap = sum(vap_black),
    hvap = sum(vap_hisp),
    tot_20 = sum(arv_20) + sum(adv_20),
    dem_20 = sum(adv_20),
    rep_20 = sum(arv_20),
    tot_16 = sum(arv_16) + sum(adv_16),
    dem_16 = sum(adv_16),
    rep_16 = sum(arv_16),
    tot_all = sum(nrv) + sum(ndv),
    dem_all = sum(ndv),
    rep_all = sum(nrv),
    rep_pct_20 = rep_20 / tot_20,
    rep_pct_16 = rep_16 / tot_16,
    rep_pct_all = rep_all / tot_all
  ) %>% 
  ungroup() %>% 
  mutate(
    fips = substr(fips_raw, 1, 5)
  )

### JOINING DISTRICTS TO COUNTIES
## Pullin County Geographies
county_geo <- get_acs(
  survey = "acs5",
  geometry = T,
  geography = "county",
  year = 2020,
  variables = c("dummy"= "B01001_001")
) %>% 
  select(GEOID, NAME, geometry) %>% 
  mutate(county = str_split_fixed(NAME, ", ", 2)[,1],
         state = str_split_fixed(NAME, ", ", 2)[,2])

cty_returns_geo <- cty_returns %>% 
  left_join(county_geo %>% select(GEOID, geometry), by = c("fips" = "GEOID")) %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  st_transform(5070)

cty_dist_returns_geo <- cty_returns_geo |> 
  st_join(dist_2020, largest = TRUE)


### Mutate State Percentages

cty_dist_returns_geo_s <- cty_dist_returns_geo %>% 
  group_by(statefp) %>% 
  mutate(
    st_tot_20 = sum(tot_20, na.rm = T),
    st_dem_20 = sum(dem_20, na.rm = T),
    st_rep_20 = sum(rep_20, na.rm = T),
    st_tot_16 = sum(tot_16, na.rm = T),
    st_dem_16 = sum(dem_16, na.rm = T),
    st_rep_16 = sum(rep_16, na.rm = T),
    st_tot_all = sum(tot_all, na.rm = T),
    st_dem_all = sum(dem_all, na.rm = T),
    st_rep_all = sum(rep_all, na.rm = T),
    st_rep_pct_20 = st_rep_20 / st_tot_20,
    st_rep_pct_16 = st_rep_16 / st_tot_16,
    st_rep_pct_all = st_rep_all / st_tot_all,
    ### GAP IS POSITIVE IF REP WINS
    st_gap_20 = st_rep_pct_20 - (st_dem_20 / st_tot_20),
    st_gap_16 = st_rep_pct_16 - (st_dem_16 / st_tot_16),
    st_gap_all = st_rep_pct_all - (st_dem_all / st_tot_all)
  ) %>% 
  ungroup()

### Mutate District Percentages

cty_dist_returns_geo_sd <- cty_dist_returns_geo_s %>% 
  group_by(geoid) %>% 
  mutate(
    dst_tot_20 = sum(tot_20, na.rm = T),
    dst_dem_20 = sum(dem_20, na.rm = T),
    dst_rep_20 = sum(rep_20, na.rm = T),
    dst_tot_16 = sum(tot_16, na.rm = T),
    dst_dem_16 = sum(dem_16, na.rm = T),
    dst_rep_16 = sum(rep_16, na.rm = T),
    dst_tot_all = sum(tot_all, na.rm = T),
    dst_dem_all = sum(dem_all, na.rm = T),
    dst_rep_all = sum(rep_all, na.rm = T),
    dst_rep_pct_20 = dst_rep_20 / dst_tot_20,
    dst_rep_pct_16 = dst_rep_16 / dst_tot_16,
    dst_rep_pct_all = dst_rep_all / dst_tot_all,
    ### GAP IS POSITIVE IF REP WINS
    dst_gap_20 = dst_rep_pct_20 - (dst_dem_20 / dst_tot_20),
    dst_gap_16 = dst_rep_pct_16 - (dst_dem_16 / dst_tot_16),
    dst_gap_all = dst_rep_pct_all - (dst_dem_all / dst_tot_all)
  ) %>% 
  ungroup()

### Find Difference by County ANDD CALC COUNTY GAP

cty_dist_returns_geo_sd_gap <- cty_dist_returns_geo_sd %>% 
  mutate(
    ### GAP IS POSITIVE IF REP OUTPERFORMED STATE
    ### GAP IS NUMBER OF PERCENTAGE POINS BY WHICH REP. OUTPERFORMED STATE
    margin_gap_20 = dst_gap_20 - st_gap_20,
    margin_gap_16 = dst_gap_16 - st_gap_16,
    margin_gap_all = dst_gap_all - st_gap_all,
    ct_gap_20 = (rep_20 / tot_20) - (dem_20 / tot_20),
    ct_gap_16 = (rep_16 / tot_16) - (dem_16 / tot_16),
    ct_gap_all = (rep_all / tot_all) - (dem_all / tot_all)
  )

### ADD URB/RUR INFO

counties_class <- read_csv("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_censusbureaucountyurbanity_11.10.2024.csv") |> 
  mutate(
    fips = (paste0(STATE, COUNTY)),
    ...32 = NULL,
    ...33 = NULL
  ) |> 
  filter(!is.na(STATE))

counties_class <- read_csv("/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_censusbureaucountyurbanity_11.10.2024.csv") |> 
  mutate(  
    fips = (paste0(STATE, COUNTY)),
    ...32 = NULL,
    ...33 = NULL
  ) |> 
  filter(!is.na(STATE))

join <- cty_dist_returns_geo_sd_gap %>% 
  left_join(counties_class, by = "fips") %>% 
  mutate(
    POPPCT_RUR = as.numeric(substr(POPPCT_RUR, 1, nchar(POPPCT_RUR) -1)),
    POPPCT_URB = as.numeric(substr(POPPCT_URB, 1, nchar(POPPCT_URB) -1))
  )

test <- join %>% 
  group_by(geoid) %>% 
  mutate(
    total_urb = sum(POP_URB), 
    total_rur = sum(POP_RUR),
    pct_rur = total_rur / sum(total_urb, total_rur)
  )
### Regress

join %>%
  ggplot(aes(x = margin_gap_20, y = POPPCT_RUR)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Rural Percent of County Populations Predicted by the Voter Harm Gap", x = "Gap of Rep/Dem Electoral Split between County and District", y = "County Population Rural Percent")
  
reg <- lm(formula = margin_gap_20 ~ POPPCT_RUR+ pct_rur, data = test)

harmed <- test %>% 
  filter(
    (ct_gap_20 < 0 & dst_gap_20 > 0) | (ct_gap_20 > 0 & dst_gap_20 < 0)
  )

harmed %>%
  ggplot(aes(x = POPPCT_RUR, y = margin_gap_20)) +
  geom_point()+
  labs(title = "Rural Percent of County Populations Predicted by the Voter Harm Gap - ONLY HARMED", y = "Gap of Rep/Dem Electoral Split between County and District", x = "County Population Rural Percent")


### EFFICIENCY GAP

eg <- cty_dist_returns_geo_sd
  
eg <- cty_dist_returns_geo %>% 
  group_by(geoid) %>% 
  summarize(
    state = first(state),
    dst_tot_20 = sum(tot_20, na.rm = T),
    dst_dem_20 = sum(dem_20, na.rm = T),
    dst_rep_20 = sum(rep_20, na.rm = T),
  ) %>% 
  ungroup() %>% 
  mutate(
    thresh = ceiling(0.5 * dst_tot_20) + 1,
    d_wasted_lose = ifelse(dst_dem_20 < dst_rep_20, dst_dem_20, 0),
    d_wasted_win = ifelse(dst_dem_20 > dst_rep_20, dst_dem_20 - thresh, 0),
    r_wasted_lose = ifelse(dst_rep_20 < dst_dem_20, dst_rep_20, 0),
    r_wasted_win = ifelse(dst_rep_20 > dst_dem_20, dst_rep_20 - thresh, 0), 
    d_wasted = d_wasted_lose + d_wasted_win,
    r_wasted = r_wasted_lose + r_wasted_win
  ) %>% 
  mutate(
    eg = (d_wasted - r_wasted) / dst_tot_20
  )
  
district_pops <- join %>% 
  group_by(geoid) %>% 
  summarize(
    urban_pop = sum(as.numeric(POP_URB), na.rm = TRUE),
    rural_pop = sum(POP_RUR),
    total_pop = sum(pop)
  ) %>% 
  select(-geometry) %>% 
  as.data.frame()
  
eg_pops <- eg %>% 
  left_join(district_pops, by = "geoid") %>% 
  mutate(
    ruralpct = rural_pop / total_pop
  )

eg_pops %>% 
  ggplot() +
  geom_point(aes(x = ruralpct, y = eg)
  ) +
  labs(x = "County Percent Rural Population", y = "Efficiency Gap (Positive = Dem Advantage", title = "Efficiency Gap against Rural Percentage by County")


  # 
  # efficiency gap 
  # needs : join by county to classed
  #         summarize districts, including urb pop/ rur population
  #         
  
  

################################################################################
### UNUSED
################################################################################

state_returns <- returns %>% 
  group_by(state) %>% 
  summarize(
    fips = substr(first(GEOID20), 1, 2),
    pop = sum(pop, na.rm = T),
    white = sum(pop_white, na.rm = T),
    black = sum(pop_black, na.rm = T),
    hisp = sum(pop_hisp, na.rm = T),
    vap = sum(vap, na.rm = T),
    wvap = sum(vap_white, na.rm = T),
    bvap = sum(vap_black, na.rm = T),
    hvap = sum(vap_hisp, na.rm = T),
    tot_20 = sum(arv_20, na.rm = T) + sum(adv_20, na.rm = T),
    dem_20 = sum(adv_20, na.rm = T),
    rep_20 = sum(arv_20, na.rm = T),
    tot_16 = sum(arv_16, na.rm = T) + sum(adv_16, na.rm = T),
    dem_16 = sum(adv_16, na.rm = T),
    rep_16 = sum(arv_16, na.rm = T),
    tot_all = sum(nrv, na.rm = T) + sum(ndv, na.rm = T),
    dem_all = sum(ndv, na.rm = T),
    rep_all = sum(nrv, na.rm = T),
  )

cty_state_returns <- cty_returns %>% 
  group_by(state) %>% 
  mutate(
    st_tot_20 = sum(tot_20, na.rm = T),
    st_dem_20 = sum(dem_20, na.rm = T),
    st_rep_20 = sum(rep_20, na.rm = T),
    st_tot_16 = sum(tot_16, na.rm = T),
    st_dem_16 = sum(dem_16, na.rm = T),
    st_rep_16 = sum(rep_16, na.rm = T),
    st_tot_all = sum(tot_all, na.rm = T),
    st_dem_all = sum(dem_all, na.rm = T),
    st_rep_all = sum(rep_all, na.rm = T),
    st_rep_pct_20 = st_rep_20 / st_tot_20,
    st_rep_pct_16 = st_rep_16 / st_tot_16,
    st_rep_pct_all = st_rep_all / st_tot_all,
    st_gap_20 = st_rep_pct_20 - (st_dem_20 / st_tot_20),
    st_gap_16 = st_rep_pct_16 - (st_dem_16 / st_tot_16),
    st_gap_all = st_rep_pct_all - (st_dem_all / st_tot_all)
  )


cty_state_returns_dff <- cty_state_returns %>% 
  mutate(
    gap_20 = rep_pct_20 - (dem_20 / tot_20),
    gap_16 = rep_pct_16 - (dem_16 / tot_16),
    gap_all = rep_pct_all - (dem_all / tot_all),
    diff_20 =  gap_20 - st_gap_20,
    diff_16 =  gap_16 - st_gap_16,
    diff_all = gap_all - st_gap_all
  )

# IF DIFF VALUES ARE POSITIVE, REP OVERPEROFMR IN COUNTY RELATIVE TO STATE
test <- (nh)



################################################################################
###### REGRESSSIONSSSS
################################################################################

gerry_dist_lm <- list(lm_sub_ <- lm(ratio2020 ~ st_rur_pt, data = st_ec_reg),
                 lm_urb <- lm(ratio2020 ~ rur_pct_edge, data = st_ec_reg),
                 lm_rur <- lm(ratio2020 ~ sub_pct_edge, data = st_ec_reg),
                 lm_st_wht <- lm(ratio2020 ~ wht_pct, data = st_ec_reg),
                 lm_st_blk <- lm(ratio2020 ~ blk_pct, data = st_ec_reg),
                 lm_st_mhi <- lm(ratio2020 ~ mhiE, data = st_ec_reg),
                 lm_st_pov <- lm(ratio2020 ~ pov_pct, data = st_ec_reg),
                 lm_st_eco <- lm(ratio2020 ~ pov_pct + mhiE, data = st_ec_reg),
                 lm_st_edu <- lm(ratio2020 ~ bch_pct, data = st_ec_reg),
                 lm_st_age <- lm(ratio2020 ~ ageE, data = st_ec_reg),
                 lm_st_rep <- lm(ratio2020 ~ rep_pct, data = st_ec_reg),
                 lm_st_demo <- lm(ratio2020 ~ wht_pct + blk_pct + mhiE + pov_pct + bch_pct + ageE, data = st_ec_reg),
                 lm_st_valid_cb <- lm(ratio2020 ~ st_rur_pt + wht_pct + rep_pct, data = st_ec_reg),
                 lm_st_valid_e  <- lm(ratio2020 ~ rur_pct_edge + wht_pct, data = st_ec_reg)
)

modelsummary(ec_st_lm, statistic = "p.value", stars = TRUE)
