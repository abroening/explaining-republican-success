################################################################################
##########     ELECTORAL COLLEGE ANALYSIS THESIS 400     #######################
################################################################################
install.packages("modelsummary")
library(modelsummary)
library(tidycensus)
library(cowplot)
install.packages("cowplot")

## Voter power - proportion of people to electoral votes

electoral <- read.csv("H:/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_electoralvotes_11.10.2024.csv")
electoral <- read.csv("/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_electoralvotes_11.10.2024.csv")

states <- get_acs(year = 2020, 
                  variables = c("pop" = "B01003_001",
                                "wht" = "B01001H_001",
                                "blk" = "B01001B_001",
                                "mhi" = "B19013_001",
                                "bch" = "B16010_041", 
                                "bchref" = "B16010_001",
                                "age" = "B01002_001"), 
                  survey = "acs5", 
                  geography = "state",
                  output = "wide",
                  geometry = T) %>%  
  select(!ends_with("M")) %>% 
  filter(!NAME == "Puerto Rico") %>% 
  mutate(bch_pct = bchE / bchrefE,
         blk_pct = blkE / popE,
         wht_pct = whtE / popE)




states_elec <- electoral %>% 
  left_join(states, by = c("state" = "NAME")) %>% 
  mutate(
    ratio2020 =  popE / college_votes_2020
    )


urban_rural_pops <- read.csv("H:/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_censusbureaucountyurbanity_11.10.2024.csv", blank.lines.skip = TRUE) %>% 
  filter(!is.na(STATE)) %>% 
  select(-X, -X.1)

urban_rural_pops <- read.csv("/Volumes/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_censusbureaucountyurbanity_11.10.2024.csv", blank.lines.skip = TRUE) %>% 
  filter(!is.na(STATE)) %>% 
  select(-X, -X.1) 
  

urban_rural_pops[, 5:ncol(urban_rural_pops)] <- lapply(5:ncol(urban_rural_pops), function(x) as.numeric(gsub(c(",|%"),"",urban_rural_pops[[x]])) )
  
  

state_aggregation <- urban_rural_pops %>% 
  group_by(STATE) %>% 
  summarize(urban_pop = sum(POP_URB),
            rural_pop = sum(POP_RUR)
            ) %>% 
  ungroup() %>% 
  mutate(STATE = as.character(STATE)) %>% 
  mutate(STATE = ifelse(nchar(STATE) < 2, paste0("0", STATE), STATE))
  
states_joined <- states_elec %>% 
  left_join(state_aggregation, by = c("GEOID" = "STATE")) %>% 
  left_join(state_pres, by = c("GEOID" = "st_fips")) %>% 
  mutate(
    white_pct = whtE / popE,
    rural_pct = rural_pop / popE
  )


ecgraph_rur <- states_joined %>% 
  ggplot(aes(y = ratio2020, x = rural_pct)) +
  geom_point()+
  geom_smooth(formula = y ~ x, se = F, color = "#b12000", method = stats::lm)+
  scale_y_reverse(label = comma) +
  scale_x_continuous(labels = scales::percent_format())+
  labs(x = "Rural Population Share", 
       y = "Voter Power (Population per EC Vote)",
       title = "Electoral Power by Rural Population",
       subtitle = "Data from U.S. Census Bureau, MIT Elections Lab")+
  theme(
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.subtitle = element_text(size = 10)
  )

ecgraph_mhi <- states_joined %>% 
  ggplot(aes(y = ratio2020, x = mhiE)) +
  geom_point()+
  geom_smooth(formula = y ~ x, se = F, color = "#b12000", method = stats::lm)+
  scale_y_reverse(label = comma) +
  scale_x_continuous(label = comma) +
  labs(x = "Median Household Income (2020 USD)", 
       y = "Voter Power (Population per EC Vote)",
       title = "Electoral Power by Household Income",
       subtitle = "Data from U.S. Census Bureau, MIT Elections Lab")+
  theme(
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.subtitle = element_text(size = 10)
  )

ecgraph_whtpct <- states_joined %>% 
  ggplot(aes(y = ratio2020, x = white_pct)) +
  geom_point()+
  geom_smooth(formula = y ~ x, se = F, color = "#b12000", method = stats::lm)+
  scale_y_reverse(label = comma)+
  scale_x_continuous(labels = scales::percent_format())+
  labs(x = "White Population Share", 
       y = "Voter Power (Population per EC Vote)",
       title = "Electoral Power by White Population",
       subtitle = "Data from U.S. Census Bureau, MIT Elections Lab")+
  theme(
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.subtitle = element_text(size = 10)
  )

ecgraph_reppct <- states_joined %>% 
  ggplot(aes(y = ratio2020, x = rep_pct)) +
  geom_point()+
  geom_smooth(formula = y ~ x, se = F, color = "#b12000", method = stats::lm)+
  scale_y_reverse(label = comma)+
  scale_x_continuous(labels = scales::percent_format())+
  labs(x = "Republican Voter Share (2020 Presidential)", 
       y = "Voter Power (Population per EC Vote)",
       title = "Electoral Power by Republican Voting",
       subtitle = "Data from U.S. Census Bureau, MIT Elections Lab")+
  theme(
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.subtitle = element_text(size = 10)
  )
plot_grid(ecgraph_rur, ecgraph_whtpct, ecgraph_mhi, ecgraph_reppct)
ggsave("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/Final Paper Graphics/graph_ecscatters_12.13.2024.png",
       width = 4500, 
       height = 2700, 
       units = "px",
       type = "cairo-png")
       
       
################################################################################
## REGRESSIONS
################################################################################

states <- get_acs(year = 2020, 
                  variables = c("pop" = "B01003_001",
                                "wht" = "B01001H_001",
                                "blk" = "B01001B_001",
                                "mhi" = "B19013_001",
                                "bch" = "B16010_041", 
                                "bchref" = "B16010_001",
                                "age" = "B01002_001",
                                "pov" = "B17003_002",
                                "povref" = "B17003_001"), 
                  survey = "acs5", 
                  geography = "state",
                  output = "wide",
                  geometry = T) %>%  
  select(!ends_with("M")) %>% 
  filter(!NAME == "Puerto Rico") %>% 
  mutate(bch_pct = bchE / bchrefE,
         blk_pct = blkE / popE,
         wht_pct = whtE / popE,
         pov_pct = povE / povrefE)

county_rur <- read.csv("H:/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_censusbureaucountyurbanity_11.10.2024.csv", blank.lines.skip = TRUE) %>% 
  filter(!is.na(STATE)) %>% 
  select(STATE, COUNTY, STATE_NAME, COUNTY_NAME, POP_COU, POP_URB, POPPCT_URB, POP_RUR, POPPCT_RUR, ALAND_COU, ALAND_URB, ALAND_RUR) %>% 
  mutate(
    STATE = ifelse(nchar(as.character(STATE)) == 1, 
                   paste0("0", as.character(STATE)), 
                          as.character(STATE)),
    COUNTY = ifelse(nchar(as.character(COUNTY)) == 1,
                    paste0("00", as.character(COUNTY)),
                    ifelse(nchar(as.character(COUNTY)) == 2, 
                           paste0("0", as.character(COUNTY)), 
                           as.character(COUNTY))),
    ALAND_COU = as.numeric(gsub(",", "", ALAND_COU)),
    ALAND_URB = as.numeric(gsub(",", "", ALAND_URB)),
    ALAND_RUR = as.numeric(gsub(",", "", ALAND_RUR)),
    POP_COU = as.numeric(gsub(",", "", POP_COU)),
    POP_URB = as.numeric(gsub(",", "", POP_URB)),
    POP_RUR = as.numeric(gsub(",", "", POP_RUR))
    )

state_rur <- county_rur %>% 
  group_by(STATE) %>% 
  summarise(
    st_pop = sum(POP_COU),
    st_urb = sum(POP_URB),
    st_rur = sum(POP_RUR),
    st_area = sum(ALAND_COU),
    st_urba = sum(ALAND_URB),
    st_rura = sum(ALAND_RUR)
  ) %>% 
  mutate(
    st_area = sum(st_urba, st_rura),
    st_urb_pt = st_urb / st_pop,
    st_rur_pt = st_rur / st_pop,
    st_urba_pt = st_urba / st_area,
    st_rura_pt = st_rura / st_area
  )

county_pres <- read.csv("//hemisphere/geopower/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/mit_countyreturns_2000-2020/countypres_2000-2020.csv") %>% 
  filter(year == 2020) %>% 
  mutate(
    fips = ifelse(nchar(as.character(county_fips)) == 4, paste0("0", as.character(county_fips)), as.character(county_fips))
  ) %>%
  select(-mode, -version, -candidate, -office, -county_fips) %>% 
  group_by(fips, party) %>% 
  summarize(
   candidate_votes = sum(candidatevotes, na.rm = T),
   party = first(party),
   total_votes = sum(totalvotes, na.rm = T)
  ) %>% 
  pivot_wider(names_from = party, values_from = candidate_votes, names_prefix = "votes_") %>% 
  mutate(
    st_fips = substr(fips, 1, 2)
  ) %>% 
  mutate(
    votes_OTHER = sum(votes_OTHER, votes_LIBERTARIAN, votes_GREEN, na.rm = T)
  ) %>% 
  select(-votes_LIBERTARIAN, -votes_GREEN) 
  
state_pres <- county_pres %>% 
  group_by(st_fips) %>% 
  summarize(
    total_votes = sum(total_votes, na.rm =T),
    rep_votes = sum(votes_REPUBLICAN, na.rm = T),
    dem_votes = sum(votes_DEMOCRAT, na.rm = T)
  ) %>% 
  mutate(
    rep_pct = rep_votes / total_votes,
    dem_pct = dem_votes / total_votes
  )

### API ACS CALL FUNCTION
## Calls the US Census American Community Survey API, with a number of user defined variables, to output a dataframe in wide format
acs_call <- function(yr = 2022, varlist = var_list, states = state_list, level = "tract", surv = "acs5", dur = FALSE) {
  start <- Sys.time()
  output <- data.frame()
  for (state in states) {
    temp <- get_acs(
      geography = level,
      variables = varlist,
      survey = surv,
      year = yr,
      state = state,
      output = "wide",
      geometry = TRUE
    )
    output <- rbind(output, temp)
  }
  if (dur == TRUE) {print(Sys.time()-start)}
  return(output)
}
state_list = get_state_list(states = "all_dc")
bg_pops <- acs_call(yr = 2020, varlist = c("pop"= "B01003_001"), states = state_list, level = "block group", surv = "acs5")
bg_locale <- st_read("H:/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_bgassignment_11.23.2024.shp") %>% 
  st_transform(5070)
#LOWER 48 ONLY
st_pops <- bg_locale %>% 
  left_join(bg_pops, by = c("bg_fips" = "GEOID")) %>% 
  as.data.frame() %>% 
  select(-geometry, -NAME.x, -NAME.y, -popM) %>% 
  mutate(
    st_fips = substr(bg_fips, 1, 2)
  ) %>% 
  group_by(st_fips, LOCALE) %>% 
  summarize(
    pop = sum(popE, na.rm = T)
  ) %>% 
  pivot_wider(names_from = LOCALE, values_from = pop, names_prefix = "pop_", values_fill = 0) %>% 
  mutate(
    urb_pop_edge = sum(pop_11, pop_12, pop_13),
    sub_pop_edge = sum(pop_21, pop_22, pop_23),
    twn_pop_edge = sum(pop_31, pop_32, pop_33),
    rur_pop_edge = sum(pop_41, pop_42, pop_43),
    pop_edge = sum(pop_11, pop_12, pop_13, pop_21, pop_22, pop_23, pop_31, pop_32, pop_33, pop_41, pop_42, pop_43),
    urb_pct_edge = urb_pop_edge / pop_edge, 
    sub_pct_edge = sub_pop_edge / pop_edge, 
    twn_pct_edge = twn_pop_edge / pop_edge,
    rur_pct_edge = rur_pop_edge / pop_edge
    ) %>% 
  select(st_fips, urb_pct_edge, sub_pct_edge, twn_pct_edge, rur_pct_edge, pop_edge)

st_ec_reg <- st_pops %>% 
  inner_join(state_rur, by = c("st_fips" = "STATE")) %>% 
  inner_join(states_elec %>% select(GEOID, ratio2020), by = c("st_fips" = "GEOID")) %>% 
  inner_join(states, by = c("st_fips" = "GEOID")) %>% 
  inner_join(state_pres, by = c("st_fips" = "st_fips"))



### MODELS
ec_st_lm <- list(lm_st_rurcb <- lm(ratio2020 ~ st_rur_pt, data = st_ec_reg),
                lm_st_rure <- lm(ratio2020 ~ rur_pct_edge, data = st_ec_reg),
                lm_st_sube <- lm(ratio2020 ~ sub_pct_edge, data = st_ec_reg),
                lm_st_wht <- lm(ratio2020 ~ wht_pct, data = st_ec_reg),
                lm_st_blk <- lm(ratio2020 ~ blk_pct, data = st_ec_reg),
                lm_st_mhi <- lm(ratio2020 ~ mhiE, data = st_ec_reg),
                lm_st_pov <- lm(ratio2020 ~ pov_pct, data = st_ec_reg),
                lm_st_edu <- lm(ratio2020 ~ bch_pct, data = st_ec_reg),
                lm_st_age <- lm(ratio2020 ~ ageE, data = st_ec_reg),
                lm_st_rep <- lm(ratio2020 ~ rep_pct, data = st_ec_reg),
                lm_st_demo <- lm(ratio2020 ~ wht_pct + blk_pct + mhiE + pov_pct + bch_pct + ageE, data = st_ec_reg),
                lm_st_eco <- lm(ratio2020 ~ pov_pct + mhiE, data = st_ec_reg),
                lm_st_valid_cb <- lm(ratio2020 ~ st_rur_pt + wht_pct + rep_pct, data = st_ec_reg),
                lm_st_valid_e  <- lm(ratio2020 ~ rur_pct_edge + wht_pct + rep_pct, data = st_ec_reg),
                lm_st_valid_esub  <- lm(ratio2020 ~ sub_pct_edge + wht_pct + rep_pct, data = st_ec_reg)
                
                )

modelsummary(ec_st_lm, statistic = "p.value", stars = TRUE)


summary(lm(ratio2020 ~ pov_pct + mhiE, data = st_ec_reg))  


### COUNTY MODELS

state_list = get_state_list(states = "all_dc")
bg_pops <- acs_call(yr = 2020, varlist = c("pop"= "B01003_001"), states = state_list, level = "block group", surv = "acs5")
bg_locale <- st_read("H:/RESEARCH/Moeser/UnpackingTheCensus/2024-2025/Alex/Thesis/poliscithesis_bgassignment_11.23.2024.shp") %>% 
  st_transform(5070)
#LOWER 48 ONLY
ct_pops <- bg_locale %>% 
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
    urb_pop_edge = sum(pop_11, pop_12, pop_13),
    sub_pop_edge = sum(pop_21, pop_22, pop_23),
    twn_pop_edge = sum(pop_31, pop_32, pop_33),
    rur_pop_edge = sum(pop_41, pop_42, pop_43),
    pop_edge = sum(pop_11, pop_12, pop_13, pop_21, pop_22, pop_23, pop_31, pop_32, pop_33, pop_41, pop_42, pop_43),
    urb_pct_edge = urb_pop_edge / pop_edge, 
    sub_pct_edge = sub_pop_edge / pop_edge, 
    twn_pct_edge = twn_pop_edge / pop_edge,
    rur_pct_edge = rur_pop_edge / pop_edge
  ) %>% 
  select(ct_fips, urb_pct_edge, sub_pct_edge, twn_pct_edge, rur_pct_edge, pop_edge)

ct_rur <- county_rur %>% 
  mutate(ct_fips = paste0(STATE, COUNTY),
         pct_rur_cb = POP_RUR / POP_COU) %>% 
  select(ct_fips, pct_rur_cb)

ct_demo <- get_acs(year = 2020, 
                  variables = c("pop" = "B01003_001",
                                "wht" = "B01001H_001",
                                "blk" = "B01001B_001",
                                "mhi" = "B19013_001",
                                "bch" = "B16010_041", 
                                "bchref" = "B16010_001",
                                "age" = "B01002_001",
                                "pov" = "B17003_002",
                                "povref" = "B17003_001"), 
                  survey = "acs5", 
                  geography = "county",
                  output = "wide",
                  geometry = T) %>%  
  select(!ends_with("M")) %>% 
  mutate(bch_pct = bchE / bchrefE,
         blk_pct = blkE / popE,
         wht_pct = whtE / popE,
         pov_pct = povE / povrefE)


ct_pres <- county_pres %>% 
  mutate(
    rep_pct = votes_REPUBLICAN / total_votes,
    dem_pct = votes_DEMOCRAT / total_votes
  )

ct_ec_reg <- ct_pops %>% 
  #CB RURALITY
  inner_join(ct_rur, by = c("ct_fips" = "ct_fips")) %>% 
  #MAKE STATE FIPS
  mutate(st_fips = substr(ct_fips, 1, 2)) %>% 
  # SSTATE VOTES
  inner_join(states_elec %>% select(GEOID, ratio2020), by = c("st_fips" = "GEOID")) %>%  
  # DEMOS FROM ACS
  inner_join(ct_demo, by = c("ct_fips" = "GEOID")) %>% 
  #Votinhg direction
  inner_join(ct_pres, by = c("ct_fips" = "fips"))

ec_ct_lm <- list(lm_ct_rurcb <- lm(ratio2020 ~ pct_rur_cb, data = ct_ec_reg),
                 lm_ct_rure <- lm(ratio2020 ~ rur_pct_edge, data = ct_ec_reg),
                 lm_ct_sube <- lm(ratio2020 ~ sub_pct_edge, data = ct_ec_reg),
                 lm_ct_wht <- lm(ratio2020 ~ wht_pct, data = ct_ec_reg),
                 lm_ct_blk <- lm(ratio2020 ~ blk_pct, data = ct_ec_reg),
                 lm_ct_mhi <- lm(ratio2020 ~ mhiE, data = ct_ec_reg),
                 lm_ct_pov <- lm(ratio2020 ~ pov_pct, data = ct_ec_reg),
                 lm_ct_edu <- lm(ratio2020 ~ bch_pct, data = ct_ec_reg),
                 lm_ct_age <- lm(ratio2020 ~ ageE, data = ct_ec_reg),
                 lm_ct_rep <- lm(ratio2020 ~ rep_pct, data = ct_ec_reg),
                 lm_ct_demo <- lm(ratio2020 ~ wht_pct + blk_pct + mhiE + pov_pct + bch_pct + ageE, data = ct_ec_reg),
                 lm_ct_eco <- lm(ratio2020 ~ pov_pct + mhiE, data = ct_ec_reg),
                 lm_ct_valid_cb <- lm(ratio2020 ~ pct_rur_cb + wht_pct + pov_pct + ageE + rep_pct, data = ct_ec_reg),
                 lm_ct_valid_e <- lm(ratio2020 ~ rur_pct_edge + wht_pct + pov_pct + ageE + rep_pct, data = ct_ec_reg),
                 lm_ct_valid_e <- lm(ratio2020 ~ sub_pct_edge + wht_pct + pov_pct + ageE + rep_pct, data = ct_ec_reg)     
)

modelsummary(ec_ct_lm, statistic = "p.value", stars = TRUE)

