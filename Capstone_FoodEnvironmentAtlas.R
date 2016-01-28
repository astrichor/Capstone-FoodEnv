### Springboard - Foundations of Data Science
### Rebecca H. Wang's Capstone Project
### Food Environment Atlas


## Function to remove commas from values
setAs("character", "num.with.commas",
      function(from) as.numeric(gsub(",", "", from)))


## Load and transform data: state programs and populations
rawstate <- read.csv("FEA_state_programs.csv", header = TRUE,
                     colClasses = c("factor", rep("num.with.commas", 32)))
colnames(rawstate) <- c("state", "WIC_count_2009", "WIC_count_2011", "WIC_count_2012",
                        "WIC_count_2013", "WIC_count_2014", "NSLP_count_2009", "NSLP_count_2011",
                        "NSLP_count_2012", "NSLP_count_2013", "NSLP_count_2014", "SBP_count_2009",
                        "SBP_count_2011", "SBP_count_2012", "SBP_count_2013", "SBP_count_2014",
                        "CAC_count_2009", "CAC_count_2011", "CAC_count_2012", "CAC_count_2013",
                        "CAC_count_2014", "SF_count_2009", "SF_count_2011", "SF_count_2012",
                        "SF_count_2013", "SF_count_2014", "pop_count_2008", "pop_count_2009",
                        "pop_count_2010", "pop_count_2011", "pop_count_2012", "pop_count_2013",
                        "pop_count_2014")
str(rawstate)
library(tidyr)
tidystate <- rawstate %>% 
  gather("program", "count", 2:33, na.rm = TRUE)
statepop <- tidystate %>% 
  separate("program", c("program", "year"), sep = "_count_", remove = TRUE, convert = TRUE)


## Load and transform data: county populations
rawcounty <- read.csv("FEA_county_populations.csv", header = TRUE,
                     colClasses = c("character", "factor", "character", rep("numeric", 6)))
colnames(rawcounty) <- c("FIPS_code", "state", "county", "pop_2007", "pop_2008", "pop_2009",
                        "pop_2010", "pop_2011", "pop_2012")
countypop <- rawcounty %>% 
  gather("pop", "count", 4:9, na.rm = TRUE) %>% 
  separate("pop", c("pop", "year"), sep = "_", remove = TRUE, convert = TRUE)
countypop$FIPS_code <- as.numeric(as.character(countypop$FIPS_code))
countypop$FIPS_code <- sprintf("%05d", countypop$FIPS_code)
countypop$pop = NULL


## Load and transform data: people who have low access to grocery stores in each county in 2010
rawaccess <- read.csv("FEA_county_access.csv", header = TRUE,
                      colClasses = c("character", "factor", "character", rep("numeric", 10)))
colnames(rawaccess) <- c("FIPS_code", "state", "county", "all", "pct_all", "low_inc",
                         "pct_low_inc", "children", "pct_children", "seniors", "pct_seniors",
                         "no_cars", "pct_no_cars")
str(countyaccess)
countyaccess1 <- rawaccess %>% 
  gather("demographic", "count", c(4, 6, 8, 10, 12), na.rm = TRUE)
countyaccess2 <- countyaccess1 %>% 
  gather("demographic1", "pct", 4:8, na.rm = TRUE)
access <- countyaccess2 %>% 
  unite(demographic, demographic1, sep = "_")


## Load data: participation in food assistance programs in each county
rawassist <- read.csv("FEA_county_assist.csv", header = TRUE,
                      colClasses = c("character", "factor", "character", rep("numeric", 52)))

# Create subset of food assistance program data, focusing on 2010
subassist <- rawassist[, c(1:5, 11:12, 14, 17, 20, 23, 26, 29, 34, 36, 43:44, 46:47)]
library(dplyr)
assist2010 <- subassist %>% 
  mutate("REDEMP_SNAPS10_est" = (REDEMP_SNAPS08 + REDEMP_SNAPS12)/2) %>% 
  mutate("PC_WIC_REDEMP10_est" = (PC_WIC_REDEMP08 + PC_WIC_REDEMP12)/2) %>% 
  mutate("REDEMP_WICS10_est" = (REDEMP_WICS08 + REDEMP_WICS12)/2)
assist2010$REDEMP_SNAPS08 = NULL
assist2010$REDEMP_SNAPS12 = NULL
assist2010$PC_WIC_REDEMP08 = NULL
assist2010$PC_WIC_REDEMP12 = NULL
assist2010$REDEMP_WICS08 = NULL
assist2010$REDEMP_WICS12 = NULL


## Load and transform data: health rates and contributing factors in each county
rawhealth <- read.csv("FEA_county_health.csv", header = TRUE,
                      colClasses = c("character", "factor", "character", rep("numeric", 16)))

# Create subset of health data, focusing on 2010 but also including some 2009
subhealth <- rawhealth[, c(1:3, 5, 7, 9:10, 12:14, 16:17, 19)]
health2010 <- subhealth %>% 
  mutate("PCT_OBESE_CHILD10_est" = (PCT_OBESE_CHILD08 + PCT_OBESE_CHILD11)/2) %>% 
  mutate("RECFAC10_est" = (RECFAC07 + RECFAC12)/2) %>% 
  mutate("RECFACPTH10_est" = (RECFACPTH07 + RECFACPTH12)/2)
health2010$PCT_OBESE_CHILD08 = NULL
health2010$PCT_OBESE_CHILD11 = NULL
health2010$RECFAC07 = NULL
health2010$RECFAC12 = NULL
health2010$RECFACPTH07 = NULL
health2010$RECFACPTH12 = NULL


## Load and transform data: households with food insecurity (i.e. "going hungry") in each county
rawinsec <- read.csv("FEA_county_insecurity.csv", header = TRUE,
                      colClasses = c("character", "factor", "character", rep("numeric", 12)))

insec <- rawinsec[, c(1:6, 9:11, 14:15)]   # Focus on the rates, not the % changes

colnames(insec) <- c("FIPS", "State", "County", "low_rate_00_02", "low_rate_07_09",
                     "low_rate_10_12", "very_low_rate_00_02", "very_low_rate_07_09",
                     "very_low_rate_10_12", "children_rate_01_07", "children_rate_03_11")
insecur <- insec %>% 
  gather("insecurity_type", "count", 4:11, na.rm = TRUE)
insecurity <- insecur %>% 
  separate("insecurity_type", c("insecurity_type", "years"), sep = "_rate_", remove = TRUE,
           convert = TRUE)


## Load and transform data: prices of milk and soda, and taxes on soda and chips, in each county
rawprices <- read.csv("FEA_county_prices.csv", header = TRUE,
                      colClasses = c("character", "factor", "character", rep("numeric", 8)))

# One data subset for prices
prices <- rawprices[, 1:5]
colnames(prices) <- c("FIPS", "State", "County", "milk", "soda")
prices <- gather(prices, "product", "price", 4:5, na.rm = TRUE)

# And a separate subset for taxes
taxes <- rawprices[, c(1:3, 7:11)]
colnames(taxes) <- c("FIPS", "State", "County", "soda_stores", "soda_vend", "chips_stores",
                     "chips_vend", "food")
taxes <- gather(taxes, "product", "tax", 4:8, na.rm = TRUE)


## Load and transform data: types of restaurants available in each county
rawrest <- read.csv("FEA_county_restaurants.csv", header = TRUE,
                    colClasses = c("character", "factor", "character", rep("numeric", 16)))

# Remove % changes again, as well as per capita expenditures (since those are at state level)
restaurants1 <- rawrest[, c(1:5, 7:8, 10:11, 13:14)]

colnames(restaurants1) <- c("FIPS", "State", "County", "fast_food_count_07", "fast_food_count_12",
                           "fast_food_pth_07", "fast_food_pth_12", "full_service_count_07",
                           "full_service_count_12", "full_service_pth_07", "full_service_pth_12")
restaurants2 <- restaurants1 %>% 
  gather("type", "count", c(4:5, 8:9), na.rm = TRUE) %>% 
  separate("type", c("type", "year"), sep = "_count_", remove = TRUE, convert = TRUE)
restaurants3 <- restaurants2 %>% 
  gather("type2", "pth", 4:7, na.rm = TRUE)
restaurants4 <- restaurants3 %>% 
  separate("type2", c("type2", "year2"), sep = "_pth_", remove = TRUE, convert = TRUE)
restaurants <- restaurants4 %>% 
  unite(type, type2) %>% 
  unite(year, year2)


## Load and transform data: types of stores available in each county
rawstores <- read.csv("FEA_county_stores.csv", header = TRUE,
                      colClasses = c("character", "factor", "character", rep("numeric", 36)))

# Remove % changes again
stores1 <- rawstores[, c(1:5, 7:8, 10:11, 13:14, 16:17, 19:20, 22:23, 25:26, 28:29, 31:32, 
                     34:35, 37:38)]

colnames(stores1) <- c("FIPS", "State", "County", "groc_count_07", "groc_count_12",
                       "groc_pth_07", "groc_pth_12", "superc_count_07", "superc_count_12",
                       "superc_pth_07", "superc_pth_12", "convs_count_07", "convs_count_12",
                       "convs_pth_07", "convs_pth_12", "specs_count_07", "specs_count_12",
                       "specs_pth_07", "specs_pth_12", "snaps_count_08", "snaps_count_12",
                       "snaps_pth_08", "snaps_pth_12", "wics_count_08", "wics_count_12",
                       "wics_pth_08", "wics_pth_12")
stores2 <- stores1 %>% 
  gather("type", "count", c(4:5, 8:9, 12:13, 16:17, 20:21, 24:25), na.rm = TRUE) %>% 
  separate("type", c("type", "year"), sep = "_count_", remove = TRUE, convert = TRUE)
stores3 <- stores2 %>% 
  gather("type2", "pth", 4:15, na.rm = TRUE) %>% 
  separate("type2", c("type2", "year2"), sep = "_pth_", remove = TRUE, convert = TRUE)
stores <- stores3 %>% 
  unite(type, type2) %>% 
  unite(year, year2)
stores$FIPS <- as.numeric(as.character(stores$FIPS))
stores$FIPS <- sprintf("%05d", stores$FIPS)
str(stores)


## Load and transform data: socioeconomic factors for each county
rawsocio <- read.csv("FEA_county_socioeconomic.csv", header = TRUE,
                     colClasses = c("character", "factor", "character", rep("numeric", 10),
                                    "factor", "numeric", rep("factor", 3)))
socioecon <- rawsocio %>% 
  gather("demo_stat", "value", c(4:13, 15), na.rm = TRUE)
socioecon$FIPS <- as.numeric(as.character(socioecon$FIPS))
socioecon$FIPS <- sprintf("%05d", socioecon$FIPS)
colnames(socioecon) <- c("FIPS", "State", "County", "has_pers_pov", "has_pers_child_pov",
                         "is_metro", "has_pop_loss", "demo_stat", "value")


## Find state with highest median household income across counties
library(dplyr)
incomestats <- socioecon %>% 
  filter(demo_stat == 'MEDHHINC10')
incomestats$value <- as.numeric(incomestats$value)
incomes <- incomestats %>% 
  group_by(State) %>% 
  summarise(med_inc = median(value)) %>% 
  arrange(desc(med_inc))
head(incomes)   # NJ

# Find state with lowest median household income across counties
tail(incomes)   # MS

## Find state with greatest increase in welfare program participation
ctyassists <- rawassist %>% 
  group_by(State) %>% 
  summarise(pch_all = max(PCH_SNAP_09_14) + max(PCH_NSLP_09_14) + max(PCH_SBP_09_14) +
                      max(PCH_SFSP_09_14) + max(PCH_WIC_09_14) + max(PCH_CACFP_09_14)) %>% 
  arrange(desc(pch_all))
head(ctyassists)   # NV

# Find state with greatest decrease in welfare program participation
tail(ctyassists)   # ND (there is no need to look for each state's MIN rate since
                   # the rate is at the state level, so the MIN and MAX are the same)

## Find state with highest mean Natural Amenity Index
nai <- rawhealth %>% 
  group_by(State) %>% 
  summarise(mean_nai = mean(NATAMEN)) %>% 
  arrange(desc(mean_nai))
head(nai)   # CA

# Find state with lowest mean Natural Amenity Index
tail(nai)   # MN (excluding NA values)


## Plot access
