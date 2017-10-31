# Data cleaning for the southeast Alaska yelloweye rockfish assessment
# ben.williams@alaska.gov
# updated 11-2017

# see Wiki for notes on data origins 
# https://github.com/commfish/southeast_alaska_yelloweye_rockfish/wiki

# load ----
source('r_code/helper.R')


# IPHC ----
# read in all files exlding the older IPHC survey data

list.files(path = "data/raw/iphc",
           pattern="*.csv", 
           full.names = T) %>% 
  .[ !grepl("data/raw/iphc/IPHC_historic_survey_data.csv",  .) ] %>%
  map_df(~read_csv(.)) %>% 
  dplyr::select(Vessel=VESSEL, station=STATION, year=YEAR, trip=TRIP, no_skt=NOSKT, area_code=AREA, number = TOT) %>% 
  mutate(effort = no_skt * 100,
         area = ifelse(area_code %in% 345607:365701, "CSEO",
                       ifelse(area_code %in% 375730:395902, "EYKT",
                              ifelse(area_code %in% 365731:365801, "NSEO",
                                     ifelse(area_code %in% 325432:355530, "SSEO", "outside"))))) %>% 
  dplyr::select(year, effort, station, area, number) %>% 
  filter(!is.na(year)) -> later_years

# vector of rocky habitat stations
rocky <- c("3106", "3078", "3084", "3086", "3092", "3097", 
           "3016", "3029", "3034", "3051", "3062", "3063", 
           "4027", "4019", "4018", "3113", "3110")

read_csv("data/raw/iphc/IPHC_historic_survey_data.csv")  %>% 
  filter(EFFECTIVE_SET=="Y") %>% 
  mutate(subset = ifelse(YEAR>2007 | is.na(SUBSAMPLE_CODE), 0, SUBSAMPLE_CODE),
         number = ifelse(subset==1, 
                         round(TOTAL_NUMBER * (HOOKS_RETRIEVED / HOOKS_OBSERVERD), 0),
                         TOTAL_NUMBER),
         effort = EFFECTIVE_SKATE_VALUE * 100) %>% 
  filter(!is.na(YEAR), !is.na(STATION)) %>% 
  dplyr::select(year = YEAR, 
  				  effort, 
  				  station = STATION, 
  				  area = MANAGEMENT_AREA_CODE, number) %>% 
  bind_rows(later_years) %>% 
	arrange(number) %>% 
	mutate(cpue = number / effort) %>% 
	group_by(year, area) %>% 
	summarise(var = var(cpue), cpue = mean(cpue)) %>% 
	ggplot(aes(year, cpue, color=area)) + geom_point() + geom_line()

# age comp ----

age_comp <- read_csv('data/raw/fishery/age_comp_1985_2016.csv')

age_comp %>% 
	dplyr::select(year = YEAR, Area = G_MANAGEMENT_AREA_CODE, age = AGE,
					  sex = SEX_CODE) %>% 
	filter(!is.na(age)) %>% 
	mutate(age = ifelse(age>75, 75, age), Year = factor(year)) %>% 
	ggplot(aes(age, fill=Area))+geom_density(adjust=0.45,alpha=.2)+
	xlab("Age")+
	ylab("Age-class proportion")

# age error matrix ----
age_error <- read.csv('data/raw/fishery/age_error_1995_2014.csv')

age_error %>% 
	dplyr::select(age = AGE, Read = READABILITY_CODE, true = RELEASE_AUTHORITATIVE,
					  Sample = SAMPLE_ID, specimen = SPECIMEN_ID, Sex = GENDER_CODE) %>% 
	filter(!is.na(age), !Read %in% c("MO", "NO"), age<=75) %>% 
	write_csv("data/fishery/age_error.csv")
	

