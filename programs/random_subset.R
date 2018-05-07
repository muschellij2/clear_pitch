library(haven)
library(dplyr)
library(readr)

set.seed(20180129)
# data = read_dta("Subject_Demographics_and_Clinical_History.dta")
# data = as_factor(data)
data = read_csv("Subject_Demographics_and_Clinical_History.csv")
cn = c("patientName", "Index_Clot_Location_Site",
       "Index_Clot_Location_RC", "Stability_Total_Blood_Volume_RC", 
       "Stability_ICH_Volume_RC", "Stability_IVH_Volume_RC", 
       "other_ich_location_RC", 
       "other_ich_location_Site", "Age_at_Consent", 
       "Gender", "Hispanic", "Race")
data = data[, cn]

data$pid = data$patientName %% 1000
data$first_250 = data$pid <= 250
xdata = data

data$other_ich_location_RC = tolower(data$other_ich_location_RC)
data$other_ich_location_Site = tolower(data$other_ich_location_Site)

data = data %>% 
  mutate(Index_Clot_Location_RC = as.character(Index_Clot_Location_RC),
         Index_Clot_Location_Site = as.character(Index_Clot_Location_Site),
         Index_Clot_Location_RC = recode(
           Index_Clot_Location_RC,
           "Globus Pallidus" = "Deep",
           Putamen = "Deep",
           "Caudate" = "Deep",
           "Frontal" = "Lobar",
           "Temporal" = "Lobar",
           Occipital = "Lobar",
           Parietal = "Lobar"
         ),
         Index_Clot_Location_Site = recode(
           Index_Clot_Location_Site,
           "Globus Pallidus" = "Deep",
           Putamen = "Deep",
           "Caudate" = "Deep",
           "Frontal" = "Lobar",
           "Temporal" = "Lobar",
           Occipital = "Lobar",
           Parietal = "Lobar"
         )
  )

data = data %>% 
  mutate(
    # fill in NA
    Index_Clot_Location_RC = if_else(
      is.na(Index_Clot_Location_RC),
      Index_Clot_Location_Site, 
      Index_Clot_Location_RC),
    # fill in other
    Index_Clot_Location_RC = if_else(
      Index_Clot_Location_RC == "Other",
      Index_Clot_Location_Site, Index_Clot_Location_RC
    ),
    Index_Clot_Location_RC = if_else(
      Index_Clot_Location_RC == "Other" & 
        grepl("corona radaita", other_ich_location_Site),
      "Deep", Index_Clot_Location_RC
    )
  )

df = data

df = df %>% 
  rename(ich_location = Index_Clot_Location_RC,
         ich_volume = Stability_ICH_Volume_RC,
         ivh_volume = Stability_IVH_Volume_RC,
         all_volume = Stability_Total_Blood_Volume_RC,
         age = Age_at_Consent,
         sex = Gender,
         race = Race,
         hispanic = Hispanic) %>% 
  select(patientName, ich_location, ich_volume, ivh_volume, all_volume,
         age, sex, race, hispanic)
write_csv(df, path = "clear_demographics.csv")


med = median(data$Stability_Total_Blood_Volume_RC, na.rm = TRUE)

data$large = data$Stability_Total_Blood_Volume_RC >= med


init_file = "initial_sample.csv"
outfile = "batches.csv"
if (file.exists(init_file)) {
  init = read_csv(init_file)
  data = data %>% 
    filter(!patientName %in% init$patientName)  
  init = init %>%
    select(patientName)
  init$batch = 1
}
n_batches = ceiling(nrow(data) / 50)
# data = xdata %>% filter(first_250)



# samp = data %>%
#   filter(Index_Clot_Location_RC != "Lobar") %>% 
#   group_by(Index_Clot_Location_RC, large) %>% 
#   sample_n(4) %>% 
#   arrange(pid) %>% 
#   ungroup
# samp = samp %>% 
#   select(patientName, Index_Clot_Location_RC, large)
# samp = samp %>% 
#   select(patientName)
# write_csv(samp, path = outfile)

################
# removing lobar
samp = data %>%
  group_by(Index_Clot_Location_RC, large) %>% 
  # add 1 for initial batch
  mutate(batch = sample.int(n_batches, size = n(),
                            replace = TRUE) + 1) %>% 
  arrange(pid) %>% 
  ungroup
samp = samp %>% 
  select(patientName, Index_Clot_Location_RC, large, batch) %>% 
  arrange(batch, patientName)
samp = samp %>% 
  select(patientName, batch) %>% 
  mutate(patientName = as.integer(patientName))
samp = full_join(samp, init)
stopifnot(!any(duplicated(samp$patientName)))
samp = samp %>% 
  arrange(batch, patientName)
write_csv(samp, path = outfile)

