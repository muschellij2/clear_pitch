library(dplyr)
library(readr)

set.seed(20180129)
x = load("Reseg_111_Filenames_with_volumes.Rda")
groups = fdf[, c("id", "group")]
ss = strsplit(fdf$id, "-")
id = lapply(ss, rev)
id = sapply(id, paste, collapse = "-")
# 3 for mistie
id = paste0(0, id)
groups$id = id

groups = groups %>% 
  mutate(rev_id = sub("-", "", id))

data = read_csv("MISTIE_II_Patient_Demographics.csv")
cn = c("patientName", 
       "Clot_Location_RC", "Pre_Rand_ICHvol", 
       "Pre_Rand_IVHvol", "Age", "Gender", "Ethnicity")
data = data[, cn]
colnames(data) = c("patientName", "ich_location", "ich_volume", "ivh_volume",
                   "age", "sex", "race")
data$rev_id = paste0("0", data$patientName %% 1000, 
                          floor(data$patientName /1000))


df = data
df = df %>% 
  select(patientName, ich_location, ich_volume, ivh_volume, 
         age, sex, race)
write_csv(df, path = "mistie_demographics.csv")

data = left_join(data, groups)
data$group[ is.na(data$group)] = "Excluded"

df = data
df = df %>% 
  mutate(batch = ifelse(group == "Train", 1, 2)) %>% 
  select(patientName, batch) 

outfile = "mistie_batches.csv"
write_csv(df, outfile)
