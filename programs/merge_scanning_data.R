#
library(methods)
library(dcmtk)
library(here)
library(tidyr)
library(dplyr)
library(readr)

#################################
# Scanning information
#################################
scan_df = read_rds("scanning_data.rds")
scan_df$StudyTime = round(scan_df$StudyTime)
scan_df$StudyTime[is.na(scan_df$StudyTime)] = 0
scan_df$StudyTime = sprintf("%06.0f", scan_df$StudyTime )
scan_df$datetime = paste0(scan_df$StudyDate, scan_df$StudyTime)
scan_df$id = sapply(
  strsplit( sub(".*raw/(.*)/.*", "\\1", scan_df$path), 
            split = "/"),
  dplyr::first)
scan_df = as_data_frame(scan_df)
dim(scan_df)
scan_df = scan_df %>% 
  select(-contains("Time", ignore.case = FALSE),
         -contains("Date", ignore.case = FALSE),
         -file) %>% 
  distinct()
dim(scan_df)

#################################
# Cross ref with the niis
#################################
files = list.files(pattern = ".nii.gz", path = here("batch"))
df = data_frame(fname = files)
df = df %>% 
  separate(col = fname, into = c("id", "datetime", "blah"),
           sep = "_") %>% 
  select(-blah) %>% 
  distinct()
# drop MISTIE 
df = df %>% 
  filter(!grepl("^0", id))

#################################
# make sure all are in there
#################################
df %>% 
  select(id, datetime)
sdf = scan_df %>% 
  select(id, datetime)

n_missing = anti_join(df, sdf) %>% distinct() %>% nrow()
if (n_missing > 0) {
  stop("Missing some DICOM data!")
}
anti_join(sdf, df) %>% distinct()

nrow(df)
df = left_join(df, scan_df, by = c("id", "datetime")) %>% 
  arrange(id, datetime)
nrow(df)

n_u = function(x) {
  x = as.character(x)
  x = x[!is.na(x)]
  n_unique = length(unique(x))
  return(n_unique)
}
u1 = function(x) {
  x = as.character(x)
  x = x[!is.na(x)]
  if (length(x) > 0) {
    unique(x)[1]
  } else {
    NA_character_
  }
}

df = df %>% 
  select(-path)
ddf = df %>%
  group_by(id, datetime) %>% 
  summarize_all(u1)

stopifnot(!any(is.na(ddf$Exposure) & !is.na(ddf$ExposureInuAs)))
ddf$ExposureInuAs = NULL


ddf$Manufacturer[ ddf$id == "4379-262" &
                  ddf$Manufacturer == "MPTronic software"] = "GE MEDICAL SYSTEMS"
# because of privatecreator area
ddf$Manufacturer[ ddf$id == "4346-232" & is.na(ddf$Manufacturer) ] = 
  "TOSHIBA"
ddf$Manufacturer[ ddf$id == "4427-104" & is.na(ddf$Manufacturer) ] = 
  "GE MEDICAL SYSTEMS"
ddf$Manufacturer[ ddf$id == "4428-104" & is.na(ddf$Manufacturer) ] = 
  ""

xdf = df %>%
  group_by(id, datetime) %>% 
  summarize_all(n_u)
