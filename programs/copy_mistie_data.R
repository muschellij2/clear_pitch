library(methods)
library(here)
library(dplyr)
library(readr)
library(tidyr)

pname_to_pid = function(patientName, study = "MISTIE") {
  patientName = as.integer(patientName)
  divisor = ifelse(study == "CLEAR", 10000, 1000)
  p_id = paste0(patientName %% divisor, "-",
                floor(patientName / divisor))
  p_id[ study == "MISTIE" ] = paste0("0",
                                     p_id[ study == "MISTIE" ])
  p_id
}

img_dir = here("mistie_data")
fnames = list.files(pattern = ".nii.gz", path = img_dir)

df = data_frame(fname = fnames)
df$type = "CT"
df$type[ grepl("ROI", df$fname)] = "Msk"
df = df %>% 
  mutate(stub = sub(".nii.gz", "", fname),
         stub = sub("ROI$", "", stub),
         stub = sub("CT_.*_CT.*", "CT", stub),
         stub = sub("(.*_CT).*", "\\1", stub),
         stub = sub("_CT$", "", stub)
  )
df = df %>% 
  separate(col = stub, into = c("patientName", "date", "time"),
           sep = "_") %>% 
  mutate(datetime = paste0(date, time)) %>% 
  select(-date, -time) %>% 
  distinct()
df = df %>% 
  mutate(id = pname_to_pid(sub("-", "", patientName)),
         stub = paste0(id, "_", datetime),
         outfile = paste0(stub, "_", type, ".nii.gz"))

wide = df %>% 
  select(stub, fname, type) %>% 
  spread(key = type, value = fname)

stopifnot(!any(is.na(wide$CT)))
stopifnot(!any(is.na(wide$Msk)))


df$outfile = file.path("batch", df$outfile)
df = df[ !file.exists(df$outfile),]

if (nrow(df) > 0) {
  file.copy(file.path(img_dir, df$fname), df$outfile)
}
