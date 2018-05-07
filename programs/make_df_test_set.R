library(here)
library(dplyr)
library(tidyr)
library(readr)
library(neurobase)

# root_dir <- "~/CLEAR_PITCH"
root_dir = here::here()
test_dir = file.path(root_dir, "test_set")
proc_dir = file.path(root_dir, "test_set_processed")
res_dir = file.path(root_dir, "results")


imgs = list.files(
  path = test_dir,
  recursive = TRUE,
  pattern = ".nii.gz",
  full.names = TRUE)

df = data_frame(file = imgs) %>% 
  mutate(fname = nii.stub(file, bn= TRUE))
df = df %>% 
  separate(fname, into =c("id", "date",
   "time", "type"),
    sep = "_") %>% 
  mutate(scan = paste0(id, "_", date, "_", time))

df = df %>% 
  spread(type, file)
df$id_proc_dir = file.path(proc_dir, df$id)
df$stub = sub("_SCAN", "", nii.stub(df$SCAN, 
  bn = TRUE))
df$CT = df$SCAN
df$Msk = df$ROI
df$SCAN = df$ROI = NULL

outfile = file.path(res_dir, "test_filenames_df.rds")
write_rds(df, path = outfile)

