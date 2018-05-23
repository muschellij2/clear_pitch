library(here)
library(dplyr)
library(tidyr)
library(readr)
library(neurobase)

# root_dir <- "~/CLEAR_PITCH"
root_dir = here::here()
img_dir = file.path(root_dir, "original")
batch_dir = file.path(root_dir, "batch")
proc_dir = file.path(root_dir, "processed")
res_dir = file.path(root_dir, "results")

pname_to_pid = function(patientName, study) {
  patientName = as.integer(patientName)
  divisor = ifelse(study == "CLEAR", 10000, 1000)
  p_id = paste0(patientName %% divisor, "-",
    floor(patientName / divisor))
  p_id[ study == "MISTIE" ] = paste0("0",
    p_id[ study == "MISTIE" ])
  p_id
}
batch_df = read_csv(
  file.path(root_dir, "batches.csv"))
batch_df$study = "CLEAR"
mistie_batch_df = read_csv(
  file.path(root_dir, "mistie_batches.csv"))
mistie_batch_df$study = "MISTIE"
batch_df = full_join(batch_df, mistie_batch_df)

batch_df = batch_df %>% 
  mutate(id = pname_to_pid(patientName, study))

imgs = list.files(
  path = img_dir,
  pattern = ".nii.gz",
  full.names = TRUE)
batch_imgs = list.files(
  path = batch_dir,
  pattern = ".nii.gz",
  full.names = TRUE)


df = bind_rows(
  data_frame(file = imgs, 
    batch_group = "original"), 
  data_frame(file = batch_imgs, 
    batch_group = "batch") 
  )

df = df %>% 
  mutate(fname = nii.stub(file, bn= TRUE))
df = df %>% 
  separate(fname, into =c("id", "date", "type"),
    sep = "_") %>% 
  mutate(scan = paste0(id, "_", date))
bad_scans = c("4108-279_20110224183701", # half a head
  "4398-279_20131203063327" # CTA scan
  )
df = df %>% 
	filter(!scan %in% bad_scans)
df = left_join(df, batch_df)

df = df %>% 
  spread(type, file)
df = df %>% 
  arrange(batch, id)
# df$train = "train"
df = df %>% 
  mutate(
    train = ifelse(batch == 1, "train", 
      ifelse(batch == 2, "test",
      "validation"))
  )

df$id_proc_dir = file.path(proc_dir, df$id)
df$stub = sub("_CT", "", nii.stub(df$CT, 
  bn = TRUE))
df = df %>% 
  arrange(study, batch, patientName)

df = df %>% 
  mutate(d2 = as.numeric(date)) %>% 
  arrange(id, d2) %>% 
  group_by(id) %>% 
  mutate(scan_index = seq(n())) %>% 
  select(-d2)

outfile = file.path(res_dir, "all_filenames_df.rds")
write_rds(df, path = outfile)




df = df %>% 
  filter(study == "CLEAR")
outfile = file.path(res_dir, "filenames_df.rds")
write_rds(df, path = outfile)

