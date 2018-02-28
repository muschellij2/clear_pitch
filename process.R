# Rnosave process.R -N PROC -l mem_free=8G,h_vmem=10G -t 1-23
library(ichseg)
library(here)
library(dplyr)
library(tidyr)
library(neurobase)
library(readr)


# root_dir <- "~/CLEAR_PITCH"
root_dir = here::here()
img_dir = file.path(root_dir, "original")
proc_dir = file.path(root_dir, "processed")

imgs = list.files(
  path = img_dir,
  pattern = ".nii.gz",
  full.names = TRUE)

df = data_frame(file = imgs) %>% 
  mutate(fname = nii.stub(file, bn= TRUE))
df = df %>% 
  separate(fname, into =c("id", "date", "type"),
    sep = "_") %>% 
  mutate(scan = paste0(id, "_", date))
bad_scans = "4108-279_20110224183701"
df = df %>% 
  filter(!scan %in% bad_scans)

df = df %>% 
  spread(type, file)
df$id_proc_dir = file.path(proc_dir, df$id)
df$stub = sub("_CT", "", nii.stub(df$CT, 
  bn = TRUE))
df$outfile = file.path(df$id_proc_dir,
  paste0(df$stub, "_",  "predictor_df.rds"))


n_ids = nrow(df)
iid = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iid)) {
  iid = 1
}

id = df$id[iid]
id_proc_dir = df$id_proc_dir[iid]
dir.create(id_proc_dir, showWarnings = FALSE)

img = df$CT[iid]
msk = df$Msk[iid]

ss_file = file.path(id_proc_dir, 
  "brain.nii.gz")
mask_file = file.path(id_proc_dir, 
  "brain_mask.nii.gz")
outfiles = c(ss_file, mask_file)
stub = sub("_CT", "", nii.stub(img, bn = TRUE))
outprefix = sub("_CT", "_", nii.stub(img))

if (file.exists(mask_file)) {
  mask = mask_file
} else {
  mask = NULL
}
outfile = file.path(id_proc_dir,
  paste0(stub, "_",  "predictor_df.rds"))

ufile = file.path(id_proc_dir, 
  paste0(stub, "_usemask.nii.gz"))

if (!file.exists(outfile)) {

  proc = ich_process_predictors(
    img = img, 
    maskfile = mask_file,
    mask = mask,
    outprefix = outprefix,
    stub = stub,
    roi  = msk,
    save_imgs = TRUE,
    outdir = id_proc_dir)

  idf = as_data_frame(proc$img.pred$df)
  idf$any_zero_neighbor = 
    as.integer(idf$any_zero_neighbor)
  idf$mask = idf$mask > 0
  proc$img.pred$df = idf
  idf = idf[ idf$mask | idf$Y > 0, ]

  write_rds(idf, path = outfile)
} else {
  idf = read_rds(outfile)
}
# usemask = readnii(ufile)

# dist_img = remake_img(df$dist_centroid,
#   usemask, usemask)


