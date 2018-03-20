# Rnosave process.R -N PROC -l mem_free=8G,h_vmem=10G -t 1-23
library(ichseg)
library(here)
library(dplyr)
library(tidyr)
library(neurobase)
library(readr)
library(extrantsr)


# root_dir <- "~/CLEAR_PITCH"
root_dir = here::here()
test_dir = file.path(root_dir, "test_set")
proc_dir = file.path(root_dir, "test_set_processed")
n4 = FALSE

imgs = list.files(
  path = test_dir,
  recursive = TRUE,
  pattern = ".nii.gz",
  full.names = TRUE)

df = data_frame(file = imgs) %>% 
  mutate(fname = nii.stub(file, bn= TRUE))
df = df %>% 
  separate(fname, into =c("id", "date", "time", "type"),
    sep = "_") %>% 
  mutate(scan = paste0(id, "_", date, "_", time))

df = df %>% 
  spread(type, file)
df$id_proc_dir = file.path(proc_dir, df$id)
df$stub = sub("_SCAN", "", nii.stub(df$SCAN, 
  bn = TRUE))
df$CT = df$SCAN
df$Msk = df$ROI
df$outfile = file.path(df$id_proc_dir,
  paste0(df$stub, "_",  "predictor_df.rds"))
df$SCAN = df$ROI = NULL

n_ids = nrow(df)
iid = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iid)) {
  iid = 8
}

id = df$id[iid]
id_proc_dir = df$id_proc_dir[iid]
dir.create(id_proc_dir, showWarnings = FALSE)

img = df$CT[iid]
msk = df$Msk[iid]
if (is.na(msk)) {
  msk = NULL
}
stub = df$stub[iid]

ss_file = file.path(id_proc_dir, 
  paste0(stub, "_brain.nii.gz"))
mask_file = file.path(id_proc_dir, 
  paste0(stub, "_brain_mask.nii.gz"))
outfiles = c(ss_file, mask_file)

ufile = file.path(id_proc_dir, 
  paste0(stub, "_usemask.nii.gz"))


if (file.exists(mask_file)) {
  mask = mask_file
} else {
  mask = NULL
}

if (n4) {
  img = readnii(img)
  brain_mask = readnii(mask_file)
  img = mask_img(img, mask = brain_mask)
  img[ img < 0] = 0  
  # n4 = bias_correct(img, correction = "N4",
  #   mask = brain_mask)
  img = window_img(img, c(0, 100))
  n4_2 = bias_correct(img, correction = "N4",
    mask = brain_mask)
  img = n4_2
  stub = paste0(stub, "_n4")
}

outprefix = file.path(
  id_proc_dir,
  paste0(stub, "_")
  )

outfile = file.path(id_proc_dir,
  paste0(stub, "_",  
    "predictor_df.rds"))



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
} 
# else {
#   idf = read_rds(outfile)
# }
# usemask = readnii(ufile)

# dist_img = remake_img(df$dist_centroid,
#   usemask, usemask)


