##########################################
# Running on a test set of our data
##########################################
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
res_dir = file.path(root_dir, "results")
n4 = TRUE

outfile = file.path(res_dir, "test_filenames_df.rds")
df = read_rds(outfile)
df$outfile = file.path(df$id_proc_dir,
  paste0(df$stub, 
    ifelse(n4, "_n4", ""), 
    "_",  "predictor_df.rds"))


n_ids = nrow(df)
iid = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iid)) {
  # 28 has bad header
  iid = 220
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
  stub = paste0(stub, "_n4")
}

outprefix = file.path(
  id_proc_dir,
  paste0(stub, "_")
  )

outfile = file.path(id_proc_dir,
  paste0(stub, "_",  
    "predictor_df.rds"))


if (!file.exists(outfile) || 
    !file.exists(mask_file)) {

  if (n4) {
    if (file.exists(mask_file)) {
      brain_mask = readnii(mask_file)
    } else {
      ss = CT_Skull_Strip_robust(img, 
        retimg = TRUE)
      brain_mask = ss > 0
    }    
    img = readnii(img)
    img = mask_img(img, mask = brain_mask)
    img[ img < 0 ] = 0  
    # n4 = bias_correct(img, correction = "N4",
    #   mask = brain_mask)
    img = window_img(img, c(0, 100))
    n4_2 = bias_correct(img, correction = "N4",
      mask = brain_mask)
    img = n4_2
  }

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
  if (!all(is.na(idf$Y))) {
    ind = which(idf$mask | idf$Y > 0)
  } else {
    ind = idf$mask
  }
  idf = idf[ ind, ]


  write_rds(idf, path = outfile)
} 

warnings()

# else {
#   idf = read_rds(outfile)
# }
# usemask = readnii(ufile)

# dist_img = remake_img(df$dist_centroid,
#   usemask, usemask)


