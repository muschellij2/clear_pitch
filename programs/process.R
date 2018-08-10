# Process data.  
library(ichseg)
library(here)
library(dplyr)
library(tidyr)
library(neurobase)
library(readr)
library(extrantsr)


# root_dir <- "~/CLEAR_PITCH"
root_dir = here::here()
# Process data.  
# Change batch_type if want to use original data
# Original data was used to train model
batches = c("batch", "test_set")
batch_type = batches[1]

img_dir = file.path(root_dir, batch_type)
proc_dir = file.path(root_dir, "processed")
res_dir = file.path(root_dir, "results")


filenames = file.path(res_dir, "all_filenames_df.rds")
df = read_rds(filenames)
df = df %>% 
  filter(batch_group == batch_type)
df$outfile = file.path(df$id_proc_dir,
  paste0(df$stub, "_",  "predictor_df.rds"))


n_ids = nrow(df)
iid = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iid)) {
  iid = 201
}

id = df$id[iid]
id_proc_dir = df$id_proc_dir[iid]
dir.create(id_proc_dir, showWarnings = FALSE)

img = df$CT[iid]
msk = df$Msk[iid]

if (is.na(msk)) {
  stop("Mask not found!")
}
ss_file = file.path(id_proc_dir, 
  "brain.nii.gz")
mask_file = file.path(id_proc_dir, 
  "brain_mask.nii.gz")

n4 = FALSE

for (n4 in c(FALSE, TRUE)) {
  print(id)
  
  stub = sub("_CT", "", nii.stub(img, bn = TRUE))

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

  if (!file.exists(outfile)) {

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
    idf = idf[ idf$mask | idf$Y > 0, ]

    write_rds(idf, path = outfile)
  } 
}

# else {
#   idf = read_rds(outfile)
# }
# usemask = readnii(ufile)

# dist_img = remake_img(df$dist_centroid,
#   usemask, usemask)


