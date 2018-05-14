# Rnosave run_predict.R -N PRED \
# -l mem_free=40G,h_vmem=41G -hold_jid MODEL
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(caret)
library(neurobase)
library(ROCR)
mlist = ichseg::rf_modlist
ichmod = mlist$mod
ich_cutoff = mlist
ich_cutoff = mlist$mod.dice.coef[, "cutoff"]
ich_sm_cutoff = ichseg::smoothed_rf_cutoffs
ich_sm_cutoff = ich_sm_cutoff$mod.dice.coef
ich_sm_cutoff = ich_sm_cutoff[, "cutoff"]

set.seed(20180227)
# root_dir <- "~/CLEAR_PITCH"
root_dir = here::here()
source(file.path(root_dir, "programs", 
  "helper_functions.R"))
batches = c("batch", "test_set")
batch_type = batches[1]

res_dir = file.path(root_dir, "results")

fname = switch(batch_type,
  batch = "all_filenames_df.rds",
  "test_set" = "test_filenames_df.rds")

filenames = file.path(res_dir, fname)
df = read_rds(filenames)

iscen = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iscen)) {
  iscen = 24
}
model_groups = "train"
model = c("ranger", "logistic", "leekasso")
studies = c("CLEAR", "BOTH")

run_frac = 0.1
eg = expand.grid(n4 = c(FALSE, TRUE),
                 run_frac = run_frac,
                 stratified = c(FALSE, TRUE),
                 model_group = model_groups,
                 model = model,  
                 study = studies,
                 stringsAsFactors = FALSE)

eg = unique(eg)
eg = eg %>% 
  mutate(
    app = ifelse(model_group == "train",
      "", paste0(model_group, "_")),    
    outfile = file.path(root_dir, 
      "models",
      paste0(model, "_", 
      ifelse(n4, "n4_", ""), 
      ifelse(run_frac != 0.1, 
        paste0(run_frac, "_"),  ""),
      ifelse(stratified, "stratified_", ""),   
      ifelse(study == "CLEAR", "", "combined_"),       
      app,
      "model.rds"))) %>% 
  select(-app)


n4 = eg$n4[iscen]
model_group = eg$model_group[iscen]
stratified = eg$stratified[iscen]
model = eg$model[iscen]
run_frac = eg$run_frac[iscen]
study = eg$study[iscen]
  
mod_file = eg$outfile[iscen]
mod_stub = tools::file_path_sans_ext(
  basename(mod_file))
mod_stub = sub("ranger_", "", mod_stub)


if (n4) {
  df$stub = paste0(df$stub, "_n4")
}

df$rds = file.path(df$id_proc_dir,
  paste0(df$stub, "_",  "predictor_df.rds"))
df$ufile = file.path(df$id_proc_dir, 
  paste0(df$stub, "_usemask.nii.gz"))
df$reg_img = file.path(df$id_proc_dir, 
  paste0(df$stub, "_image.nii.gz"))
df$reg_roi = file.path(df$id_proc_dir, 
  paste0(df$stub, "_roi.nii.gz"))
df$reg_brain_mask = file.path(df$id_proc_dir, 
  paste0(df$stub, "_mask.nii.gz"))

df$prob_file = file.path(df$id_proc_dir, 
  paste0(df$stub, "_",
    mod_stub,
    "_phat.nii.gz"))
df$ich_prob_file = file.path(df$id_proc_dir, 
  paste0(df$stub, "_",
    "ichmodel",
    "_phat.nii.gz"))
df$ich_sm_prob_file = file.path(df$id_proc_dir, 
  paste0(df$stub, "_",
    "ichmodel",
    "_smoothed",    
    "_phat.nii.gz"))
df$ich_pred_file = file.path(df$id_proc_dir, 
  paste0(df$stub, "_",
    "ichmodel",
    "_prediction.nii.gz"))
df$ich_sm_pred_file = file.path(df$id_proc_dir, 
  paste0(df$stub, "_",
    "ichmodel",
    "_smoothed",    
    "_prediction.nii.gz"))
df$sm_prob_file = file.path(df$id_proc_dir, 
  paste0(df$stub, "_",
    mod_stub,
    "_smoothed",
    "_phat.nii.gz"))



###############################
# Keep only those with masks for this part
# still probably want prediction for scoring
df = df %>% 
  filter(!is.na(Msk))

model = read_rds(mod_file)
model = reduce_train_object(model)

# sample only 10
# df = df %>% 
#   sample_n(10)
iid = 6

for (iid in seq(nrow(df))) {
  
  print(iid)
  rds = df$rds[iid]
  prob_file = df$prob_file[iid]
  sm_prob_file = df$sm_prob_file[iid]
  ich_prob_file = df$ich_prob_file[iid]
  ich_sm_prob_file = df$ich_sm_prob_file[iid]
  ich_sm_pred_file = df$ich_sm_pred_file[iid]
  ich_pred_file = df$ich_pred_file[iid]

  if (file.exists(rds)) {
    
    if (!all(file.exists(
      c(prob_file, ich_prob_file, ich_pred_file))
      )) {
      proc_df = read_rds(rds)
      Y = proc_df$Y
      # need to remove for the cases where missing
      # values in Y but still want a prediction
      proc_df = proc_df %>% 
        select(-Y)
      if (any(is.na(proc_df))) {
        print(paste0(iid, " is bad"))
        next; 
      }
      p = predict(model, newdata = proc_df, 
        type = "prob")
      p = p[, "lesion"]
      proc_df$p = p
      p = predict(ichmod,
        newdata = proc_df, type = "prob")
      proc_df$p2 = p[, "1"]
      # putting Y back
      proc_df$Y = Y
      rm(Y);
      proc_df = proc_df %>% 
        select(p, p2, Y, mask)
      # all_df[[iid]] = proc_df
      
      # need this because may include y > 0 but 
      # not in mask
      proc_df = proc_df %>% 
        dplyr::filter(mask > 0)
      mask = readnii(df$ufile[iid])
      prob_img = remake_img(proc_df$p, mask, mask)
      writenii(prob_img, prob_file)

      ich_prob_img = remake_img(
        proc_df$p2, mask, mask)
      writenii(ich_prob_img, ich_prob_file)  

      ich_pred_img = ich_prob_img > ich_cutoff
      writenii(ich_pred_img, ich_pred_file)      
      rm(proc_df); 
    } 
    
    # prob_imgs[[iid]] = prob_img
    # rm(mask)

    if (!file.exists(sm_prob_file)) {
      if (!exists("prob_img")) {
        prob_img = readnii(prob_file)
      }
      sm.pimg = ichseg::mean_image(prob_img, 
        nvoxels = 1)
      sm.pimg[
        abs(sm.pimg) < .Machine$double.eps^0.5
        ] = 0
      sm.pimg = niftiarr(prob_img, sm.pimg)
      writenii(sm.pimg, sm_prob_file)
      rm(sm.pimg)
      rm(prob_img)
    } 

    if (!all(
      file.exists(c(
        ich_sm_prob_file,
        ich_sm_pred_file)
      ))) {
      if (!exists("ich_prob_img")) {
        ich_prob_img = readnii(ich_prob_file)
      }
      prob_img = ich_prob_img
      rm(ich_prob_img)        
      sm.pimg = ichseg::mean_image(prob_img, 
        nvoxels = 1)
      sm.pimg[
        abs(sm.pimg) < .Machine$double.eps^0.5
        ] = 0
      sm.pimg = niftiarr(prob_img, sm.pimg)
      writenii(sm.pimg, ich_sm_prob_file)

      sm.pimg = sm.pimg > ich_sm_cutoff
      writenii(sm.pimg, ich_sm_pred_file)

      rm(sm.pimg)
      rm(prob_img)
    }     

    # sm_prob_imgs[[iid]] = sm.pimg

  }
}

