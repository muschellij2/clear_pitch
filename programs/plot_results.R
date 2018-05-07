# Rnosave run_predict.R -N PRED \
# -l mem_free=40G,h_vmem=41G -hold_jid MODEL
library(here)
library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(caret)
library(neurobase)
library(ROCR)
ichmod = ichseg::rf_modlist$mod
ich_cutoff = ichseg::rf_modlist
ich_cutoff = ich_cutoff$mod.dice.coef[, "cutoff"]
ich_sm_cutoff = ichseg::smoothed_rf_cutoffs
ich_sm_cutoff = ich_sm_cutoff$mod.dice.coef
ich_sm_cutoff = ich_sm_cutoff[, "cutoff"]

set.seed(20180227)
# root_dir <- "~/CLEAR_PITCH"
root_dir = here::here()
source(file.path(root_dir, "programs", 
  "helper_functions.R"))

res_dir = file.path(root_dir, "results")
mod_dir = file.path(root_dir, "models")

batches = c("batch", "test_set")
batch_type = batches[1]

fname = switch(batch_type,
  batch = "filenames_df.rds",
  "test_set" = "test_filenames_df.rds")
filenames = file.path(res_dir, fname)

if (batch_type == "batch") {
  groups = c("train", "test", "validation")
} else {
  groups = "test"
}

model_groups = "train"
model = c("ranger", "logistic", "leekasso",
    "ichmodel")

run_frac = 0.1

iscen = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iscen)) {
  iscen = 37
}


eg = expand.grid(n4 = c(FALSE, TRUE),
                 run_frac = run_frac,
                 stratified = c(FALSE, TRUE),
                 group = groups, 
                 model = model,                 
                 model_group = model_groups,  
                 stringsAsFactors = FALSE)
eg = eg %>% 
  filter(!(model == "ichmodel" & (n4 | stratified)))


df = read_rds(filenames)


n4 = eg$n4[iscen]
group = eg$group[iscen]
model_group = eg$model_group[iscen]
stratified = eg$stratified[iscen]
run_frac = eg$run_frac[iscen]
eg = eg %>% 
  mutate(
    app = ifelse(model_group == "train",
      "", paste0(model_group, "_")),    
    outfile = file.path(mod_dir, 
      paste0(model, "_", 
      ifelse(n4, "n4_", ""), 
      ifelse(run_frac != 0.1, 
        paste0(run_frac, "_"),  ""),
      ifelse(stratified, "stratified_", ""),   
      app,
      "model.rds"))) %>% 
  select(-app)
mod_file = eg$outfile[iscen]
model_group = eg$model_group[iscen]
mod_stub = tools::file_path_sans_ext(
  basename(mod_file))
mod_stub = sub("ranger_", "", mod_stub)
if (grepl("ichmodel", mod_stub)) {
  mod_stub = "ichmodel"
}


# Compute the dice on sub-set
if (batch_type == "batch") {
  df = df[ df$train %in% group, ]
}

df$condensed_file = file.path(df$id_proc_dir, 
  paste0(df$stub, "_",
    mod_stub,
    "_condensed_roc.rds"))

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
df$sm_prob_file = file.path(df$id_proc_dir, 
  paste0(df$stub, "_",
    mod_stub,
    "_smoothed",
    "_phat.nii.gz"))
df$image_plot_file = file.path(res_dir,
  paste0(df$stub, "_image.png"))

df$plot_file = file.path(res_dir,
  paste0(df$stub, "_",
    mod_stub,
    ".png"))
df$sm_plot_file = file.path(res_dir,
  paste0(df$stub, "_",
    mod_stub,
    "_smoothed",
    ".png"))

# use the test set (out of sample)
# cutoff if using validation set
cutoff_group = "train"
if (batch_type == "batch" &&
  group == "validation") {
  cutoff_group = "test"
}



###############################
# Keep only those with masks for this part
# still probably want prediction for scoring
df = df %>% 
  filter(!is.na(Msk))

perf_file = file.path(mod_dir, 
  paste0("ranger_performance_", 
    mod_stub, "_",
    cutoff_group,
    ".rds"))
if (file.exists(perf_file)) {
  x = read_rds(perf_file)
  cutoff = x$dice_cutoff
} else {
  if (grepl("ichmodel", mod_stub)) {
    cutoff = ich_cutoff
  } else {
    cutoff = ifelse(n4, 0.396, 0.4)
  }  
}

perf_file = file.path(mod_dir, 
  paste0("ranger_performance_", 
    mod_stub, "_",
    cutoff_group,
    "_smoothed",    
    ".rds"))
if (file.exists(perf_file)) {
  x = read_rds(perf_file)
  smooth_cutoff = x$dice_cutoff
} else {
  if (grepl("ichmodel", mod_stub)) {
    smooth_cutoff = ich_sm_cutoff
  } else {
    smooth_cutoff = ifelse(n4, 0.3811111, 0.3737037)
  }  
}

# sample only 10
# df = df %>% 
#   sample_n(10)
iid = 1

for (iid in seq(nrow(df))) {

  idf = df[iid,]
  print(iid)
  prob_file = idf$prob_file
  sm_prob_file = idf$sm_prob_file
  roi_fname = idf$reg_roi
  condensed_file = idf$condensed_file
  img_fname = idf$reg_img
  fnames = c(idf$plot_file,
    idf$sm_plot_file,
    idf$image_plot_file)

  if (!all(file.exists(fnames))) {
    
    img = readnii(img_fname)    
    roi = readnii(roi_fname)    
    check_mask_fail(roi)    
    # brain_mask = readnii(df$reg_brain_mask[iid])
    # check_mask_fail(brain_mask)    

    prob_img = readnii(prob_file)    
    sm_prob_img = readnii(sm_prob_file)
    
    images = list(
      non_smoothed = prob_img,
      smoothed = sm_prob_img)
    cutoffs = list(
      non_smoothed = cutoff,
      smoothed = smooth_cutoff)
    files = list(
      non_smoothed = idf$plot_file,
      smoothed = idf$sm_plot_file)    

    img = window_img(img)  
    plot_xyz = xyz(roi)
    ################################
    # Plot
    ################################
    itype = "non_smoothed"
    if (!file.exists(idf$image_plot_file)) {
      png(idf$image_plot_file, 
        res = 600, width = 10, height=5,
        units = "in", type = "cairo")
        ortho2(img, xyz = plot_xyz)
      dev.off()
    }

    for (itype in c("non_smoothed", "smoothed")) {
      print(itype)  
      pred = images[[itype]]
      pred = pred > cutoffs[[itype]]
      fname = files[[itype]]      
      cols = c("#56B4E9", "#D55E00", "#009E73")
      levels = c("False Negative", 
        "False Positive", 
        "True Positive")  

      if (!file.exists(fname)) {

        diff = roi + pred*2
        diff[roi == 0 & pred == 0] = NA

        png(fname, 
          res = 600, width = 10, height=5,
          units = "in", type = "cairo")
        ortho2(x = img, 
               y = diff, 
               # don't do alpha blending
               col.y = cols,
               xyz = plot_xyz, 
               addlegend = TRUE,
               legend = levels, 
               leg.col = cols, 
               leg.cex = 1.5,
               ybreaks = c(0, 1.1, 2.1, 3.1))
        dev.off()
      }
    }
  }
}

