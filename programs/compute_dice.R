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
  batch = "all_filenames_df.rds",
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
studies = c("CLEAR", "BOTH")

run_frac = 0.1

iscen = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iscen)) {
  iscen = 45
}


eg = expand.grid(n4 = c(FALSE, TRUE),
                 run_frac = run_frac,
                 stratified = c(FALSE, TRUE),
                 group = groups, 
                 model = model,                 
                 model_group = model_groups,  
                 study = studies,
                 stringsAsFactors = FALSE)
eg = eg %>% 
  filter(!(model == "ichmodel" & (n4 | stratified)))

  
n4 = eg$n4[iscen]
group = eg$group[iscen]
model_group = eg$model_group[iscen]
stratified = eg$stratified[iscen]
run_frac = eg$run_frac[iscen]
study = eg$study[iscen]

df = read_rds(filenames)

# keep first scan 
df = df %>% 
  mutate(d2 = as.numeric(date)) %>% 
  arrange(id, d2) %>% 
  group_by(id) %>% 
  dplyr::slice(1) %>% 
  select(-d2)

if (study == "CLEAR") {
  df = df %>% 
    filter(study == "CLEAR")
}

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
      ifelse(study == "CLEAR", "", "combined_"),
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

all_df = vector(mode = "list",
	length = nrow(df))
names(all_df) = df$scan
iid = 1

for (iid in seq(nrow(df))) {

  print(iid)
  prob_file = df$prob_file[iid]
  sm_prob_file = df$sm_prob_file[iid]
  roi_fname = df$reg_roi[iid]
  condensed_file = df$condensed_file[iid]

  if (file.exists(roi_fname)) {
    
    reg_df = NULL 
    sm_df = NULL  

    if (!file.exists(condensed_file)) {
      roi_mask = readnii(roi_fname)    
      check_mask_fail(roi_mask)    
      brain_mask = readnii(df$reg_brain_mask[iid])
      check_mask_fail(brain_mask)    
      roi_vals = mask_vals(roi_mask, brain_mask)

      if (file.exists(prob_file)) {
        prob_img = readnii(prob_file)
        p_vals = mask_vals(prob_img, brain_mask)    
        p_df = condense_prediction(p_vals, roi_vals)
        p_df$smoothed = FALSE      
        reg_df = p_df
      }

      if (file.exists(sm_prob_file)) {
        sm.pimg = readnii(sm_prob_file)
        p_vals = mask_vals(sm.pimg, brain_mask)    
        p_df = condense_prediction(p_vals, roi_vals)
        p_df$smoothed = TRUE
        sm_df = p_df
      }
      
      p_df = bind_rows(reg_df, sm_df)
      if (nrow(p_df) > 0) {
        write_rds(p_df, path = condensed_file)
      }

    } else {
      p_df = read_rds(condensed_file)      
    }

    all_df[[iid]] = p_df

  }
}

########################
# Stopped here
########################
masked_df = bind_rows(all_df, .id = "scan")

# perf = run_roc(full_df$p, full_df$Y)
# perf2 = run_roc(full_df$p2, full_df$Y)
any_null = function(...) {
  x = list(...)
  any(sapply(x, is.null))
}


dice_df = masked_df %>% 
  mutate(
    diff = cutoffs - cutoff
    ) %>% 
  filter(diff > 0) %>% 
  group_by(scan, smoothed) %>% 
  filter(diff == min(diff))

pop_dice_df = masked_df %>% 
  mutate(
    diff = cutoffs - cutoff
    ) %>% 
  filter(diff > 0) %>% 
  group_by(smoothed) %>% 
  filter(diff == min(diff)) %>% 
  summarize_at(vars(
    tn, fp, fn, tp), sum) %>% 
  mutate(dice = (2*tp)/ (2*tp + fn + fp))



outfile = file.path(mod_dir, 
  paste0("dice_", 
    mod_stub, "_",
    if_else(batch_type != "batch", 
      batch_type, group),
    ".rds"))
write_rds(dice_df, path = outfile)

# collapse over all the individual 
# scans
sum_df = masked_df %>% 
  group_by(cutoffs, smoothed) %>% 
  summarize_at(vars(
    tn, fp, fn, tp, n.pos, 
    n.neg, n.pos.pred, n.neg.pred, vol, pred_vol),
  sum) %>% 
  ungroup()

sum_df = sum_df %>% 
  group_by(smoothed, fp, tp, fn, tn,
    n.pos.pred, n.neg.pred, 
    n.pos, n.neg) %>% 
  arrange(cutoffs) %>% 
  dplyr::slice(1) %>% 
  arrange(desc(cutoffs)) %>% 
  ungroup()

sub_perf = run_df_roc(
  sum_df[ !sum_df$smoothed,])
sub_perf$dice = pop_dice_df$dice[
  !pop_dice_df$smoothed]

outfile = file.path(mod_dir, 
  paste0("ranger_performance_", 
    mod_stub, "_",
    ifelse(batch_type != "batch", 
      batch_type, group),
    ".rds"))
write_rds(sub_perf, path = outfile)


sub_perf2 = run_df_roc(
  sum_df[ sum_df$smoothed,])
sub_perf2$dice = pop_dice_df$dice[
  pop_dice_df$smoothed]  


outfile = file.path(mod_dir, 
  paste0("ranger_performance_", 
    mod_stub, "_",
    ifelse(batch_type != "batch", 
      batch_type, group),
    "_smoothed",
    ".rds"))
write_rds(sub_perf2, path = outfile)


