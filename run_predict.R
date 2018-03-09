# Rnosave run_predict.R -N PRED \
# -l mem_free=40G,h_vmem=41G -hold_jid MODEL
library(here)
library(ichseg)
library(dplyr)
library(tidyr)
library(readr)
library(caret)
library(neurobase)
library(ranger)
library(ROCR)

set.seed(20180227)
# root_dir <- "~/CLEAR_PITCH"
root_dir = here::here()
img_dir = file.path(root_dir, "original")
proc_dir = file.path(root_dir, "processed")
res_dir = file.path(root_dir, "results")

iscen = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iscen)) {
  iscen = 1
}
groups = c("train", "test")
eg = expand.grid(n4 = c(FALSE, TRUE),
  group = groups, stringsAsFactors = FALSE)
n4 = eg$n4[iscen]
group = eg$group[iscen]


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
n_ids = min(12, nrow(df))
df$train = c(rep(TRUE, n_ids),
  rep(FALSE, nrow(df) - n_ids))
df$train = ifelse(df$train, "train", "test")
df = df[ df$train %in% group, ]

df$id_proc_dir = file.path(proc_dir, df$id)
df$stub = sub("_CT", "", nii.stub(df$CT, 
  bn = TRUE))
if (n4) {
  df$stub = paste0(df$stub, "_n4")
}

df$rds = file.path(df$id_proc_dir,
  paste0(df$stub, "_",  "predictor_df.rds"))
df$ufile = file.path(df$id_proc_dir, 
  paste0(df$stub, "_usemask.nii.gz"))
df$prob_file = file.path(df$id_proc_dir, 
  paste0(df$stub, "_phat.nii.gz"))
df$reg_img = file.path(df$id_proc_dir, 
  paste0(df$stub, "_image.nii.gz"))
df$reg_roi = file.path(df$id_proc_dir, 
  paste0(df$stub, "_roi.nii.gz"))
df$reg_brain_mask = file.path(df$id_proc_dir, 
  paste0(df$stub, "_mask.nii.gz"))


perf_file = file.path(root_dir, 
  paste0("ranger_performance_", 
    ifelse(n4, "n4_", ""),
    "train", ".rds"))
if (file.exists(perf_file)) {
  x = read_rds(perf_file)
  cutoff = x$dice_cutoff
} else {
  cutoff = ifelse(n4, 0.396, 0.4)
}

perf_file = file.path(root_dir, 
  paste0("ranger_performance_", 
    ifelse(n4, "n4_", ""),
    "smoothed_", "train", ".rds"))
if (file.exists(perf_file)) {
  x = read_rds(perf_file)
  smooth_cutoff = x$dice_cutoff
} else {
  smooth_cutoff = ifelse(n4, 0.3811111, 0.3737037)
}


mod_file = file.path(root_dir, 
  paste0("ranger_", ifelse(n4, "n4_", ""), 
    "model.rds"))

model = read_rds(mod_file)

all_df = vector(mode = "list",
	length = nrow(df))
names(all_df) = df$scan
iid = 1

all_imgs = all_masks = prob_imgs = all_df
sm_prob_imgs = all_brain_masks = all_masks

for (iid in seq(nrow(df))) {
  print(iid)
  proc_df = read_rds(df$rds[iid])
  p = predict(model, newdata = proc_df, 
    type = "prob")
  p = p[, "lesion"]
  proc_df$p = p
  p = predict(rf_modlist$mod,
    newdata = proc_df, type = "prob")
  proc_df$p2 = p[, "1"]
  proc_df = proc_df %>% 
    select(p, p2, Y, mask)
  # all_df[[iid]] = proc_df
  
  # need this because may include y > 0 but 
  # not in mask
  proc_df = proc_df %>% 
    filter(mask > 0)
  mask = readnii(df$ufile[iid])
  prob_img = remake_img(proc_df$p, mask, mask)
  writenii(prob_img, df$prob_file[iid])

  prob_imgs[[iid]] = prob_img
  rm(mask)

  sm.pimg = mean_image(prob_img, nvoxels = 1)
  sm.pimg[
    abs(sm.pimg) < .Machine$double.eps^0.5
    ] = 0
  sm.pimg = niftiarr(prob_img, sm.pimg)

  sm_prob_imgs[[iid]] = sm.pimg

  mask = readnii(df$reg_roi[iid])
  all_masks[[iid]] = mask  
  rm(mask)

  mask = readnii(df$reg_brain_mask[iid])
  all_brain_masks[[iid]] = mask  
  rm(mask)  

  mask = readnii(df$reg_img[iid])
  all_imgs[[iid]] = mask  
  rm(mask)  

  rm(proc_df); 
}

# full_df = bind_rows(all_df, .id = "scan")
# rm(all_df); gc()
# full_df$y = ifelse(full_df$Y > 0,
# 	"lesion", "non_lesion")
# full_df$y = factor(full_df$y,
#   levels = c("non_lesion", "lesion"))


opt.cut = function(perf){
    cut.ind = mapply(
      FUN=function(x, y, p){
        d = (x - 0)^2 + (y-1)^2
        ind = which(d == min(d))
        c(sensitivity = y[[ind]], 
          specificity = 1-x[[ind]], 
            cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, 
    perf@alpha.values)
}

tab_dice = function(tab) {
  2*tab[2,2] / (2*tab[2,2] + tab[1,2] + tab[2,1])
}

dice = function(x, y) {
  tab = extrantsr:::my.tab(c(x), c(y))
  tab_dice(tab)
}

opt.dice = function(pred){
    cut.ind = mapply(
      FUN=function(tp, fp, fn, p) {
        dice = 2*tp / (2*tp + fn + fp)
        ind = which(dice == max(dice))
        c(dice = dice[[ind]], 
          cutoff = p[[ind]])        
    }, pred@tp, pred@fp, pred@fn, 
    pred@cutoffs)
}


run_roc = function(x, y, fpr.stop = 0.01) {
  pred = prediction(x, y)
  auc = performance(pred, "auc", 
    fpr.stop = fpr.stop)
  pauc = unlist(auc@y.values)/fpr.stop
  print(pauc)
  perf = performance(pred, "tpr", "fpr")
  cutoff = opt.cut(perf)
  dice_cutoff = opt.dice(pred)

  cutoff = cutoff["cutoff",]
  dice_cutoff = dice_cutoff["cutoff",]
  return(list(pauc = pauc,
    perf = perf,
    cutoff = cutoff,
    dice_cutoff = dice_cutoff))
}


# perf = run_roc(full_df$p, full_df$Y)
# perf2 = run_roc(full_df$p2, full_df$Y)


masked_df = mapply(
  function(prob, sm_prob, brain_mask, y) {
  inds = which(brain_mask > 0)
  df = data_frame(p = prob[inds],
    sm_p = sm_prob[inds],
    Y = y[ inds])
  if (!all(unique(df$Y) %in% c(0, 1))) {
    stop("Non-unique things!")
  }
  df
}, prob_imgs, sm_prob_imgs, all_brain_masks,
  all_masks, SIMPLIFY = FALSE)

masked_df = bind_rows(masked_df, .id = "scan")
masked_df$y = ifelse(masked_df$Y > 0,
  "lesion", "non_lesion")
masked_df$y = factor(masked_df$y,
  levels = c("non_lesion", "lesion"))

dice_df = masked_df %>% 
  mutate(
    pred = p > cutoff,
    smooth_pred = sm_p > smooth_cutoff,
    Y = Y > 0) %>% 
  group_by(scan) %>% 
  summarize(dice_est = dice(pred, Y),
    smooth_dice_est = dice(smooth_pred, Y),
    vol = sum(Y),
    pred_vol = sum(pred),
    smooth_pred_vol = sum(smooth_pred))
dice_df$n4 = n4


outfile = file.path(root_dir, 
  paste0("dice_", 
    ifelse(n4, "n4_", ""),
    group, ".rds"))
write_rds(dice_df, path = outfile)

sub_perf = run_roc(masked_df$p, masked_df$Y)
tab = table(masked_df$p > cutoff, 
  masked_df$Y)
sub_perf$dice = tab_dice(tab)
sub_perf2 = run_roc(masked_df$sm_p, masked_df$Y)
tab = table(masked_df$sm_p > smooth_cutoff, 
  masked_df$Y)
sub_perf2$dice = tab_dice(tab)

outfile = file.path(root_dir, 
  paste0("ranger_performance_", 
    ifelse(n4, "n4_", ""),
    group, ".rds"))
write_rds(sub_perf, path = outfile)

outfile = file.path(root_dir, 
  paste0("ranger_performance_", 
    ifelse(n4, "n4_", ""),
    "smoothed_",
    group, ".rds"))
write_rds(sub_perf2, path = outfile)


outfiles = file.path(res_dir,
  paste0(names(all_imgs), 
    ifelse(n4, "_n4", ""),
    "_prediction.png"))
compare_files = file.path(res_dir,
  paste0(names(all_imgs), 
    ifelse(n4, "_n4", ""),
    "_image.png"))
mapply(function(img, mask, pred, roi,
  fname, fname2) {

  pred = pred > cutoff
  img = mask_img(img, mask = mask)  
  img = window_img(img)  
  plot_xyz = xyz(roi)
  print(fname2)
  diff = roi + pred*2
  diff[roi == 0 & pred == 0] = NA
  cols = c("#56B4E9", "#D55E00", "#009E73")
  levels = c("False Negative", "False Positive", 
    "True Positive")  

  png(fname2, res = 600, width = 10, height=5,
    units = "in", type = "cairo")
    ortho2(img, xyz = plot_xyz)
  dev.off()


  png(fname, res = 600, width = 10, height=5,
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
}, all_imgs, all_brain_masks, 
  prob_imgs, all_masks, outfiles, compare_files)
