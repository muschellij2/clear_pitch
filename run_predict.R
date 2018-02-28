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
keep_rows = 13:nrow(df)
df = df[ keep_rows, ]

df$id_proc_dir = file.path(proc_dir, df$id)
df$stub = sub("_CT", "", nii.stub(df$CT, bn = TRUE))
df$rds = file.path(df$id_proc_dir,
  paste0(df$stub, "_",  "predictor_df.rds"))


mod_file = file.path(root_dir, 
  "ranger_model.rds")
model = read_rds(mod_file)

all_df = vector(mode = "list",
	length = length(keep_rows))
names(all_df) = df$scan
iid = 1

for (iid in seq(length(keep_rows))) {
	print(iid)
	proc_df = read_rds(df$rds[iid])
  proc_df$candidate = ich_candidate_voxels(proc_df)
  p = predict(model, newdata = proc_df, 
    type = "prob")
  p = p[, "lesion"]
  proc_df$p = p
  p = predict(rf_modlist$mod,
    newdata = proc_df, type = "prob")
  proc_df$p2 = p[, "1"]
  proc_df = proc_df %>% 
    select(p, p2, Y, candidate)
	all_df[[iid]] = proc_df
	rm(proc_df); 
}

full_df = bind_rows(all_df, .id = "scan")
rm(all_df); gc()
full_df$y = ifelse(full_df$Y > 0,
	"lesion", "non_lesion")
full_df$y = factor(full_df$y)

pred = prediction(full_df$p, full_df$Y)
perf = performance(pred, "tpr", "fpr")
auc = performance(pred, "auc", fpr.stop = 0.01)
unlist(auc@y.values[[1]])/0.01
perf = performance(pred, "tpr", "fpr")


sub_df = full_df  %>% 
  filter(candidate > 0 | Y > 0)

sub_pred = prediction(sub_df$p, sub_df$Y)
sub_perf = performance(sub_pred, "tpr", "fpr")
sub_auc = performance(sub_pred, "auc", fpr.stop = 0.01)
unlist(sub_auc@y.values[[1]])/0.01
sub_perf = performance(sub_pred, "tpr", "fpr")


