# Rnosave combine_df.R -N MODEL -l mem_free=100G,h_vmem=101G -hold_jid PROC
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(caret)
library(neurobase)
library(ranger)

set.seed(20180227)
# root_dir <- "~/CLEAR_PITCH"
root_dir = here::here()
img_dir = file.path(root_dir, "original")
proc_dir = file.path(root_dir, "processed")
n4 = TRUE
run_frac = 0.1

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
group = "train"
df = df[ df$train %in% group, ]

df$id_proc_dir = file.path(proc_dir, df$id)
df$stub = sub("_CT", "", nii.stub(df$CT, bn = TRUE))
df$stub = paste0(df$stub, ifelse(n4, "_n4", ""))

df$rds = file.path(df$id_proc_dir,
  paste0(df$stub, "_",  "predictor_df.rds"))

all_df = vector(mode = "list",
	length = nrow(df))
names(all_df) = df$scan

for (iid in seq(n_ids)) {
	print(iid)
	proc_df = read_rds(df$rds[iid])
	all_df[[iid]] = proc_df
	rm(proc_df); 
}

full_df = bind_rows(all_df, .id = "scan")
rm(all_df); gc()
full_df$y = ifelse(full_df$Y > 0,
	"lesion", "non_lesion")
full_df$y = factor(full_df$y,
  levels = c( "non_lesion", "lesion"))

samp = full_df %>% 
	group_by(scan, Y) %>% 
	sample_frac(size = run_frac) %>% 
	ungroup()

print(nrow(samp))

cn = colnames(full_df)
keep = !cn%in% c("mask", "scan", "Y")
xdf = full_df[, !keep]
full_df = full_df[, keep]

xsamp = samp[, !keep]
samp = samp[, keep]

# mod = ranger(Y ~ ., data = samp)
# mod = ranger(Y ~ ., data = full_df)
rm(full_df); gc()
# Create custom trainControl: myControl
myControl <- trainControl(
  method = "cv", number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)
y = samp$y
samp = samp %>% 
  select(-y)

model <- train(
  x = samp,
  y = y,
  tuneLength = 1,
  method = "ranger",
  trControl = myControl,
  save.memory = TRUE
)

# model <- train(
#   y ~ .,
#   tuneLength = 1,
#   data = samp, method = "ranger",
#   trControl = myControl,
#   save.memory = TRUE
# )

outfile = file.path(root_dir, 
	paste0("ranger_", ifelse(n4, "n4_", ""), 
    ifelse(size != 0.1, paste0("_", run_frac), 
      ""),
    "model.rds"))
write_rds(model, path = outfile)

