# Rnosave combine_df.R -N MODEL -l mem_free=60G,h_vmem=61G -hold_jid PROC
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

df = df[ seq(n_ids), ]

df$id_proc_dir = file.path(proc_dir, df$id)
df$stub = sub("_CT", "", nii.stub(df$CT, bn = TRUE))
df$rds = file.path(df$id_proc_dir,
  paste0(df$stub, "_",  "predictor_df.rds"))

all_df = vector(mode = "list",
	length = n_ids)
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
full_df$y = factor(full_df$y)

samp = full_df %>% 
	group_by(scan, Y) %>% 
	sample_frac(size = 0.1) %>% 
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

model <- train(
  y ~ .,
  tuneLength = 3,
  data = samp, method = "ranger",
  trControl = myControl
)

outfile = file.path(root_dir, 
	"ranger_model.rds")
write_rds(model, path = outfile)

