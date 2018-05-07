# Rnosave run_predict.R -N PRED \
# -l mem_free=40G,h_vmem=41G -hold_jid MODEL
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
devtools::source_gist("524eade46135f6348140",
	filename = "ggplot_smooth_func.R")

# root_dir <- "~/CLEAR_PITCH"
root_dir = here::here()
img_dir = file.path(root_dir, "original")
proc_dir = file.path(root_dir, "processed")
res_dir = file.path(root_dir, "results")
mod_dir = file.path(root_dir, "models")


batches = c("batch", "test_set")
batch_type = batches[1]

fname = switch(batch_type,
  batch = "filenames_df.rds",
  "test_set" = "test_filenames_df.rds")
filenames = file.path(res_dir, fname)
df = read_rds(filenames)
if (batch_type == "batch") {
  groups = c("train", "test", "validation")
} else {
  groups = "test"
}


model_groups = "train"
models = c("ranger", "logistic", "leekasso",
  "ichmodel")

run_frac = 0.1
eg = expand.grid(n4 = c(FALSE, TRUE),
                 run_frac = run_frac,
                 stratified = c(FALSE, TRUE),
                 model = models,                 
                 model_group = model_groups,  
                 stringsAsFactors = FALSE)
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
eg = eg %>% 
  filter(!(model == "ichmodel" & (n4 | stratified)))

mod_stub = tools::file_path_sans_ext(
  basename(eg$outfile))
mod_stub = sub("ranger_", "", mod_stub)
mod_stub[grepl("ichmodel", mod_stub)] = "ichmodel"


group = groups[3]

eg = eg %>% 
  mutate(outfile = file.path(mod_dir, 
	  paste0("dice_", 
	    mod_stub, "_",
	    ifelse(batch_type != "batch", 
	      batch_type, group),
	    ".rds"))
	)

res = vector(mode = "list", 
	length = nrow(eg))
for (iscen in seq(nrow(eg))) {
	outfile = eg$outfile[iscen]
	if (file.exists(outfile)) {
		x = read_rds(outfile)
		x$group = eg$group[iscen]
		x$stratified = eg$stratified[iscen]
		x$n4 = eg$n4[iscen]
		x$model = eg$model[iscen]
		res[[iscen]] = x
	}
}

df = bind_rows(res)
df$n4 = ifelse(df$n4, "N4-corrected HU", 
	"Standard HU")
df$stratified = ifelse(df$stratified, 
	"Stratified Sampling", 
	"Case-Control Sampling")
df$n4 = factor(df$n4,
	levels = c("Standard HU", "N4-corrected HU"))
df$smooth = ifelse(df$smoothed, 
	"Smoothed", "Non-Smoothed")
df$smooth = factor(df$smooth, 
	levels = c("Non-Smoothed", "Smoothed"))

df$diff_vol = (df$vol - df$pred_vol)/1000
df$avg_vol = (df$vol + df$pred_vol) / (2 * 1000)

sds = df %>% 
	group_by(model, n4, smooth, stratified) %>% 
	summarize(
		mn = mean(diff_vol, na.rm = TRUE),
		sd = sd(diff_vol, na.rm = TRUE)) %>% 
	mutate(
		lower = mn - qnorm(0.975)*sd,
		upper = mn + qnorm(0.975)*sd
		)


fname = file.path(res_dir,
	paste0("dice_compare_smooth_", 
		batch_type, "_", group, 
		".png"))

png(fname, res = 600, width = 5, height=5,
    units = "in", type = "cairo")
dice = df %>% ggplot(
	aes(y = dice, x =n4, colour = smooth)) + 
	geom_boxplot() + 
	facet_wrap(stratified ~ model, nrow = 2)
print(dice)
dev.off()

fname = file.path(res_dir,
	paste0("dice_compare_noN4_CaseControl_", 
		batch_type, "_", group, 
		".png"))

png(fname, res = 600, width = 5, height=5,
    units = "in", type = "cairo")
dice = df %>% 
	filter(
		stratified == "Case-Control Sampling",
		n4 == "Standard HU") %>% 
	ggplot(
	aes(y = dice, x = model, colour = smooth)) + 
	geom_boxplot()
print(dice)
dev.off()

fname = sub("[.]png", "_sameaxis.png", fname)
png(fname, res = 600, width = 5, height=5,
    units = "in", type = "cairo")
print({dice + ylim(0, 1)})
dev.off()

fname = file.path(res_dir,
	paste0("dice_compare_n4_", 
		batch_type, "_", group, 
		".png")
	)
png(fname, res = 600, width = 5, height=5,
    units = "in", type = "cairo")	
dice2 = df %>% ggplot(
	aes(y = dice,  colour =n4, x = smooth)) + 
	geom_boxplot() 	+ 
	facet_wrap(model ~ stratified, nrow = 2)
print(dice2)
dev.off()

fname = file.path(res_dir,
	paste0("dice_compare_stratified_", 
		batch_type, "_", group, 
		".png")
	)	
png(fname, res = 600, width = 5, height=5,
    units = "in", type = "cairo")	
dice2 = df %>% ggplot(
	aes(y = dice, 
		colour = stratified, x = smooth)) + 
	geom_boxplot() 	+ facet_wrap(n4 ~ model)
print(dice2)
dev.off()



vol = df %>% ggplot(
	aes(x = pred_vol / 1000, 
		y = vol / 1000)) + 
	geom_point() +
	geom_abline(intercept = 0, slope = 1) + 
	  stat_smooth_func(geom="text",
	  	method="lm",
	  	hjust=0,parse=TRUE) +
	geom_smooth(method = "lm", se = FALSE) +
	ylab("Manual Volume") +
	xlab("Automated Volume")

fname = file.path(res_dir,
	paste0("volume_compare_free_", 
		batch_type, "_", group, 
		".png")
	)	
png(fname, res = 600, width = 12, height=4,
    units = "in", type = "cairo")	
v1 = vol + facet_grid(model ~ n4 + 
	smooth + stratified,
		scales = "free_y")
print(v1)
dev.off()

for (model in models) {
	fname = file.path(res_dir,
		paste0("volume_compare_free_", 
			model, "_", 
			batch_type, "_", group, 
			".png")
		)	
	png(fname, res = 600, width = 12, height=4,
	    units = "in", type = "cairo")
	ddf = df %>% 
		filter(model == "ranger")	
	v1 = vol + facet_grid(n4 ~ smooth + 
		stratified, scales = "free_y")
	v1 = v1 %+% ddf
	print(v1)
	dev.off()
}


fname = file.path(res_dir,
	"volume_compare.png")
png(fname, res = 600, width = 10, height=4,
    units = "in", type = "cairo")	
v2 = vol + 
	facet_grid(model ~ n4 + smooth + stratified)
print(v2)
dev.off()



fname = file.path(res_dir,
	"ba_volume.png")
png(fname, res = 600, width = 5, height=5,
    units = "in", type = "cairo")	
ba = df %>% ggplot(
	aes(y = avg_vol, 
		x = diff_vol,
		colour = model)) + 
	geom_point() +
	geom_hline(yintercept = 0) + 
	geom_hline(data = sds,
		aes(colour = group,
			yintercept = lower),
		linetype = "dashed") +
	geom_hline(data = sds,
		aes(colour = group,
			yintercept = upper),
		linetype = "dashed") +	
	geom_smooth(se = FALSE) +
	facet_grid(n4 ~ smooth + stratified) +
	ylab("Mean of Manual/Auto") +
	xlab("Difference: Manual - Auto")
print(ba)
dev.off()
	# geom_line(stat = "density") + 
	# facet_wrap(~ n4)

