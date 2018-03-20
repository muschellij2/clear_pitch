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


groups = c("train", "test")
run_frac = 0.1
eg = expand.grid(
  n4 = c(FALSE, TRUE),
  run_frac = run_frac,
  stratified = c(FALSE, TRUE),
  group = groups,   
  stringsAsFactors = FALSE)

eg = eg %>% 
  mutate(outfile = file.path(root_dir, 
    paste0("dice_", 
      ifelse(n4, "n4_", ""), 
      ifelse(run_frac != 0.1, 
        paste0(run_frac, "_"),  ""),
      ifelse(stratified, "stratified_", ""),    
      group,
    ".rds")))

res = vector(mode = "list", 
	length = nrow(eg))
for (iscen in seq(nrow(eg))) {
	outfile = eg$outfile[iscen]
	if (file.exists(outfile)) {
		x = read_rds(outfile)
		x$group = eg$group[iscen]
		x$stratified = eg$stratified[iscen]
		res[[iscen]] = x
	}
}

df = bind_rows(res)
df = gather(df, type, value, 
	dice_est, smooth_dice_est,
	pred_vol, smooth_pred_vol
	)
df$smooth = grepl("smooth", df$type)
df$type = ifelse(grepl("dice", df$type), 
	"dice", "volume")
df = spread(df, type, value)
df$n4 = ifelse(df$n4, "N4-corrected HU", 
	"Standard HU")
df$stratified = ifelse(df$stratified, 
	"Stratified Sampling", 
	"Case-Control Sampling")
df$n4 = factor(df$n4,
	levels = c("Standard HU", "N4-corrected HU"))
df$smooth = ifelse(df$smooth, 
	"Smoothed", "Non-Smoothed")
df$smooth = factor(df$smooth, 
	levels = c("Non-Smoothed", "Smoothed"))

df$diff_vol = (df$vol - df$volume)/1000
df$avg_vol = (df$vol + df$volume) / (2 * 1000)

sds = df %>% 
	group_by(group, n4, smooth, stratified) %>% 
	summarize(mn = mean(diff_vol, na.rm = TRUE),
		sd = sd(diff_vol, na.rm = TRUE)) %>% 
	mutate(
		lower = mn - qnorm(0.975)*sd,
		upper = mn + qnorm(0.975)*sd
		)

test = df %>% 
	filter(group == "test")


fname = file.path(res_dir,
	"dice_compare_smooth.png")
png(fname, res = 600, width = 5, height=5,
    units = "in", type = "cairo")
dice = df %>% ggplot(
	aes(y = dice, x =n4, colour = smooth)) + 
	geom_boxplot() + facet_wrap(stratified ~ group)
print(dice)
dev.off()

fname = file.path(res_dir,
	"dice_compare_n4.png")
png(fname, res = 600, width = 5, height=5,
    units = "in", type = "cairo")	
dice2 = df %>% ggplot(
	aes(y = dice,  colour =n4, x = smooth)) + 
	geom_boxplot() 	+ 
	facet_wrap(group ~ stratified)
print(dice2)
dev.off()

fname = file.path(res_dir,
	"dice_compare_stratified.png")
png(fname, res = 600, width = 5, height=5,
    units = "in", type = "cairo")	
dice2 = df %>% ggplot(
	aes(y = dice, 
		colour = stratified, x = smooth)) + 
	geom_boxplot() 	+ facet_wrap(n4 ~ group)
print(dice2)
dev.off()



vol = df %>% ggplot(
	aes(x = volume / 1000, 
		y = vol / 1000,
		colour = group)) + 
	geom_point() +
	geom_abline(intercept = 0, slope = 1) + 
	  stat_smooth_func(geom="text",
	  	method="lm",
	  	hjust=0,parse=TRUE) +
	geom_smooth(method = "lm", se = FALSE) +
	ylab("Manual Volume") +
	xlab("Automated Volume")

fname = file.path(res_dir,
	"volume_compare_free.png")
png(fname, res = 600, width = 12, height=4,
    units = "in", type = "cairo")	
v1 = vol + facet_grid(group ~ n4 + 
	smooth + stratified,
		scales = "free_y")
print(v1)
dev.off()

fname = file.path(res_dir,
	"volume_compare.png")
png(fname, res = 600, width = 10, height=4,
    units = "in", type = "cairo")	
v2 = vol + 
	facet_grid(group ~ n4 + smooth + stratified)
print(v2)
dev.off()



fname = file.path(res_dir,
	"ba_volume.png")
png(fname, res = 600, width = 5, height=5,
    units = "in", type = "cairo")	
ba = df %>% ggplot(
	aes(x = avg_vol, 
		y = diff_vol,
		colour = group)) + 
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

