rm(list = ls())
library(fslr)
library(plyr)

make_time = function(times){
  hour = substr(times, 1, 2)
  min = substr(times, 3, 4)
  add.day = rep(0, length=length(times))
  
  add.hour = (min == "60")
  min[add.hour] = "00"
  hour[add.hour] = sprintf("%02.0f", 
                           as.numeric(hour[add.hour]) + 1)
  
  #########################
  # Paste together and make a Date Object
  #########################
  time = paste0(hour, ":", min)
}

##########
# Date Converter Function
##########
ctdate_to_date = function(x){
  days = gsub("(.*)_(.*)", "\\1", x)
  dyear = substr(x=days, start=1, stop=4)
  mon = substr(x=days, start=5, stop=6)
  day = substr(x=days, start=7, stop=8)  
  
  times = gsub("(.*)_(.*)", "\\2", x)
  times[times == "NA"] = "0000"
  time = make_time(times)
  dt = as.POSIXct(paste(days, time), format = "%Y%m%d %H:%M")
  dt = dt
  dt
}

basedir = "~/Dropbox/CTR/DHanley/CT_Registration/Segmentation"
paperdir = file.path(basedir, "Segmentation_Paper")


abc2 = read.csv(file.path(paperdir, "All_ABC2_Data.csv"), stringsAsFactors = FALSE)
abc2$time = make_time(sub(":", "", abc2$time))
abc2$date = paste0(abc2$Date_Time_CT, " ", abc2$time)
abc2$date = strptime(abc2$date, format = "%d%b%Y %H:%M")
abc2$date = as.POSIXct(abc2$date)
stopifnot(all(!is.na(abc2$date)))
# abc2[is.na(abc2$date),c("time", "Date_Time_CT")]

abc2$Date_Time_CT = abc2$time = NULL
abc2$Dataset = NULL

########
# Filling in the abc/2 values
######
noabc2 = is.na(abc2$calculated_ICS)
abc2$calculated_ICS[ noabc2 ] = abc2$entered_ICS[noabc2 ]
abc2$entered_ICS = NULL

##########
# Filling in some zeroes
##########
for (icol in c("cathetertractvol", "cathetertractvol2", "othervol")) {
  abc2[is.na(abc2[, icol]),icol] = 0
}


##########
# Loading in ROI data
##########

pickmods = c("mod_agg", "gam", "rf")
reval_vec = c("mod_agg" = "Logistic", 
              "gam" = "GAM", 
              "rf" = "Random Forest")

cols = paste0("vol_", pickmods, "_zval2", "_dice", "_pred")
##############################
# Getting Predicted Volumes
################################
resdir = path.expand(file.path(basedir, "results"))
vol.file = file.path(resdir, "No_Registration_Cutoff_predictions.Rda")
voldata = load(vol.file)
# 
# total_rda = file.path(basedir, "111_Filenames_with_volumes_stats.Rda")
# load(total_rda)
fdf$patientName = as.numeric(gsub("-", "", fdf$id))

# Format data to put into converter
x = t(sapply(strsplit(nii.stub(fdf$roi, bn=TRUE), "_"), `[`, 2:3))
x = apply(x, 1, paste, collapse = "_")
stopifnot(length(x) == nrow(fdf))
fdf$dt = ctdate_to_date(x)

### subset relevant columns
fdf = fdf[, c("patientName", "dt", "truevol", "group", cols)]

stopifnot(all(!is.na(fdf$dt)))
stopifnot(all(!is.na(abc2$date)))

abc2 = abc2[ abc2$patientName %in% unique(fdf$patientName), ]

fdf = merge(fdf, abc2, by="patientName", all=TRUE)
fdf$tdiff = abs(fdf$dt - fdf$date)
units(fdf$tdiff) = "mins"


fdf = fdf[ order(fdf$patientName, fdf$tdiff), ]

fdf$patientName = factor(fdf$patientName)

fdf = ddply(fdf, .(patientName), function(x){
  x[1,]
})
#### must be withintan hour
stopifnot(all(fdf$tdiff <= 61 ))

fdf$total_volume = fdf$ICHVolume + fdf$IVHVolume + 
  fdf$othervol + fdf$cathetertractvol
cor(fdf$truevol, fdf$total_volume)
lm(total_volume ~ truevol, data = fdf)

plot(fdf$vol_mod_agg_zval2_dice_pred, fdf$truevol)
cor(fdf$calculated_ICS, fdf$truevol, use = "na.or.complete")
cor(fdf$vol_mod_agg_zval2_dice_pred, fdf$truevol, use = "na.or.complete")

