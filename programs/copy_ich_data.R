# copy the MISTIE III data
library(neurobase)
library(here)

out_dir = here("batch")

ich_dir = "/legacy/dexter/disk2/smart/stroke_ct/ident/Registration"
fnames = file.path(ich_dir,
	c("134-304/134-304_20060518_2325_CT_5383_CT-HEAD_Brain_1.nii.gz", 
"152-302/152-302_20060405_0906_CT_2_Head^HeadSeq_BRAIN.nii.gz", 
"173-364/173-364_20100211_2043_CT_5.nii.gz", 
"173-368/173-368_20100308_0850_CT_2_BRAIN_S.nii.gz", 
"173-384/173-384_20100913_0723_CT_3_CT_Head.nii.gz", 
"191-400/191-400_20110309_0818_CT_2_CT_HEAD.nii.gz", 
"216-390/216-390_20101213_0743_CT_2_CT_HEAD.nii.gz", 
"222-357/222-357_20090802_1936_CT_2_CT_ROUTINE.nii.gz", 
"222-358/222-358_20091220_1538_CT_32_CT_3D.nii.gz", 
"265-389/265-389_20101111_1312_CT_2_CT_HEAD.nii.gz"))

roi_dir = "/legacy/dexter/disk2/smart/stroke_ct/ident/ROI_data/"

roi_fnames = file.path(roi_dir, 
	c("134-304/134-304_20060518_2325_CT_5383_CT-HEAD_Brain_1ROI.nii.gz", 
"152-302/152-302_20060405_0906_CT_2_Head^HeadSeq_BRAINROI.nii.gz", 
"173-364/173-364_20100211_2043_CT_5ROI.nii.gz", 
"173-368/173-368_20100308_0850_CT_2_BRAIN_SROI.nii.gz", 
"173-384/173-384_20100913_0723_CT_3_CT_HeadROI.nii.gz", 
"191-400/191-400_20110309_0818_CT_2_CT_HEADROI.nii.gz", 
"216-390/216-390_20101213_0743_CT_2_CT_HEADROI.nii.gz", 
"222-357/222-357_20090802_1936_CT_2_CT_ROUTINEROI.nii.gz", 
"222-358/222-358_20091220_1538_CT_32_CT_3DROI.nii.gz", 
"265-389/265-389_20101111_1312_CT_2_CT_HEADROI.nii.gz")
)

out_ss = function(x) {
	ss = strsplit(x, "_")[[1]]
	id = ss[1]
	id = strsplit(id, "-")[[1]]
	id = rev(id)
	id = paste(id, collapse = "-")
	# 3 for mistie
	id = paste0(0, id)
	ss = paste0(id, "_", paste(ss[2:3], collapse = ""))
	app = ifelse(grepl("ROI", x), "_Msk", "_CT")
	out = paste0(ss, app, ".nii.gz")
}

files = c(fnames, roi_fnames)
outfiles = nii.stub(files,
	bn = TRUE)
outfiles = sapply(outfiles, out_ss)
outfiles = file.path(out_dir, outfiles)
file.copy(files, outfiles)

