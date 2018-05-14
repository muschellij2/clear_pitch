rm(list = ls())
dcmtk_ver = packageVersion("dcmtk")
if (dcmtk_ver < "0.5.5") {
  devtools::install_github("muschellij2/dcmtk")
}
library(dcmtk)
library(oro.nifti)
library(neurobase)
library(dplyr)
library(dcm2niir)
library(tidyr)
library(fslr)


# id = "6007-300"
ids = list.dirs(full.names = FALSE, recursive = FALSE)
ids = grep(pattern = "\\d{4}-\\d{3}", ids, value = TRUE)
id = ids[12]
print(id)
directory = file.path("/Volumes/LACIE/Nifti", 
                      id)
hdr_data = file.path(directory, "all_hdr.rds")

if (!file.exists(hdr_data)) {
  new_dirs = list.dirs(directory, 
                       recursive = FALSE)
  
  
  ## how many files in that directory - under 5 - remove
  ## Keep only CT? What about MR?
  # for ( i in 1:20) {
  n_dirs = length(new_dirs)
  all_data = vector(mode = "list", 
                    length = n_dirs)
  pb = txtProgressBar(min = 1, 
                      max = n_dirs, style = 3)
  for (i in seq(n_dirs)) {
    setTxtProgressBar(pb, value = i)
    basedir = new_dirs[i]
    files_in_dir = list.files(path = basedir)
    dirs_in_dir = list.dirs(
      path = basedir,
      full.names = FALSE,
      recursive = FALSE)
    files_in_dir = files_in_dir[ 
      !(files_in_dir %in% dirs_in_dir)]
    if (length(files_in_dir) > 0) {
      hdr = dcmtk::read_dicom_header(
        file = paste0(shQuote(basedir), 
                      "/*"))
      hdr$dir = basedir
    } else {
      hdr = NULL
    }
    all_data[[i]] = hdr
  }
  close(pb)
  
  all_hdr = dplyr::bind_rows(all_data)
  saveRDS(all_hdr, file = hdr_data)
} else {
  all_hdr = readRDS(hdr_data)
}


# ContrastBolusAgent - (0018,0010)
# SourceImageSequence - (0008,2112)
# PixelSpacing - (0028,0030)
# SliceThickness - (0018,0050)
# SeriesDescription- (0008,103e)
#  StudyDescription (0008,1030)
# WindowCenter - "(0028,1050)"
# WindowWidth - (0028,1051)
# WindowCenterWidthExplanation - (0028,1055)
keep_tags = c("(0008,0008)", "(0008,0060)", "(0018,1210)",
              "(0018,1160)", "(0018,1151)", "(0018,0081)",
              "(0018,1150)", "(0018,0080)", "(0008,9007)",
              "(0018,9316)", "(0018,0050)", "(0018,0010)",
              "(0028,0030)", "(0008,2112)", "(0008,103e)", 
              "(0008,1030)", "(0028,1050)", "(0028,1051)",
              "(0028,1055)", 
              "(0008,0050)", #AccessionNumber 
              "(0020,0012)", #AcquisitionNumber
              "(0020,0011)" # SeriesNumber
              
)
# (0040,a040)
sub_hdr = all_hdr %>% 
  filter(tag %in% keep_tags) %>% 
  select(file, tag, name, value)
# saveRDS(sub_hdr, file = file.path(
#   dir, "relevant_tags.rds"))

keep_files = sub_hdr %>% 
  group_by(file, tag) %>% 
  filter(!is.na(value)) %>% 
  mutate(ind = seq(n())) %>% 
  ungroup() 

multi = keep_files %>% 
  filter(ind > 1)
u_name = unique(multi$name)
no_double = c("ConvolutionKernel", "ImageType", "Modality", "PixelSpacing", 
              "SeriesDescription", 
              "SliceThickness", "StudyDescription", 
              # "SeriesNumber", 
              "AccessionNumber", "AcquisitionNumber")
if (any(u_name %in% no_double)) {
  stop("Double records in no double data!")
}
# keep 
keep_files = keep_files %>% 
  filter(ind == 1)



wide = keep_files %>% 
  select(-tag) %>% 
  spread(key = name, value = value) %>% 
  mutate(ImageType = toupper(ImageType),
         ConvolutionKernel = toupper(ConvolutionKernel),
         StudyDescription = toupper(StudyDescription))
orig_wide = wide
print(nrow(wide))

###################
# Keep only CT
###################
good_files = sub_hdr %>% 
  filter(tag %in% "(0008,0060)") %>% 
  distinct()

good_files = good_files %>% 
  filter(grepl("CT", value))
good_files = good_files$file

###################
# Keep only CT
###################
keep_files = keep_files %>% 
  filter(file %in% good_files)

wide = wide %>% 
  filter(file %in% good_files)


removed = orig_wide %>% 
  filter(!(file %in% wide$file))
if (nrow(removed) > 0) {
  print(table(removed$Modality))
  print(nrow(wide))
}
wide$ImageType = gsub("\\\\", ",", wide$ImageType)
######################################
# Remove localizers/Dose Reports/
# Circle of WILLIS (TERARECON)
######################################
bad_files = wide$file[
  grepl(paste(
    "(SCREEN SAVE)", "LOCALIZER", "TERARECON", 
    "(CT_SOM5 PROT)", "(DOSE REPORT)",
    "(SECONDARY,OTHER)", "DOSE_INFO", sep = "|"), 
    wide$ImageType)]

keep_files = keep_files %>% 
  filter(!(file %in% bad_files))

wide = wide %>% 
  filter(!(file %in% bad_files))

removed = orig_wide %>% 
  filter(!(file %in% wide$file))
if (nrow(removed) > 0) {
  print(table(removed$ImageType))
  print(nrow(wide))
}



######################################
# Remove ANGIO
######################################
# bad_files = wide$file[
# grepl("ANGIO", wide$StudyDescription)]
angio = grepl("COW|WILLIS", toupper(wide$SeriesDescription))
bad_files = wide$file[angio]

keep_files = keep_files %>% 
  filter(!(file %in% bad_files))

wide = wide %>% 
  filter(!(file %in% bad_files))

wide = wide %>% 
  mutate(directory = dirname(file)) %>% 
  group_by(directory) %>% 
  mutate(n = n()) %>% 
  ungroup()

removed = orig_wide %>% 
  filter(!(file %in% wide$file))
if (nrow(removed) > 0) {
  print(table(removed$SeriesDescription))
  print(nrow(wide))
}


######################################
# Remove Spine/CHEST
######################################
angio = grepl("SPINE", toupper(wide$SeriesDescription))
angio = angio | grepl("CHEST", toupper(wide$SeriesDescription))
angio = angio | (
  !grepl("BRAIN|BLOOD", wide$SeriesDescription) &   
    grepl("SPINE", wide$StudyDescription))
bad_files = wide$file[angio]

keep_files = keep_files %>% 
  filter(!(file %in% bad_files))

wide = wide %>% 
  filter(!(file %in% bad_files))

wide = wide %>% 
  mutate(directory = dirname(file)) %>% 
  group_by(directory) %>% 
  mutate(n = n()) %>% 
  ungroup()

removed = orig_wide %>% 
  filter(!(file %in% wide$file))
if (nrow(removed) > 0) {
  print(table(removed$SeriesDescription))
  print(nrow(wide))
}

just_removed = orig_wide %>% 
  filter((file %in% bad_files))
if (nrow(just_removed) > 0) {
  print(table(just_removed$SeriesDescription))
}



# fewer than 10 scans
small = wide[ wide$n < 10,]

noseries = wide[ is.na(wide$SeriesDescription), ]


### still a bone window
run_dirs = unique(wide$directory)

wide_file = file.path(directory, 
                      "selected_hdr.rds")
saveRDS(wide, file = wide_file)
table(wide$SeriesDescription, useNA = "ifany")
table(wide$StudyDescription, useNA = "ifany")
table(wide$StudyDescription, wide$SeriesDescription)
table(wide$ImageType, useNA = "ifany")

removed = orig_wide[ !(orig_wide$file %in% wide$file),]
rm_file = file.path(directory, 
                      "removed_hdr.rds")
saveRDS(removed, file = rm_file)
### still need to remove if BONE is in SeriesDescription


print(length(run_dirs))
merge_files = TRUE

for (i in seq_along(run_dirs)) {
  print(i)
  basedir = run_dirs[i]
  bn = basename(basedir)
  opts = paste0("-9 -i y ", 
                ifelse(merge_files, " -m y ", ""),
                " -f %t ")
  opts = gsub("\\s+", " ", opts)
  opts = trimws(opts)
  res = dcm2nii(
    basedir = basedir, 
    opts = opts )
  # result
  # if (res$result <= 1 ) {
  #   if (res$result != 0) {
  #     warning(paste0("directory is ", basedir, 
  #                 "has non-zero result"))
  #   }
  if (res$result == 0) {
    outfile = check_dcm2nii(res)
    new_file = file.path(
      directory, 
      paste0(bn, "_", basename(outfile)))    
    if (length(outfile) > 0 & !all(file.exists(new_file))) {
      for (ifile in seq_along(outfile)) {
        ofile = outfile[ifile]
        if (dim_(ofile)[4] > 1) {
          window = c(-1024, 3071)
          img = readnii(ofile)
          img = window_img(img, window = window)
          img = cal_img(img)
          scl_slope(img) = 1
          scl_inter(img) = 0
          aux_file(img) = ""
          descrip(img) = ""
          ####################################
          # write out the image
          ####################################  
          writenii(img, filename = new_file[ifile])
        }
      }
      
      # file.copy(outfile, new_file, 
      #   overwrite = TRUE)
    }
  } else {
    warning(paste0("directory is ", basedir))
  }
}


