#
library(methods)
library(dcmsort)
library(here)
library(dplyr )
library(readr)

proc_dir = file.path(here(), "processed")
df = read_csv("batches.csv")
df$folder = ifelse(df$batch == 1, "original", "batch")
df$folder = file.path(here(), df$folder)
df = df %>% 
  filter(batch > 1)
df$p_id = paste0(df$patientName %% 10000, "-", floor(df$patientName/10000))
ids = list.dirs(
  path = proc_dir,
  full.names = FALSE, 
  recursive = FALSE)
ids = grep(pattern = "\\d{4}-\\d{3}", ids, value = TRUE)
ids = ids[ids %in% df$p_id]

files = list.files(pattern = ".nii.gz", path = here("batch"))
ids_with_data = sapply(strsplit(files, "_"), function(x) x[1])
ids_with_data = unique(ids_with_data)
ids = setdiff(ids, ids_with_data)
ids

iid = 31  # 
# 31 is bad
n_ids = length(ids)
for (iid in 1:n_ids) {
  id = ids[iid]
  print(id)
  
  directory = file.path(proc_dir, id)
  files = list.files(directory)
  if (length(files) == 0) {
    next;
  }
  
  # sorted = dcmsort(directory = directory, copy_files = FALSE)
  
  sorted = list(directory = directory)
  
  outfile = file.path(sorted$directory, "all_hdr.rds")
  
  all_hdr = read_all_hdr(
    directory = sorted$directory,
    outfile = outfile,
    # overwrite = TRUE
    overwrite = TRUE
  )
  all_hdr = all_hdr[ !is.na(all_hdr$file), ]
  sub_hdr = subset_hdr(all_hdr)
  ct_hdr = noncon_brain_ct(
    sub_hdr = sub_hdr,
    delete_localizers = FALSE)
  ct_hdr$ct_data = ct_hdr$ct_data %>% 
    filter(!grepl("ROI(no|)Mask", file))
  
  str_separator = "_RRR_"
  
  converted = convert_hdr_nii(
    sub_hdr = ct_hdr$ct_data, 
    merge_files = TRUE, 
    ignore_derived = TRUE,
    overwrite = FALSE,
    rename = FALSE,
    opts = paste0("-f %t", str_separator, "%d"),
    outdir = directory
    # opts = "-f %d"
  )
  
  outfiles = converted$output_files
  outfiles = unique(outfiles)
  outfiles = outfiles[ file.exists(outfiles) ]
  new_files = strsplit(outfiles, str_separator)
  new_files = sapply(new_files, function(x) {
    subber = paste0(".*", str_separator, "\\d_(.*).nii.gz")
    x[2] = sub(subber, "\\1", x[2])
    x[2] = sub(".*\\d\\d_([^\\d].*)[.]nii.gz", "\\1.nii.gz", x[2])
    x[2] = sub("Mask", "Msk", x[2])
    x[2] = sub("msk2", "Msk", x[2])
    paste0(x[1], "_", x[2])
  })
  new_files = sub("Eq_1", "", new_files)
  new_files = sub("Tilt_1", "", new_files)
  new_files = sub("_Tilt", "", new_files)
  new_files = sub("_Eq", "", new_files)
  new_files = sub("_[.]nii", ".nii", new_files)
  new_files = file.path(dirname(new_files), 
                        paste0(id, "_", basename(new_files)))
  
  file.rename(outfiles, new_files)
  out_fol = df$folder[ df$p_id %in% id]
  out_fol = unique(out_fol)
  stopifnot(length(out_fol) == 1)
  
  file.copy(new_files, to = out_fol, overwrite = TRUE)
}
