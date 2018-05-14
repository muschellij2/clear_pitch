#
library(methods)
library(dcmtk)
library(here)
library(tidyr)
library(dplyr)
library(readr)

raw_dir = here("raw")
all_files = list.files(
  path = raw_dir, pattern = "[.](DCM|dcm)$",
  recursive = TRUE, full.names = TRUE)
all_dirs = unique(dirname(all_files))
ct_dirs = all_dirs[ grepl("CT", all_dirs)]
ct_dirs = ct_dirs[ !grepl("Mask", ct_dirs)]

outfile = "long_scanning_data.rds"
rerun = TRUE


if (!file.exists(outfile) || rerun) {
  run_path = ct_dirs[2]
  n_dirs = length(ct_dirs)
  print(n_dirs)
  all_df = vector(mode = "list", length = n_dirs)
  ipath = 1
  
  for (ipath in seq_along(ct_dirs)) {
    print(ipath)
    run_path = ct_dirs[ipath]
    xx = read_dicom_header(path = run_path, recursive = TRUE)
    xx$path = run_path
    # "0008-0070-Manufacturer"
    # (0008,0070) Manufacturer
    # (0018,0060) KVP
    # (0018,1151)	XRayTubeCurrent
    # (0018,8151) XRayTubeCurrent in muA
    # (0018,1152) exposure
    # (0018,1150) exposureTime
    # (0018,1153) exposureTime in muS
    # (0018,0050) slicethickness
    # (0018,1120) Gantry Tilt 
    
    tags = c("(0008,0070)", "(0018,0060)", "(0018,1151)", "(0018,8151)", 
             "(0018,1152)",  "(0018,1150)", "(0018,1153)", "(0018,0050)",
             "(0018,1120)", 
             # scanning times
             "(0008,0030)", "(0008,0031)", "(0008,0032)", "(0008,0033)",
             # scanning dates
             "(0008,0020)", "(0008,0021)", "(0008,0022)", "(0008,0023)",
             # Private Headers for SIEMENS
             "(0029,0010)", "(0029,0011)",
             "(0018,1016)", # secondary manufacturer
             "(0018,1154)", "(0018,9073)",
             "(0018,1170)", "(0018,9307)",
             "(0018,9311)", "(0018,9345)",
             "(0008,1090)")
    write_rds(tags, path = "keeping_tags.rds")
    df = xx %>% 
      filter(tag %in% tags) %>% 
      select(file, path, name, value, tag) %>% 
      mutate(value = sub("\\[", "", value),
             value = sub("\\]", "", value)) %>% 
      distinct()
    df = df %>% 
      group_by(file, name) %>% 
      mutate(value = if_else(name == "PrivateCreator",
                             paste(value, collapse = "; "),
                             value)) %>% 
      slice(1) %>% 
      ungroup()
    
    all_df[[ipath]] = df
  }
  done_df = bind_rows(all_df)

  write_rds(done_df, path = outfile)
  
} else { 
  done_df = read_rds(outfile)
}

df = done_df %>% 
  select(file, path, name, value) %>% 
  spread(key = name, value = value)


make_numeric = function(x) {
  na_x = is.na(x)
  num_x = as.numeric(x)
  na_numx = is.na(num_x)
  # no new na
  if (!any(na_numx[!na_x])) {
    return(num_x)
  } else {
    warning("Some made NA")
    return(x)
  }
}
df = df %>% 
  mutate_at(.vars = vars( -file, -path, 
                          -Manufacturer,
                          -ManufacturerModelName,
                          -PrivateCreator), .funs = make_numeric)

write_rds(df, path = "scanning_data.rds")
