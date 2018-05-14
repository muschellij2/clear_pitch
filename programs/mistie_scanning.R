#
rm(list=ls())
library(methods)
library(dcmtk)
library(here)
library(tidyr)
library(dplyr)
library(readr)
library(matrixStats)

tags = read_rds("keeping_tags.rds")

pname_to_pid = function(patientName, study = "MISTIE") {
  patientName = as.integer(patientName)
  divisor = ifelse(study == "CLEAR", 10000, 1000)
  p_id = paste0(patientName %% divisor, "-",
                floor(patientName / divisor))
  p_id[ study == "MISTIE" ] = paste0("0",
                                     p_id[ study == "MISTIE" ])
  p_id
}
xx = load("Scanning_Parameters.Rda")

rn = sapply(alltabs, function(x) {
  unique(dirname(rownames(x)))
})
rn = sub(".*Sorted/", "", rn)
rn = sub("_CT.*", "", rn)
names(alltabs) = rn
df = bind_rows(alltabs, .id = "fname")

cn = colnames(df)
cn = sapply(strsplit(cn, split = "-"), 
            function(x) {
              if (length(x) < 2) {
                return(x)
              }
              return(paste(x[1:2], collapse = "-"))
            })
colnames(df) = cn

tag_df = dcmtk::dicom_tags[, c("tag", "keyword")]
tag_df = tag_df[ tag_df$tag %in% tags,]

tag_colnames = sub("\\(", "", tag_df$tag)
tag_colnames = sub("\\)", "", tag_colnames)
tag_colnames = sub(",", "-", tag_colnames)
tag_df$cn = tag_colnames
tag_df = tag_df[ tag_df$cn %in% colnames(df),]
df = df[, c("fname", tag_df$cn)]
tag_df$keyword = make.names(tag_df$keyword)
tag_df$keyword = gsub(".", "", tag_df$keyword, fixed = TRUE)
colnames(df) = c("fname", tag_df$keyword)

df = df %>% 
  separate(col = fname, into = c("patientName", "date", "time"),
           sep = "_") %>% 
  mutate(datetime = paste0(date, time)) %>% 
  select(-date, -time) %>% 
  distinct()
df$id = pname_to_pid(sub("-", "", df$patientName))

scan_df = df

#################################
# Cross ref with the niis
#################################
files = list.files(pattern = ".nii.gz", path = here("batch"))
df = data_frame(fname = files)
df = df %>% 
  separate(col = fname, into = c("id", "datetime", "blah"),
           sep = "_") %>% 
  select(-blah) %>% 
  distinct()
# keep MISTIE 
df = df %>% 
  filter(grepl("^0", id))


#################################
# make sure all are in there
#################################
df %>% 
  select(id, datetime)
sdf = scan_df %>% 
  select(id, datetime)

n_missing = anti_join(df, sdf) %>% distinct() %>% nrow()
if (n_missing > 0) {
  stop("Missing some DICOM data!")
}

scan_df = left_join(df, scan_df, by = c("id", "datetime"))

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
scan_df = scan_df %>% 
  mutate_at(.vars = vars( -Manufacturer,
                          -ManufacturerModelName), .funs = make_numeric)



write_rds(scan_df, path = "mistie_scanning_params.rds")
