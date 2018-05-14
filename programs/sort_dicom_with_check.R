library(dcm2niir)
tractor_version = packageVersion("tractor.base")
if (compareVersion(as.character(tractor_version), "3.1.3") < 0) {
  devtools::install_github(
    "tractor/tractor",
    subdir = "tractor.base")
}

library(tractor.base)
library(tidyr)
ids = list.dirs(full.names = FALSE, recursive = FALSE)
ids = grep(pattern = "\\d{4}-\\d{3}", ids, value = TRUE)
id = ids[15]
print(id)
# id = "6008-321"
# directory = "/Users/johnmuschelli/Desktop/test_dcmsort/6096-210_S_copy"
directory = file.path("/Volumes/LACIE/Nifti/", id)
before_run = list.dirs(directory, recursive = FALSE)

# remove DICOMDIRs
dcm_dirs = list.files(
  path = directory, 
  pattern = "DICOMDIR", 
  recursive = TRUE)
if (length(dcm_dirs) > 0) {
  dcm_dirs = dcm_dirs[ toupper(basename(dcm_dirs) == "DICOMDIR") ]
  if (length(dcm_dirs) > 0) {
    file.remove(dcm_dirs)
  }
}

# find all zip files - uncompress them, then delete zip files
all_zip = list.files(
  path = directory,
  pattern = "[.]zip$",
  recursive = TRUE, full.names = TRUE)
if (length(all_zip) > 0) {
  message("zip files found")
  file.remove(all_zip)
}

all_zip = list.files(
  path = directory,
  pattern = "[.]rar$",
  recursive = TRUE, full.names = TRUE)
if (length(all_zip) > 0) {
  message("rar files found")
  file.remove(all_zip)
}

# sort the data
res =  sortDicomDirectories(
  directory,
  deleteOriginals = TRUE,
  ignoreTransferSyntax = TRUE)

after_run = list.dirs(directory, recursive = FALSE)

new_dirs = setdiff(after_run, before_run)
old_dirs = intersect(after_run, before_run)

non_converted_files = lapply(
  old_dirs, list.files, 
  recursive = TRUE, 
  all.files = TRUE,
  full.names = TRUE)
non_converted_files = lapply(
  non_converted_files,
  function(x) {
    bn = basename(x)
    x = x[ !grepl(".DS_Store", bn, fixed = TRUE)]
    x
  })
lengths = sapply(non_converted_files, length)
keep = lengths > 0
non_converted_files = non_converted_files[keep]
if (any(keep)) {
  show_files = lapply(non_converted_files, head)
}
potential_dcm = sapply(non_converted_files, function(x){
  if (length(x) > 0) {
    x = x[basename(toupper(x)) != "DICOMDIR"]
    keep = grepl("[.]dcm$", tolower(x))
    keep = keep | !grepl("[.]", x)
    x = x[keep]
  }
  return(x)
})
any_dcm = sapply(potential_dcm, length) > 0

if (any(any_dcm)) {
  warning("Some DICOMs may have not been converted")
  print(show_files[any_dcm])
}
unlink(old_dirs, recursive = TRUE)











