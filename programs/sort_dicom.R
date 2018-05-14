library(tractor.base)
dir = "/Volumes/LACIE/Nifti/6003-157/"
before_run = list.dirs(dir, recursive = FALSE, full.names = TRUE)

res = sortDicomDirectories(dir, deleteOriginals = TRUE)

after_run= list.files(dir, recursive = FALSE, full.names = TRUE)

new_dirs = setdiff(after_run, before_run)
old_dirs = intersect(after_run, before_run)

unlink(old_dirs, recursive = TRUE)


for (i in seq_along(new_dirs)) {
  basedir = new_dirs[i]
  res = dcm2nii(basedir = basedir)
  if (res$result == 0) {
    outfile = check_dcm2nii(res)
    if (length(outfile) > 0) {
      file.copy(outfile, dir, overwrite = TRUE)
    }
  } else {
    stop(paste0("directory is ", basedir))
  }
  
}