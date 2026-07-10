# Call py_require_forest but do cleanup for CRAN
files = list.files()
walking::py_require_forest()
reticulate::import("forest")
new_files = list.files()
sd = setdiff(new_files, files)
sd = sd[grepl("^uv.*[.]lock", sd)]
if (length(sd) > 0) {
  file.remove(sd)
}
