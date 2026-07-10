# Call py_require_forest but do cleanup for CRAN
walking::py_require_forest()
reticulate::import("forest")
walking:::cleanup_uv_lock_files()
