.onUnload = function(libpath) {
  cleanup_uv_lock_files()
}
