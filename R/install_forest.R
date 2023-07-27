#' Install the `forest` Python Module
#'
#' @param ref Git reference to install, default is `main`.  Use `develop` for
#' the current development branch on GitHub.
#' @param ... Additional arguments to pass to [reticulate::py_install()],
#' other than `pip` (`pip = TRUE` enforced)
#'
#' @return Output of [reticulate::py_install]
#' @export
install_forest = function(ref = "develop", ...) {
  url = "https://github.com/onnela-lab/forest"
  url = paste0("git+", url)
  ref = sub("^@", "", ref)
  url = paste0(url, "@", ref)
  reticulate::py_install(url, pip = TRUE, ...)
}
