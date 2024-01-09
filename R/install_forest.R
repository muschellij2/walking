#' Install the `forest` Python Module
#'
#' @param ref Git reference to install, default is `main`.  Use `develop` for
#' the current development branch on GitHub.
#' @param ... Additional arguments to pass to [reticulate::py_install()],
#' other than `pip` (`pip = TRUE` enforced)
#'
#' @return Output of [reticulate::py_install]
#' @export
#' @rdname forest_setup
#' @examples
#' if (have_forest()) {
#'    forest_version()
#' }
#'
install_forest = function(ref = "develop",
                          ...
) {
  url = "https://github.com/onnela-lab/forest"
  url = paste0("git+", url)
  ref = sub("^@", "", ref)
  url = paste0(url, "@", ref)
  reticulate::py_install(
    url,
    pip = TRUE, ...)
}

#' @export
#' @rdname forest_setup
have_forest = function() {
  reticulate::py_module_available("forest")
}


module_version = function(module = "numpy") {
  assertthat::is.scalar(module)
  if (!reticulate::py_module_available(module)) {
    stop(paste0(module, " is not installed!"))
  }
  df = reticulate::py_list_packages()
  df$version[df$package == module]
}

#' @export
#' @rdname forest_setup
forest_version = function() {
  module_version("forest")
}

