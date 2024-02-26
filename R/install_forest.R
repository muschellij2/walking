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
#'    forest_version_pip()
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


module_info = function(module = "numpy") {
  assertthat::is.scalar(module)
  if (!reticulate::py_module_available(module)) {
    stop(paste0(module, " is not installed!"))
  }
  df = reticulate::py_list_packages()
  df[df$package == module,]
}

module_version = function(module = "numpy") {
  module_info(module = module)$version
}

module_requirement = function(module = "numpy") {
  module_info(module = module)$requirement
}

#' @export
#' @rdname forest_setup
forest_version = function() {
  module_version("forest")
}

module_version_pip = function(module = "numpy") {
  python = reticulate::py_config()$python
  python = shQuote(python)
  cmd = paste0(python, " -m pip freeze")
  out = system(cmd, intern = TRUE)
  out = out[grep(paste0("^", module, "(=|\\s|@)"), out)]
  out
}

#' @export
#' @rdname forest_setup
forest_version_pip = function() {
  module_version_pip("forest")

}

#' @export
#' @rdname forest_setup
forest_requirement = function() {
  module_requirement("forest")
}

