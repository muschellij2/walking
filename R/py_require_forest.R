#' Require Forest module
#'
#' @param python_version Python version to use, passed to [reticulate::py_require()]
#' @param ... additional arguments passed to [reticulate::py_require()]
#'
#' @returns invisible NULL
#' @export
#'
#' @examples
#' \donttest{
#' files <- list.files()
#' py_require_forest()
#' walking:::cleanup_uv_lock_files()
#' stopifnot(length(setdiff(list.files(), files)) == 0L)
#' }
py_require_forest = function(python_version = "3.11", ...) {
  on.exit(cleanup_uv_lock_files(), add = TRUE)
  reticulate::py_require(
    "git+https://github.com/onnela-lab/forest@45fb41038bd46c25d9e6a4442aa74fa03b501317",
    python_version = python_version,
    ...)
}
