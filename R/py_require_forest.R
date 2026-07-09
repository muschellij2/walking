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
#' py_require_forest()
#' }
py_require_forest = function(python_version = "3.11", ...) {
  reticulate::py_require(
    "git+https://github.com/onnela-lab/forest@45fb41038bd46c25d9e6a4442aa74fa03b501317",
    python_version = python_version,
    ...)
}
