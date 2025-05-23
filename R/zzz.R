.onLoad <- function(...) {
  # ref = "v0.2"
  ref = "45fb41038bd46c25d9e6a4442aa74fa03b501317"
  url = "https://github.com/onnela-lab/forest"
  url = paste0("git+", url)
  ref = sub("^@", "", ref)
  url = paste0(url, "@", ref)
  reticulate::py_require(url, python_version = "3.11")
}
