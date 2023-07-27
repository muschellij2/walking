oak_base = function() {
  fr = reticulate::import("forest")
  oak = fr$oak$base
  oak
}

oak_base_noconvert = function() {
  fr = reticulate::import("forest", convert = FALSE)
  oak = fr$oak$base
  oak
}
