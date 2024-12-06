## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rhub)

## ----list---------------------------------------------------------------------
imgs <- local_check_linux_images()
imgs
knitr::kable(imgs, row.names = FALSE)

## -----------------------------------------------------------------------------
system.file(package = "rhub", "bin", "rhub-linux-docker.sh")

