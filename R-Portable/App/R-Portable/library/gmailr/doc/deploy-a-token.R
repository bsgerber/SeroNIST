## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup, message = FALSE, eval = TRUE--------------------------------------
library(gmailr)

## ----eval = TRUE--------------------------------------------------------------
writeLines(list.files(
  system.file("deployed-token-demo", package = "gmailr")
))

## -----------------------------------------------------------------------------
#  gm_auth("jane@example.com", cache = FALSE)

## -----------------------------------------------------------------------------
#  gm_auth_configure("path/to/your/oauth_client.json")
#  gm_auth("jane@example.com", cache = FALSE)

## -----------------------------------------------------------------------------
#  gm_profile()

## -----------------------------------------------------------------------------
#  gm_token_write(path = "path/to/gmailr-token.rds")

## -----------------------------------------------------------------------------
#  gm_token_write(
#    path = "path/to/gmailr-token.rds",
#    key = "GMAILR_KEY"
#  )

## -----------------------------------------------------------------------------
#  gm_auth(token = gm_token_read(
#    path = "path/to/gmailr-token.rds",
#    key = "GMAILR_KEY"
#  ))

