## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(rsvg)

## -----------------------------------------------------------------------------
str <- charToRaw('<svg viewBox="0 0 300 300" xmlns="http://www.w3.org/2000/svg">
  <style>
    circle {
      fill: gold;
      stroke: maroon;
      stroke-width: 10px;
    }
  </style>

  <circle cx="150" cy="150" r="100" />
</svg>')
rsvg_png(str, file = 'ex1.png')

## ----echo=FALSE, fig.cap="Rendering of SVG with embedded CSS"-----------------
knitr::include_graphics("ex1.png")

## -----------------------------------------------------------------------------
svg <- charToRaw('<svg viewBox="0 0 300 300" xmlns="http://www.w3.org/2000/svg">
  <circle cx="150" cy="150" r="100" />
</svg>')

# Render without style:
rsvg_png(svg, file = 'ex2.png')

## ----echo=FALSE, fig.cap="Rendering of SVG without CSS"-----------------------
knitr::include_graphics("ex2.png")

## -----------------------------------------------------------------------------
css <- charToRaw('circle {
  fill: navy;
  stroke: hotpink;
  stroke-width: 10px;
}')
# Render without style:
rsvg_png(svg, css = css, file = 'ex3.png')

## ----echo=FALSE, fig.cap="Rendering of SVG with external CSS"-----------------
knitr::include_graphics("ex3.png")

## -----------------------------------------------------------------------------
bitmap <- rsvg_raw('https://jeroen.github.io/images/tiger.svg', width = 600)
str(bitmap)

## -----------------------------------------------------------------------------
magick::image_read(bitmap)

