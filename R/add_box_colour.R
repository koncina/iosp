#' Generate additional ioslides_plus box colours
#'
#' This function generates the css code to support additional box colours
#' 
#' @param name The name of the colour. The `bg-` prefix is added if it is missing to avoid a collapse with other defined colours.
#' 
#' @param bg The background colour defined as a colour name, hexadecimal string or a positive integer as supported by the `col2rgb()` function.
#' 
#' @param header_bg The optional background colour of the header bar. If missing, the function will define it.
#' 
#' @param text The optional text colour. If missing, the function will determine if the text should be rendered in black or white.
#' 
#' @param header_text The optional header bar text colour. If missing, the function will determine if the text should be rendered in black or white.
#' 
#' @return css code to be included in the ioslides_plus HTML document.
#' 
#' @export
add_box_colour <- function(name, bg, header_bg = NULL, text = NULL, header_text = NULL) {
  if (any(lapply(list(name, bg, header_bg, text, header_text), length) > 1)) stop("Only vectors of length 1 are supported")
  # To avoid any collapse with already defined css colours we are prepending "bg-" to the name if it is missing
  if (!grepl("^bg-.*", name)) name <- paste0("bg-", name)
  
  # We are calling col2rgb and rgb (to get hexcode colours from the colours defined in R)
  m <- col2rgb(bg)
  colour_bg <- rgb(m[1], m[2], m[3], maxColorValue = 255)
  
  # If no text colour is provided we try to detect if we should render it black or white
  if (is.null(text)) colour_text <- black_or_white(m)
  else colour_text <- do.call(rgb, as.list(c(col2rgb(text), maxColorValue = 255)))
  
  if (is.null(header_bg)) m <- round(0.5 * m)
  else m <- col2rgb(header_bg)
  
  colour_header_bg <- rgb(m[1], m[2], m[3], maxColorValue = 255)
  
  if (is.null(header_text)) colour_header_text <- black_or_white(m)
  else colour_header_text <- do.call(rgb, as.list(c(col2rgb(header_text), maxColorValue = 255)))
  
  css_colour <- sprintf("
  .%s {
    background-color: %s;
  }

  .box.%s {
    border: 2px solid %s;
    color: %s;
  }

  .box.%s > h3:first-child {
    background-color: %s;
    color: %s;
  }",
                        name, colour_bg, name, colour_header_bg, colour_text,
                        name, colour_header_bg, colour_header_text)
  
  if (isTRUE(getOption('knitr.in.progress'))) class(css_colour) <- "box_colour"
  
  css_colour
}

#' @rdname add_box_colour
#' @export
add_box_color <- add_box_colour

#' @export
knit_print.box_colour = function(x, options, ...) {
  knitr::asis_output(cat(paste("\n<style type=\"text/css\">", x, "</style>", sep = "\n")))
}

# From http://stackoverflow.com/a/1855903
# We are determining whether text should be black or white...
black_or_white <- function(rgb) {
  d = 1 - (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3])/255;
  if (d < 0.5) text_colour <- "#000"
  else text_colour <- "#fff"
  text_colour
}
