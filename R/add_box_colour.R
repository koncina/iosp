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
  bg <- do.call("rgb", c(as.list(col2rgb(bg)), maxColorValue = 255))
  
  #  Generate header_bg if missing 
  if (is.null(header_bg)) m <- col2rgb(bg) * 0.5
  else m <- col2rgb(header_bg)
  header_bg <- do.call("rgb", c(as.list(m), maxColorValue = 255))
  
  if (is.null(text)) {
    text <- ifelse(invert_to_white(bg), "#FFFFFF", "#000000")
  } else text <- do.call(rgb, as.list(c(col2rgb(text), maxColorValue = 255)))
  
  if (is.null(header_text)) {
    header_text <- ifelse(invert_to_white(header_bg), "#FFFFFF", "#000000")
  } else header_text <- do.call(rgb, as.list(c(col2rgb(header_text), maxColorValue = 255)))

  css_colour <- sprintf("
  .%s {
    background-color: %s;
    color: %s;
  }

  .box.%s {
    border: 2px solid %s;
    color: %s;
  }

  .box.%s > h3:first-child {
    background-color: %s;
    color: %s;
  }
                        
  blockquote.%s::before {
    color : %s;
  }
 
  blockquote.%s {
    color: %s;
    border-color: %s;
  }",
                        name, bg, text,
                        name, header_bg, text,
                        name, header_bg, header_text,
                        name, header_bg,
                        name, text, header_bg)
  
  
  if (invert_to_white(bg)) {
    # We should enhance the output for code and links in the box
    css_colour <- c(css_colour, sprintf("
  slide.%s > hgroup > h2 {
    color: rgba(255, 255, 255, 0.8);
  }

  slide.%s > hgroup > h3 {
    color: rgba(255, 255, 255, 0.7);
  }

  .%s a {
    color: rgb(200, 240, 255);
    border-bottom: 1px solid rgba(200, 240, 255, 0.5);
  }

  .%s code {
    background-color: rgba(255, 255, 255, 0.7);
  }
                                        ",
                                        name, name, name, name))
  }
  
  
  if (invert_to_white(header_bg)) {
    code_alpha <- 0.7
    link_colour <- "200, 240, 250"
  } else {
    code_alpha <- 0.3
    link_colour <- "42, 124, 223"
  }
  
  css_colour <- c(css_colour, sprintf(" 
  .box.%s > h3:first-child > code {
    background-color: rgba(255, 255, 255, %s);
  }
  .box.%s > h3:first-child > a {
    color: rgb(%s);
    border-bottom: 1px solid rgba(%s, 0.5);
  }
                                      ",
                                      name, code_alpha,
                                      name, link_colour, link_colour
                                      ))


  # Fade out box if contrast is not good enough
  # if (any(apply(matrix(c(bg, text, header_bg, header_text), ncol = 2), 2, get_contrast_ratio) < 4.5)) {
  #   css_colour <- c(css_colour, sprintf("
  # .box.%s {
  #   opacity: 0.2;
  # }
  # .box.%s:hover {
  #   opacity: 1;
  # }",
  #                                       name, name))
  # }
  # 

  css_colour <- paste(css_colour, collapse = "\n")  
  
  
  contrast_warning(c(bg, text), message = FALSE)
  contrast_warning(c(header_bg, header_text), message = FALSE)
  
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
invert_to_white <- function(colour) {
  rgb <- col2rgb(colour)
  d = 1 - (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3])/255;
  if (d < 0.5) return(FALSE)
  TRUE
}

# Adapted from https://www.w3.org/TR/WCAG20/
#' @export
get_contrast_ratio <- function(colours, show_warning = FALSE) {
  if (length(colours) > 2 || !is.vector(colours)) stop("Argument is a vector of length > 2!")
  r <- col2rgb(colours) / 255
  r[r <= 0.03928] <- r[r <= 0.03928] / 12.92
  r[r > 0.03928] <- ((r[r > 0.03928] + 0.055) / 1.055)^2.4
  r <- r * c(0.2126, 0.7152, 0.0722)
  r <- apply(r, 2, sum) + 0.05
  max(r) / min(r)
}

contrast_warning <- function(colours, message = TRUE) {
  r <- get_contrast_ratio(colours)
  if (r < 4.5) warning("Fails WCAG 2.0 level AA")
  else if (message & r < 7) message("WCAG 2.0 level AA")
  else if (message) message("WCAG 2.0 level AAA")
}
