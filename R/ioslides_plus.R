#' @export
ioslides_plus <- function(logo = NULL,
                          slide_level = 2,
                          incremental = FALSE,
                          fig_width = 7.5,
                          fig_height = 4.5,
                          fig_retina = 2,
                          fig_caption = TRUE,
                          dev = 'png',
                          df_print = "default",
                          smart = TRUE,
                          self_contained = TRUE,
                          widescreen = FALSE,
                          smaller = FALSE,
                          transition = "default",
                          mathjax = "default",
                          analytics = NULL,
                          template = NULL,
                          css = NULL,
                          includes = NULL,
                          keep_md = FALSE,
                          lib_dir = NULL,
                          md_extensions = NULL,
                          pandoc_args = NULL,
                          extra_dependencies = NULL,
                          footer = NULL,
                          box_colours = NULL,
                          box_colors = NULL,
                          ...) {

  # base pandoc options for all output
  args <- c()
  library(iosp)
  # widescreen
  if (widescreen)
    args <- c(args, "--variable", "widescreen");

  # pagedtables
  if (identical(df_print, "paged")) {
    extra_dependencies <- append(extra_dependencies,
                                 list(rmarkdown:::html_dependency_pagedtable()))
  }

  # transition
  if (is.numeric(transition))
    transition <- as.character(transition)
  else if (transition %in% c("default", "faster", "slower"))
    transition <- switch(transition,
                         "default" = "0.4",
                         "faster" = "0.2",
                         "slower" = "0.6")
  else
    stop('transition must be "default", "faster", "slower" or a ',
         'numeric value (representing seconds)', call. = FALSE)
  args <- c(args, rmarkdown::pandoc_variable_arg("transition", transition))

  # additional css
  for (css_file in css)
    args <- c(args, "--css", rmarkdown::pandoc_path_arg(css_file))

  # Path to the box_colours css file which is created in the pre_processor
  # The file will be removed after knitting
  css_colour_file <- NULL
  
  # Path to the logo file (will be updated in the pre_processor)
  logo_path <- NULL

  # content includes
  args <- c(args, rmarkdown::includes_to_pandoc_args(includes))

  # template path and assets
  if (!is.null(template) && file.exists(template))
    args <- c(args, "--template", template)
  else
    args <- c(args,
              "--template",
              system.file("rmd", "iosp", "default.html", package = "iosp"))

  # html dependency for ioslides
  extra_dependencies <- append(extra_dependencies,
                               list(html_dependency_ioslides(),
                                    html_dependency_iosplus()))

  # analytics
  if (!is.null(analytics))
    args <- c(args, rmarkdown::pandoc_variable_arg("analytics", analytics))

  # pre-processor for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <- files_dir

    # extra args
    args <- c()

    # create the files dir if it doesn't exist
    if (!rmarkdown:::dir_exists(files_dir))
      dir.create(files_dir)

    # logo
    if (!is.null(logo)) {
      logo_path <- logo
      if (!self_contained) {
        # use same extension as specified logo (default is png if unspecified)
        logo_ext <- tools::file_ext(logo)
        if (nchar(logo_ext) < 1)
          logo_ext <- "png"
        logo_path <- file.path(files_dir, paste("logo", logo_ext, sep = "."))
        file.copy(from = logo, to = logo_path)
        logo_path <<- rmarkdown:::normalized_relative_to(output_dir, logo_path)
      } else {
        logo_path <<- rmarkdown::pandoc_path_arg(logo_path)
      }
      args <- c(args, "--variable", paste("logo=", logo_path, sep = ""))
    }

    # Generate the list of box colours that we ship with iosp
    iosp_colours <- list(
      red = list(bg = "#ffad99", header_bg = "#991f00"),
      green = list(bg = "#adebad", header_bg = "#1f7a1f"),
      blue = list(bg = "#99d6ff", header_bg = "#005c99"),
      yellow = list(bg = "#ffec8b", header_bg = "#eeb422"),
      gray = list(bg = "#fafafa", header_bg = "#cecece"),
      white = list(bg = "#ffffff", header_bg = "#cecece"),
      cobalt = list(bg = "#2b557a", header_bg = "#002240")
    )
    
    # Adding custom colours
    if (!is.null(box_colors) && is.null(box_colours)) box_colours <- box_colors

    # Trying to lazy load colours
    lazy_colours <- unique(sub(".*\\.bg-(\\w+).*", "\\1", grep("\\.bg-(\\w+)", readLines(input_file), value = TRUE)))
    # filtering out iosp colours and colours not in colors()
    lazy_colours <- lazy_colours[!lazy_colours %in% names(iosp_colours) & lazy_colours %in% colors()]
    if (is.list(box_colours)) {
      # filtering out colours defined in the header
      lazy_colours <- lazy_colours[!lazy_colours %in% sub("^bg-", "", names(box_colours))]
    }
    
    box_colours <- c(box_colours, as.list(setNames(lazy_colours, nm = lazy_colours)))

    # Allow override of standard iosp colours
    box_colours <- c(box_colours, iosp_colours[!names(iosp_colours) %in% names(box_colours)])
    
    if (is.list(box_colours)) {
      # Calling unlist followed by as.list to support a single string argument (add_box_colour mandatory argument)
      css_content <- lapply(seq_along(box_colours), function(x) {do.call(add_box_colour, as.list(c(names(box_colours)[[x]], unlist(box_colours[[x]]))))})
      css_content <- paste(css_content, collapse = "\n")
      css_content <- paste("<style type=\"text/css\">", css_content, "</style>", sep = "\n");
      css_colour_file <<- file.path(dirname(input_file), "box_colours.css")
      tryCatch({
        suppressWarnings(writeLines("", css_colour_file, useBytes = TRUE))
      },
      error = function(...) {
        css_colour_file <<- file.path(output_dir, "box_colours.css")
      })

      writeLines(css_content, css_colour_file, useBytes = TRUE)
      args <- c(args, rmarkdown::includes_to_pandoc_args(list(in_header = css_colour_file)))
    }

    # return additional args
    args
  }

  # post processor that renders our markdown using our custom lua
  # renderer and then inserts it into the main file
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    # setup args
    args <- c()

    # add any custom pandoc args
    args <- c(args, pandoc_args)

    # Converting md links to html
    footer <- gsub("\\[([^\\[\\]\\(\\)]*)\\]\\(([^\\[\\]\\(\\)]*)\\)", "<a href='\\2'>\\1</a>", footer, perl = TRUE)
    # Creating html links from urls (http://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url)
    footer <- gsub("(?<!href=')(https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*))", "<a href=\\1>\\1</a>", footer, perl = TRUE)

    # attempt to create the output writer alongside input file
    lua_writer <- file.path(dirname(input_file), "ioslides_presentation.lua")
    tryCatch({
      suppressWarnings(writeLines("", lua_writer, useBytes = TRUE))
    },
    error = function(...) {
      # The input directory may not be writable (on e.g. Shiny Server), so write
      # to the output directory in this case. We don't always do this since
      # supplying a fully qualified path to the writer can trigger a bug on some
      # Linux configurations.
      lua_writer <<- file.path(dirname(output_file),
                               "ioslides_presentation.lua")
    })
    on.exit(unlink(lua_writer), add = TRUE)

    # determine whether we need to run citeproc
    input_lines <- readLines(input_file, warn = FALSE)
    run_citeproc <- rmarkdown:::citeproc_required(metadata, input_lines)

    # write settings to file
    settings <- c()
    add_setting <- function(name, value) {
      settings <<- c(settings, paste("local", name, "=",
                                     ifelse(value, "true", "false")))
    }
    add_setting("fig_caption", fig_caption)
    add_setting("incremental", incremental)
    add_setting("smaller", smaller)
    add_setting("smart", smart)
    add_setting("mathjax", !is.null(mathjax))

    # Set level of slide header (used by ioslides_presentation.lua)
    settings <- c(settings, sprintf("local slide_level = %s", slide_level))
    # Adding footer to lua (paste0 will handle NULL or character(0) better than sprintf)
    settings <- c(settings, paste0("local footer = \"", footer, "\""))
    if (!is.null(logo_path))  settings <- c(settings, paste0("local logo = \"", logo_path, "\""))

    writeLines(settings, lua_writer, useBytes = TRUE)

    # For consistency add as pandoc argument
    args <- c(args, "--slide-level", as.character(slide_level))

    # append main body of script
    file.append(lua_writer,
                system.file("rmd", "iosp", "ioslides_plus.lua", package = "iosp"))
    output_tmpfile <- tempfile("ioslides-output", fileext = ".html")
    on.exit(unlink(output_tmpfile), add = TRUE)
    on.exit(unlink(css_colour_file), add = TRUE)
    # on Windows, cache the current codepage and set it to 65001 (UTF-8) for the
    # duration of the Pandoc command. Without this, Pandoc fails when attempting
    # to hand UTF-8 encoded non-ASCII characters over to the custom Lua writer.
    # See https://github.com/rstudio/rmarkdown/issues/134
    if (rmarkdown:::is_windows()) {
      # 'chcp' returns e.g., "Active code page: 437"; strip characters and parse
      # the number
      codepage <- as.numeric(gsub("\\D", "", system2("chcp", stdout = TRUE)))

      if (!is.na(codepage)) {
        # if we got a valid codepage, restore it on exit
        on.exit(system2("chcp", args = codepage, stdout = TRUE), add = TRUE)

        # change to the UTF-8 codepage
        system2("chcp", args = 65001, stdout = TRUE)
      }
    }

    rmarkdown::pandoc_convert(input = input_file,
                              to = rmarkdown::relative_to(dirname(input_file), lua_writer),
                              from = rmarkdown:::from_rmarkdown(fig_caption),
                              output = output_tmpfile,
                              options = args,
                              citeproc = run_citeproc,
                              verbose = verbose)

    # read the slides
    slides_lines <- readLines(output_tmpfile, warn = FALSE, encoding = "UTF-8")

    # base64 encode if needed
    if (self_contained) {
      base64_encoder <- rmarkdown:::base64_image_encoder()
      slides_lines <- base64_encoder(slides_lines)
    }

    # read the output file
    output_lines <- readLines(output_file, warn = FALSE, encoding = "UTF-8")

    # substitute slides for the sentinel line
    sentinel_line <- grep("^RENDERED_SLIDES$", output_lines)
    if (length(sentinel_line) == 1) {
      preface_lines <- c(output_lines[1:sentinel_line[1] - 1])
      suffix_lines <- c(output_lines[-(1:sentinel_line[1])])
      output_lines <- c(preface_lines, slides_lines, suffix_lines)
      writeLines(output_lines, output_file, useBytes = TRUE)
    } else {
      stop("Slides placeholder not found in slides HTML", call. = FALSE)
    }

    output_file
  }

  hook_chunk <- function(x, options) {
    # Adapted from the knitr hook_chunk (hooks-md.R)
    fence_char = '`'
    fence = paste(rep(fence_char, 3), collapse = '')
    x = gsub(paste0('[\n]{2,}(', fence, '|    )'), '\n\n\\1', x)

    # If "row" is set (TRUE or a vector with 2 values), we wrap the chunk in a row.
    if (options$engine == "R" && is.numeric(options$row) && length(options$row) == 2 && sum(options$row) < 13) {
      row <- TRUE
      col_width <- options$row
    } else if (isTRUE(options$row)) {
      row <- TRUE
      col_width <- c(6, 6)
    } else row <- FALSE

    # Should I change the following lines and use options$engine instead of the hardcoded r?
    if (row) {
      # Trying to detect multiple source chunks to place them in different rows
      # Matching the fence and extracting positions
      m <- gregexpr(paste0("\n([", fence_char, "]{3,})\n+\\1r\n"), x)[[1]]
      if (m != -1) {
        m <-  c(1, m + 4, nchar(x))
        x <- lapply(1:(length(m) - 1), function(i) substr(x, m[i], m[i + 1]))
      }
      # Splitting columns (After the source chunk is closed)
      x <- lapply(x, function(s) {p <- regexpr("\n+```[^r]\n+", s) + 4; c(substr(s, 1, p), substr(s, p, nchar(s)))})
      if (isTRUE(options$collapse)) {
        x <- lapply(1:2, function(i) gsub(paste0('\n([', fence_char, ']{3,})\n+\\1(r)?\n'), "\n", paste0(unlist(lapply(x, `[[`, i)), collapse = "\n")))
        x <- paste0("\n<div class = \"col col-", col_width, "\">\n", x, "\n</div>\n", collapse = "\n")
      } else {
        x <- lapply(x, function(v) paste0("\n<div class = \"col col-", col_width, "\">\n", v, "\n</div>\n", collapse = "\n"))
      }
    }

    # If code chunks are present we wrap them in a container div
    if (grepl("\n+```(r)?\n+", x)) {
      options$class <- c("col", paste("col", options$width, sep = "-"), "box", "chunk", "bg-cobalt", options$class)
    }
    if (!is.null(options$title)) x <- paste0("<h3>", options$title, "</h3>", x)
    x <- paste0("\n<div id = \"", options$label, "\" class = \"", paste(options$class, collapse = " "), " \">\n", x, "\n</div>\n")
    x = gsub('[\n]+$', '', x)
    x = gsub('^[\n]+', '\n', x)
    if (is.null(s <- options$indent)) return(x)
    knitr:::line_prompt(x, prompt = s, continue = s)
  }

  knitr = rmarkdown::knitr_options_html(fig_width, fig_height, fig_retina, keep_md, dev)
  knitr$knit_hooks$chunk  <- hook_chunk
  knitr$opts_chunk$comment <- NA
  knitr$opts_chunk$width <- 12
  knitr$opts_chunk$class <- ""
  knitr$opts_chunk$title <- NULL

  # return format
  rmarkdown::output_format(
    knitr = knitr,
    pandoc = rmarkdown::pandoc_options(to = "html",
                                       from = rmarkdown:::from_rmarkdown(fig_caption, md_extensions),
                                       args = args),
    keep_md = keep_md,
    clean_supporting = self_contained,
    df_print = df_print,
    pre_processor = pre_processor,
    post_processor = post_processor,
    base_format = rmarkdown::html_document_base(smart = smart, lib_dir = lib_dir,
                                                self_contained = self_contained,
                                                mathjax = mathjax,
                                                pandoc_args = pandoc_args,
                                                extra_dependencies = extra_dependencies,
                                                bootstrap_compatible = TRUE, ...))
}

html_dependency_ioslides <- function() {
  htmltools::htmlDependency(
    name = "ioslides",
    version = "13.5.1",
    src = rmarkdown:::rmarkdown_system_file("rmd/ioslides/ioslides-13.5.1"),
    script = c(
      "js/modernizr.custom.45394.js",
      "js/prettify/prettify.js",
      #"js/prettify/lang-r.js", # Removing lang-r as we will override it
      "js/prettify/lang-yaml.js",
      "js/hammer.js",
      "js/slide-controller.js",
      "js/slide-deck.js"
    ),
    stylesheet = c(
      "fonts/fonts.css",
      "theme/css/default.css",
      "theme/css/phone.css")
  )
}

html_dependency_iosplus <- function() {
  htmltools::htmlDependency(
    name = "iosp",
    version = "0.1",
    src = system.file("rmd", "iosp", "libs", package = "iosp"),
    script = c(
      "js/lang-r.js"
    ),
    stylesheet = c(
      "css/iosp.css"
    )
  )
}