# IOSP changelog

## [0.1.1.9000]

### Added

* add lazy colour definition: colours listed in `colors()` can be used without definition in the header
* `column` class (level 2 headers) renders the boxes by columns instead of rows.
* added support for slide classes. In a level 2 header, use the `slide-class` attribute or define and use a class starting with `slide-` which will act as a shortcut and be associated to the slide instead of the article.
* ~~background colours on level 2 headers change the slide background~~ (removed/changed by the previously described feature: use the `slide-class` attribute or use the `.slide-*` class shortcut.
* add inline code and link colour adjustement depending on background
* add `striped`, `condensed` and `bordered` bootstrap table themes to support fancy tables using `kableExtra`.
* support for the IOSlides image background feature ([from rmarkdown >= 1.7](https://github.com/rstudio/rmarkdown/pull/687))
* add `show` class to ignore parent's `build` class on a specific child

### Changed

* boxes are rendered using the css flexbox layout
* code chunks are displayed as boxes (`shadow` option has been removed)
* `title` chunk option to adjust the title of the chunk box.
* version number now uses .9000 for the development branch. The `iosp` library folder is named according to the current version number.
* replaced the `rmdtable` css class by the `table` one (adapted from bootstrap).
* merged changes from IOSlides in rmarkdown 1.8
* updated rmarkdown dependency to 1.8
* moved `slide-deck.js` from rmarkdown to iosp
* propagate `build` class to the body of boxes leaving the box header visible.
* removed unrecommended very compact box shortcuts (`.box-<width>-<offset>-<colour>` and `.box-<width>-<colour>`)

### Fixed

* knit does not fail anymore when using a horizontal rule (generates a new slide)
* knit works again with rmarkdown 1.8 when `self_contained` is TRUE (base64 encoding issue).
* do not build css `style` tags (which do not show up on the slide anyway).

## [0.1.1]

### Added

* `NEWS.md` file to track changes to the package.
* modify font size using `<span class = "xx-small">` like described in the [htmlref](http://www.htmlref.com/examples/chapter10/font_properties_src.html)
* shortcut for framed colums: `{.box-6}` for `{.box .col-6}`
* all chunks are now placed in a `div` container with adjustable class (`class` chunk option). Default class is now empty ("") and not "shadow" anymore (which is now a new option: see next point). The current chunk label is set as the `div` `id`.
* add `shadow` chunk option (defaults to TRUE) to disable the rendering of a shadowed box for code outputs.

### Changed

* blockquotes exhibit rounded corners like boxes
* remove shadow for all chunks or blockquotes placed in a box
* footer logo is directly integrated in HTML and not using javascript

### Fixed

* `.build` attribute on columns and/or boxes now correctly allows incremental display
* `.build` attribute on slices enables display row by row
* fix nested blockquotes which were not properly rendered
* fix paragraph spacing in boxes
* fix wrong top margin when chunks are placed in columns
* remove css code for nested rows (not supported yet)
* fix top margin for first `img` in column to align with the top of a box

## [0.1.0]

### Added

* footer from issue #8
* bg-white and bg-gray colours from issue #9


