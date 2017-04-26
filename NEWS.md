# IOSP changelog

## [Unreleased]

### Added

* `NEWS.md` file to track changes to the package.
* modify font size using `<span class = "xx-small">` like described in the [htmlref](http://www.htmlref.com/examples/chapter10/font_properties_src.html)
* shortcut for framed colums: `{.box-6}` for `{.box .col-6}`

### Fixed

* `.build` attribute on columns and/or boxes now correctly allows incremental display
* fix nested blockquotes which were not properly rendered
* fix paragraph spacing in boxes
* fix wrong top margin when chunks are placed in columns
* remove shadow for all chunks palced in a box
* remove css code for nested rows (not supported yet)

## [0.1.0]

### Added

* footer from issue #8
* bg-white and bg-gray colours from issue #9


