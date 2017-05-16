# IOSP changelog

## [Unreleased]

### Added

* add lazy colour definition: colours listed in `colors()` can be used without definition in the header
* `column` class (level 2 headers) renders the boxes by columns instead of rows.
* background colours on level 2 headers change the slide background

### Changed

* boxes are rendered using the css flexbox layout
* code chunks are displayed as boxes (`shadow` option has been removed)
* `title` chunk option to adjust the title of the chunk box.

### Fixed

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


