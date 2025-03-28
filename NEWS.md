# mrggsave 0.4.7

## Bugs Fixed

- Fixed bug when using a `tag` to name plots when a full path to `script` was
  provided (#53).

# mrggsave 0.4.6

## Bugs Fixed

- Fixed a bug where an error was encountered when trying to save a list 
  of plots with no annotation (`labeller = NULL`) (#49).

- Updates for compatibility with glue 1.8.0 (#44).

# mrggsave 0.4.5

- New option `mrggsave.res` to set default resolution for devices 
  like `png()` and `jpeg()` (#42).

# mrggsave 0.4.4

- `mrggsave()` gains an argument called `path.type` which controls how the 
  path to the figure file is rendered in the figure annotation; the default 
  is `"proj"`, which will render the path relative to an RStudio project 
  file, if it can be found; alternatives are `"none"` and `"raw"`; `path.type`
  can be set through the `mrggsave.path.type` option (#38, #39).

- Unit tests were numbered and user stories added to build validation 
  documents (#35, #37).

# mrggsave 0.4.3

- Refactored how the `glue()` environment was captured so that string 
  interpolation is more robust when attempted in unique environments (for 
  example inside function calls) (#26)

# mrggsave 0.4.2

- Calling the `cairo_pdf` device now allows multiple plots to be written to the 
  same file (#21)

# mrggsave 0.4.1

- Revert behavior where named list automatically uses the names for output 
  file names #18
- Fix typo in `mrggsave:::output_file_sep()` title

# mrggsave 0.4.0 

## Breaking changes
- The default separator when auto-constructing file names is changed from 
  underscore to hyphen; this is a breaking change that will result in different
  output filenames if users were using `tag` #2
- Use the unexported (but documented) function `mrggsave:::output_file_sep()`
  to globally change the output file separator character #12
- The `prefix` argument to `mrggsave_common()` now defaults to `NULL`; the 
  `Source graphic:` annotation defaults to only showing the base file name 
  of the plot

## Other changes and refactoring

- If a named list of plots is passed (checked via `rlang::is_named`), `use_names` 
  will be set to `TRUE` and names used for output filename #8
- Passing a list of plots generated by `named_plots` will set `use_names` to 
  `TRUE` and the names will be use for output file names #8
- When a named list of plots is passed, the names are adjusted to remove
  spaces, periods and underscores when creating the output file stem; these
  characters are replaced with hyphens #8
- `mrggsave_common()` now has a `labeller` argument; pass a function to create
  the annotation for the plot; pass `NULL` to save a plot with no annotation #8
- `named_plots()` gains an argument called `add_context`; when `TRUE`, the 
  context (usually the script name) will be pre-pended to the output file name; 
  this argument is `FALSE` by default #8
- Add option called `mrggsave.file.tolower` which, if set to `TRUE` will pass
  the file stem through `base::tolower()`, making it all lower case, regardless
  of how the stem was generated #8
- Add ability to save to multiple devices on one function call; pass a character
  vector of devices or a comma-separated string as `dev`
  
# mrggsave 0.3.2
- Add support for ggsurvplot objects #11
- Force unit of `textGrob.y` to be `inch` to ensure consistent bottom margin
- Add support for patchwork objects #26
- Add support for gTree objects #34
- Fix file extension to pdf when cairo_pdf device is used #36

# mrggsave 0.3.0
- Validate as CR 102

# mrggsave 0.2.0.9001
- Add support for the following devices: pdf, png, tiff, jpeg, eps, ps, 
  bmp, svg (#13)
- Add ability to save last plot when it was created with ggplot2 (see the 
  `mrggsave_last` function); this gets the last plot via ggplot2::last_plot (#12)
- Add ability to save a named list of plots; when the list is named and 
  the `use_names` argument is invoked, plots will get written out to their
  own file names (#14)
- Add method for handing `ggsurvplot` objects (#11)
- Pass the file name through `glue::glue` once it is formed from either 
  `stem` or `tag` (e.g. `stem = "dv_pred_{runnumber}"`) (#15)
- Add `named_plots` function to generate plot names (and eventually file names)
  from the code used to generate the plot (#14)
- Add non-exported `context` function to set the output file name when 
  using `tag` argument; `context` is set and is in effect via `option` until
  the context is changed or reset; use `context_clear`
- Removed support for objects created with patchwork

# mrggsave 0.2.0
- Validate and release

# mrggsave 0.1.0.9002
- Added `ncol` as a formal argument; when `ncol` > 1, we set arrange to `TRUE`
- Added `mrggpage` function as a wrapper around `gridExtra::arrangeGrob`
- Added `mrggsave_list`, which takes in a list of objects that are ready 
to label and to save without arrangement
- Changed `mrggdraw` so that all it does is draw a plot or plots, 
possibly after arranging
- `mrggdraw` now returns processed plots invisibly so that they may 
be passed into `mrggsave`
- Added internal generic `mrggsave_prep_object` and methods so 
that plots of different types can be passed directly to `mrggsave_common`
- Added `mrggsave.gList` for saving `gList` objects
- When `tag` or `stem` are given as a vector, the vector is collapsed
using `_` as a separator first prior to forming any output file names
- When a file name is formed from `tag`, a `_` separator is automatically
placed between `script` and the (possibly collapsed) `tag` value
- Added support for `gtable` objects
- Added a default `mrggsave` method that assumes the object is a grob
- Added argument to `mrggpage` to allow a list of plots to be arranged
across multiple pages; added methods for handling `arrangelist` objects
coming out of `gridExtra::marrangeGrob`

# mrggsave 0.1.0
- Initial validation

# mrggsave 0.0.1.9001

- Pre-release
