# mrggsave 0.2.0.9001


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
