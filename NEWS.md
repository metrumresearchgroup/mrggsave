# 0.1.0.9002
- Added ncol as a formal argument; when ncol > 1, we set arrange to TRUE
- Added defaults for script and stem for mrggdraw so that 
plots could be drawn without adding this information
- Added `mrggpage` function as a wrapper around `gridExtra::arrangeGrob`
- Added `mrggsave_list`, which takes in a list of objects that are ready 
to label and to save without arrangement
- Changed `mrggdraw` so that all it does is draw a plot or plots, 
possibly after arranging
- `mrggdraw` now returns processed plots invisibly so that they may 
be passed into `mrggsave`
- Added internal generic `mrggsave_prep_object` and methods so 
that plots of different types can be passed directly to `mrggsave_common`
- Added `mrggsave.gList`


# 0.1.0
- Initial validation

# 0.0.1.9001

- Pre-release
