Changes in Version 1.0.1 (2017-10-18)
--------------------------------------------------------

BUG FIXES

* Removed `flipRegression` from Suggests

Changes in Version 1.0.0 (2017-10-11)
--------------------------------------------------------

NEW FEATURES

* Begin using semantic versioning
* `CopyAttributes` now copies all attributes except those
specified in a new argument "attr.not.to.copy" which defaults
to `c("dimnames", "names", "row.names", "dim", "class", "levels")`

BUG FIXES

* In the list/data.frame case, `CopyAttributes` will now 
copy attributes of the entire list, in addition to the
attributes of each element
