Changes in Version 1.2.0 (2017-12-18)
--------------------------------------------------------

NEW FEATURES

* New function `RemoveAt` for removing elements from an object (such
  as rows and columns from a matrix)
* New functions `InterceptWarnings`, `ExpectNoWarning`, and
  `ExpectNoWarning` for checking for warnings in code/expressions and
  replacing particular warning messages with alternative messages.


Changes in Version 1.0.6 (2017-11-13)
--------------------------------------------------------

BUG FIXES

* `OutcomeName` again supports the supplied `formula` actually being
a `terms` object


Changes in Version 1.0.6 (2017-11-02)
--------------------------------------------------------

BUG FIXES

* `AllVariablesNames` now correctly handles interactions
in formulae (DS-1527)
* `AllVariablesNames` and `OutcomeName`now correctly 
handle function calls in formulae, e.g. `y~log(x)+I(x^2)` (DS-1527)

Changes in Version 1.0.3 (2017-10-31)
--------------------------------------------------------

BUG FIXES

* Fix for `AllVariablesNames` in case where `$` is being used in a
 formula without backticks (DS-1527)


Changes in Version 1.0.2 (2017-10-30)
--------------------------------------------------------

BUG FIXES

* Improved support for `.` on right hand side of formulae
in `AllVariablesNames` (DS-1527)
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
