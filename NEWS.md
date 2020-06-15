
# teproj 0.0.0.9000

The following functions are no longer exported/included in this package.

+ __`*_proj_io()`__ functions - "Meta" functions for an RStudio project.
`parse_proj_io()` attempts to create a `tibble` with
information regarding the input/output files in each script in an RStudio project.
The output could be used in a number of ways, such as in a node-dependency graph
visualizing how the project's files relate to one another.
`render_proj_io()` (a wrapper for `rmarkdwon::render()`) converts .R scripts
formatted with Roxygen2 style comments to .Rmd files. This is intended to be used
by users (like me) who prefer the "feel" of an R script when working with data that will
eventually be presented in a .Rmd file/report and don't want to work directly with
a .Rmd file. [^r2rmd] __NOTE:__


[^r2rmd]:
See [this blog post](http://brooksandrew.github.io/simpleblog/articles/render-reports-directly-from-R-scripts/) for more discussion.

+ __`set_pkg_*_opts()`__ functions - Functions to facilitate setting this package's options
(This is preferable to calling `options()` directly,
which requires knowing exactly what the name of the option is).

# teproj 0.0.1.0

+ Added a `NEWS.md` file to track changes to the package.
+ Between development version and current version, changed `filename` and `filepath` arguments
to `file` and `path` (to emulate more common terminology).
+ Removed `.` prefixes to internal functions. (Personal preference no longer
favors this syntax for non-API functions.)

# teproj 0.0.1.1

+ Added some new functions, include `import_path()` and `export_path()`.
+ Exported functions that were previously internal only, such as `do_call_with()` and `warningf()`.
+ Cleaned up the internal of existing functions to reduce redundancy.
+ Removed the package options for printing messages and `ggplot2` defaults
because I decided that they are superfluous.

# teproj 0.0.1.2

+ Made return from `create_kable()` not invisible for non-html formats.
(It is returned as invisible for html in order to avoid showing it in the Viewer pane.
+ Added Word document template `te`.

# teproj 0.0.1.3

+ Added `use_*()` functions.
+ Prefixed all non-exported functions with `.`.
+ Removed badges from README file.

# teproj 0.0.1.4

+ Un-exporting some of the "never used" functions (and prefixing them with ".")
+ Fixed "bug" with `export_path()` where it would try to export `data.frame` to csv
even if a path is specified.
+ Considering this to (maybe) be the final release.
    + Considering creating new packages `{teio}` and `{teauto}` to handle
    files and common project functions. Doing this will allow development
    with improved personal R skills without having to worry about revising old
    conventions used in this package. (Perhaps this might be viewed similarly
    to how `{dplyr}` was created to improve upon `{plyr}`.)
    With this in mind, this package should be viewed as a "success" in learning
    about package development and creating functions that are very useful (to me).
    
# teproj 0.0.1.5

+ Removed message when saving ggplots.
+ suppressed warnings in tests

