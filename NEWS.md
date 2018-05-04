
# teproj 0.0.1.0

+ Added a `NEWS.md` file to track changes to the package.
+ Between development version and current version, changed `filename` and `filepath` arguments
to `file` and `path` (to emulate more common terminology).
+ Removed `.` prefixes to internal functions. (Personal preference no longer
favors this syntax for non-API functions.)

# teproj 0.0.1.1

+ Added some new functions, include `import_path()` and `export_path()`.
+ Exported functions that were previously internal only, such as `do_call_with()` and `warningf()`.
+ Cleaned up the internal of existing functions to redunce redundancy.
+ Removed the package options for printing messages and `ggplot2` defaults
because I determined that they were superfluous.
+ 
