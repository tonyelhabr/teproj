
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

teproj <img src="man/figures/logo.png" align="right"/>
======================================================

Introduction
------------

This package contains functions that I use often in projects.

### Installation

`devtools::install_github("tonyelhabr/teproj")`.

Notes
-----

Here is a list of all functions in the package.

    #>  [1] "create_dir"            "create_kable"         
    #>  [3] "create_kable_html"     "create_kable_md"      
    #>  [5] "do_call_with"          "export_ext"           
    #>  [7] "export_ext_csv"        "export_ext_png"       
    #>  [9] "export_ext_rda"        "export_ext_rdata"     
    #> [11] "export_ext_rds"        "export_gg"            
    #> [13] "export_path"           "get_path_lazily"      
    #> [15] "get_path_safely"       "import_ext"           
    #> [17] "import_ext_csv"        "import_ext_rda"       
    #> [19] "import_ext_rdata"      "import_ext_rds"       
    #> [21] "import_ext_xlsx"       "import_path"          
    #> [23] "import_path_cleanly"   "set_pkg_render_opts"  
    #> [25] "sort_named_list"       "unrowname"            
    #> [27] "use_r_profile"         "use_task_schedule"    
    #> [29] "warningf"              "word_document_rstudio"
    #> [31] "word_document_te"

### Inspiration

The original purpose of this package was to provide a convenient interface to the `{readr}` and `{rio}` functions for importing and exporting data to various file formats. Because I often found myself using the variable name as the name of the file, I figured it would be nice to implement "lazy" methods for doing so. Under the hood, this packages's functions use "Non-Standared Evaluation" (NSE) (using techniques that may or may not be up to date with the latest `{tidyverse}` methods of NSE 😄).

Additionally, while implementing these `import`/`export` functions, I realized a couple of other functions that I often use in projects (e.g. `render_proj_io()` could be added. (I don't necessarily recommend using these "other" functions, however, because they are tailored towards my use cases.)

### Syntax \[^readme\]

-   **`import_ext*()`** functions - Implement `{rio}`-like interface (note the use of the verb "import") for reading data from files. The main function is `import_ext()`, which requires that the file extension be specified with the `ext` paramater. Shorcuts for the extensions that I find myself using most often are implemented via the syntax `import_ext_[ext]()` (e.g. `import_ext_csv()`). Under the hood, preference is given to `{readr}` methods if they exist and can be recognized. (This is because it reads in `data.frame`s as `tibble`s by default. Otherwise, an attempt is made to use a `{rio}` method (with subsequent coercion to a `tibble`).

-   **`export_ext*()`** functions - Counterpart to the `import_ext_*()` functions. Notably, exporting of plots is supported. However, the support is not robust-- it is intended only for use with `ggplot2` plots, and only to the `png` file format. The `units`, `width`, and `height` are set to package option values (see `options("teproj.ggsave.units"`, etc.) if they are not all specified explicitly together.

Examples
--------

TODO!
