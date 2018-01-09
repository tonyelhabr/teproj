
Introduction
============

This package containts functions that I keep often in projects.

Installation
------------

`devtools::install_github("aelhabr/teproj")`.

Notes
-----

Here is a list of all functions in the package.

``` r
library("teproj")
ls("package:teproj")
#>  [1] "create_dir"          "export_excel"        "export_ext"         
#>  [4] "export_ext_csv"      "export_ext_png"      "export_ext_rda"     
#>  [7] "export_ext_rdata"    "export_ext_RData"    "export_ext_rds"     
#> [10] "export_ext_xlsx"     "export_fig"          "export_viz"         
#> [13] "import_excel"        "import_ext"          "import_ext_csv"     
#> [16] "import_ext_rda"      "import_ext_rdata"    "import_ext_RData"   
#> [19] "import_ext_rds"      "import_ext_xlsx"     "parse_proj_io"      
#> [22] "render_proj_io"      "set_pkg_print_opts"  "set_pkg_render_opts"
```

Here is the code coverage of the unit tests.

``` r
covr::package_coverage()
#> Warning: running command 'C:/Users/aelhabr/DOCUME~1/R/R-34~1.0/bin/x64/R --
#> vanilla CMD config CC' had status 1

#> Warning: running command 'C:/Users/aelhabr/DOCUME~1/R/R-34~1.0/bin/x64/R --
#> vanilla CMD config CC' had status 1
#> teproj Coverage: 68.45%
#> R/zzz.r: 0.00%
#> R/utils.r: 45.53%
#> R/render_proj_io.R: 53.33%
#> R/export.R: 65.85%
#> R/import.R: 81.82%
#> R/parse_proj_io.R: 96.99%
```

Examples
--------

None (yet).
