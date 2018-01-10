
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
#>  [1] "create_dir"          "create_example"      "export_excel"       
#>  [4] "export_ext"          "export_ext_csv"      "export_ext_png"     
#>  [7] "export_ext_rda"      "export_ext_rdata"    "export_ext_RData"   
#> [10] "export_ext_rds"      "export_ext_xlsx"     "export_fig"         
#> [13] "export_viz"          "import_excel"        "import_ext"         
#> [16] "import_ext_csv"      "import_ext_rda"      "import_ext_rdata"   
#> [19] "import_ext_RData"    "import_ext_rds"      "import_ext_xlsx"    
#> [22] "parse_proj_io"       "render_proj_io"      "set_pkg_print_opts" 
#> [25] "set_pkg_render_opts"
```

Here is the code coverage of the unit tests.

``` r
covr::package_coverage()
#> Warning: running command 'C:/Users/aelhabr/DOCUME~1/R/R-34~1.0/bin/x64/R --
#> vanilla CMD config CC' had status 1

#> Warning: running command 'C:/Users/aelhabr/DOCUME~1/R/R-34~1.0/bin/x64/R --
#> vanilla CMD config CC' had status 1
#> teproj Coverage: 66.22%
#> R/create_example.R: 0.00%
#> R/zzz.r: 0.00%
#> R/utils.r: 45.53%
#> R/render_proj_io.R: 58.95%
#> R/export.R: 65.85%
#> R/import.R: 81.82%
#> R/parse_proj_io.R: 96.99%
```

Examples
--------

None (yet).
