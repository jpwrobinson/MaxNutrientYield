# MaxNutrientYield
R scripts and model simulations accompanying Robinson et al. Managing Fisheries for Maximum Nutrient Yield. 2022. *Fish & Fisheries*. doi: 10.111/faf.12649.

https://onlinelibrary.wiley.com/doi/10.1111/faf.12649

Run numbered R scripts (01.., 02) for model simulations. Figures 1 and 2 can be recreated using objects in [NorthSea_mMNY_simulated.rds](NorthSea_mMNY_simulated.rds) and [BalticSea_mMNY_simulated.rds](BalticSea_mMNY_simulated.rds).

This manuscript was produced using the following packages


```
R version 4.0.5 (2021-03-31)
Platform: x86_64-apple-darwin17.0 (64-bit)

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] C

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] here_0.1             skimr_2.1.1          forcats_0.5.0       
 [4] stringr_1.4.0        dplyr_1.0.7          purrr_0.3.4         
 [7] readr_2.0.2          tidyr_1.1.4          tibble_3.1.6        
[10] ggplot2_3.3.5        tidyverse_1.3.0.9000 mizer_2.3.0.1       

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.7        lubridate_1.8.0   rprojroot_2.0.2   assertthat_0.2.1 
 [5] digest_0.6.28     utf8_1.2.2        R6_2.5.1          cellranger_1.1.0 
 [9] plyr_1.8.6        repr_1.1.0        backports_1.3.0   reprex_0.3.0     
[13] httr_1.4.2        pillar_1.6.4      rlang_0.4.12      lazyeval_0.2.2   
[17] readxl_1.3.1      rstudioapi_0.13   data.table_1.14.2 htmlwidgets_1.5.4
[21] munsell_0.5.0     broom_0.7.10      compiler_4.0.5    modelr_0.1.8     
[25] xfun_0.27         base64enc_0.1-3   pkgconfig_2.0.3   htmltools_0.5.2  
[29] tidyselect_1.1.1  fansi_0.5.0       viridisLite_0.4.0 crayon_1.4.2     
[33] tzdb_0.1.2        dbplyr_2.1.1      withr_2.4.2       grid_4.0.5       
[37] jsonlite_1.7.2    gtable_0.3.0      lifecycle_1.0.1   DBI_1.1.1        
[41] pacman_0.5.1      magrittr_2.0.1    scales_1.1.1      cli_3.1.0        
[45] stringi_1.7.5     reshape2_1.4.4    fs_1.5.0          xml2_1.3.2       
[49] ellipsis_0.3.2    generics_0.1.1    vctrs_0.3.8       tools_4.0.5      
[53] glue_1.5.0        hms_1.1.1         fastmap_1.1.0     colorspace_2.0-2 
[57] rvest_1.0.1       plotly_4.10.0     knitr_1.28        haven_2.3.0      

```