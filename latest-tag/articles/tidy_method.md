# Using tidy

\
[`suppressPackageStartupMessages`](https://rdrr.io/r/base/message.html)`(``{`\
`  `[`library`](https://rdrr.io/r/base/library.html)`(`[`crmPack`](https://docs.crmpack.org/)`)`\
`  `[`library`](https://rdrr.io/r/base/library.html)`(`[`knitr`](https://yihui.org/knitr/)`)`\
`  `[`library`](https://rdrr.io/r/base/library.html)`(`[`kableExtra`](https://haozhu233.github.io/kableExtra/)`)`\
`  `[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyr`](https://tidyr.tidyverse.org)`)`\
`  `[`library`](https://rdrr.io/r/base/library.html)`(`[`magrittr`](https://magrittr.tidyverse.org)`)`\
`  `[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
`}``)`

## Introducing tidy methods to crmPack

The latest release of `crmPack` introduces `broom`-like tidy methods for
all `crmPack` classes. These methods convert the underlying S4 classes
to (lists of) `tibble`s. This should facilitate reporting of all aspects
of CRM trials as well as making it easier to integrate `crmPack` with
other packages such as `ggplot2`.

### Basic approach

The following is the general approach we take to tidying `crmPack`
classes:

- All slots that are not functions are converted to `tibble`s or a list
  of `tibble`s.
- If the slot’s value is a `list`, these rules are applied to each
  element of the list in turn.
- If the slot’s value is scalar, the slot is converted to a 1x1
  `tibble`. This will ease downstream operations such as `row_bind`ing.
- If the object being tidied contains multiple slots of (potentially)
  different lengths, the result is a list of `tibble`s. The list may be
  nested to multiple levels. (See, for example, `LogisticLogNormal`.)
- The column names of the tidied `tibble` correspond to the slot names
  of the parent object.
  - Exception: where the slot has name in the plural and contains a
    `vector` or `list`, the column name will be singular. See, for
    example, `CohortSizeParts` below.
- When the value of a slot has not been set, a zero-row `tibble` is
  returned.
- When the value of a slot has scalar attributes, these attributes are
  added as columns of the `tibble`, whose name is the name of the
  attribute and whose value is the value of the attribute for every row
  of the tibble. Vector attributes can be added, by default, as a nested
  tibble. The nested tibble is 1 row x n column, with column names
  defined by the name of the attribute and values given by the value of
  the corresponding attribute.
- `tbl_<className>` is prepended to the class of the (list of) tidy
  `tibble`(s).

### Exceptions

- Where a vector slot (or series of vector slots) define a range ()for
  example, the `intervals` slot in various `CohortSize` and `Increments`
  classes, then the naming convention described above is not followed.
  Instead, columns named `min` and `max` define the extent of the range.

### Examples

`CohortSizeConst` is a trivial example and illustrates the default
approach for all classes.

\
[`CohortSizeConst`](https://docs.crmpack.org/reference/CohortSizeConst-class.md)`(``size ``=`` ``3``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `[`tidy`](https://docs.crmpack.org/reference/tidy.md)`(``)`\
`#> # A tibble: 1 × 1`\
`#>    size`\
`#>   <int>`\
`#> 1     3`

`IncrementsRelative` illustrate how ranges are handled.

\
[`IncrementsRelative`](https://docs.crmpack.org/reference/IncrementsRelative-class.md)`(`\
`  intervals ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``20``)``,`\
`  increments ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``0.33``)`\
`)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)\
`  `[`tidy`](https://docs.crmpack.org/reference/tidy.md)`(``)`\
`#> # A tibble: 2 × 3`\
`#>     min   max increment`\
`#>   <dbl> <dbl>     <dbl>`\
`#> 1     0    20      1   `\
`#> 2    20   Inf      0.33`

`CohortSizeMax` contains a slot whose value is a list.

\
`cs_max`` ``<-`` `[`maxSize`](https://docs.crmpack.org/reference/maxSize.md)`(`\
`  `[`CohortSizeConst`](https://docs.crmpack.org/reference/CohortSizeConst-class.md)`(``3``)``,`\
`  `[`CohortSizeDLT`](https://docs.crmpack.org/reference/CohortSizeDLT-class.md)`(``intervals ``=`` ``0``:``1``, cohort_size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``)``)`\
`)`\
`cs_max`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `[`tidy`](https://docs.crmpack.org/reference/tidy.md)`(``)`\
`#> [[1]]`\
`#> # A tibble: 1 × 1`\
`#>    size`\
`#>   <int>`\
`#> 1     3`\
`#> `\
`#> [[2]]`\
`#> # A tibble: 2 × 3`\
`#>     min   max cohort_size`\
`#>   <dbl> <dbl>       <int>`\
`#> 1     0     1           1`\
`#> 2     1   Inf           3`\
`#> `\
`#> attr(,"class")`\
`#> [1] "tbl_CohortSizeMax" "tbl_CohortSizeMax" "list"`

The `Samples` class likely to the most useful when making presentations
not yet supported by `crmPack` directly.

\
`options`` ``<-`` `[`McmcOptions`](https://docs.crmpack.org/reference/McmcOptions-class.md)`(`\
`  burnin ``=`` ``100``,`\
`  step ``=`` ``1``,`\
`  samples ``=`` ``2000`\
`)`\
\
`emptydata`` ``<-`` `[`Data`](https://docs.crmpack.org/reference/Data-class.md)`(``doseGrid ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``, ``5``, ``10``, ``15``, ``20``, ``25``, ``40``, ``50``, ``80``, ``100``)``)`\
\
`model`` ``<-`` `[`LogisticLogNormal`](https://docs.crmpack.org/reference/LogisticLogNormal-class.md)`(`\
`  mean ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``0.85``, ``1``)``,`\
`  cov ``=`\
`    `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``-``0.5``, ``-``0.5``, ``1``)``,`\
`      nrow ``=`` ``2`\
`    ``)``,`\
`  ref_dose ``=`` ``56`\
`)`\
`samples`` ``<-`` `[`mcmc`](https://docs.crmpack.org/reference/mcmc.md)`(``emptydata``, ``model``, ``options``)`\
`tidySamples`` ``<-`` ``samples`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `[`tidy`](https://docs.crmpack.org/reference/tidy.md)`(``)`\
`tidySamples`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `[`head`](https://rdrr.io/r/utils/head.html)`(``)`\
`#> $data`\
`#> # A tibble: 2,000 × 10`\
`#>    Iteration Chain alpha0 alpha1 nChains nParameters nIterations nBurnin nThin`\
`#>        <int> <int>  <dbl>  <dbl>   <int>       <int>       <int>   <int> <int>`\
`#>  1         1     1 -1.02   7.58        1           1        2100     100     1`\
`#>  2         2     1 -1.26   3.00        1           1        2100     100     1`\
`#>  3         3     1 -1.97   5.29        1           1        2100     100     1`\
`#>  4         4     1  1.51   0.946       1           1        2100     100     1`\
`#>  5         5     1 -0.710  1.76        1           1        2100     100     1`\
`#>  6         6     1 -2.11   4.08        1           1        2100     100     1`\
`#>  7         7     1 -1.44   9.27        1           1        2100     100     1`\
`#>  8         8     1 -1.49  10.0         1           1        2100     100     1`\
`#>  9         9     1 -2.46  15.7         1           1        2100     100     1`\
`#> 10        10     1 -1.45   4.61        1           1        2100     100     1`\
`#> # ℹ 1,990 more rows`\
`#> # ℹ 1 more variable: parallel <lgl>`\
`#> `\
`#> $options`\
`#> # A tibble: 1 × 5`\
`#>   iterations burnin  step rng_kind rng_seed`\
`#>        <int>  <int> <int> <chr>       <int>`\
`#> 1       2100    100     1 <NA>           NA`

### Using tidy `crmPack` data

Tidy `crmPack` data can be easily reported using `knitr` or similar
packages in the obvious way.

#### Cohort size

The cohort size for this trial is determined by the dose to be used in
the current cohort according to the rules described in the table below:

\
[`CohortSizeRange`](https://docs.crmpack.org/reference/CohortSizeRange-class.md)`(`\
`  intervals ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``50``, ``300``)``,`\
`  cohort_size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``, ``5``)`\
`)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)\
`  `[`tidy`](https://docs.crmpack.org/reference/tidy.md)`(``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)\
`  `[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(`\
`    col.names ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Min"``, ``"Max"``, ``"Cohort size"``)``,`\
`    caption ``=`` ``"Rules for selecting the cohort size"`\
`  ``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)\
`  `[`add_header_above`](https://rdrr.io/pkg/kableExtra/man/add_header_above.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"Dose"`` ``=`` ``2``, ``" "`` ``=`` ``1``)``)`

[TABLE]

Rules for selecting the cohort size {.table}

Or presentations not directly supported by `crmPack` can be easily
produced. Here, we create plots of the dose-specific PDFs for prior
probabilities of toxicity after the first DLT is observed in a fictional
trial.

\
`options`` ``<-`` `[`McmcOptions`](https://docs.crmpack.org/reference/McmcOptions-class.md)`(`\
`  burnin ``=`` ``5000``,`\
`  step ``=`` ``1``,`\
`  samples ``=`` ``40000`\
`)`\
\
`data`` ``<-`` `[`Data`](https://docs.crmpack.org/reference/Data-class.md)`(`\
`  doseGrid ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``, ``5``, ``10``, ``15``, ``20``, ``25``, ``40``, ``50``, ``80``, ``100``)``,`\
`  x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``3``, ``5``, ``10``, ``15``, ``15``, ``15``)``,`\
`  y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``, ``0``, ``0``, ``0``, ``1``, ``0``)``,`\
`  ID ``=`` ``1L``:``7L``,`\
`  cohort ``=`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``:``4``, ``5``, ``5``, ``5``)``)`\
`)`\
\
`model`` ``<-`` `[`LogisticLogNormal`](https://docs.crmpack.org/reference/LogisticLogNormal-class.md)`(`\
`  mean ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``1``, ``0``)``,`\
`  cov ``=`\
`    `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``3``, ``-``0.1``, ``-``0.1``, ``4``)``,`\
`      nrow ``=`` ``2`\
`    ``)``,`\
`  ref_dose ``=`` ``56`\
`)`\
`samples`` ``<-`` `[`mcmc`](https://docs.crmpack.org/reference/mcmc.md)`(``data``, ``model``, ``options``)`\
`tidySamples`` ``<-`` ``samples`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)` `[`tidy`](https://docs.crmpack.org/reference/tidy.md)`(``)`\
\
`# The magrittr pipe is necessary here`\
`tidySamples``$``data`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)\
`  `[`expand`](https://tidyr.tidyverse.org/reference/expand.html)`(`\
`    `[`nesting`](https://tidyr.tidyverse.org/reference/expand.html)`(``!``!``!``.``[``1``:``10``]``)``,`\
`    Dose ``=`` ``data``@``doseGrid``[``2``:``11``]`\
`  ``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``Prob ``=`` `[`probFunction`](https://docs.crmpack.org/reference/probFunction.md)`(``model``, alpha0 ``=`` ``alpha0``, alpha1 ``=`` ``alpha1``)``(``Dose``)``)`` `[`%>%`](https://docs.crmpack.org/reference/pipe.md)\
`  `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``)`` ``+`\
`  `[`geom_density`](https://ggplot2.tidyverse.org/reference/geom_density.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``Prob``, colour ``=`` `[`as.factor`](https://rdrr.io/r/base/factor.html)`(``Dose``)``)``, adjust ``=`` ``1.5``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`\
`    title ``=`` ``"Posterior dose-specific PDFs for p(Tox)"``,`\
`    caption ``=`` ``"Dose 1 omitted as p(Tox) is essentially 0"``,`\
`    x ``=`` ``"p(Tox)"`\
`  ``)`` ``+`\
`  `[`scale_colour_discrete`](https://ggplot2.tidyverse.org/reference/scale_colour_discrete.html)`(``"Dose"``)`` ``+`\
`  `[`theme_light`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(`\
`    axis.ticks.y ``=`` `[`element_blank`](https://ggplot2.tidyverse.org/reference/element.html)`(``)``,`\
`    axis.text.y ``=`` `[`element_blank`](https://ggplot2.tidyverse.org/reference/element.html)`(``)``,`\
`    axis.title.y ``=`` `[`element_blank`](https://ggplot2.tidyverse.org/reference/element.html)`(``)`\
`  ``)`

![plot of chunk
unnamed-chunk-7](tidy_method-figures/unnamed-chunk-7-1.png)

plot of chunk unnamed-chunk-7

## Environment

\
[`sessionInfo`](https://rdrr.io/r/utils/sessionInfo.html)`(``)`\
`#> R version 4.6.1 (2026-06-24)`\
`#> Platform: aarch64-apple-darwin23`\
`#> Running under: macOS Golden Gate 27.0.1`\
`#> `\
`#> Matrix products: default`\
`#> BLAS:   /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRblas.0.dylib `\
`#> LAPACK: /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1`\
`#> `\
`#> locale:`\
`#> [1] C.UTF-8/C.UTF-8/C.UTF-8/C/C.UTF-8/C.UTF-8`\
`#> `\
`#> time zone: Europe/Zurich`\
`#> tzcode source: internal`\
`#> `\
`#> attached base packages:`\
`#> [1] stats     graphics  grDevices utils     datasets  methods   base     `\
`#> `\
`#> other attached packages:`\
`#>  [1] magrittr_2.0.5     kableExtra_1.4.0   tibble_3.3.1       ggmcmc_1.5.1.2    `\
`#>  [5] tidyr_1.3.2        dplyr_1.2.1        data.tree_1.2.0    knitr_1.52        `\
`#>  [9] decider_0.0.0.9012 crmPack_2.3.0      testthat_3.3.2     ggplot2_4.0.3     `\
`#> [13] webshot2_0.1.2    `\
`#> `\
`#> loaded via a namespace (and not attached):`\
`#>   [1] RColorBrewer_1.1-3   tensorA_0.36.2.1     sys_3.4.3           `\
`#>   [4] rstudioapi_0.19.0    jsonlite_2.0.0       farver_2.1.2        `\
`#>   [7] rmarkdown_2.32       fs_2.1.0             ragg_1.5.2          `\
`#>  [10] vctrs_0.7.3          rhub_2.0.1           memoise_2.0.1       `\
`#>  [13] askpass_1.2.1        webshot_0.5.5        htmltools_0.5.9     `\
`#>  [16] usethis_3.2.2        distributional_0.8.1 lambda.r_1.2.4      `\
`#>  [19] curl_8.0.0           rjags_4-17           GenSA_1.1.15        `\
`#>  [22] parallelly_1.48.0    StanHeaders_2.32.10  htmlwidgets_1.6.4   `\
`#>  [25] desc_1.4.3           futile.options_1.0.1 cachem_1.1.0        `\
`#>  [28] lifecycle_1.0.5      iterators_1.0.14     pkgconfig_2.0.3     `\
`#>  [31] Matrix_1.7-6         R6_2.6.1             fastmap_1.2.0       `\
`#>  [34] rbibutils_2.4.1      digest_0.6.39        GGally_2.4.0        `\
`#>  [37] rprojroot_2.1.1      pkgload_1.5.3        textshaping_1.0.5   `\
`#>  [40] labeling_0.4.3       abind_1.4-8          httr_1.4.9          `\
`#>  [43] compiler_4.6.1       rngtools_1.5.2       withr_3.0.3         `\
`#>  [46] doParallel_1.0.17    S7_0.2.2             backports_1.5.1     `\
`#>  [49] inline_0.3.21        ggstats_0.13.0       QuickJSR_1.11.0     `\
`#>  [52] pkgbuild_1.4.8       R.utils_2.13.0       MASS_7.3-66         `\
`#>  [55] openssl_2.4.2        rappdirs_0.3.4       sessioninfo_1.2.4   `\
`#>  [58] loo_2.10.1           tools_4.6.1          chromote_0.5.1      `\
`#>  [61] otel_0.2.0           zip_3.0.2            R.oo_1.27.1         `\
`#>  [64] glue_1.8.1           DiagrammeR_1.0.12    R.cache_0.17.0      `\
`#>  [67] promises_1.5.0       grid_4.6.1           checkmate_2.3.4     `\
`#>  [70] generics_0.1.4       gtable_0.3.6         R.methodsS3_1.8.2   `\
`#>  [73] flock_0.7            websocket_1.4.4      utf8_1.2.6          `\
`#>  [76] xml2_1.6.0           foreach_1.5.2        pillar_1.11.1       `\
`#>  [79] stringr_1.6.0        posterior_1.7.0      later_1.4.8         `\
`#>  [82] splines_4.6.1        lattice_0.23-1       survival_3.8-12     `\
`#>  [85] tidyselect_1.2.1     httpgd_2.1.4         gitcreds_0.1.2      `\
`#>  [88] gridExtra_2.3.1      V8_8.2.0             svglite_2.2.2       `\
`#>  [91] stats4_4.6.1         futile.logger_1.4.9  xfun_0.61           `\
`#>  [94] devtools_2.5.2       credentials_2.0.3    brio_1.1.5          `\
`#>  [97] matrixStats_1.5.0    rstan_2.32.7         visNetwork_2.1.4    `\
`#> [100] stringi_1.8.9        rematch_2.0.0        unigd_0.2.0         `\
`#> [103] yaml_2.3.12          evaluate_1.0.5       codetools_0.2-20    `\
`#> [106] sess_3.0.1           cli_3.6.6            RcppParallel_6.2.0  `\
`#> [109] systemfonts_1.3.2    Rdpack_2.6.6         processx_3.9.0      `\
`#> [112] styler_1.11.0        Rcpp_1.1.2           gert_2.3.1          `\
`#> [115] coda_0.19-4.1        parallel_4.6.1       ellipsis_0.3.3      `\
`#> [118] whoami_1.3.0         doRNG_1.8.6.3        viridisLite_0.4.3   `\
`#> [121] mvtnorm_1.4-2        scales_1.4.0         openxlsx_4.2.8.1    `\
`#> [124] purrr_1.2.2          rlang_1.3.0          formatR_1.14`
