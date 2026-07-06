# From PROC COMPARE

This article maps SAS `PROC COMPARE` syntax onto `ksCompare`. The goal
is spirit-equivalence, not byte-identical output: `ksCompare` returns
tibbles you can render however you like.

## Cheat sheet

| SAS | ksCompare |
|----|----|
| `BASE = ds1` | `base = ds1` |
| `COMPARE = ds2` | `comp = ds2` |
| `ID v1 v2` | `by = c("v1", "v2")` |
| `BY v1 v2` | `by = c("v1", "v2")` (also covers BY-group reporting) |
| `VAR x y` | `mapping = c(x = "x", y = "y")` |
| `WITH x2` | `mapping = c(x = "x2")` |
| `CRITERION = 1e-5` | `tolerance = ks_tol(abs = 1e-5)` |
| `METHOD = RELATIVE` / `PERCENT` | `tolerance = ks_tol(rel = 1e-5)` |
| `METHOD = EXACT` | `tolerance = ks_tol()` (default, strict) |
| (floating-point round-trip noise) | `tolerance = ks_tol(ulp = 4)` |
| `NOMISS` | `options = ks_comp_options(na_equal = FALSE)` |
| `OUT=` | [`as_outbase()`](https://al-garik.github.io/ksCompare/reference/sas-out.md) / [`as_outcomp()`](https://al-garik.github.io/ksCompare/reference/sas-out.md) |
| `OUTDIF` | [`as_outdif()`](https://al-garik.github.io/ksCompare/reference/sas-out.md) |
| `OUTNOEQUAL` | [`as_outnoequal()`](https://al-garik.github.io/ksCompare/reference/sas-out.md) |
| `&SYSINFO` bitmask | [`ks_sysinfo()`](https://al-garik.github.io/ksCompare/reference/ks_sysinfo.md) |
| `NOPRINT` | use the returned object directly; no auto-print |
| `LISTALL` / `LISTBASE` / `LISTCOMP` | rows are tagged in `cmp$row_diff$status` |

## Row matching strategies

| Situation | ksCompare argument |
|----|----|
| No keys, match by row position | `by = NULL` (default) |
| Same key column on both sides | `by = "USUBJID"` |
| Renamed key column | `by = c(USUBJID = "SUBJID")` |
| Discover the key automatically | `by = "auto"` |
| Duplicate keys: keep first occurrence | `dup_keys = "first"` (default) |
| Duplicate keys: keep last occurrence | `dup_keys = "last"` |
| Duplicate keys: pair within group (1\<-\>1) | `dup_keys = "keep_all"` |
| Duplicate keys: cartesian product | `dup_keys = "all_pairs"` |
| Duplicate keys: hard fail | `dup_keys = "error"` |

## OUT\* outputs

``` r

a <- data.frame(id = 1:3, x = c(1, 2, 3))
b <- data.frame(id = 1:3, x = c(1, 2, 4))
cmp <- ks_compare(a, b, by = "id")
#> ⚠ a vs b — 1 value diff across 1 column

as_outbase(cmp)
#> # A tibble: 1 × 3
#>      id key_id x    
#>   <int>  <int> <chr>
#> 1     3      3 3
as_outcomp(cmp)
#> # A tibble: 1 × 3
#>      id key_id x    
#>   <int>  <int> <chr>
#> 1     3      3 4
as_outdif(cmp)
#> # A tibble: 1 × 3
#>      id key_id     x
#>   <int>  <int> <dbl>
#> 1     3      3    -1
as_outnoequal(cmp)
#> # A tibble: 1 × 12
#>      id key_id base_row comp_row column_base column_comp kind  base  comp   diff
#>   <int>  <int>    <int>    <int> <chr>       <chr>       <chr> <chr> <chr> <dbl>
#> 1     3      3        3        3 x           x           doub… 3     4        -1
#> # ℹ 2 more variables: na_flow <chr>, note <chr>
```

## SYSINFO bitmask

``` r

si <- ks_sysinfo(cmp)
si
#> 
#> ── SYSINFO = 4096 ──
#> 
#> • value comparison unequal (4096)
attr(si, "bits")
#>              data set labels differ               data set types differ 
#>                                   0                                   0 
#>           variable informat differs             variable format differs 
#>                                   0                                   0 
#>             variable length differs              variable label differs 
#>                                   0                                   0 
#> base has observation not in compare compare has observation not in base 
#>                                   0                                   0 
#>    base has BY group not in compare    compare has BY group not in base 
#>                                   0                                   0 
#>    base has variable not in compare    compare has variable not in base 
#>                                   0                                   0 
#>            value comparison unequal          conflicting variable types 
#>                                4096                                   0 
#>           BY variables do not match                         fatal error 
#>                                   0                                   0
```

The bit names follow the SAS documentation; only flags with a clear
correspondence in tibble-land are populated. Extra ksCompare findings
(verdict, recommendations, patterns) live on the `ks_comparison` object
directly, and helper summaries are available via
[`ks_cause_summary()`](https://al-garik.github.io/ksCompare/reference/ks_cause_summary.md)
/
[`ks_row_diff_summary()`](https://al-garik.github.io/ksCompare/reference/ks_row_diff_summary.md).

## Things ksCompare does not try to replicate

- The fixed-width LISTING-style `PROC COMPARE` printout. Use
  `print(cmp)`, `summary(cmp)`, or `ks_report_html(cmp)`.
- `OUT=` data sets in PDV order. Output tibbles use a clean long/wide
  layout that is easier to filter and join.
