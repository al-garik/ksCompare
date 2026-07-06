# Smart features

`ksCompare` adds several conveniences on top of plain
`PROC COMPARE`-style diffing.

## ULP tolerance

`ks_tol(ulp = N)` accepts pairs whose IEEE-754 binary64 representations
are within `N` units in the last place.

``` r

x <- 1.0
y <- x + .Machine$double.eps
ks_compare(
  data.frame(id = 1, x = x),
  data.frame(id = 1, x = y),
  by = "id",
  tolerance = ks_tol(ulp = 4)
)
#> ✔ data.frame(id = 1, x = x) vs data.frame(id = 1, x = y)
#> — identical
```

## Auto-key inference

``` r

df <- data.frame(study = "S1", id = 1:3, x = 1:3)
suppressMessages(ks_compare(df, df, by = "auto"))
```

`by = "auto"` searches for the smallest combination of shared columns
whose values are unique on both sides.

## Diff triage helpers

[`ks_cause_summary()`](https://al-garik.github.io/ksCompare/reference/ks_cause_summary.md)
groups value diffs by likely cause, while
[`ks_row_diff_summary()`](https://al-garik.github.io/ksCompare/reference/ks_row_diff_summary.md)
shows which matched observations changed the most.

``` r

a <- data.frame(id = 1:4, x = c("A", "b ", "c", "d"), y = c(1, 2, 3, 4))
b <- data.frame(id = 1:4, x = c("a", "b",  "c", "d"), y = c(1, 9, 3, 9))
cmp <- ks_compare(a, b, by = "id")
#> ✖ a vs b — 4 value diffs across 2 columns
ks_cause_summary(cmp)
#> # A tibble: 3 × 4
#>   cause                      n_cells n_columns columns
#>   <chr>                        <int>     <int> <chr>  
#> 1 plain value change               2         1 y      
#> 2 letter case differs              1         1 x      
#> 3 whitespace padding on base       1         1 x
ks_row_diff_summary(cmp)
#> # A tibble: 3 × 6
#>   key_id base_row comp_row key_label n_diffs columns
#>    <int>    <int>    <int> <chr>       <int> <chr>  
#> 1      2        2        2 id = 2          2 x, y   
#> 2      1        1        1 id = 1          1 x      
#> 3      4        4        4 id = 4          1 y
```

## First / last unequal observations

For SAS-style review tables,
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
also stores the first and last differing observations per matched
column.

``` r

ks_compare(a, b, by = "id", n_first_last = 1)$first_last_unequal
#> ✖ a vs b — 4 value diffs across 2 columns
#> # A tibble: 4 × 13
#>   column_base column_comp position  rank key_id base_row comp_row kind     base 
#>   <chr>       <chr>       <chr>    <int>  <int>    <int>    <int> <chr>    <chr>
#> 1 x           x           first        1      1        1        1 charact… "A"  
#> 2 x           x           last         1      2        2        2 charact… "b " 
#> 3 y           y           first        1      2        2        2 double   "2"  
#> 4 y           y           last         1      4        4        4 double   "4"  
#> # ℹ 4 more variables: comp <chr>, diff <dbl>, na_flow <chr>, note <chr>
```

## Duplicate-key strategies

``` r

a <- data.frame(id = c(1, 1, 2), x = c(10, 99, 20))
b <- data.frame(id = c(1, 2),    x = c(10, 20))

ks_compare(a, b, by = "id", dup_keys = "first")
#> ✔ a vs b — identical
ks_compare(a, b, by = "id", dup_keys = "last")
#> ⚠ a vs b — 1 value diff across 1 column
ks_compare(a, b, by = "id", dup_keys = "all_pairs")
#> Warning in ks_match_rows_dup(bkey, ckey, dup_keys, uniq_check): `dup_keys` = "all_pairs": cardinality mismatch on 1 key.
#> ℹ Cartesian pairing inflates row counts; consider "keep_all" for positional
#>   within-group pairing.
#> ⚠ a vs b — 1 value diff across 1 column
```

`dup_keys = "keep_all"` pairs duplicate rows positionally within each
key group (1\<-\>1, 2\<-\>2, …). Leftover rows on the longer side become
base- or compare-only, and `cmp$row_diff` carries a `pair_rank` /
`pair_total` column so you can tell which pair each diff belongs to. The
HTML report shows a *Pair* column when this strategy is active.

## SAS format normalisation

Format comparison is trailing-dot- and case-tolerant, so `haven` import
quirks do not surface as false schema diffs:

``` r

a <- data.frame(d = as.Date("2024-01-01"))
attr(a$d, "format.sas") <- "DATE9."
b <- a
attr(b$d, "format.sas") <- "date9"   # no trailing dot, lowercase

ks_compare(a, b)$schema_diff   # format_match == TRUE
```

## Encoding-safe string comparison

Strings imported from mixed Latin1 / UTF-8 sources are converted to
UTF-8 before equality testing, so the same character in two encodings
does not register as a diff. Combine with `str_norm = "NFC"` for fully
Unicode-normalised comparison.

``` r

ks_compare(
  base, comp, by = "id",
  options = ks_comp_options(str_norm = "NFC")
)
```

## Pattern detection

Pattern detection is opt-in because the detectors scan every column with
at least one diff. When many cells differ in the same way (constant
offset, sign flip, trim-only string differences, etc.), set
`find_patterns = TRUE` to populate `cmp$pattern_summary`:

``` r

a <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
b <- data.frame(id = 1:5, x = c(2, 3, 4, 5, 6))
ks_compare(a, b, by = "id", find_patterns = TRUE)$pattern_summary
#> ✖ a vs b — 5 value diffs across 1 column
#> # A tibble: 1 × 4
#>   column pattern         coverage detail
#>   <chr>  <chr>              <dbl> <chr> 
#> 1 x      constant_offset        1 -1
```

Common labels include `constant_offset`, `constant_scale`, `sign_flip`,
`integer_round`, `trim_only`, `case_only`, `epoch_swap`, and
`factor_recoded`, depending on the column type.
