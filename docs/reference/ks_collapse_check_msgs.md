# Collapse list-column of check messages to a character column

Converts the list-column produced by
[`ks_check_rules`](https://al-garik.github.io/ksCompare/reference/ks_check_rules.md)
into a plain character column by pasting multiple messages together with
a separator.

## Usage

``` r
ks_collapse_check_msgs(data, sep = "; ", col)
```

## Arguments

- data:

  A data frame containing a list-column of check messages.

- sep:

  Character scalar. Separator used when collapsing multiple messages.
  Default `"; "`.

- col:

  `<tidy-select>` Bare name of the list-column to collapse. Must be
  provided explicitly.

## Value

`data` with `col` replaced by a character column.

## Examples

``` r
if (FALSE) { # \dontrun{
df_checked <- ks_check_rules(data, rules, mode = "all")
ks_collapse_check_msgs(df_checked, col = check_msgs)
} # }
```
