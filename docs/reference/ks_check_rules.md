# Apply a list of validation rules to a data frame

Evaluates each rule formula against `data` and records a message for
every row that violates a rule. Two modes are supported:

- `"first"`:

  Each row gets the message of the *first* rule it fails; subsequent
  rules are not evaluated for that row.

- `"all"`:

  All rules are evaluated for every row; the row receives a list of all
  messages for rules it fails.

## Usage

``` r
ks_check_rules(data, rules, mode = c("first", "all"))
```

## Arguments

- data:

  A data frame.

- rules:

  A named list of rule objects, each with two fields:

  `expr`

  : A one-sided formula whose RHS is a logical expression (evaluated
    with
    [`rlang::eval_tidy()`](https://rlang.r-lib.org/reference/eval_tidy.html))
    that is `TRUE` for *failing* rows.

  `msg`

  : Character scalar. Message written to failing rows.

- mode:

  Character. Either `"first"` (default) or `"all"`.

## Value

`data` with an additional list-column `check_msgs`. In `"first"` mode
each element is a single string (`"OK"` for passing rows). In `"all"`
mode each element is a character vector of all violated messages (or
`"OK"`).

## Examples

``` r
if (FALSE) { # \dontrun{
rules <- list(
  list(expr = ~ AGE < 0, msg = "Negative age"),
  list(expr = ~ is.na(SEX), msg = "Missing sex")
)
ks_check_rules(adsl, rules, mode = "all")
} # }
```
