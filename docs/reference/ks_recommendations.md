# Actionable recommendations for a comparison

Synthesises
[`ks_match_health()`](https://al-garik.github.io/ksCompare/reference/ks_match_health.md)
flags,
[`ks_suggest_key()`](https://al-garik.github.io/ksCompare/reference/ks_suggest_key.md)
hints, and a few other heuristics into a short list of user-facing "what
to look at first" recommendations.

## Usage

``` r
ks_recommendations(x)
```

## Arguments

- x:

  A `ks_comparison`.

## Value

A tibble with one row per recommendation:

- `severity` (`"critical"`, `"warn"`, `"info"`, `"ok"`)

- `title` short label

- `message` longer human-readable text

- `action` suggested next step (may be `NA`)

## See also

[`ks_match_health()`](https://al-garik.github.io/ksCompare/reference/ks_match_health.md),
[`ks_suggest_key()`](https://al-garik.github.io/ksCompare/reference/ks_suggest_key.md).
