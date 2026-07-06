# SAS SYSINFO-compatible bitmask for a comparison

SAS's `&SYSINFO` macro variable encodes the result of `PROC COMPARE` as
a bitmask. `ks_sysinfo()` returns a compatible-style integer summarizing
the same kinds of facts, plus a named integer vector showing which bits
are set. Bit positions follow SAS documentation:

## Usage

``` r
ks_sysinfo(x)
```

## Arguments

- x:

  A `ks_comparison` object.

## Value

An integer with class `ks_sysinfo`. Use
[`as.integer()`](https://rdrr.io/r/base/integer.html) for the raw value
or print to see the named bits.

## Details

|     |       |                                              |
|-----|-------|----------------------------------------------|
| Bit | Mask  | Meaning                                      |
| 1   | 1     | Data set labels differ                       |
| 2   | 2     | Data set types differ                        |
| 3   | 4     | Variable has different informat              |
| 4   | 8     | Variable has different format                |
| 5   | 16    | Variable has different length                |
| 6   | 32    | Variable has different label                 |
| 7   | 64    | Base data set has observation not in compare |
| 8   | 128   | Compare data set has observation not in base |
| 9   | 256   | Base data set has BY group not in compare    |
| 10  | 512   | Compare data set has BY group not in base    |
| 11  | 1024  | Base data set has variable not in compare    |
| 12  | 2048  | Compare data set has variable not in base    |
| 13  | 4096  | A value comparison was unequal               |
| 14  | 8192  | Conflicting variable types                   |
| 15  | 16384 | BY variables do not match                    |
| 16  | 32768 | Fatal error - comparison not done            |

Length and informat are not currently tracked; their bits are always
clear. `haven` does not surface SAS informats, and column "length" is
not a meaningful R-side concept for numerics.

## Examples

``` r
a <- data.frame(id = 1:3, x = c(1, 2, 3))
b <- data.frame(id = 1:3, x = c(1, 2, 4), z = 1:3)
cmp <- ks_compare(a, b, by = "id")
#> ⚠ a vs b — 1 value diff across 1 column
ks_sysinfo(cmp)
#> 
#> ── SYSINFO = 6144 ──
#> 
#> • compare has variable not in base (2048)
#> • value comparison unequal (4096)
as.integer(ks_sysinfo(cmp))
#> [1] 6144
#> attr(,"bits")
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
#>                                   0                                2048 
#>            value comparison unequal          conflicting variable types 
#>                                4096                                   0 
#>           BY variables do not match                         fatal error 
#>                                   0                                   0 
```
