# ks_tol validates inputs

    Code
      ks_tol(abs = -1)
    Condition
      Error in `ks_tol()`:
      ! `abs` must be a single non-negative number.

---

    Code
      ks_tol(rel = "x")
    Condition
      Error in `ks_tol()`:
      ! `rel` must be a single non-negative number.

---

    Code
      ks_tol(per_column = list(a = 1))
    Condition
      Error in `ks_tol()`:
      ! Each element of `per_column` must be created with `ks_tol()`.

# ks_comp_options validates choices

    Code
      ks_comp_options(str_case = "upper")
    Condition
      Error in `ks_comp_options()`:
      ! `str_case` must be one of "sensitive" or "fold", not "upper".

---

    Code
      ks_comp_options(na_equal = NA)
    Condition
      Error in `ks_comp_options()`:
      ! `na_equal` must be a single `TRUE` or `FALSE`.

