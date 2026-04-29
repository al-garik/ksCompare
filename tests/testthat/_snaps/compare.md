# duplicate keys raise a classed error when dup_keys = 'error'

    Code
      ks_compare(a, b, by = "id", dup_keys = "error")
    Condition
      Error in `ks_match_rows_dup()`:
      ! Duplicate key values detected in base.
      i Pass `dup_keys` (e.g. "first", "last", "keep_all", "all_pairs") to choose a strategy.

