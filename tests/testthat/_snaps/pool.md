# object operations: enforces maxSize

    Code
      poolCheckout(pool)
    Condition
      Error in `poolCheckout()`:
      ! Maximum number of objects in pool has been reached

# pool has useful print method

    Code
      pool
    Output
      <Pool> of numeric objects
        Objects checked out: 0
        Available in pool: 1
        Max size: Inf
        Valid: TRUE
    Code
      x1 <- poolCheckout(pool)
      x2 <- poolCheckout(pool)
      pool
    Output
      <Pool> of numeric objects
        Objects checked out: 2
        Available in pool: 0
        Max size: Inf
        Valid: TRUE
    Code
      poolReturn(x1)
      pool
    Output
      <Pool> of numeric objects
        Objects checked out: 1
        Available in pool: 1
        Max size: Inf
        Valid: TRUE
    Code
      poolReturn(x2)

