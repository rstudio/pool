# it requires a valid factory

    Code
      poolCreate(1)
    Condition
      Error in `poolCreate()`:
      ! `factory` must be a function.
    Code
      poolCreate(function(x) NULL)
    Condition
      Error in `poolCreate()`:
      ! Object creation failed.
      * The `factory` must not return `NULL`

# pool can't be closed twice

    Code
      poolCheckout(pool)
    Condition
      Error in `poolCheckout()`:
      ! This pool is no longer valid. Cannot fetch new objects.

# max size is enforced

    Code
      poolCheckout(pool)
    Condition
      Error in `poolCheckout()`:
      ! Maximum number of objects in pool has been reached

# can't return the same object twice

    Code
      poolReturn(obj)
    Condition
      Error in `poolReturn()`:
      ! This object was already returned to the pool.

# poolClose() warns about taken objects, but they can still be returned

    Code
      poolClose(pool)
    Output
      <pool> You still have checked out objects.
      <pool> Use `poolReturn()` them to the pool so they can be destroyed.

# warns if object can't be returned

    Code
      pool <- poolCreate(function() 1)
      obj <- poolCheckout(pool)
      rm(obj)
      . <- gc()
    Output
      <pool> Checked-out object deleted before being returned.
      <pool> Make sure to `poolReturn()` all objects retrieved with `poolCheckout().`
    Code
      poolClose(pool)
    Output
      <pool> You still have checked out objects.
      <pool> Use `poolReturn()` them to the pool so they can be destroyed.

# poolReturn() errors if object is not valid

    Code
      poolReturn("x")
    Condition
      Error in `poolReturn()`:
      ! `object` is not an pooled object.

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

# empty pool has useful print method

    Code
      pool
    Output
      <Pool> of unknown objects
        Objects checked out: 0
        Available in pool: 0
        Max size: Inf
        Valid: TRUE

# useful warning if onDestroy fails

    Code
      poolReturn(b)
      later::run_now()
    Output
      <pool> Object could not be destroyed, but was removed from the pool.
      <pool> Error message:
      <pool>   Destruction failed...

# throws if onPassivate fails

    Code
      poolReturn(obj)
    Condition
      Error in `poolReturn()`:
      ! Object could not be returned back to the pool.
      * It was destroyed instead
      Caused by error in `onPassivate()`:
      ! Passivation failed...

# throws if onActivate fails

    Code
      poolCheckout(pool)
    Output
      <pool> Failed to activate and/or validate existing object.
      <pool> Trying again with a new object.
    Condition
      Error in `poolCheckout()`:
      ! Freshly created object does not appear to be valid.
      Caused by error in `onActivate()`:
      ! Activation failed...

# throws if onValidate fails

    Code
      poolCheckout(pool)
    Output
      <pool> Failed to activate and/or validate existing object.
      <pool> Trying again with a new object.
    Condition
      Error in `poolCheckout()`:
      ! Freshly created object does not appear to be valid.
      Caused by error in `onValidate()`:
      ! Validation failed...

