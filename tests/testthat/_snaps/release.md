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

# warns if onPassivate fails

    Code
      poolReturn(obj)
    Condition
      Error in `poolReturn()`:
      ! Object could not be returned back to the pool.
      * It was destroyed instead
      Caused by error in `onPassivate()`:
      ! Passivation failed...

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

