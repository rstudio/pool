# release: returns the object back to the pool, and it can be recycled

    Code
      poolCheckout(pool)
    Condition
      Error in `poolCheckout()`:
      ! Maximum number of objects in pool has been reached

# release: throws if object was already released

    Code
      poolReturn(obj)
    Condition
      Error in `poolReturn()`:
      ! This object was already returned to the pool.

# release: throws if object is not valid

    Code
      poolReturn(obj)
    Condition
      Error in `poolReturn()`:
      ! Invalid object.

# release: warns if onPassivate fails

    Code
      poolReturn(obj)
    Condition
      Error in `poolReturn()`:
      ! Object could not be returned back to the pool.
      * It was destroyed instead
      Caused by error in `onPassivate()`:
      ! Passivation failed...

# release: is allowed after the pool is closed

    Code
      poolClose(pool)
    Output
      <pool> You still have checked out objects.
      <pool> Use `poolReturn()` them to the pool so they can be destroyed.

---

    Code
      poolClose(pool)
    Condition
      Error in `pool$close()`:
      ! The pool was already closed.

# release: warns if object can't be returned

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

