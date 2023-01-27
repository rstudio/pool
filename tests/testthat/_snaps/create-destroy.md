# createObject throws if `factory` throws or returns NULL

    Code
      poolCreate(MockPooledObj)
    Condition
      Error in `private$createObject()`:
      ! attempt to apply non-function
    Code
      poolCreate(function(x) NULL)
    Condition
      Error in `private$createObject()`:
      ! Object creation failed.
      * The `factory` must not return `NULL`

# destroyObject: throws if onDestroy fails

    Code
      poolReturn(b)
      later::run_now()
    Output
      <pool> Object could not be destroyed, but was removed from the pool.
      <pool> Error message:
      <pool>   Destruction failed...

