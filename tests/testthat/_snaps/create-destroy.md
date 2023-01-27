# createObject throws if `factory` throws or returns NULL

    Code
      poolCreate(MockPooledObj)
    Error <simpleError>
      attempt to apply non-function
    Code
      poolCreate(function(x) NULL)
    Error <simpleError>
      Object creation was not successful. The `factory` argument must be a function that creates and returns the object to be pooled.

# destroyObject: throws if onDestroy fails

    Code
      poolReturn(b)
      later::run_now()
    Output
      <pool> Object could not be destroyed, but was removed from the pool.
      <pool> Error message:
      <pool>   Destruction failed...

