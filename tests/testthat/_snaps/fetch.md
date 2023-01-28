# fetch: throws if onActivate fails

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

# fetch: throws if onValidate fails

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

# fetch: warns if validation fails once, creates new object and tries again

    Code
      obj <- get_private(pool)$checkValid(badObject)
    Output
      <pool> Failed to activate and/or validate existing object.
      <pool> Trying again with a new object.

---

    Code
      get_private(pool)$checkValid(obj)
    Output
      <pool> Failed to activate and/or validate existing object.
      <pool> Trying again with a new object.
    Condition
      Error:
      ! Freshly created object does not appear to be valid.
      Caused by error in `onValidate()`:
      ! Validation failed...

# fetch: throws if the pool was closed

    Code
      poolCheckout(pool)
    Condition
      Error in `poolCheckout()`:
      ! This pool is no longer valid. Cannot fetch new objects.

