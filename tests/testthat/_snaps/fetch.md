# warns if validation fails once, creates new object and tries again

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

