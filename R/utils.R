pool_metadata <- function(x,
                          check_valid = TRUE,
                          error_call = caller_env(),
                          error_arg = caller_arg(x)) {
  meta <- attr(x, "pool_metadata", exact = TRUE)

  if (is.null(meta)) {
    abort(
      paste0("`", error_arg, "` is not an pooled object."),
      call = error_call
    )
  }
  if (check_valid && !meta$valid) {
    abort(
      paste0("`", error_arg, "` is no longer valid."),
      call = error_call
    )
  }

  meta
}

