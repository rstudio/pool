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

# Lightweight equivalent of withr::defer()
defer <- function(expr, envir = parent.frame(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(on.exit, list(thunk, TRUE, after), envir = envir)
}

#' Create a demo SQLite database
#'
#' This function creates a temporary SQLite database for demonstration purposes.
#' It populates the database with two tables: 'mtcars' and 'faithful'.
#'
#' @export
#' @keywords internal
demoDb <- function() {
  check_installed("RSQLite")

  path <- file.path(tools::R_user_dir("pool", "cache"), "demo.sqlite3")
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  if (!file.exists(path)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con))

    DBI::dbWriteTable(con, "mtcars", datasets::mtcars, row.names = "model")
    DBI::dbWriteTable(con, "faithful", datasets::faithful)
  }

  path
}
