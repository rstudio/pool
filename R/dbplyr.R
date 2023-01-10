#' @include DBI.R
NULL

#' Use pool with dbplyr
#'
#' Wrappers for key dplyr (and dbplyr) methods so that pool works seemlessly
#' with [dbplyr](https://dbplyr.tidyverse.org/).
#'
#' @inheritParams dplyr::tbl
#' @param src,dest A [dbPool].
#' @param from Name table or [dbplyr::sql()] string.
#' @param vars A character vector of variable names in `src`.
#'   For expert use only.
#' @examples
#' library(dplyr)
#'
#' pool <- dbPool(RSQLite::SQLite())
#' # copy a table into the database
#' copy_to(pool, mtcars, "mtcars", temporary = FALSE)
#'
#' # retrieve a table
#' mtcars_db <- tbl(pool, "mtcars")
#' mtcars_db
#' mtcars_db %>% select(mpg, cyl, disp)
#' mtcars_db %>% filter(cyl == 6) %>% collect()
#'
#' poolClose(pool)
tbl.Pool <- function(src, from, ..., vars = NULL) {
  con <- poolCheckout(src)
  on.exit(poolReturn(con))

  from <- dbplyr::as.sql(from, con)
  if (is.null(vars)) {
    vars <- dplyr::db_query_fields(con, from)
  }

  dbplyr::tbl_sql("Pool", dbplyr::src_dbi(src), from, ..., vars = vars)
}

#' @rdname tbl.Pool
#' @inheritParams dbplyr::copy_to.src_sql
copy_to.Pool <- function(dest,
                         df,
                         name = deparse(substitute(df)),
                         overwrite = FALSE,
                         temporary = TRUE,
                         ...) {
  stop_if_temporary(temporary)

  db_con <- poolCheckout(dest)
  on.exit(poolReturn(db_con))

  dplyr::copy_to(
    dest = db_con,
    df = df,
    name = name,
    overwrite = overwrite,
    temporary = temporary,
    ...
  )
}

# Lazily registered wrapped functions ------------------------------------------

dbplyr_register_methods <- function() {
  register_s3_method("dplyr", "tbl", "Pool")
  register_s3_method("dplyr", "copy_to", "Pool")
  register_s3_method("dbplyr", "dbplyr_edition", "Pool", function(con) 2L)

  # Wrappers inspect formals so can only be executed if dbplyr is available
  on_package_load("dbplyr", {
    register_dbplyr_s3_method <- function(fun_name) {
      register_s3_method("dbplyr", fun_name, "Pool", dbplyr_wrap(fun_name))
    }
    register_dbplyr_s3_method("db_collect")
    register_dbplyr_s3_method("db_compute")
    register_dbplyr_s3_method("db_connection_describe")
    register_dbplyr_s3_method("db_sql_render")
    register_dbplyr_s3_method("sql_translation")
  })
}

dbplyr_wrap <- function(fun_name) {
  fun <- utils::getFromNamespace(fun_name, "dbplyr")

  args <- formals(fun)
  if (!has_name(args, "con")) {
    abort("`fun` must have `con` argument")
  }

  if ("temporary" %in% names(args)) {
    temporary <- quote(stop_if_temporary(temporary))
  } else {
    temporary <- NULL
  }

  call_args <- syms(set_names(names(args)))
  call_args[[1]] <- quote(db_con)
  recall <- call2(call2("::", quote(dbplyr), sym(fun_name)), !!!call_args)

  con <- NULL # quiet R CMD check note
  body <- expr({
    !!temporary

    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))

    !!recall
  })

  new_function(args, body, env = ns_env("pool"))
}

stop_if_temporary <- function(temporary) {
  if (!temporary) {
    return()
  }

  abort(
    paste0(
      "You cannot use `temporary = TRUE` ",
      "when using a Pool object, since temporary tables are local to a ",
      "connection, and there's no guarantee you'll get the same ",
      "connection back next time. You must either create a permanent ",
      "table, or checkout a connection from `pool` directly with ",
      "`con <- poolCheckout(pool)`, and then release the connection ",
      "back to the pool when you're finished (`poolReturn(con)`)."
    ),
    call = NULL
  )
}

# Method registration helpers ---------------------------------------------

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
