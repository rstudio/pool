.onLoad <- function(...) {
  register_s3_method("dplyr", "copy_to", "Pool")
  register_s3_method("dplyr", "db_insert_into", "Pool")
  register_s3_method("dplyr", "db_write_table", "Pool")

  register_s3_method("dplyr", "tbl", "Pool")
  register_s3_method("dplyr", "copy_to", "Pool")

  register_s3_method("dbplyr", "db_collect", "Pool")
  register_s3_method("dbplyr", "db_compute", "Pool")
  register_s3_method("dbplyr", "db_connection_describe", "Pool")
  register_s3_method("dbplyr", "db_sql_render", "Pool")
  register_s3_method("dbplyr", "dbplyr_edition", "Pool")
  register_s3_method("dbplyr", "sql_escape_logical", "Pool")
  register_s3_method("dbplyr", "sql_join_suffix", "Pool")
  register_s3_method("dbplyr", "sql_table_analyze", "Pool")
  register_s3_method("dbplyr", "sql_table_index", "Pool")
  register_s3_method("dbplyr", "sql_translation", "Pool")
  register_s3_method("dbplyr", "sql_query_explain", "Pool")
  register_s3_method("dbplyr", "sql_query_fields", "Pool")
  register_s3_method("dbplyr", "sql_query_join", "Pool")
  register_s3_method("dbplyr", "sql_query_save", "Pool")
  register_s3_method("dbplyr", "sql_query_select", "Pool")
  register_s3_method("dbplyr", "sql_query_semi_join", "Pool")
  register_s3_method("dbplyr", "sql_query_wrap", "Pool")
}

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
