.onLoad <- function(...) {
  register_s3_method("dplyr", "copy_to", "Pool")
  register_s3_method("dplyr", "db_analyze", "Pool")
  register_s3_method("dplyr", "db_begin", "Pool")
  register_s3_method("dplyr", "db_commit", "Pool")
  register_s3_method("dplyr", "db_create_index", "Pool")
  register_s3_method("dplyr", "db_create_indexes", "Pool")
  register_s3_method("dplyr", "db_create_table", "Pool")
  register_s3_method("dplyr", "db_data_type", "Pool")
  register_s3_method("dplyr", "db_desc", "Pool")
  register_s3_method("dplyr", "db_drop_table", "Pool")
  register_s3_method("dplyr", "db_explain", "Pool")
  register_s3_method("dplyr", "db_has_table", "Pool")
  register_s3_method("dplyr", "db_insert_into", "Pool")
  register_s3_method("dplyr", "db_list_tables", "Pool")
  register_s3_method("dplyr", "db_query_fields", "Pool")
  register_s3_method("dplyr", "db_query_rows", "Pool")
  register_s3_method("dplyr", "db_rollback", "Pool")
  register_s3_method("dplyr", "db_save_query", "Pool")
  register_s3_method("dplyr", "db_write_table", "Pool")
  register_s3_method("dplyr", "sql_escape_ident", "Pool")
  register_s3_method("dplyr", "sql_escape_string", "Pool")
  register_s3_method("dplyr", "sql_join", "Pool")
  register_s3_method("dplyr", "sql_select", "Pool")
  register_s3_method("dplyr", "sql_semi_join", "Pool")
  register_s3_method("dplyr", "sql_subquery", "Pool")
  register_s3_method("dplyr", "sql_translate_env", "Pool")

  register_s3_method("dplyr", "tbl", "Pool")
  register_s3_method("dplyr", "copy_to", "Pool")

  register_s3_method("dbplyr", "db_collect", "Pool")
  register_s3_method("dbplyr", "db_compute", "Pool")
  register_s3_method("dbplyr", "db_copy_to", "Pool")
  register_s3_method("dbplyr", "db_sql_render", "Pool")
  register_s3_method("dbplyr", "sql_escape_logical", "Pool")
  
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
