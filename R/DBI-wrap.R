#' Custom DBI methods for Pool
#'
#' @description
#' Wrappers around all DBI methods that check out a connection
#' (with [poolCheckout()]), perform the operation, and the return the connection
#' to the pool (with [poolReturn()]).
#'
#' @name DBI-wrap
#' @keywords internal
NULL

setClass("Pool")

# Wrapped methods ---------------------------------------------------------

DBI_wrap <- function(fun_name) {
  fun <- utils::getFromNamespace(fun_name, "DBI")

  args <- formals(fun)
  con_arg <- sym(names(args)[[1]])

  call_args <- syms(set_names(names(args)))
  call_args[[1]] <- quote(db_con)
  ns_fun <- call2("::", quote(DBI), sym(fun_name))
  recall <- call2(ns_fun, !!!call_args)

  con <- NULL # quiet R CMD check note
  body <- expr({
    db_con <- poolCheckout(!!con_arg)
    on.exit(poolReturn(db_con))

    !!recall
  })

  new_function(args, body, env = ns_env("pool"))
}

#' @export
#' @rdname DBI-wrap
setMethod("dbGetQuery", "Pool", DBI_wrap("dbGetQuery"))

#' @export
#' @rdname DBI-wrap
setMethod("dbExecute", "Pool", DBI_wrap("dbExecute"))

#' @export
#' @rdname DBI-wrap
setMethod("dbListResults", "Pool", function(conn, ...) {
  list()
})

#' @export
#' @rdname DBI-wrap
setMethod("dbListFields", "Pool", DBI_wrap("dbListFields"))

#' @export
#' @rdname DBI-wrap
setMethod("dbListTables", "Pool", DBI_wrap("dbListTables"))

#' @export
#' @rdname DBI-wrap
setMethod("dbListObjects", "Pool", DBI_wrap("dbListObjects"))

#' @export
#' @rdname DBI-wrap
setMethod("dbReadTable", "Pool", DBI_wrap("dbReadTable"))

#' @export
#' @rdname DBI-wrap
setMethod("dbWriteTable", "Pool", DBI_wrap("dbWriteTable"))

#' @export
#' @rdname DBI-wrap
setMethod("dbCreateTable", "Pool", DBI_wrap("dbCreateTable"))

#' @export
#' @rdname DBI-wrap
setMethod("dbAppendTable", "Pool", DBI_wrap("dbAppendTable"))

#' @export
#' @rdname DBI-wrap
setMethod("dbExistsTable", "Pool", DBI_wrap("dbExistsTable"))

#' @export
#' @rdname DBI-wrap
setMethod("dbRemoveTable", "Pool", DBI_wrap("dbRemoveTable"))

#' @export
#' @rdname DBI-wrap
setMethod("dbIsReadOnly", "Pool", DBI_wrap("dbIsReadOnly"))

#' @export
#' @rdname DBI-wrap
setMethod("make.db.names", "Pool", DBI_wrap("make.db.names"))

#' @export
#' @rdname DBI-wrap
setMethod("isSQLKeyword", "Pool", DBI_wrap("isSQLKeyword"))

#' @export
#' @rdname DBI-wrap
setMethod("SQLKeywords", "Pool", DBI_wrap("SQLKeywords"))

#' @export
#' @rdname DBI-wrap
setMethod("sqlData", "Pool", DBI_wrap("sqlData"))

#' @export
#' @rdname DBI-wrap
setMethod("sqlCreateTable", "Pool", DBI_wrap("sqlCreateTable"))

#' @export
#' @rdname DBI-wrap
setMethod("sqlAppendTable", "Pool", DBI_wrap("sqlAppendTable"))

#' @export
#' @rdname DBI-wrap
setMethod("sqlInterpolate", "Pool", DBI_wrap("sqlInterpolate"))

#' @export
#' @rdname DBI-wrap
setMethod("sqlParseVariables", "Pool", DBI_wrap("sqlParseVariables"))

#' @export
#' @rdname DBI-wrap
setMethod("dbQuoteIdentifier", "Pool", DBI_wrap("dbQuoteIdentifier"))

#' @export
#' @rdname DBI-wrap
setMethod("dbUnquoteIdentifier", "Pool", DBI_wrap("dbUnquoteIdentifier"))

#' @export
#' @rdname DBI-wrap
setMethod("dbQuoteLiteral", "Pool", DBI_wrap("dbQuoteLiteral"))

#' @export
#' @rdname DBI-wrap
setMethod("dbQuoteString", "Pool", DBI_wrap("dbQuoteString"))
