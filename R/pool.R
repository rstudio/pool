#' @include utils.R
NULL

#' Connection Pooling for Databases in R
#'
#' Creates connection pools for various ypes of databases in R to
#' make it less computationally expensive to get a connection.
#'
#' @name pool
#' @docType package
#' @import DBI R6
NULL

#' @import methods
NULL

#' Pool Class
#'
#' Some more details...
Pool <- R6Class("Pool",
  public = list(
    ## initialize the pool with min number of connection
    initialize = function(connectionFactory, connectionClass,
                          minConn, maxConn) {
      private$factory <- connectionFactory
      private$connectionClass <- connectionClass
      private$minConn <- minConn
      private$maxConn <- maxConn
      private$freeConnections <- new.env()
      private$takenConnections <- new.env()
      for (i in seq_len(private$minConn)) {
        private$createConnection()
      }
      reg.finalizer(self,
        function(self) {
          freeEnv <- private$freeConnections
          takenEnv <- private$takenConnections
          if (length(freeEnv) > 0 || length(takenEnv) > 0) {
            warning("Closing leaked connections")
            self$close()
          }
        },
        onexit = TRUE)
    },
    ## calls activate and returns a connection
    fetch = function() {
      freeEnv <- private$freeConnections
      takenEnv <- private$takenConnections
      ## see if there's any free connections
      if (length(freeEnv > 0)) {
        ## get first free connection we find
        id <- ls(freeEnv)[[1]]
        conn <- freeEnv[[id]]
      } else {
        ## if we get here, there are no free connections
        ## and we must create a new one
        connection <- private$createConnection()
        id <- connection$id
        conn <- connection$conn
      }
      ## activate connection and return it
      private$toggleConnectionStatus(id, freeEnv, takenEnv)
      onActivate(conn)
      attr(conn, "id") <- id
      attr(conn, "pool") <- self
      class(conn) <- c(private$connectionClass, class(conn))
      return(conn)
    },
    ## passivates the connection and returns it back to
    ## the pool (possibly destroys the connection if the
    ## number of idle connections exceeds the maximum)
    release = function(id) {
      takenEnv <- private$takenConnections
      freeEnv <- private$freeConnections
      connection <- takenEnv[[id]]
      passivate(connection)
      private$toggleConnectionStatus(id, takenEnv, freeEnv)
      if (length(freeEnv) > private$maxConn) {
        destroy(connection, envir = freeEnv)
        rm(id, envir = freeEnv)
      }
    },
    ## cleaning up and closing the pool
    close = function() {
      freeEnv <- private$freeConnections
      takenEnv <- private$takenConnections
      ## disconnect all connections
      eapply(freeEnv, destroy)
      eapply(takenEnv, destroy)
      ## empty the connections' environments
      rm(list = ls(freeEnv), envir = freeEnv)
      rm(list = ls(takenEnv), envir = takenEnv)
    }
  ),
  private = list(
    freeConnections = NULL,
    takenConnections = NULL,
    factory = NULL,
    connectionClass = NULL,
    minConn = NULL,
    maxConn = NULL,
    ## creates a connection and assigns it to the
    ## free environment; returns the connection
    ## object and the accompanying id
    createConnection = function() {
      ## always create a connection in the free envir
      ## to guarantee that ids are unique
      envir <- private$freeConnections
      id <- as.character(length(envir) + 1)
      conn <- private$factory()
      assign(id, conn, envir = envir)
      return(list(id, conn))
    },
    ## change the connection's environment when a
    ## free connection gets taken and vice versa
    toggleConnectionStatus = function(id, from, to) {
      conn <- from[[id]]
      rm(id, envir = from)
      assign(id, conn, envir = to)
    }
  )
)

# PooledConnection <- R6Class("PooledConnection",
#   public = list(
#     conn = NULL,
#     id = NULL,
#     free = NULL,
#     initialize = function(conn, id) {
#       self$conn <- conn
#       self$id <- id
#       self$free <- TRUE
#     },
#     activate = function() {
#       self$free <- FALSE
#     },
#     passivate = function() {
#       self$free <- TRUE
#     },
#     destroy = function() {
#       dbDisconnect(self$conn)
#     }
#   )
# )

# DBIConnectionFactory <- function() {
#   function() {
#
#   }
# }


DBIConnectionFactory <- R6Class("DBIConnectionFactory",
  public = list(
    drv = NULL,
    initialize = function(drv) {
      self$drv <- drv
    },
    generator = function(...) {
      function() {
        DBI::dbConnect(self$drv, ...)
      }
    }
  )
)

## set S4 classes for consistency with DBI conventions
## (I'm pretty sure this naive approach won't work, however)
setClass("Pool")
setClass("DBIConnectionFactory")

PooledDBIConnection <- setClass("PooledDBIConnection")

setGeneric("release", function(conn) {
  standardGeneric("release")
})

setGeneric("onActivate", function(conn) {
  standardGeneric("onActivate")
})

setGeneric("onPassivate", function(conn) {
  standardGeneric("onPassivate")
})

setGeneric("onDestroy", function(conn) {
  standardGeneric("onDestroy")
})

setGeneric("onValidate", function(conn) {
  standardGeneric("onValidate")
})

## Set method defaults
setMethod("release", "ANY", function(conn) {
  id <- attr(conn, "id", exact = TRUE)
  pool <- attr(conn, "pool", exact = TRUE)
  pool$release(id)
})

setMethod("onActivate", "ANY", function(conn) {})
setMethod("onPassivate", "ANY", function(conn) {})


## Empty method because there's no need to deviate from the
## defaults (or even set defaults)
setMethod("release", "PooledDBIConnection", function(conn) {})
setMethod("onActivate", "PooledDBIConnection", function(conn) {})

setMethod("onPassivate", "PooledDBIConnection", function(conn) {
  rs <- dbListResults(conn)
  lapply(rs, dbRollback)
  lapply(rs, dbClearResult)
})

setMethod("onDestroy", "PooledDBIConnection", function(conn) {

})

setMethod("onValidate", "PooledDBIConnection", function(conn) {

})


## set S4 methods for consistency with DBI conventions

#**************************************************************#
#**************************************************************#
#********************** OPENING THE POOL **********************#
#**************************************************************#

setGeneric("createPool", function(drv, connectionClass, ...) {
  standardGeneric("createPool")
})

setMethod("createPool", "DBIDriver", function(drv, connectionClass, ...) {
  createPool(DBIConnectionFactory$new(drv), connectionClass, ...)
})

setMethod("createPool", "DBIConnectionFactory",
  function(drv, connectionClass, ...) {
    args <- list(...)
    if ("minConn" %in% names(args)) minConn <- args$minConn
    else minConn <- 3
    if ("maxConn" %in% names(args)) maxConn <- args$maxConn
    else maxConn <- 10
    drvArgs <- args[setdiff(names(args), c("minConn", "maxConn"))]
    connectionFactory <- do.call(drv$generator, drvArgs)
    Pool$new(connectionFactory = connectionFactory,
             connectionClass = connectionClass,
             minConn = minConn,
             maxConn = maxConn)
  }
)

#**************************************************************#
#**************************************************************#
#********* CREATING METHODS FOR DBI-DEFINED GENERICS **********#
#**************************************************************#

## I don't need to setGeneric() for all existing DBI methods, right?
# setGeneric("dbConnect",
#   def = function(drv, ...) standardGeneric("dbConnect")
# )

## To be used with care, since this puts the onus of releasing
## the connection on you
## (here, drv is the Pool object)
setMethod("dbConnect", "Pool", function(drv, ...) {
  # fetch a connection
  drv$fetch()
})

#***************** THIS MAKES NO SENSE DUM DUM *****************#
# You would never call dbConnect if you already had a connection!
# That's the whole point of this function -- to get a connection!
#***************************************************************#
## To be used with care, since this puts the onus of passivating
## the connection on you
## (here, drv is the PooledConnection object)
# setMethod("dbConnect", "PooledConnection", function(drv, ...) {
#   drv$activate()
#   drv$conn
# })

#******************* IS THIS EVEN POSSIBLE? ********************#
# There's no way we can actually support this within a pool logic,
# right?
#***************************************************************#
# ## Always use this, except if dealing with transsactions that
# ## cannot be dealt with using withTransaction(...)
# ## (here, conn is the Pool object)
# setMethod("dbSendQuery", "Pool", function(conn, statement, ...) {
#   # fetch a connection
#   DBI::dbSendQuery(conn$fetch(), statement, ...)
# })

## To be used with care, since this puts the onus of calling
## conn <- dbConnect(pool) and dbDisconnect(conn) (which is really
## just pool$release(conn), because no way to do dbDisconnect(conn))
## on you
setMethod("dbSendQuery", "PooledDBIConnection", function(conn, statement, ...) {
  # get the actual connection from the PooledDBIConnection object
  DBI::dbSendQuery(conn$conn, statement, ...)
})



## Always use this, except if dealing with transsactions that
## cannot be dealt with using withTransaction(...)
## (here, conn is the Pool object)
setMethod("dbGetQuery", "Pool", function(conn, statement, ...) {
  connection <- conn$fetch()
  on.exit(conn$release(connection))
  ## relay to dbGetQuery method for Pooled Connection class
  dbGetQuery(connection, statement, ...)
})

## To be used with care, since this puts the onus of calling
## pool$release(conn) on you
setMethod("dbGetQuery", "PooledDBIConnection", function(conn, statement, ...) {
  # get the actual connection from the PooledDBIConnection object
  DBI::dbGetQuery(conn$conn, statement, ...)
})
