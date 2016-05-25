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
    connections = NULL,
    factory = NULL,
    min = NULL,
    max = NULL,
    ## initialize the pool with min number of connection
    initialize = function(ConnectionFactory, min = 3, max = 10) {
      self$factory <- ConnectionFactory
      self$min <- min
      self$max <- max
      for (i in seq_len(self$min)) {
        connection <- self$factory()
        self$connections[[i]] <- PooledConnection$new(
          conn = connection,
          id = digest::digest(connection)
        )
      }
    },
    ## calls activate and returns a connection
    fetch = function() {
      connection <- NULL
      ## see if there's any free connections
      for (i in seq_len(length(self$connections))) {
        if (self$connections[[i]]$free) {
          connection <- self$connections[[i]]
          break
        }
      }
      if (is.null(connection)) {
        ## if we get here, there are no free connections
        ## and we must create a new one
        connection <- PooledConnection$new(
          connection = self$factory(),
          id = digest::digest(connection)
        )
        self$connections <- c(self$connections, connection)
      }
      ## activate connection and return it
      connection$activate()
      return(connection)
    },
    ## passivates all connections and closes the pool
    ## (actually calls connnection$detroy())
    release = function(connection) {
      connection$passivate()
      if (length(self$connections) > self$max) {
        id <- connection$id
        connection$destroy()
        for (i in seq_len(length(self$connections))) {
          if (self$connections[[i]]$id == id) {
            self$connections[[i]] <- NULL
          }
        }
      }
    }
  )
)

PooledConnection <- R6Class("PooledConnection",
  public = list(
    conn = NULL,
    id = NULL,
    free = NULL,
    initialize = function(conn, id) {
      self$conn <- conn
      self$id <- id
      self$free <- TRUE
    },
    activate = function() {
      self$free <- FALSE
    },
    passivate = function() {
      self$free <- TRUE
    },
    destroy = function() {
      DBI::dbDisconnect(self$conn)
    }
  )
)

DBIConnectionFactory <- R6Class("DBIConnectionFactory",
  public = list(
    drv = NULL,
    initialize = function(drv) {
      self$drv <- drv
    },
    generate = function(...) {
      function() {
        DBI::dbConnect(self$drv, ...)
      }
    }
  )
)

## set S4 classes for consistency with DBI conventions
## (I'm pretty sure this naive approach won't work, however)
setClass("Pool")
setClass("PooledConnection")
setClass("DBIConnectionFactory")

## set S4 methods for consistency with DBI conventions

#**************************************************************#
#**************************************************************#
#********************** OPENING THE POOL **********************#
#**************************************************************#

setGeneric("createPool", function(drv, ...) {
  standardGeneric("createPool")
})

setMethod("createPool", "DBIDriver", function(drv, ...) {
  createPool(DBIConnectionFactory$new(drv), ...)
})

setMethod("createPool", "DBIConnectionFactory", function(drv, ...) {
  Pool$new(drv$generate(...))
})


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

## No way this works, because we cannot call the Pool object
## given just a PooledConnection object
# setMethod("dbDisconnect", "PooledConnection", function(conn, ...) {
#   # fetch a connection
#   drv$fetch()
# })

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
setMethod("dbSendQuery", "PooledConnection", function(conn, statement, ...) {
  # get the actual connection from the PooledConnection object
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
setMethod("dbGetQuery", "PooledConnection", function(conn, statement, ...) {
  # get the actual connection from the PooledConnection object
  DBI::dbGetQuery(conn$conn, statement, ...)
})
