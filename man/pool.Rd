\docType{data}
\name{Pool}
\alias{Pool}
\alias{poolCreate}
\alias{poolClose}
\alias{poolClose,Pool-method}
\title{Pool Class.}
\usage{
poolCreate(factory, ..., minSize = 1, maxSize = Inf, idleTimeout = 60000)

poolClose(pool)
}
\arguments{
\item{factory}{A factory function responsible for the generation of
the objects that the pool will hold (ex: for DBI database connections,
this function is \code{dbConnect}).}

\item{...}{Additional arguments needed by the \code{factory}
function. Depending on your \code{factory}, these may or may
not exist. For example, if \code{factory} is \code{dbConnect},
you need to specify the database driver and possibly other
arguments such as \code{user}, \code{password}, \code{host}, etc.
For more info, see \code{\link{dbPool}}.}

\item{minSize}{An optional number specifying the minimum
number of objects that the pool should have at all times.}

\item{maxSize}{An optional number specifying the maximum
number of objects that the pool may have at any time.}

\item{idleTimeout}{The number of milliseconds that an idle
object will be kept in the pool before it is destroyed (only
applies if the number of objects is over the \code{minSize}).
Use \code{Inf} if you want created objects never to be destroyed
(there isn't a great reason for this usually).}

\item{pool}{A Pool object previsouly created with
\code{poolCreate}.}
}
\description{
A generic pool class that holds objects. These can be fetched
from the pool and released back to it at will, with very
little computaional cost. The pool should be created only once
and closed when it is no longer needed, to prevent leaks. See
\code{\link{dbPool}} for an example of
object pooling applied to DBI database connections.
}
\seealso{\code{\link{dbPool}},
\code{\link{poolCheckout}}, \code{\link{poolReturn}}}
\keyword{datasets}
\examples{
\dontrun{
pool <- poolCreate(dbConnect,
  drv = RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest"
)

dbGetQuery(pool, "SELECT * from City LIMIT 5;")
#>   ID           Name CountryCode      District Population
#> 1  1          Kabul         AFG         Kabol    1780000
#> 2  2       Qandahar         AFG      Qandahar     237500
#> 3  3          Herat         AFG         Herat     186800
#> 4  4 Mazar-e-Sharif         AFG         Balkh     127800
#> 5  5      Amsterdam         NLD Noord-Holland     731200

poolClose()
}
}

