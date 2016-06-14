\docType{data}
\name{Pool}
\alias{Pool}
\alias{poolCreate}
\alias{poolClose}
\alias{poolClose,Pool-method}
\title{Pool Class.}
\usage{
poolCreate(src, minSize = 3, maxSize = Inf, ...)

poolClose(pool)
}
\arguments{
\item{src}{The object responsible for the generation of
the objects that the pool will hold (for database connections,
this object is the appropriate database driver).}

\item{minSize}{An optional number specifying the minimum
number of objects that the pool should have at all times.}

\item{maxSize}{An optional number specifying the maximum
number of objects that the pool may have at any time.}

\item{...}{Authorization arguments needed by the \code{src}
object. Depending on your \code{src}, these may or may not
exist. For example, if \code{src} is a database driver, you
may need to include arguments such as \code{user},
\code{password}, \code{host}, etc. For more info, see
\code{\link{poolCreate,DBIDriver-method}}.}

\item{pool}{A Pool object previsouly created with
\code{poolCreate}.}
}
\description{
A generic pool class that holds objects. These can be fetched
from the pool and released back to it at will, with very
little computaional cost. The pool should be created only once
and closed when it is no longer needed, to prevent leaks. See
\code{\link{poolCreate,DBIDriver-method}} for an example of
object pooling applied to DBI database connections.
}
\seealso{\code{\link{poolCreate,DBIDriver-method}},
\code{\link{poolCheckout}}, \code{\link{poolReturn}}}
\keyword{datasets}

