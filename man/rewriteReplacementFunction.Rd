% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphFunctions.R
\name{rewriteReplacementFunction}
\alias{rewriteReplacementFunction}
\title{Rewrite replacement function}
\usage{
rewriteReplacementFunction(expr)
}
\arguments{
\item{expr}{}
}
\value{
A standardized call expression that calls a replacement function by the same syntax as a standard function is calles
}
\description{
Turns calls of replacement functions into a logically equivalent, parser-friendly form
}
\details{
See http://adv-r.had.co.nz/Functions.html
}
\examples{
> rewriteReplacementFunction(quote(attr(t, "semantics") <- "test"))
 # t <- `attr<-`(t, "semantics", "test")

> rewriteReplacementFunction(quote(functionalType(meuse) <- "SField"))
 #meuse <- `functionalType<-`(meuse, "SField")


}

