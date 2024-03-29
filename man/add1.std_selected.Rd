% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add1_stdmod.R
\name{add1.std_selected}
\alias{add1.std_selected}
\title{The 'add1' Method for a 'std_selected' Class Object}
\usage{
\method{add1}{std_selected}(object, ...)
}
\arguments{
\item{object}{The output of \code{\link[=std_selected]{std_selected()}} or \code{\link[=std_selected_boot]{std_selected_boot()}}.}

\item{...}{Additional arguments. They will be ignored.}
}
\value{
It returns nothing. It is called for its side effect.
}
\description{
Intercept the \code{\link[=add1]{add1()}} method and raise an error.
}
\details{
\code{\link[=add1]{add1()}} should not be used after the output of \code{\link[=lm]{lm()}} is
processed by \code{\link[=std_selected]{std_selected()}} or \code{\link[=std_selected_boot]{std_selected_boot()}}.
}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
