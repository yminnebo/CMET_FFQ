% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FFQ_calculate_fruit_fibres.R
\name{FFQ.calculate.fruit.fibres}
\alias{FFQ.calculate.fruit.fibres}
\title{Calculate the fibre consumptions from fruit}
\usage{
FFQ.calculate.fruit.fibres(FFQobject, ignore.groups = FALSE)
}
\arguments{
\item{FFQobject}{FFQ object with all FFQ data}

\item{ignore.groups}{Check if FFQ food items are present in reference list. Default = FALSE.}
}
\value{
Calculate the fibre consumptions from fruit out of the FFQ object.
}
\description{
Calculate the fibre consumptions from fruit
}
\examples{
FFQobject_fruit_fibres <- FFQ.calculate.fruit.fibres(FFQobject)
}
\seealso{
CMET.FFQ::FFQ.calculate.fibres() .
}
