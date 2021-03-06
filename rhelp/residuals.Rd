% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rinterface.R
\name{residuals}
\alias{residuals}
\alias{residuals.gamljGlmResults}
\alias{residuals.gamljGzlmResults}
\alias{residuals.gamljMixedResults}
\alias{residuals.gamljGlmMixedResults}
\title{Residuals values from GAMLj models}
\usage{
\method{residuals}{gamljGlmResults}(object, ...)

\method{residuals}{gamljGzlmResults}(object, type = "deviance", ...)

\method{residuals}{gamljMixedResults}(object, ...)

\method{residuals}{gamljGlmMixedResults}(object, type = "deviance", ...)
}
\arguments{
\item{object}{a gamlj results object of the class `gamlj*Results`}

\item{...}{additional arguments for specific residuals methods.}

\item{type}{the type of residuals for generalized models. The alternatives are: 'deviance' (default), 'pearson', 'working', 'response', and 'partial'. Can be abbreviated.
Cf. \code{\link[stats:residuals]{stats::residuals()}}, \code{\link[stats:residuals.lm]{stats::residuals.lm()}}, \code{\link[stats:residuals.glm]{stats::residuals.glm()}}}
}
\value{
a R object of the class of the estimated model
}
\description{
Returns residuals values from the estimated model
}
\examples{
data('qsport')
gmod<-gamlj::gamljGlm(
   formula = performance ~ hours,
   data = qsport)
 preds<-residuals(gmod)
 
}
\author{
Marcello Gallucci
}
