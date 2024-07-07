\name{summary-methods}
\docType{methods}
\alias{summary,Silp-method}
\title{ Methods for Class \code{Silp} in Package \pkg{silp}}
\description{
 Summary Methods for Class \code{Silp} in Package \pkg{silp}.
}
\section{Methods}{
\describe{
\item{\code{signature(object = "Silp", method = "Bootstrap")}}{
   Returns the summary result of `silp` or `resilp`. This method is for `resilp` only. If \code{method = "Bootstrap"}, the percentile bootstrap result is presented. If \code{method = "BC_b"}, the bias-corrected bootstrap result is presented. 
}
}}
