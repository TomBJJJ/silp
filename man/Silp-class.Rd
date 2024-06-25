\name{Silp-class}
\docType{class}
\alias{Silp-class}
\alias{summary,Silp-method}

\title{Class \code{"Silp"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
A formal Silp class contain CFA model, raw data, raw model, and bootstrap result
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{silp}, \code{resilp}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{raw_model}:}{Object of class \code{"character"} which stores users specified model }
    \item{\code{rapi_model}:}{Object of class \code{"character"} revied-model applying RAPI method to estimation residual variance }
    \item{\code{time}:}{Object of class \code{"numeric"} operating time of silp }
    \item{\code{type}:}{Object of class \code{"character"} model input type. Default cov }
    \item{\code{raw_data}:}{Object of class \code{"data.frame"}  }
    \item{\code{fa}:}{Object of class \code{"lavaan"} result of CFE model }
    \item{\code{reliability}:}{Object of class \code{"data.frame"}  }
    \item{\code{composite_data}:}{Object of class \code{"data.frame"} result of composite data }
    \item{\code{pa}:}{Object of class \code{"lavaan"} result of estimeation by RAPI method }
    \item{\code{boot}:}{Object of class \code{"data.frame"} estimation of bootstrap }
    \item{\code{statistic}:}{Object of class \code{"data.frame"} estimation applying percentile bootstrap }
    \item{\code{origine}:}{Object of class \code{"data.frame"} estimation of wald test result }
    \item{\code{time_resilp}:}{Object of class \code{"numeric"} operating time od bootstrap }
  }
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "Silp")}:  }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
data = generate_data(100, 0.3, 0.42, c(1,1,1,1), 0.9)
fit = silp(model, data)
refit = resilp(fit, 2000)
summary(fit)
summary(refit)
}
\keyword{classes}
