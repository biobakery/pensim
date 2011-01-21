\name{opt1D}
\alias{opt1D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function is a wrapper to the optL1 and optL2 functions of the
penalized R package, 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
opt1D(nsim = 50, nprocessors = 1, setpen = "L1", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nsim}{
%%     ~~Describe \code{nsim} here~~
}
  \item{nprocessors}{
%%     ~~Describe \code{nprocessors} here~~
}
  \item{setpen}{
%%     ~~Describe \code{setpen} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(nsim=50,nprocessors=1,setpen="L1",...){
  if (!(identical(setpen,"L1")|identical(setpen,"L2"))) stop("setpen must be L1 or L2")
  if(nprocessors>1){
    nprocessors <- as.integer(round(nprocessors))
    library(snow)
    library(rlecuyer)
    cl <- makeCluster(nprocessors, type="SOCK")
    myseed=round(2^32*runif(6)) ##rlecuyer wants a vector of six seeds according to the SNOW manual
    clusterSetupRNG(cl,seed=myseed)
    if(identical(setpen,"L1")){
      thisopt <- parLapply(cl,1:nsim,function(n,...){
        library(penalized)
        optL1(...)
      },...)
    }else{   ##if(identical(setpen,"L1")){
      thisopt <- parLapply(cl,1:nsim,function(n,...){
        library(penalized)
        optL2(...)
      },...)
###      thisopt <- parLapply(cl,1:nsim,function(n,...) optL2(...),...)
    }
    stopCluster(cl)
  }else{  ##if(nprocessors>1){
    library(penalized)
    if(identical(setpen,"L1")){
      thisopt <- lapply(1:nsim,function(n,...) optL1(...),...)
    }else{
      thisopt <- lapply(1:nsim,function(n,...) optL2(...),...)
    }
  }
  output <- sapply(thisopt,function(x){
    coefs <- coefficients(x$fullfit,"all")
    tmp <- c(x$lambda,x$cvl,coefs)
    names(tmp) <- c(setpen,"cvl",names(coefs))
    return(tmp)
  })
  return(t(output))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
