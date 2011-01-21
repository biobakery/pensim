\name{scan.l1l2}
\alias{scan.l1l2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function calculate cross-validated likelihood on a regular grid of L1/L2 penalties
}
\description{
This function generates a grid of values of L1/L2 penalties,
then calculated cross-validated likelihood at each point on the grid.
The grid can be regular (linear progression of the penalty values), or
polynomial (finer grid for small penalty values, and coarser grid for
larger penalty values).
}
\usage{
scan.l1l2(L1range = c(0.1, 100.1), L2range = c(0.1, 100.1), L1.ngrid = 50, L2.ngrid = 50, nprocessors = 1, polydegree = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{L1range}{
    numeric vector of length two, giving minimum and maximum constraints
    on the L1 penalty
}
  \item{L2range}{
    numeric vector of length two, giving minimum and maximum constraints
    on the L2 penalty
}
  \item{L1.ngrid}{
    Number of values of the L1 penalty in the regular grid of L1/L2 penalties
}
  \item{L2.ngrid}{
    Number of values of the L2 penalty in the regular grid of L1/L2 penalties
}
  \item{nprocessors}{
        An integer number of processors to use.
}
  \item{polydegree}{
    power of the polynomial on which the L1/L2 penalty values are fit.
    ie if polydegree=2, penalty values could be y=x^2, x=1,2,3,..., so y=1,4,9,...
}
  \item{\dots}{
    arguments passed on to cvl function of the penalized R package
}
}
\details{
  This function sets up a SNOW (Simple Network of Workstations) "sock"
  cluster to parallelize the task of scanning a grid of penalty values
  to search for suitable starting values for two-dimensional
  optimization of the Elastic Net.
}
\value{
  \item{cvl}{matrix of cvl values along the grid}
  \item{L1range}{L1range,L2range}{L2range}
  \item{xlab}{A note giving the range of L1 penalties}
  \item{ylab}{A note giving the range of L2 penalties}
  \item{zlab}{A note giving the range of cvl values}
  \item{note}{A note to the user that rows of cvl correspond to values of lambda1, columns to lambda2}
	    
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
function(L1range=c(0.1,100.1),L2range=c(0.1,100.1),L1.ngrid=50,L2.ngrid=50,nprocessors=1,polydegree=1,...){ #... arguments for cvl
  library(penalized)
  #a function for scanning L1 for a given value of L2
  scan.l1 <- function(L2,lambda1vals,...){
    sapply(lambda1vals,
           function(thisL1,thisL2=L2,...){
             cvl(lambda1=thisL1,lambda2=thisL2,...)$cvl
           },...)
  }
  #set up parallel processing and random number generation
  if(nprocessors>1){
    library(snow)
    library(rlecuyer)
    cl <- makeCluster(nprocessors, type="SOCK")
    myseed=round(2^32*runif(6)) #rlecuyer wants a vector of six seeds according to the SNOW manual
    clusterSetupRNG(cl,seed=myseed)
  }
  #create the L1 and L2 sequences
  L1vals <- seq(L1range[1]^(1/polydegree),L1range[2]^(1/polydegree),length.out=L1.ngrid)^polydegree
  L2vals <- seq(L2range[1]^(1/polydegree),L2range[2]^(1/polydegree),length.out=L2.ngrid)^polydegree
  #do the actual work
  if(nprocessors>1){
    #randomize the order of L1vals and L2vals, so that slow
    #computations get more evenly distributed across the cluster.  In
    #some situations where a low value of one of the penalties results
    #in longer computation time, this should speed up the final
    #result.
    L1.reorder <- sample(1:length(L1vals),length(L1vals))
    L2.reorder <- sample(1:length(L2vals),length(L2vals))
    L1vals <- L1vals[L1.reorder]
    L2vals <- L2vals[L2.reorder]
    cvl.matrix <- parSapply(cl,L2vals,function(thisL2,...){
      library(penalized)
      scan.l1(L2=thisL2,lambda1vals=L1vals,...)},
                            ...)
    #now re-rorder things
    cvl.matrix <- cvl.matrix[order(L1.reorder),order(L2.reorder)]
    L1vals <- L1vals[order(L1.reorder)]
    L2vals <- L2vals[order(L2.reorder)]
  #shut down the cluster
    stopCluster(cl)
  }else{
    cvl.matrix <- sapply(L2vals,function(thisL2,...){
      scan.l1(L2=thisL2,lambda1vals=L1vals,...)},...)
  }
  #rows correspond to values of L1, columns to values of L2
  rownames(cvl.matrix) <- L1vals
  colnames(cvl.matrix) <- L2vals
  #return the results in a list
  x <- list(cvl=cvl.matrix,
            L1range=L1range,L2range=L2range,
            xlab=paste("L1 (",L1range[1]," to ",L1range[2],")",sep=""),
            ylab=paste("L2 (",L2range[1]," to ",L2range[2],")",sep=""),
            zlab=paste("Log-likelihood (",round(min(cvl.matrix),1)," to ",round(max(cvl.matrix),1),")",sep=""),
            note="rows of cvl correspond to values of lambda1, columns to lambda2")
  return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
