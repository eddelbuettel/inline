\name{cxxsignature}
\alias{cxxsignature}
\title{
C++ function signature
}
\description{
C++ function signature
}
\usage{
cxxsignature(...)
}
\arguments{
  \item{\dots}{
      Named arguments to form a signature. The names must be
      valid C++ symbols, and the elements must be types that 
      Rcpp::as can handle : int, double, std::string, IntegerVector, etc ...
  }
}
\value{
    An object of class C++Signature, used internally by \code{\link{cxxfunction}}
}
\note{This only works when Rcpp is being included, e.g. with the use of 
the Rcpp plugin.}
\seealso{\code{\link{cxxfunction}}}
\examples{
\dontrun{

# Rcpp plugin
if( require( Rcpp ) ){

	fx <- cxxfunction( 
	    cxxsignature(x = "int", y = "double" ) , 
	    '
	    	return wrap( x * y ) ;
	    ', 
	    plugin = "Rcpp" )
	fx( 2L, 5 )

}

    
}
}
\keyword{interface}
