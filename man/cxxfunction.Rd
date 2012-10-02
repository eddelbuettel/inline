\name{cxxfunction}
\alias{cxxfunction}
\title{
inline C++ function
}
\description{
Functionality to dynamically define an R function with inlined C++ code
using the \code{\link{.Call}} calling convention
}
\usage{
cxxfunction(sig = character(), body = character(), 
	plugin = "default", includes = "", 
	settings = getPlugin(plugin), ..., verbose = FALSE)
}
\arguments{
  \item{sig}{Signature of the function. A named character vector}
  \item{body}{A character vector with C++ code to include in the body of the compiled C++ function}
  \item{plugin}{Name of the plugin to use. See \code{\link{getPlugin}} for details about plugins. }
  \item{includes}{User includes, inserted after the includes provided by the plugin. }
  \item{settings}{Result of the call to the plugin}
  \item{\dots}{Further arguments to the plugin}
  \item{verbose}{verbose output}
}
\value{
A function
}
\seealso{
	\code{\link{cfunction}}
}
\examples{
\dontrun{

# default plugin
fx <- cxxfunction( signature(x = "integer", y = "numeric" ) , '
	return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
' )
fx( 2L, 5 )

# Rcpp plugin
if( require( Rcpp ) ){

	fx <- cxxfunction( signature(x = "integer", y = "numeric" ) , '
		return wrap( as<int>(x) * as<double>(y) ) ;
	', plugin = "Rcpp" )
	fx( 2L, 5 )

}

# RcppArmadillo plugin
if( require( RcppArmadillo ) ){
	
	fx <- cxxfunction( signature(x = "integer", y = "numeric" ) , '
		int dim = as<int>( x ) ;
		arma::mat z = as<double>(y) * arma::eye<arma::mat>( dim, dim ) ;
		return wrap( arma::accu(z) ) ;
	', plugin = "RcppArmadillo" )
	fx( 2L, 5 )
	
	
}

}
}
\keyword{programming}
\keyword{interface}

