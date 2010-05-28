\name{plugins}
\alias{getPlugin}
\alias{registerPlugin}
\title{
Plugin system for cxxfunction
}
\description{
\code{\link{cxxfunction}} uses a plugin system to assembly the 
code that it compiles. These functions allow to register and get 
plugins by their name. 
}
\usage{
getPlugin(name, ...)
registerPlugin(name, plugin)
}
\arguments{
  \item{name}{name of the plugin. }
  \item{\dots}{Further argments to pass to the plugin. }
  \item{plugin}{plugin function. }
}
\details{
plugins are functions that return a list with : 
\describe{
	\item{includes}{mandatory. it is included at the top of the compiled file by \code{\link{cxxfunction}} }
	\item{body}{optional. a function that takes one argument (the body of the c++ function) and returned
	a modified version of the body. The "Rcpp" plugin uses this to surround the code with the 
	\code{BEGIN_RCPP} and \code{END_RCPP} macros
	}	
	\item{LinkingTo}{optional. character vector containing the list of packages
	that the code needs to link to. This adds the include path of the given 
	packages. The "Rcpp" and "RcppArmadillo" plugins use this.
	}
	\item{env}{optional. named list of environment variables. For example, the "Rcpp" 
	plugin uses this to add Rcpp user library to the \code{PKG_LIBS} environment
	variable.
	}
}
}
\value{
	\code{getPlugin} retrieves the plugin and invokes it with the
	\dots arguments
	
	\code{registerPlugin} does not return anything.
}
\seealso{
	\code{\link{cxxfunction}}
}
\examples{
\dontrun{
getPlugin( "Rcpp" )
}
}
\keyword{programming}
\keyword{interface}

