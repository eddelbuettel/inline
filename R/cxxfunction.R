
plugins <- new.env()

Makevars.Rcpp <- '
## Use the R_HOME indirection to support installations of multiple R version
PKG_LIBS = $(shell $(R_HOME)/bin/Rscript -e "Rcpp:::LdFlags()" )
'

Makevars.win.Rcpp <- '
## Use the R_HOME indirection to support installations of multiple R version
PKG_LIBS = $(shell "${R_HOME}/bin${R_ARCH_BIN}/Rscript.exe" -e "Rcpp:::LdFlags()")
'

Makevars.RcppArmadillo <- '
PKG_LIBS = $(shell $(R_HOME)/bin/Rscript -e "Rcpp:::LdFlags()" ) $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)
'
Makevars.win.RcppArmadillo <- '
PKG_LIBS = $(shell $(R_HOME)/bin${R_ARCH_BIN}/Rscript.exe -e "Rcpp:::LdFlags()") $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)
'


Rcpp.plugin.maker <- function( include.before = "", include.after = "", 
	LinkingTo = "Rcpp", Depends = "Rcpp", libs = "", 
	Makevars = Makevars.Rcpp, 
	Makevars.win = Makevars.win.Rcpp
){
	function( ... ){
includes <- sprintf( "%s
#include <Rcpp.h>
%s

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;
", include.before, include.after )

	list( 
		env = list( PKG_LIBS = paste( Rcpp:::RcppLdFlags(), libs ) ), 
		includes = includes, 
		LinkingTo = LinkingTo , 
		body = function( x ){
			sprintf( "BEGIN_RCPP\n%s\nEND_RCPP", x )	
		}, 
		Depends = Depends, 
		Makevars = Makevars, 
		Makevars.win = Makevars.win
	)
}
}

plugins[["default"]] <- function( ){
	includes = '#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>
'
	list( 
		includes = includes, 
		body = function( x ) paste( x, '\nRf_warning("your C++ program does not return anything"); \n return R_NilValue ; ' ) 
	)
}

plugins[["Rcpp"]] <- Rcpp.plugin.maker() 
plugins[["RcppArmadillo"]] <- Rcpp.plugin.maker(
	include.before = "#include <RcppArmadillo.h>", 
	LinkingTo = c("Rcpp", "RcppArmadillo"), 
	Depends = c("Rcpp", "RcppArmadillo"),
	libs = "$(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)", 
	Makevars = Makevars.RcppArmadillo, 
	Makevars.win = Makevars.win.RcppArmadillo
)

registerPlugin <- function( name, plugin ){
	plugins[[ name ]] <- plugin
}
getPlugin <- function( name, ... ){
	if( !name %in% ls( plugins ) ){
		stop( sprintf( "no such inline plugin : '%s' ", name ) )
	}
	plugins[[ name ]]( ... )
}
   
paste0 <- function(...) paste(..., sep="")

addLineNumbers <- function( code ){
	code <- strsplit( paste( code, collapse = "\n" ), "\n" )[[1]]
	sprintf( "%4d : %s", 1:length(code), code) 
}

cxxfunction <- function (
	sig = character(), body = character(), 
	plugin = "default", 
	includes = "", 
	settings = getPlugin(plugin), 
	..., 
	verbose = FALSE
	){
	    
		
	f <- basename( tempfile( ) )	
	
	signature <- if( ! length( sig ) ){ "" } else {
		paste( sprintf( "SEXP %s", names(sig) ), collapse = ", " )
	}
	
	decl <- sprintf( 'extern "C" {
	SEXP %s( %s) ;
}
', f, signature )

	def <- sprintf( '
SEXP %s( %s ){
%s
}
', f, signature, if(is.null(settings$body)) body else settings$body(body) )
	
	settings_includes <- if( is.null( settings$includes ) ) "" else paste( settings$includes, collapse = "\n" )

	code <- sprintf( '// includes from the plugin
%s

// user includes
%s

// declaration
%s

// definition
%s

', settings_includes , paste( includes, collapse = "\n" ), decl, def )

	
	if( !is.null( env <- settings$env ) ){
		do.call( Sys.setenv, env )
		if( isTRUE(verbose) ){
			 cat( " >> setting environment variables: \n" )
			 writeLines( sprintf( "%s = %s", names(env), env ) )
		}
	}
	
	LinkingTo <- settings$LinkingTo
	if( !is.null( LinkingTo ) ){
		paths <- .find.package(LinkingTo, quiet=TRUE)
		if( length( paths ) ){
			flag <- paste( 
				paste0( '-I"', paths, '/include"' ), 
				collapse = " " )
			Sys.setenv( CLINK_CPPFLAGS = flag )
			if( isTRUE( verbose ) ){
				cat( sprintf( "\n >> LinkingTo : %s\n", paste( LinkingTo, collapse = ", " ) ) )
				cat( "CLINK_CPPFLAGS = ", flag, "\n\n" )
			}
		}
		
	}
	
	if( isTRUE( verbose ) ){
		writeLines( " >> Program source :\n" )
		writeLines( addLineNumbers( code ) )
	}
	
	language <- "C++"
	
	## WRITE AND COMPILE THE CODE
  	libLFile <- compileCode( f, code, language = language, verbose = verbose ) 
	
	## SET A FINALIZER TO PERFORM CLEANUP
  	cleanup <- function(env) {
  	  if ( f %in% names(getLoadedDLLs()) ) dyn.unload(libLFile)
  	  unlink(libLFile)
  	}
  	reg.finalizer(environment(), cleanup, onexit=TRUE)
    
  	## Create new objects of class CFunc, each containing the code of ALL inline
  	## functions. This will be used to recompile the whole shared lib when needed
  	res <- new("CFunc", code = code)
  	
  	## this is the skeleton of the function, the external call is added below using 'body'
  	## important here: all variables are kept in the local environment
  	fn <- function(arg) {
  	  if ( !file.exists(libLFile) )
  	    libLFile <<- compileCode(f, code, "C++", verbose)
  	  if ( !( f %in% names(getLoadedDLLs()) ) ) dyn.load(libLFile)
  	}
  	
  	## Modify the function formals to give the right argument list
  	args <- formals(fn)[ rep(1, length(sig)) ]
  	names(args) <- names(sig)
  	formals(fn) <- args
  	
  	## create .C/.Call function call that will be added to 'fn'
  	  body <- quote( .Call( "EXTERNALNAME", PACKAGE=f, ARG) )[ c(1:3, rep(4, length(sig))) ]
  	  for ( j in seq(along = sig) ) body[[j+3]] <- as.name(names(sig)[j])
  	
  	body[[2]] <- f
  	## update the body of 'fn'
  	body(fn)[[4]] <- body
  	## set fn as THE function in CFunc of res[[i]]
  	res@.Data <- fn
  	
  	## clear the environment
  	rm( j )
  	
  	res
}

