
plugins <- new.env()

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

registerPlugin <- function( name, plugin ){
	plugins[[ name ]] <- plugin
}
getPlugin <- function( name, ... ){
	if( name %in% ls( plugins ) ){
		plugins[[ name ]]( ... )
	} else if( sprintf("package-%s", name) %in% search() || require( name, character.only = TRUE, quietly = TRUE) ){
		plugin <- get( "inlineCxxPlugin" , asNamespace(name) )
		if( is.null(plugin) ){
			stop( sprintf( "package '%s' does not define an inline plugin", name ) )
		}
		registerPlugin( name, plugin )
		plugin( ... )
	} else {
		stop( sprintf( "could not find plugin '%s'", name ) )
	}
	
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
	
	if( ! is.list( sig ) ){
		sig <- list( sig )
		names( sig ) <- f
		if( ! length( body ) ) body <- ""
		names( body ) <- f
	}
	if( length(sig) != length(body) )
	    stop("mismatch between the number of functions declared in 'sig' and the number of function bodies provided in 'body'")
	    
	signature <- lapply( sig, function(x) {
		if( ! length(x) ){ 
			"" 
		} else {
			paste( sprintf( "SEXP %s", names(x) ), collapse = ", " )
		} 
	} )
	
	decl <- lapply( 1:length(sig) , function(index) {
		sprintf( 'SEXP %s( %s) ;', names(signature)[index] , signature[[index]] ) 
	} )

	def <- lapply( 1:length(sig), function(index){
		sprintf( '
SEXP %s( %s ){
%s
}
', names(signature)[index], 
	signature[[index]], 
	if(is.null(settings$body)) body[[index]] else settings$body(body[[index]]) )
	} )
	
	settings_includes <- if( is.null( settings$includes ) ) "" else paste( settings$includes, collapse = "\n" )

	reg <- sapply( 1:length(sig), function(i){
	    sprintf( "CALLDEF(%s,%d), ", names(sig)[i], length(sig[[i]]) )
	} )
	
	code <- sprintf( '
#include <R_ext/Rdynload.h>

// includes from the plugin
%s

// user includes
%s

// declarations
extern "C" {
%s
}

// definition
%s

// registration
#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}
R_CallMethodDef callMethods[]  = {
    %s
    {NULL, NULL, 0}
};

extern "C" void R_init_%s( DllInfo* info ){
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
     
', settings_includes , paste( includes, collapse = "\n" ), 
	paste( decl, collapse = "\n" ), 
	paste( def, collapse = "\n"), 
	paste( reg, collapse = "\n" ), 
	f
	)

	
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
  	res <- vector("list", length(sig))
  	names(res) <- names(sig)
  	res <- new( "CFuncList", res )
  	
  	DLL <- dyn.load( libLFile )
  	
  	for( i in seq_along(sig) ){
  		res[[i]] <- new( "CFunc", code = code )
  		
  		fn <- function(arg) {
  		  NULL
  		}
  		
    	## Modify the function formals to give the right argument list
    	args <- formals(fn)[ rep(1, length(sig[[i]])) ]
    	names(args) <- names(sig[[i]])
    	formals(fn) <- args
  		  
    	## create .Call function call that will be added to 'fn'
  		body <- quote( .Call( "EXTERNALNAME", ARG) )[ c(1:2, rep(3, length(sig[[i]]))) ]
  		for ( j in seq(along = sig[[i]]) ) body[[j+2]] <- as.name(names(sig[[i]])[j])
  		
  		body[[1L]] <- .Call
  		body[[2L]] <- getNativeSymbolInfo( names(sig)[[i]], DLL )$address
  		## update the body of 'fn'
  		body(fn) <- body
  		## set fn as THE function in CFunc of res[[i]]
  		res[[i]]@.Data <- fn
  	}
  	
  	## clear the environment
  	rm( j )
  	
  	if( identical( length(sig), 1L ) ) res[[1L]] else res
}

