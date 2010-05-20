setGeneric("getDynLib", function(x, ...) standardGeneric("getDynLib") )

setMethod( "getDynLib", signature( x = "character" ), 
function( x ){
	dlls <- getLoadedDLLs()
	if( x %in% names( dlls ) ){
		dlls[[ x ]]
	} else {
		stop( sprintf( "dll %s not loaded" ) )	
	}
} )

setMethod( "getDynLib", signature( x = "CFunc" ), 
function( x ){
	env <- environment( x@.Data )
	f <- get( "f", env )
	dlls <- getLoadedDLLs()
	dll <- if( ! f %in% names(dlls) ){
		dyn.load( get( "libLFile", env ) )
	} else{
		dlls[[ f ]]
	}
	dll
} )

