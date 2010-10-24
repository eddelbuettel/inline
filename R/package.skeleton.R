
setGeneric( "package.skeleton" )

setMethod( "package.skeleton", signature( name = "character", list = "CFuncList" ), 
function (name = "anRpackage", list = new( "CFuncList" ), environment = .GlobalEnv, 
    path = ".", force = FALSE, namespace = FALSE, code_files = character()) {

	env <- environment( list[[1]]@.Data )
	clean.env <- new.env( parent = environment )
	
	functions <- names( list )
	for( i in seq_along(list) ){
		f <- functions[i]
		call <- body( list[[i]]@.Data )[[4]]
		call[["PACKAGE"]] <- name
		fun <- list[[i]]@.Data
		body( fun ) <- call
		environment(fun) <- clean.env 
		assign( f, fun, clean.env )
	}
	
	message( ">> standard package.skeleton from utils" )
	package.skeleton( name, functions , clean.env, path, force, namespace = TRUE, code_files )
	
	if( !file.exists( R <- file.path(name, "R") ) ){
		dir.create( R )
		con <- file( file.path( R, sprintf("%s.R", name) ), open = "w" )
		for( i in seq_along(list) ){
			fun <- functions[i]
			writeLines( sprintf( ' %s <- %s\n' , fun, paste( deparse(clean.env[[ fun ]] ), collapse = "\n" ) ), con )
		}   
		close( con )
		message( ">> added R directory with function calling compiled code" )
	}
	
	Rdfiles <- file.path( name, "man", sprintf( "%s.Rd", functions ) )
	sapply( Rdfiles, function( Rdfile) {
		if( file.exists( Rdfile ) ){
			content <- readLines( Rdfile )
			content <- sub( "%%  ~~function to do ... ~~", "insert here the title", content )
			writeLines( content, Rdfile )
		}
		message( ">> workaround for empty title in Rd file" )
	} )
	
	package.doc <- file.path( name, "man", sprintf("%s-package.Rd", name) )
	if( file.exists( package.doc) ){
		lines <- readLines( package.doc )
		lines <- sub( "~~ simple examples of", "%% ~~ simple examples of", lines )
		writeLines( lines, package.doc )
	}
	
	dir.create( file.path( name, "src" ) )
	message( ">> created src directory" )
	
	NAMESPACE <- sprintf( '
	useDynLib(%s)
	
	%s
	', name, paste( sprintf( 'export("%s")', functions ), collapse = "\n" ) )
	writeLines( NAMESPACE, file.path( name, "NAMESPACE" ) )
	
	language <- env$language
	extension <- switch(language, "C++"=".cpp", C=".c", Fortran=".f", F95=".f95",
                                ObjectiveC=".m", "ObjectiveC++"=".mm")
  
	cfile <- file.path( name, "src", sprintf( "%s%s", name, extension ) )
	writeLines( list[[1]]@code, cfile )
	message( ">> added compiled code in src directory" )

	settings <- env$settings
	if( !is.null( settings ) ){
		DESCRIPTION_file <- file.path( name, "DESCRIPTION" )
		DESCRIPTION <- read.dcf( DESCRIPTION_file )
		
		depends <- settings$Depends
		if( !is.null( depends ) ){
			DESCRIPTION <- cbind( DESCRIPTION, "Depends" = depends )
		}
		
		linkingTo <- settings$LinkingTo
		if( !is.null( linkingTo ) ){
			DESCRIPTION <- cbind( DESCRIPTION, "LinkingTo" = linkingTo )
		}
		write.dcf( DESCRIPTION, DESCRIPTION_file )
		
		message( ">> updated DESCRIPTION file" )
		
		Makevars <- settings$Makevars
		if( !is.null( Makevars ) ){
			Makevars_file <- file.path( name, "src", "Makevars" )
			writeLines( Makevars, Makevars_file )
			message( ">> added Makevars ")
		}
		
		Makevars.win <- settings$Makevars.win
		if( !is.null( Makevars.win ) ){
			Makevars.win_file <- file.path( name, "src", "Makevars.win" )
			writeLines( Makevars.win, Makevars.win_file )
			message( ">> added Makevars.win" )
		}
		
	}
	invisible(NULL)
	
} )


setMethod( "package.skeleton", signature( name = "character", list = "CFunc" ), 
function (name = "anRpackage", list = new( "CFunc" ), environment = .GlobalEnv, 
    path = ".", force = FALSE, namespace = FALSE, code_files = character()) {
	
    funclist <- new( "CFuncList", base::list( fun = list ) )    
    package.skeleton( name = name, list = funclist,
		environment = environment, path = path , force = force, 
		namespace = namespace , code_files = code_files )
} )

