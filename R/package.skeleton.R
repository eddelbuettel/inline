
setGeneric( "package.skeleton" )
setMethod( "package.skeleton", signature( name = "character", list = "CFunc" ), 
function (name = "anRpackage", list = new( "CFunc" ), environment = .GlobalEnv, 
    path = ".", force = FALSE, namespace = FALSE, code_files = character()) 
{
	
	env <- environment( list@.Data )
	
	f <- env[["f"]]
	call <- body( list@.Data )[[4]]
	call[["PACKAGE"]] <- f
	fun <- list@.Data
	body( fun ) <- call
	clean.env <- new.env( parent = environment )
	environment(fun) <- clean.env 
	assign( f, fun, clean.env )
	
	message( ">> standard package.skeleton from utils" )
	package.skeleton( name, f , clean.env, path, force, namespace, code_files )
	
	if( !file.exists( R <- file.path(name, "R") ) ){
		dir.create( R )
		writeLines( sprintf( ' %s <- %s\n' , f, deparse(fun) ), file.path( R, sprintf( "%s.R", f ) ) )
		message( ">> added R directory with function calling compiled code" )
	}
	
	Rdfile <- file.path( name, "man", sprintf( "%s.Rd", f ) )
	if( file.exists( Rdfile ) ){
		content <- readLines( Rdfile )
		content <- sub( "%%  ~~function to do ... ~~", "insert here the title", content )
		writeLines( content, Rdfile )
		message( ">> workaround for empty title in Rd file" )
	}
	
	dir.create( file.path( name, "src" ) )
	message( ">> created src directory" )
	
	language <- env$language
	extension <- switch(language, "C++"=".cpp", C=".c", Fortran=".f", F95=".f95",
                                ObjectiveC=".m", "ObjectiveC++"=".mm")
  
	cfile <- file.path( name, "src", sprintf( "%s%s", f, extension ) )
	writeLines( list@code, cfile )
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

