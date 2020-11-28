setGeneric("moveDLL",
  function(x, ...) {
    standardGeneric("moveDLL")
  }
)
 
setMethod("moveDLL",
  signature(x = "CFunc"),
  function(x, name, directory, unload = FALSE, overwrite = FALSE, verbose = FALSE) {

    # Check arguments
    if (length(name) > 1) stop("Please only supply only one name")
    if (length(directory) > 1) stop("Please only supply only one directory name")

    # Obtain path to DLL
    old_path <- environment(x)$libLFile

    # Create new path
    if (!dir.exists(directory)) stop("There is no directory ", directory)
    extension <- tools::file_ext(old_path)
    new_path <- file.path(normalizePath(directory), paste(name, extension, sep = "."))

    active_paths <- sapply(getLoadedDLLs()[-1],
      function(di) normalizePath(di[["path"]]))
    if (new_path %in% active_paths) {
      if (unload) {
        if (inherits(try(dyn.unload(new_path)), "try-error"))
          stop("Could not unload ", new_path)
      } else {
        stop("DLL from ", new_path, " is in use")
      }
    }

    # Copy the DLL
    copy_success <- file.copy(old_path, new_path, overwrite = overwrite)
    if (!copy_success) stop("Failed to copy DLL from ", old_path, " to ", new_path)
    if (verbose) message("Copied DLL from ", old_path, " to ", new_path)

    # Unload DLL and reload from its new location
    dyn.unload(old_path)
    new_dll_info <- dyn.load(new_path)

    # Adjust the path that getDynLib uses
    environment(x)$libLFile <- new_path

    # Adjust the symbol info in the function body
    function_name <- environment(x)$name
    body(x)[[2]] <- getNativeSymbolInfo(function_name, new_dll_info[["name"]])$address

    invisible(new_dll_info)
  }
)

writeCFunc <- function(x, file) {
  env <- environment(x)
  if (identical(env$libLFile, env$libLFile_orig))
    stop("Use moveDLL to prevent losing the DLL by garbage collection or session termination")

  saveRDS(x, file = file)
}

readCFunc <- function(file) {
  x <- readRDS(file)
  if (class(x) != "CFunc") stop(file, " does not contain a serialized CFunc object")

  # Get code for restoring after updating the function body
  source_code <- x@code

  # Load the DLL
  env <- environment(x)
  dll_info <- dyn.load(env$libLFile)

  # Set the symbol info in the function body
  body(x)[[2]] <- getNativeSymbolInfo(env$name, dll_info[["name"]])[["address"]]
  x_cf <- as(x, "CFunc")
  x_cf@code <- source_code
  
  return(x_cf)
}

setGeneric("code", function(x, ...) standardGeneric("code") )
setMethod( "code", signature( x = "character" ),
function( x, linenumbers = TRUE ){
  lines <- strsplit(x, "\n")
  if (linenumbers)
   for (i in 1:length(lines[[1]])) cat(format(i, width = 3),
    ": ", lines[[1]][i], "\n", sep = "")
  else
   for (i in 1:length(lines[[1]])) cat(lines[[1]][i], "\n", sep = "")

} )
setMethod( "code", signature( x = "CFunc" ), function( x, linenumbers = TRUE  ) code (x@code, linenumbers))
setMethod( "code", signature( x = "CFuncList" ), function(x, linenumbers = TRUE ) code( x[[1L]], linenumbers ) )

## Printing methods

setMethod( "print", signature( x = "CFunc" ),
function( x ){
  cat("An object of class 'CFunc'\n")
  Dat <- x@.Data
  print(Dat)
  cat("code:\n")
  code(x)
} )

setMethod( "print", signature( x = "CFuncList" ), function(x) {
  cat("An object of class 'CFuncList'\n")
  for (i in 1:length(x)) {
    print(names(x)[i])
    print(x[[i]]@.Data )
    cat("\n")
  }
  cat("code:\n")
  code(x)
} )



