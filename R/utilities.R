## ---------------------------------------------------------------------------
# saving and loading CFunc objects

writeDynLib <- function(x, file, directory = ".") {

  DLL <- getDynLib(x)

  if (is.null(DLL))
    stop ("'x' DLL not loaded")

  DLLpath <- DLL[["path"]]
  if (!file.exists(DLLpath))
    stop ("'x' does not point to an existing DLL")

  # get extension of filename  (dll, so)
  extension <- unlist(strsplit(basename(DLLpath), ".", fixed = TRUE))[2]
  newDLLpath <- file.path(directory, paste(file, extension, sep = "."))

  file.copy(from = DLLpath, to = newDLLpath, overwrite = TRUE)

  # accessory file with compiled code information (DLL path has changed)
  fileCF <- file.path(directory, paste(file, "CFunc", sep = "."))

  environment(x@.Data)$libLFile <- newDLLpath
  environment(x@.Data)$f <- file # getDynLib uses f as DLL name to check if the DLL is loaded

  save(file = fileCF, x)

  invisible(fileCF)
}

## ---------------------------------------------------------------------------

readDynLib <- function(file, directory = ".") {

  # open all the required files
  fileCF <- file.path(directory, paste(file, "CFunc", sep = "."))

  if (!file.exists(fileCF))
    stop (fileCF,  " does not exist")

  CF <- get(load(file = fileCF))

  CF_env <- environment(CF)
  DLLpath <- CF_env$libLFile

  if (!file.exists(DLLpath))
    stop ("'file' does not point to valid CFunc object: DLL ", DLLpath, " does not exist")

  # load routines in DLL

  DLL <- dyn.load(DLLpath)
  fn <- CF_env$name
  CFi <- CF
  code <- CFi@code
  body(CFi)[[2]] <- getNativeSymbolInfo(fn, DLL)$address
  CF@.Data <- CFi
  return(CF)
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



