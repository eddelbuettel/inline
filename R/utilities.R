## ---------------------------------------------------------------------------
# saving and loading CFunc objects

writeDynLib <- function(x, bname, directory = ".") {

  DLL <- getDynLib(x)

  if (is.null(DLL))
    stop ("'x' DLL not loaded")

  DLLname <- DLL[["path"]]
  if (!file.exists(DLLname))
    stop ("'x' does not point to an existing DLL")

  # get extension of filename  (dll, so)
  extension <- unlist(strsplit(basename(DLLname), ".", fixed = TRUE))[2]
  newDLLname <- file.path(directory, paste(bname, extension, sep = "."))

  file.copy(from = DLLname, to = newDLLname, overwrite = TRUE)

  # accessory file with compiled code information (DLL name has changed)
  fileCF <- file.path(directory, paste(bname, "CFunc", sep = "."))

  attributes(x)$DLL <- newDLLname
  environment(x@.Data)$libLFile <- newDLLname
  environment(x@.Data)$f <- bname

  # names of functions in compiled code
  if (class(x) == "CFunc")
    attributes(x)$fname <- DLL[["name"]]
  else
    attributes(x)$fname <- names(x)

  save(file = fileCF, x)

  invisible(fileCF)
}

## ---------------------------------------------------------------------------

readDynLib <- function(bname, directory = ".") {

  # open all the required files
  fileCF <- file.path(directory, paste(bname, "CFunc", sep = "."))

  if (!file.exists(fileCF))
    stop (fileCF,  " does not exist")

  CF <- get(load(file = fileCF))
  attrs <- attributes(CF)
  DLLname <- attrs$DLL

  if (!file.exists(DLLname))
    stop ("'file' does not point to valid CFunc object: DLL ", DLLname, " does not exist")

# load routines in DLL

  DLL <- dyn.load(DLLname)
  fn <- attributes(CF)$fname
  if (class(CF) == "CFunc") {
    CFi <- CF
    code <- CFi@code
    body(CFi)[[2]] <- getNativeSymbolInfo(fn, DLL)$address
    CF@.Data <- CFi
  } else
    for (i in 1:length(CF))  {
      CFi <- CF[[i]]
      code <- CFi@code
      body(CFi)[[2]] <- getNativeSymbolInfo(fn[i], DLL)$address
      CF[[i]]@.Data <- CFi
    }

  attributes(CF) <- attrs
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



