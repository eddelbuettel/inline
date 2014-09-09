## ---------------------------------------------------------------------------
# saving and loading CFunc objects (called write and read as it needs to
# be assigned.

writeDynLib <- function(x, file) {

  DLL <- getDynLib(x)
   
  if (is.null(DLL))
    stop ("'x' DLL not loaded")
    
  DLLname <- DLL[["path"]] 
  if (!file.exists(DLLname))
    stop ("'x' does not point to an existing DLL")

  # correct extension of filename  (dll, so)  
  dname <- dirname(file)
  bname <- unlist(strsplit(basename(file), ".", fixed = TRUE))[1]
  extension <- unlist(strsplit(basename(DLLname), ".", fixed = TRUE))[2]
  file <- paste(dname,bname, extension, sep = ".")

  try(dyn.unload(file), silent = TRUE)

  file.copy(from = DLLname, to = file, overwrite = TRUE)

  # accessory file with compiled code information (DLL name has changed)
  fileCF <- paste(dname,"/",bname, ".Cfunc", sep = "")
  attributes(x)$DLL <- file
  
  # names of functions in compiled code
  if (class(x) == "CFunc")
    attributes(x)$fname <- DLL[["name"]]
  else
    attributes(x)$fname <- names(x)
    
  save(file = fileCF, x)
}

## ---------------------------------------------------------------------------

readDynLib <- function(file) {

# open all the required files
  extension <- unlist(strsplit(basename(file), ".", fixed = TRUE))[2]

  if (is.na(extension)) {
    extension <- "CFunc"
    file <- paste(file, extension, sep = ".")
  }
    
  if (extension != "CFunc")
    stop ("'file' should point to a CFunc object, extension '.CFunc'")

  if (!file.exists(file))
    stop ("'file' does not exist")

  CF <- get(load(file = file))
  attrs <- attributes(CF)
  DLLname <- attrs$DLL
    
  if (!file.exists(DLLname))
    stop ("'file' does not point to valid CFunc object: DLL ", DLLname, " does not exist")

#    cleanup <- function(env) {
#        unlink(DLLname)
#    }
#    reg.finalizer(environment(), cleanup, onexit = TRUE)


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



