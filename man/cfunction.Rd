\name{cfunction}

\alias{cfunction}
\alias{setCMethod}

\concept{inline function call}

\title{ Inline C, C++, Fortran function calls from R }

\description{
  Functionality to dynamically define R functions and S4 methods with in-lined C, 
  C++ or Fortran code supporting .C and .Call calling conventions.
}

\usage{
  cfunction(sig=character(), body=character(), includes=character(),
            otherdefs=character(), 
            language=c("C++", "C", "Fortran", "F95", "ObjectiveC", "ObjectiveC++"),
            verbose=FALSE, 
            convention=c(".Call", ".C", ".Fortran"),
            Rcpp=FALSE,
            cppargs=character(), cxxargs=character(), libargs=character(),
            dim=NULL, implicit=NULL, module=NULL, name=NULL)

  ## S4 methods for signatures
  #  f='character', sig='list', body='list'
  #  f='character', sig='character', body='character'
  
  setCMethod(f, sig, body, ...)

  ## Further arguments:
  #  setCMethod(f, sig, body, includes="", otherdefs="", cpp=TRUE,
  #  verbose=FALSE, where=topenv(.GlobalEnv), ...)
}

\arguments{
  \item{f}{A single character value if \code{sig} and \code{body} are character vectors
    or a character vector of the same length and the length of \code{sig} or
    \code{body} with the name(s) of methods to create.}
    
  \item{sig}{A match of formal argument names for the function with the
    character-string names of corresponding classes. Alternatively, 
    a list of such character vectors. }

  \item{body}{ A character vector with C, C++ or Fortran code omitting function
    declaration (only the body, i.e. in case of C starting after the function 
    opening curly bracket and ending before the closing curly bracket, 
    brackets excluded). In case of \code{setCMethod} with signature
    \code{list} -- a list of such character vectors. } 

  \item{includes}{ A character vector of additional includes and preprocessor
    statements etc that will be put between the R includes and the user function(s).}

  \item{otherdefs}{ A characted vector with the code for any further definitions of 
    functions, classes, types, forward declarations, namespace usage clauses etc 
    which is inserted between the includes and the declarations of the functions
    defined in \code{sig}.}

  \item{language}{ A character value that specifies the source language of the
    inline code. The possible values for \code{language} include all those 
    supported by \code{R CMD SHLIB} on any platform, which are currently C, 
    C++, Fortran, F95, ObjectiveC and ObjectiveC++; they may not all be supported
    on your platform. One can specify the language either in full as above, or 
    using any of the following case insensitive shortened forms: \code{c, cpp, 
    c++, f, f95, objc, objcpp, objc++}. Defaults to \code{C++}.}
    
  \item{verbose}{ If \code{TRUE} prints the compilation output, the source
    code of the resulting program and the definitions of all declared
    methods. If \code{FALSE}, the function is silent, but it prints compiler
    warning and error messages and the source code if compilation fails. }
    
  \item{convention}{ Which calling convention to use?  See the Details section.}

  \item{Rcpp}{If \code{TRUE} adds inclusion of \code{Rcpp.h} to
  \code{includes}, also queries the \code{Rcpp} package about
  the location of header and library files and sets environment
  variables \code{PKG_CXXFLAGS} and \code{PKG_LIBS} accordingly so that
  the R / C++ interface provided by the \code{Rcpp} package can be
  used. Default value is \code{FALSE}.}

  \item{cppargs}{Optional character vector of tokens to be passed to
  the compiler via the \code{PKG_CPPFLAGS} environment
  variable. Elements should be fully formed as for example
  \code{c("-I/usr/local/lib/foo", "-DDEBUG")} and are passed along verbatim.}

  \item{cxxargs}{Optional character vector of tokens to be passed to
  the compiler via the \code{PKG_CXXFLAGS} environment
  variable. Elements should be fully formed as for example
  \code{c("-I/usr/local/lib/foo", "-DDEBUG")} and are passed along verbatim.}

  \item{libargs}{Optional character vector of tokens to be passed to the
  compiler via the \code{PKG_LIBS} environment variable. Elements should
  be fully formed as for example \code{c("-L/usr/local/lib/foo -lfoo",
  "--lpthread")} and are passed along verbatim.}
    
  \item{dim}{Optional character vector defining the dimensionality of the 
  function arguments. Of same length as \code{sig}. Fortran or F95 only.}
    
  \item{implicit}{A character vector defining the implicit declaration in 
  Fortran or F95; the default is to use the implicit typing rules for Fortran, 
  which is \code{integer} for names starting with the letters \code{I} through
  \code{N}, and \code{real} for names beginning with any other letter.
  As \code{R} passes double precision, this is not the best choice. 
  Safest is to choose \code{implicit = "none"} which will require all names
  in the subroutine to be explicitly declared.}

  \item{module}{Name(s) of any modules to be used in the \code{Fortran} or 
  \code{F95} subroutine.}

  \item{name}{Function name to be used in the code. Only used if 
    \code{sig} is not a list.}

  \item{...}{ Reserved.}
}

\value{
  If \code{sig} is a single character vector, \code{cfunction} returns a single
  \code{\link{function}}; if it is a list, it returns a list of functions.
  
  \code{setCMethod} declares new methods with given names and signatures and
  returns invisible \code{NULL}.
}

\details{
  
  To declare multiple functions in the same library one can use \code{setCMethod}
  supplying lists of signatures and implementations. In this case, provide as
  many method names in \code{f} as you define methods. Avoid clashes when selecting
  names of the methods to declare, i.e. if you provide the same name several times
  you must ensure that signatures are different but can share the same generic!

  The source code in the \code{body} should not include the header or
  "front-matter" of the function or the close, e.g. in C or C++ it
  must start after the C-function opening curly bracket and end before
  the C-function closing curly bracket, brackets should not be
  included. The header will be automatically generated from the R-\code{signature} 
  argument. Arguments will will carry the same name as used in the signature, 
  so avoid variable names that are not legal in the target language 
  (e.g. names with dots).
  
  C/C++: If \code{convention == ".Call"} (the default), the \code{\link{.Call}} mechanism 
  is used and its result is returned directly as the result of the call of the 
  generated function.  As the last line of the generated C/C++ code a 
  \code{return R_NilValue;} is added in this case and a warning is generated 
  in case the user has forgotten to provide a return value. To suppress the 
  warning and still return NULL, add \code{return R_NilValue;} explicitly.
  
  Special care is needed with types, memory allocation and protection
  -- exactly the same as if the code was not inline: see the
  Writing R Extension manual for information on \code{\link{.Call}}.  
  
  If \code{convention == ".C"} or \code{convention == ".Fortran"}, the
  \code{\link{.C}} or \code{\link{.Fortran}} mechanism respectively is
  used, and the return value is a list containing all arguments.
  
  Attached R includes include \code{R.h} for \code{".C"}, and
  additionally \code{Rdefines.h} and \code{R_ext\\Error.h} for
  \code{".Call"}.
}

\seealso{ \code{
    \link{Foreign} Function Interface
}}

\examples{

x <- as.numeric(1:10)
n <- as.integer(10)

\dontrun{
## A simple Fortran example - n and x: assumed-size vector
code <- "
      integer i
      do 1 i=1, n(1)
    1 x(i) = x(i)**3
"
cubefn <- cfunction(signature(n="integer", x="numeric"), code, convention=".Fortran")
print(cubefn)

cubefn(n, x)$x

## Same Fortran example - now n is one number
code2 <- "
      integer i
      do 1 i=1, n 
    1 x(i) = x(i)**3
"
cubefn2 <- cfunction(signature(n="integer", x="numeric"), implicit = "none", 
  dim = c("", "(*)"), code2, convention=".Fortran")

cubefn2(n, x)$x

## Same in F95, now x is fixed-size vector (length = n)
code3 <- "x = x*x*x"
cubefn3 <- cfunction(sig = signature(n="integer", x="numeric"), implicit = "none", 
  dim = c("", "(n)"), code3, language="F95")
cubefn3(20, 1:20)
print(cubefn3)

## Same example in C
code4 <- "
      int i;
      for (i = 0; i < *n; i++) 
        x[i] = x[i]*x[i]*x[i];
"
cubefn4 <- cfunction(signature(n="integer", x="numeric"), code4, language = "C", convention = ".C")
cubefn4(20, 1:20)

}

 ## use of a module in F95
modct <- "module modcts
double precision, parameter :: pi = 3.14159265358979
double precision, parameter :: e = 2.71828182845905
end"

getconstants <- "x(1) = pi
x(2) = e"

cgetcts <- cfunction(getconstants, module = "modcts", implicit = "none",
  includes = modct, sig = c(x = "double"), dim = c("(2)"), language = "F95")

cgetcts(x = 1:2)  
print(cgetcts)

## Use of .C convention with C code
## Defining two functions, one of which calls the other
sigSq <- signature(n="integer", x="numeric")
codeSq <- "
  for (int i=0; i < *n; i++) {
    x[i] = x[i]*x[i];
  }"
sigQd <- signature(n="integer", x="numeric")
codeQd <- "
  squarefn(n, x);
  squarefn(n, x);
"

fns <- cfunction( list(squarefn=sigSq, quadfn=sigQd), 
                  list(codeSq, codeQd), 
                  convention=".C")

squarefn <- fns[["squarefn"]]
quadfn <- fns[["quadfn"]]

squarefn(n, x)$x
quadfn(n, x)$x

## Alternative declaration using 'setCMethod'
setCMethod(c("squarefn", "quadfn"), list(sigSq, sigQd), 
           list(codeSq, codeQd), convention=".C")
           
squarefn(n, x)$x
quadfn(n, x)$x

## Use of .Call convention with C code
## Multyplying each image in a stack with a 2D Gaussian at a given position
code <- "
  SEXP res;
  int nprotect = 0, nx, ny, nz, x, y;
  PROTECT(res = Rf_duplicate(a)); nprotect++;
  nx = INTEGER(GET_DIM(a))[0];
  ny = INTEGER(GET_DIM(a))[1];
  nz = INTEGER(GET_DIM(a))[2];
  double sigma2 = REAL(s)[0] * REAL(s)[0], d2 ;
  double cx = REAL(centre)[0], cy = REAL(centre)[1], *data, *rdata;
  for (int im = 0; im < nz; im++) {
    data = &(REAL(a)[im*nx*ny]); rdata = &(REAL(res)[im*nx*ny]);
    for (x = 0; x < nx; x++)
      for (y = 0; y < ny; y++) {
        d2 = (x-cx)*(x-cx) + (y-cy)*(y-cy);
        rdata[x + y*nx] = data[x + y*nx] * exp(-d2/sigma2);
      }
  }
  UNPROTECT(nprotect);
  return res;
"
funx <- cfunction(signature(a="array", s="numeric", centre="numeric"), code)

x <- array(runif(50*50), c(50,50,1))
res <- funx(a=x, s=10, centre=c(25,15))
if (interactive()) image(res[,,1])

## Same but done by registering an S4 method
setCMethod("funy", signature(a="array", s="numeric", centre="numeric"), code, verbose=TRUE)

res <- funy(x, 10, c(35,35))
if (interactive()) { x11(); image(res[,,1]) }

}

\author{
  Oleg Sklyar, Duncan Murdoch, Mike Smith, Dirk Eddelbuettel
}

\keyword{file}

