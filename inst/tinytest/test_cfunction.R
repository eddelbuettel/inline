library(inline)

isSolaris <- Sys.info()[["sysname"]] == "SunOS"

n <- 10L
x <- 1:10

## A simple Fortran example - n and x: assumed-size vector
code <- "
      integer i
      do 1 i=1, n(1)
    1 x(i) = x(i)**3
"

cubefn <- cfunction(signature(n = "integer", x = "numeric"), code,
  convention = ".Fortran")

res_cube <- list(
  n = 10L,
  x = c(1, 8, 27, 64, 125, 216, 343, 512, 729, 1000))

res_1 <- cubefn(n, x)
expect_identical(res_cube, res_1)

cubefn_named <- cfunction(signature(n = "integer", x = "numeric"), code,
  convention = ".Fortran", name = "cubefn")
expect_identical(cubefn_named(n, x), res_1)

expect_true(grepl("cubefn", cubefn_named@code))

## Same Fortran example - now n is one number
code2 <- "
      integer i
      do 1 i=1, n
    1 x(i) = x(i)**3
"
cubefn2 <- cfunction(signature(n = "integer", x = "numeric"),
  implicit = "none", dim = c("", "(*)"), code2, convention=".Fortran")

res_2 <- cubefn2(n, x)
expect_identical(res_2, res_cube)

## Same in F95, now x is fixed-size vector (length = n)
code3 <- "x = x*x*x"
cubefn3 <- cfunction(signature(n = "integer", x = "numeric"),
  implicit = "none", dim = c("", "(n)"), code3, language="F95")
res_3 <- cubefn3(n, x)
expect_identical(res_3, res_cube)

## Same example in C
code4 <- "
      int i;
      for (i = 0; i < *n; i++)
        x[i] = x[i]*x[i]*x[i];
"
cubefn4 <- cfunction(signature(n = "integer", x = "numeric"), code4,
  language = "C", convention = ".C")
res_4 <- cubefn4(n, x)
expect_identical(res_4, res_cube)

if (isSolaris) exit_file("Skip remainder")

## use of a module in F95
modct <- "module modcts
double precision, parameter :: pi = 3.14159265358979
double precision, parameter :: e = 2.71828182845905
end"

getconstants <- "x(1) = pi
x(2) = e"

cgetcts <- cfunction(body = getconstants, module = "modcts", implicit = "none",
  includes = modct, sig = c(x = "double"), dim = c("(2)"), language = "F95")

res_5 <- cgetcts(x = c(1, 2))
expect_equal(res_5$x, c(pi, exp(1)), tolerance = 1e-7)

## Use of .C convention with C code
## Defining two functions, one of which calls the other
sigSq <- signature(n = "integer", x = "numeric")
codeSq <- "
  for (int i=0; i < *n; i++) {
    x[i] = x[i]*x[i];
  }"
sigQd <- signature(n = "integer", x = "numeric")
codeQd <- "
  squarefn(n, x);
  squarefn(n, x);
"

fns <- cfunction(
  sig = list(squarefn = sigSq, quadfn = sigQd),
  body = list(codeSq, codeQd),
  convention = ".C")

res_square <- list(
  n = 10L,
  x = c(1, 4, 9, 16, 25, 36, 49, 64, 81, 100))

res_quad <- list(
  n = 10L,
  x = c(1, 16, 81, 256, 625, 1296, 2401, 4096, 6561, 10000))

res_6_square <- fns[["squarefn"]](n, x)
res_6_quad <- fns[["quadfn"]](n, x)

expect_identical(res_6_square, res_square)
expect_identical(res_6_quad, res_quad)

## Alternative declaration using 'setCMethod'
setCMethod(c("squarefn", "quadfn"), list(sigSq, sigQd),
           list(codeSq, codeQd), convention = ".C")

res_7_square <- squarefn(n, x)
res_7_quad <- quadfn(n, x)

expect_identical(res_7_square, res_square)
expect_identical(res_7_quad, res_quad)
