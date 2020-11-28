## inline: Inline C, C++ and Fortran code from R

[![Build Status](https://travis-ci.org/eddelbuettel/inline.png)](https://travis-ci.org/eddelbuettel/inline) 
[![Build Status](https://github.com/eddelbuettel/inline/workflows/ci/badge.svg)](https://github.com/eddelbuettel/inline/actions?query=workflow%3Aci)
[![License](https://img.shields.io/badge/license-LGPL%20%28%3E%3D%202%29-brightgreen)](https://www.gnu.org/licenses/lgpl-3.0.html) 
[![CRAN](https://www.r-pkg.org/badges/version/inline)](https://cran.r-project.org/package=inline) 
[![CRAN use](https://jangorecki.gitlab.io/rdeps/inline/CRAN_usage.svg?sanitize=true)](https://cran.r-project.org/package=inline) 
[![CRAN indirect](https://jangorecki.gitlab.io/rdeps/inline/indirect_usage.svg?sanitize=true)](https://cran.r-project.org/package=inline)  
[![Dependencies](https://tinyverse.netlify.com/badge/inline)](https://cran.r-project.org/package=inline) 
[![Downloads](https://cranlogs.r-pkg.org/badges/inline?color=brightgreen)](https://www.r-pkg.org/pkg/inline) 
[![Debian package](https://img.shields.io/debian/v/r-cran-inline/sid?color=brightgreen)](https://packages.debian.org/sid/r-cran-inline)
[![Last Commit](https://img.shields.io/github/last-commit/eddelbuettel/inline)](https://github.com/eddelbuettel/inline)

### About

The inline package provides functionality to dynamically define R functions
(and corresponding S4 objects) from in-line C, C++ or Fortran code. It
supports the `.C`, `.Call` and `.Fortran` calling conventions.

### History

The package was originally written while Oleg Sklyar was at
[EMBL-EBI](https://www.ebi.ac.uk/).  It was then extended by Dirk
Eddelbuettel and Romain Francois for use by
[Rcpp](https://dirk.eddelbuettel.com/code/rcpp.html). Years later, Karline
Soetaert added support for Fortran. Johannes Ranke refactored some internals
and added the ability to store and retrieve compiled code.

### Authors

Oleg Sklyar, Dirk Eddelbuettel, Romain Francois, Karline Soetaert, Johannes Ranke

### Maintainer

Dirk Eddelbuettel

### License

LGPL (>= 2)
