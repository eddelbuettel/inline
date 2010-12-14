setClass( "C++Signature", 
    representation( signature = "character" )
)

cxxsignature <- function(...){
    value <- list(...)
    names <- names(value)
    for (i in seq_along(value)) {
        sigi <- el(value, i)
        if (!is.character(sigi) || length(sigi) != 1L) 
            stop(gettextf("bad class specified for element %d (should be a single character string)", 
                i), domain = NA)
    }
    value <- as.character(value)
    names(value) <- names
    new( "C++Signature", signature = value )
}

