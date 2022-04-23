#!/usr/bin/env Rscript

f <- function(x) {
    2*x^3-21*x^2+60*x-36
}

vals <- c(1:8)
writeLines(paste(vals, ":", f(vals)))
