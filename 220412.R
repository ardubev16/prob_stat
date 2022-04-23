#!/usr/bin/env Rscript

fun1 <- function(x) { ifelse(x>0, 1-(1+x^6)^2.61, 0) }

fun3 <- function(x) { ifelse(x<0, 1/3*exp(x/2.61), 1-1/3*exp(-x/2.61)) }

diff <- function(fun, a, b) { fun(b)-fun(a) }

diff(fun3, -0.88, 1.87)

