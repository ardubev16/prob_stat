#!/usr/bin/env Rscript

fun <- function(x) {
    if (x<0) {
        0
    } else {
        1-exp(-1.14*x)
    }
}

diff <- function(a,b,F) {
    F(b) - F(a)
}

diff(-0.868, 0.868, fun)
diff(0.362, 0.525, fun)
diff(1.23, 2.031, fun)
diff(0.362, 1.23, fun) + diff(2.031, 2.489, fun)
