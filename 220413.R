#!/usr/bin/env Rscript

F_x <- function(x) {
    if (x<=0) {
        0
    } else if (x>9.5) {
        1
    } else {
        4/361*x^2
    }
}

F_inv <- function(x) { sqrt(x/(4/361)) }

diff <- function(fun, a, b) { fun(b) - fun(a) }

diff(F_x, -2.65, 2.28)
F_inv(F_x(2.37) + 0.81)
