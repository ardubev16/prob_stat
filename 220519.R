#!/usr/bin/env Rscript

# Dati =====================================================
X <- c(-2, 4.5, 8.5)
Y <- c(-3, -2.5, 6)
p_X.data <- c(0.2, 0.3, 0.5)
cond.data <- c(0.28, 0.08, 0.64, 0.22, 0.35, 0.43, 0.05, 0.16, 0.79)
q1_x <- 8.5
q1_y <- -3

cond <- matrix(cond.data, nrow=3, ncol=3, byrow=TRUE)
# ==========================================================

# Functions ================================================
print_q <- function(n, q) {
    library(MASS)
    cat(sprintf("Q%d:\n", n))
    cat(sprintf("    %.7g\t%s\n", q, fractions(q)))
}

E_d <- function(X, x) {
    sum(x*X(x))
}
E2_d <- function(X, x) {
    sum(x^2*X(x))
}
Var_d <- function(X, x) {
    E2_d(X,x) - E_d(X,x)^2
}
E_d_vv <- function(XY, x, y) {
    res <- 0
    for (xs in x)
        res <- res + xs*sum(y*XY(xs,y))
    res
}

E_c <- function(X, lower, upper) {
    integrand <- function(x) { x*X(x) }
    integrate(integrand, lower, upper)$value
}
# ==========================================================

version <- function() {
    p_X <- function(x) { p_X.data[match(x, X)] }
    p_YX <- function(y, x) { cond[match(x, X), match(y, Y)] }
    p_X_Y <- function(x, y) { p_YX(y, x)*p_X(x) }
    p_Y <- function(y) {
        res <- 0
        for (x in X)
            res <- res + p_X_Y(x, y)
        res
    }
# Q1
    q1 <- p_X_Y(q1_x, q1_y)

    print_q(1, q1)

# Q2
    E_X <- E_d(p_X, X)
    E_Y <- E_d(p_Y, Y)
    E_XY <- E_d_vv(p_X_Y, X, Y)

    Cov_XY <- E_XY - E_X*E_Y

    print_q(2, Cov_XY)

# Q3
    Var_X <- Var_d(p_X, X)
    Var_Y <- Var_d(p_Y, Y)
    
    rho_XY <- Cov_XY/sqrt(Var_X*Var_Y)

    print_q(3, rho_XY)
}

version()
