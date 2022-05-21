#!/usr/bin/env Rscript

# Dati =====================================================
p_not <- 0.07
biglietti_RC <- 263
posti_RC <- 250
biglietti_RL <- 318
posti_RL <- 300
q3_prob <- 0.95

p <- 1 - p_not
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
# Q1
    q1 <- pbinom(posti_RC, biglietti_RC, p, lower.tail=FALSE)

    print_q(1, q1)

# Q2
    q2 <- pbinom(posti_RL, biglietti_RL, p)

    print_q(2, q2)

# Q3
    RC_n <- posti_RC
    found <- FALSE
    while (!found) {
        RC_n <- RC_n + 1
        if (pbinom(posti_RC, RC_n, p) <= q3_prob) {
            found <- TRUE
            res <- RC_n - 1
        }
    }

    print_q(3, res)

# Q4
    E_RL <- biglietti_RL*p

    print_q(4, E_RL)
}

version()
