#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=TRUE)
# if (length(args) != 4) stop("./canial <VERSION> <p> <D> <q2_H>")
argn <- as.numeric(args)

VERSION <- argn[1]
p <- argn[2]
D <- argn[3]
q2_H <- argn[4]

# Dati =====================================================
VERSION <- 1

p <- 0.359      # prob. testa
D <- 8          # facce dado
q2_H <- 7       # teste ottenute q2

Rd <- c(1:D)    # vettore di supporto
Rn <- c(0:D)    # vettore di supporto
# ==========================================================

# Functions ================================================
print_q <- function(n, q) {
    library(MASS)
    cat(sprintf("Q%d:\n", n))
    cat(sprintf("    %.7g\t%s\n", q, fractions(q)))
}

E <- function(X, x) {
    sum(x*X(x))
}
# ==========================================================

version1 <- function() {
# Q1
    p_D_N <- function(d,n) { 1/D*dbinom(n, d, p) }
    p_N <- function(n) {
        res <- 0
        for (d in Rd)
            res <- res + p_D_N(d, n)
        res
    }

    E_N <- E(p_N, Rn)

    print_q(1, E_N)

# Q2
    p_D_H <- function(d) { p_D_N(d, q2_H)/p_N(q2_H) }

    E_H <- E(p_D_H, Rd)

    print_q(2, E_H)

# Q3
    E_ND <- sum(Rd^2*p/D)

    print_q(3, E_ND)
}

version2 <- function() {
# Q1
    p_D_N <- function(d,n) { 1/D*dbinom(n, d, p) }
    p_N <- function(n) {
        res <- 0
        for (d in Rd)
            res <- res + p_D_N(d, n)
        res
    }

    E_N <- E(p_N, Rn)

    print_q(1, E_N)

# Q2
    p_D_H <- function(d) { p_D_N(d, q2_H)/p_N(q2_H) }

    E_H <- E(p_D_H, Rd)

    print_q(2, E_H)

# Q3
    E_ND <- sum(Rd^2*p/D)

    print_q(3, E_ND)
}

if (VERSION == 1) version1() else version2()

