#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 7) stop("./canial <VERSION> <lambda> <alpha> <q2_lower> <q2_upper> <q5_max_x> <q5_max_y>")
argn <- as.numeric(args)

VERSION <- argn[1]
lambda <- argn[2]
alpha <- argn[3]
q2_lower <- argn[4]
q2_upper <- argn[5]
q5_max_x <- argn[6]
q5_max_y <- argn[7]


print_q <- function(n, q) {
    library(MASS)
    cat(sprintf("Q%d:\n", n))
    cat(sprintf("    %.7g\t%s\n", q, fractions(q)))
}

version1 <- function() {
# Dati
    # lambda <- 0.88
    # alpha <- 1.05
    
    f_X_c <- function(x) { ifelse(x>0, lambda*x*exp(-lambda*x^2), 0) }
    f_Y <- function(y) { ifelse(y>=0, alpha*exp(-alpha*y), 0) }
    
# Q1
    c <- 1/integrate(f_X_c, 0, Inf)$value
    f_X <- function(x) { c*f_X_c(x) }
    
    print_q(1, c)
    
# Q2
    # q2_lower <- 1.16
    # q2_upper <- 1.94
    
    q2 <- integrate(f_X, q2_lower, q2_upper)$value
    
    print_q(2, q2)
    
# Q3
    to_integrate <- function(y) { y*f_Y(y) }
    m1_y <- integrate(to_integrate, 0, Inf)$value
    
    print_q(3, m1_y)
    
# Q4
    to_integrate <- function(y) { y^2*f_Y(y) }
    m2_y <- integrate(to_integrate, 0, Inf)$value
    
    print_q(4, m2_y)
    
# Q5
    # q5_max_x <- 1.55
    # q5_max_y <- 0.95
    
    q5 <- integrate(f_X, 0, q5_max_x)$value * integrate(f_Y, 0, q5_max_y)$value
    
    print_q(5, q5)
}

version2 <- function() {
# Dati
    # lambda <- 0.88
    # alpha <- 1.05
    
    f_X_c <- function(x) { ifelse(x>0, lambda*x*exp(-lambda*x^2), 0) }
    f_Y <- function(y) { ifelse(y>=0, alpha*exp(-alpha*y), 0) }
    
# Q1
    c <- 1/integrate(f_X_c, 0, Inf)$value
    f_X <- function(x) { c*f_X_c(x) }
    
    print_q(1, c)
    
# Q2
    # q2_lower <- 1.16
    # q2_upper <- 1.94
    
    q2 <- integrate(f_X, q2_lower, q2_upper)$value
    
    print_q(2, q2)
    
# Q3
    to_integrate <- function(y) { y*f_Y(y) }
    m1_y <- integrate(to_integrate, 0, Inf)$value
    
    print_q(3, m1_y)
    
# Q4
    to_integrate <- function(y) { y^2*f_Y(y) }
    m2_y <- integrate(to_integrate, 0, Inf)$value
    
    print_q(4, m2_y)
    
# Q5
    # q5_max_x <- 1.55
    # q5_max_y <- 0.95
    
    q5 <- (1 - integrate(f_X, 0, q5_max_x)$value) * integrate(f_Y, 0, q5_max_y)$value
    
    print_q(5, q5)
}

if (VERSION == 1) version1() else version2()

# Q1:
#     2	2
# Q2:
#     0.2695685	26215/97248
# Q3:
#     0.952381	20/21
# Q4:
#     1.814059	800/441
# Q5:
#     0.5549951	23655/42622
