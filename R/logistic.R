logistic <- function(L = 1, x, x0, k){
    return (L/(1 + exp(-k*(x-x0))))
}

# Logistic function may be used to model a 1 - probability of sampling of a case if
# case existed, where x be the distance from a healthcare facility. 
# This can then be used to sample background points for species distribution
# modelling
# One potential parameter set would be: x0=50, k=0.1 
# 

