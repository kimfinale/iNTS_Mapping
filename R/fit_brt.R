fit_brt = function(dat, niter, hyper_params, iter_per_param=1){

  fits <- vector("list", niter*iter_per_param)

  for (i in 1:niter) {
    for (j in 1:iter_per_param) {
      cat("i =", i, ", j =", j, ", time =", tstamp(hour=T, minute=T), "\n")
      set.seed(j)

      fits[[(i-1)*iter_per_param+j]] <-
        dismo::gbm.step(data = dat,
                        gbm.x = c(2:ncol(dat)),
                        gbm.y = 1,
                        family = "bernoulli",
                        tree.complexity = hyper_params$tc[i],
                        learning.rate = hyper_params$lr[i],
                        bag.fraction = hyper_params$bf[i],
                        silent = TRUE,
                        plot.main = FALSE)
    }
  }

  return(fits)
}
