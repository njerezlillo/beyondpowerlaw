# devtools::install_github("njerezlillo/pldis")
library(pldis)
library(poweRlaw)
library(parallel)
library(doParallel)

n_grid <- seq(10, 100, by = 10)

FF <- function (n) {
  set.seed(2024)

  xm <- 4
  palpha <- 4
  pp <- 0.05
  B <- 2000

  o <- 0
  ite <- 0
  lim1 <- 1.01
  lim2 <- 20

  est1 <- rep(0, times = B)
  est2 <- rep(0, times = B)
  est3 <- rep(0, times = B)
  est4 <- rep(0, times = B)

  pc1 <- rep(0, times = B)
  pc2 <- rep(0, times = B)
  pc3 <- rep(0, times = B)
  pc4 <- rep(0, times = B)

  while (o < B) {
    df <- poweRlaw::rpldis(n, xm, palpha)

    amle <- 1 + n * sum(log(df / (xm - 0.5))) ^ -1
    mle <- try(optimize(f = function(z) loglik_pldis(z, xm, df), interval = c(lim1, lim2), maximum = T)$`maximum`)
    apost1 <- try(optimize(f = function(z) post_pldis(z, xm, df), interval = c(lim1, lim2), maximum = T)$`maximum`)
    apost2 <- try(optimize(f = function(z) post2_pldis(z, xm, df), interval = c(lim1, lim2), maximum = T)$`maximum`)

    # puntual
    est1[o] <- mle
    est2[o] <- amle
    est3[o] <- apost1
    est4[o] <- apost2

    fun_aux_1 <- Vectorize(function(x) exp(post_pldis(x, xm, df)), "x")
    fun_aux_2 <- Vectorize(function(x) exp(post2_pldis(x, xm, df)), "x")

    dx_1 <- integrate(fun_aux_1, lower = lim1, upper = lim2)$value
    dx_2 <- integrate(fun_aux_2, lower = lim1, upper = lim2)$value

    integrate(fun_aux_1, lower = lim1, upper = lim2)$value/dx_1
    integrate(fun_aux_2, lower = lim1, upper = lim2)$value/dx_2

    l_aux_1 <- function (x) {
      integrate(fun_aux_1, lower = lim1, upper = x)$value/dx_1 - pp/2
    }

    s_aux_1 <- function (x) {
      integrate(fun_aux_1, lower = x, upper = lim2)$value/dx_1 - pp/2
    }

    cred_post_1 <- c(try(uniroot(l_aux_1, c(lim1, lim2), tol = 1e-10)$root),
                     try(uniroot(s_aux_1, c(lim1, lim2), tol = 1e-10)$root))

    l_aux_2 <- function (x) {
      integrate(fun_aux_2, lower = lim1, upper = x)$value/dx_2 - pp/2
    }

    s_aux_2 <- function (x) {
      integrate(fun_aux_2, lower = x, upper = lim2)$value/dx_2 - pp/2
    }

    cred_post_2 <- c(try(uniroot(l_aux_2, c(lim1, lim2), tol = 1e-10)$root),
                     try(uniroot(s_aux_2, c(lim1, lim2), tol = 1e-10)$root))

    # se
    se.mle <- sqrt((exp(Jprior_pldis(mle, xm))^2 * n)^-1)
    se.amle <- sqrt((exp(Jprior_pldis(amle, xm))^2  * n)^-1)

    # cp
    if(is.double(mle) & is.double(amle) &
       is.double(apost1) & is.double(apost1)) {
      o <- o + 1
      cc <- qnorm(0.975)
      li.mle <- mle - cc * se.mle
      ls.mle <- mle + cc * se.mle

      li.amle <- amle - cc * se.amle
      ls.amle <- amle + cc * se.amle

      if (li.mle <= palpha & ls.mle >= palpha) pc1[o] <- 1
      if (li.amle <= palpha & ls.amle >= palpha) pc2[o] <- 1
      if (cred_post_1[1] <= palpha & cred_post_1[2] >= palpha) pc3[o] <- 1
      if (cred_post_2[1] <= palpha & cred_post_2[2] >= palpha) pc4[o] <- 1

    }
    ite <- ite + 1
  }

  bias <- c(mean(est1 - palpha),
            mean(est2 - palpha),
            mean(est3 - palpha),
            mean(est4 - palpha))

  mse <- c(mean((est1 - palpha) ^ 2),
           mean((est2 - palpha) ^ 2),
           mean((est3 - palpha) ^ 2),
           mean((est4 - palpha) ^ 2))

  cp <- c(mean(pc1), mean(pc2), mean(pc3), mean(pc4))

  return(list(bias = round(bias, 3), mse = round(mse, 3), cp = round(cp, 3)))
}

number_of_cores <- 6
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)
registerDoParallel(cores = number_of_cores)
estimates <- foreach(n = n_grid,
  .multicombine = TRUE,
  .packages = c("poweRlaw", "VGAM")) %dopar% FF(n)

results <-
  do.call(rbind, lapply(estimates, function(x) {
  data.frame(Bias = x$bias, MSE = x$mse, CP = x$cp)
  }))

results$n <- rep(n_grid, each = 4)
results$method <- c("MLE", "AMLE", "MAP1", "MAP2")

save(results, file = "simulation/results/sim1_case1.RData")
