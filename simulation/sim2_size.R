# devtools::install_github("njerezlillo/pldis")
library(msm)
library(pldis)
library(igraph)
library(parallel)
library(doParallel)
source("clauset.R")

FF <- function(n){
  set.seed(2024)

  B <- 20
  xm <- 10
  palpha <- 2.4

  p_1 <- rep(NA, B) # clauset
  p_2 <- rep(NA, B) # ks.test
  p_3 <- rep(NA, B) # cvm.test
  p_4 <- rep(NA, B) # watson
  p_5 <- rep(NA, B) # anderson.darling

  for (i in 1:B){
    x <- poweRlaw::rpldis(n, xmin = xm, alpha = palpha)

    x_grid <- sort(unique(x))
    theo_cdf <- poweRlaw::ppldis(x_grid, xm, palpha)
    step_aux <- stepfun(x_grid, c(0, theo_cdf))

    p_1[i] <- gof_trad_pldis(x, xm, palpha)$p
    p_2[i] <- dgof::ks.test(x, step_aux)$p.value
    p_3[i] <- dgof::cvm.test(x, step_aux, type = "W2")$p.value
    p_4[i] <- dgof::cvm.test(x, step_aux, type = "U2")$p.value
    p_5[i] <- dgof::cvm.test(x, step_aux, type = "A2")$p.value

    cat("| iter:", i, "| n:", n, "| ",
        sprintf(mean(p_1 < .05, na.rm = T), fmt = '%.3f'),
        sprintf(mean(p_2 < .05, na.rm = T), fmt = '%.3f'),
        sprintf(mean(p_3 < .05, na.rm = T), fmt = '%.3f'),
        sprintf(mean(p_4 < .05, na.rm = T), fmt = '%.3f'),
        sprintf(mean(p_5 < .05, na.rm = T), fmt = '%.3f'), "\r")
  }

  list(size_1 = mean(p_1 < .05, na.rm = T),
       size_2 = mean(p_2 < .05, na.rm = T),
       size_3 = mean(p_3 < .05, na.rm = T),
       size_4 = mean(p_4 < .05, na.rm = T),
       size_5 = mean(p_5 < .05, na.rm = T))
}

number_of_cores <- 5
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)
registerDoParallel(cores = number_of_cores)

n_grid <- seq(100, 500, by = 100)
estimates <-
  foreach(n = n_grid, .multicombine = T,
          .packages = c("igraph", "msm", "VGAM")) %dopar% FF(n)

stopCluster(clusters)

results <- cbind(do.call(rbind, lapply(estimates, function(x) {
  data.frame(size_1 = x$size_1, size_2 = x$size_2, size_3 = x$size_3,
             size_4 = x$size_4, size_5 = x$size_5)
})), data.frame(n = n_grid))

save(results, file = "simulation/results/sim2_size_case1.RData")
