FF <- function (z) {
  all_networks <- dir("application/icon/data")
  
  tbl <- data.frame(Data = z, n = NA, xmin = NA, alpha = NA,
                    K = NA, W2 = NA, U2 = NA, A2 = NA)
  
  etiq <- paste0("application/icon/data/", all_networks[z])
  my_data <- read.delim(etiq, sep = ",")
  x <- rep(my_data$xvalue, my_data$counts)
  x <- x[x > 0]
  
  mle <- fit_pldis(x, bayesian = F)
  bay <- fit_pldis(x, bayesian = T)
  
  tbl[1, 2] <- length(x)
  tbl[1, 3] <- as.character(bay$xmin)
  tbl[1, 4] <- bay$alpha
  tbl[1, 5] <- plpva(x, xmin =  mle$xmin, vec = seq(1.5, 3.5, .01))$p
  
  x_grid <- sort(unique(x[x >= bay$xmin]))
  est_cdf <- poweRlaw::ppldis(x_grid, bay$xmin, bay$alpha)
  step_aux <- stepfun(x_grid, c(0, est_cdf))
  
  tbl[1, 6] <- dgof::cvm.test(x[x >= bay$xmin], step_aux, type = "W2")$p.value
  tbl[1, 7] <- dgof::cvm.test(x[x >= bay$xmin], step_aux, type = "U2")$p.value
  tbl[1, 8] <- dgof::cvm.test(x[x >= bay$xmin], step_aux, type = "A2")$p.value
  
  cat("| iter:", z, "|", "\n")
  
  return(tbl)
}