library(dplyr)
library(tidyr)
library(xtable)
library(patchwork)

# Case 1 ------------------------------------------------------------------

load("simulation/results/sim4_size_case5.RData")

K_case1 <- sprintf(results$size_1, fmt = '%.2f')
W_case1 <- sprintf(results$size_2, fmt = '%.2f')
U_case1 <- sprintf(results$size_3, fmt = '%.2f')
A_case1 <- sprintf(results$size_4, fmt = '%.2f')

# Case 2 ------------------------------------------------------------------

load("simulation/results/sim4_size_case6.RData")

K_case2 <- sprintf(results$size_1, fmt = '%.2f')
W_case2 <- sprintf(results$size_2, fmt = '%.2f')
U_case2 <- sprintf(results$size_3, fmt = '%.2f')
A_case2 <- sprintf(results$size_4, fmt = '%.2f')

# Case 3 ------------------------------------------------------------------

load("simulation/results/sim4_size_case7.RData")
size_c3 <- results 
load("simulation/results/sim4_power_case7.RData")
powr_c3 <- results

K_case3 <- paste0(sprintf(size_c3$size_1, fmt = '%.2f'),
                  " (", sprintf(powr_c3$power_1, fmt = '%.2f'), ")")
W_case3 <- paste0(sprintf(size_c3$size_2, fmt = '%.2f'),
                  " (", sprintf(powr_c3$power_2, fmt = '%.2f'), ")")
U_case3 <- paste0(sprintf(size_c3$size_3, fmt = '%.2f'),
                  " (", sprintf(powr_c3$power_3, fmt = '%.2f'), ")")
A_case3 <- paste0(sprintf(size_c3$size_4, fmt = '%.2f'),
                  " (", sprintf(powr_c3$power_4, fmt = '%.2f'), ")")

# Case 4 ------------------------------------------------------------------

load("simulation/results/sim4_size_case8.RData")
size_c4 <- results 
load("simulation/results/sim4_power_case8.RData")
powr_c4 <- results

K_case4 <- paste0(sprintf(size_c4$size_1, fmt = '%.2f'),
                  " (", sprintf(powr_c4$power_1, fmt = '%.2f'), ")")
W_case4 <- paste0(sprintf(size_c4$size_2, fmt = '%.2f'),
                  " (", sprintf(powr_c4$power_2, fmt = '%.2f'), ")")
U_case4 <- paste0(sprintf(size_c4$size_3, fmt = '%.2f'),
                  " (", sprintf(powr_c4$power_3, fmt = '%.2f'), ")")
A_case4 <- paste0(sprintf(size_c4$size_4, fmt = '%.2f'),
                  " (", sprintf(powr_c4$power_4, fmt = '%.2f'), ")")

# Table -------------------------------------------------------------------

print(xtable(
  cbind(
    data.frame(n = paste(results$n)),
    data.frame(K_case1), data.frame(W_case1), 
    data.frame(U_case1), data.frame(A_case1),
    data.frame(NA),
    data.frame(K_case2), data.frame(W_case2), 
    data.frame(U_case2), data.frame(A_case2)
  )
), include.rownames = FALSE)


print(xtable(
  cbind(
    data.frame(n = paste(results$n)),
    data.frame(K_case3), data.frame(W_case3), 
    data.frame(U_case3), data.frame(A_case3),
    data.frame(NA),
    data.frame(K_case4), data.frame(W_case4), 
    data.frame(U_case4), data.frame(A_case4)
  )
), include.rownames = FALSE)
