library(dplyr)
library(ggplot2)
library(reshape2)

df <- NULL

for (k in 1:4){
  load(paste0("simulation/results/sim2_power_case", k + 4, ".RData"))
  colnames(results) <- c("n", "K", "W2", "U2", "A2")  
  results$case <- paste("Case", k)
  
  df <- rbind(df, results)
}

df_long <- melt(df, id.vars = c("n", "case"), 
                variable.name = "pc", value.name = "value")

ggplot(df_long, aes(x = n, y = value, color = pc, linetype = pc)) + 
  facet_wrap(~case, scales = "free") +
  geom_line(lwd = 0.6, size = 3, col = "black") +
  scale_linetype_manual(values = c("K" = 3, "W2" = 4, "U2" = 1, "A2" = 2),
                        labels = c(expression(K), expression(W^2), 
                                   expression(U^2), expression(A^2))) +
  labs(x = "Sample size (n)", y = "Power of the test") + 
  ylim(c(0, 1)) + theme_classic() +
  theme(strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        plot.caption = element_text(size = 17, hjust = 0.5),
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        strip.text = element_text(size = 17),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.key.width = unit(1.1, "cm"))

ggsave("output/fig11.pdf", width = 11.5, height = 9, dpi = 320)
