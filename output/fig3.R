library(dplyr)
library(ggplot2)
library(reshape2)

df <- NULL

for (k in 1:4){
  load(paste0("simulation/results/sim2_size_case", k, ".RData"))
  colnames(results) <- c("n", "K", "W2", "U2", "A2")  
  results$case <- paste("Case", k)
  
  df <- rbind(df, results)
}

df_long <- melt(df, id.vars = c("n", "case"), 
                variable.name = "pc", value.name = "value")

ggplot(df_long, aes(x = n, y = value)) + 
  facet_wrap(~case, scales = "free") +
  geom_segment(x = 100, xend = 500, y = 0.0365, yend = 0.0365, 
               color = "gray40", linetype = 3, lwd = 0.5) +
  geom_segment(x = 100, xend = 500, y = 0.0635, yend = 0.0635, 
               color = "gray40", linetype = 3, lwd = 0.5) +
  geom_point(aes(shape = pc), size = 3) +
  scale_shape_manual(values = c("K" = 1, "W2" = 2, "U2" = 3, "A2" = 4),
                     labels = c(expression(K), expression(W^2), 
                                expression(U^2), expression(A^2))) +
  ylim(0, 0.43) + labs(x = "Sample size (n)", y = "Size of the test") + 
  theme_classic() +
  theme(strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        plot.caption = element_text(size = 17, hjust = 0.5),
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        strip.text = element_text(size = 17),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 15))

ggsave("output/fig3.pdf", width = 11.5, height = 9, dpi = 320)
