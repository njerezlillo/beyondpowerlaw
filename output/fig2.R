library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

load("simulation/results/sim1_case1.RData")
results_long1 <- results %>% 
  pivot_longer(cols = c(Bias, MSE, CP),
               names_to = "metric", values_to = "value") %>% 
  mutate(metric = factor(metric, levels = c("Bias", "MSE", "CP")),
         method = factor(method, levels = c("MLE", "AMLE", "MAP1", "MAP2")))

load("simulation/results/sim1_case2.RData")
results_long2 <- results %>% 
  pivot_longer(cols = c(Bias, MSE, CP),
               names_to = "metric", values_to = "value") %>% 
  mutate(metric = factor(metric, levels = c("Bias", "MSE", "CP")),
         method = factor(method, levels = c("MLE", "AMLE", "MAP1", "MAP2")))


load("simulation/results/sim1_case3.RData")
results_long3 <- results %>% 
  pivot_longer(cols = c(Bias, MSE, CP),
               names_to = "metric", values_to = "value") %>% 
  mutate(metric = factor(metric, levels = c("Bias", "MSE", "CP")),
         method = factor(method, levels = c("MLE", "AMLE", "MAP1", "MAP2")))

load("simulation/results/sim1_case4.RData")
results_long4 <- results %>% 
  pivot_longer(cols = c(Bias, MSE, CP),
               names_to = "metric", values_to = "value") %>% 
  mutate(metric = factor(metric, levels = c("Bias", "MSE", "CP")),
         method = factor(method, levels = c("MLE", "AMLE", "MAP1", "MAP2")))

p1 <- ggplot(results_long1) + aes(n, value, color = method) +
  geom_segment(data = data.frame(
    metric = factor(c("Bias", "MSE", "CP")), yinter = c(0, 0, 0.95)),
    aes(x = 10, xend = 100, y = yinter, yend = yinter), color = "gray70") +
  geom_line(aes(linetype = method), lwd = 0.6) +
  facet_wrap(~metric, scale = "free_y") +
  scale_color_manual(NULL, values = c("#000000", "#000000", "#000000", "#000000")) +
  #scale_color_manual(NULL, values = c("#999999", "#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_linetype_manual(NULL, values = c(3, 4, 1, 2)) +
  scale_x_continuous(breaks = seq(20, 100, by = 20), limits = c(10, 100)) +
  labs(x = NULL, y = "Case 1:") + theme_classic() +
  theme(strip.background = element_blank(), legend.position = "top",
        axis.title.y = element_text(angle = 0, vjust = 1.07),
        legend.key.width = unit(1.1, "cm"))
  
p2 <- ggplot(results_long2) + aes(n, value, color = method) +
  geom_segment(data = data.frame(
    metric = factor(c("Bias", "MSE", "CP")), yinter = c(0, 0, 0.95)),
    aes(x = 10, xend = 100, y = yinter, yend = yinter), color = "gray70") +
  geom_line(aes(linetype = method), lwd = 0.6) +
  facet_wrap(~metric, scale = "free_y") +
  scale_color_manual(NULL, values = c("#000000", "#000000", "#000000", "#000000")) +
  #scale_color_manual(NULL, values = c("#999999", "#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_linetype_manual(NULL, values = c(3, 4, 1, 2)) +
  scale_x_continuous(breaks = seq(20, 100, by = 20), limits = c(10, 100)) +
  labs(x = NULL, y = "Case 2:") + theme_classic() +
  theme(strip.background = element_blank(), 
        strip.text = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = 1))

p3 <- ggplot(results_long3) + aes(n, value, color = method) +
  geom_segment(data = data.frame(
    metric = factor(c("Bias", "MSE", "CP")), yinter = c(0, 0, 0.95)),
    aes(x = 10, xend = 100, y = yinter, yend = yinter), color = "gray70") +
  geom_line(aes(linetype = method), lwd = 0.6) +
  facet_wrap(~metric, scale = "free_y") +
  scale_color_manual(NULL, values = c("#000000", "#000000", "#000000", "#000000")) +
  #scale_color_manual(NULL, values = c("#999999", "#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_linetype_manual(NULL, values = c(3, 4, 1, 2)) +
  scale_x_continuous(breaks = seq(20, 100, by = 20), limits = c(10, 100)) +
  labs(x = NULL, y = "Case 3:") + theme_classic() +
  theme(strip.background = element_blank(), 
        strip.text = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = 1))

p4 <- ggplot(results_long4) + aes(n, value, color = method) +
  geom_segment(data = data.frame(
    metric = factor(c("Bias", "MSE", "CP")), yinter = c(0, 0, 0.95)),
    aes(x = 10, xend = 100, y = yinter, yend = yinter), color = "gray70") +
  geom_line(aes(linetype = method), lwd = 0.6) +
  facet_wrap(~metric, scale = "free_y") +
  scale_color_manual(NULL, values = c("#000000", "#000000", "#000000", "#000000")) +
  #scale_color_manual(NULL, values = c("#999999", "#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_linetype_manual(NULL, values = c(3, 4, 1, 2)) +
  scale_x_continuous(breaks = seq(20, 100, by = 20), limits = c(10, 100)) +
  labs(x = "Sample size (n)", y = "Case 4:") + theme_classic() +
  theme(strip.background = element_blank(), 
        strip.text = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = 1))

combined_plot <- p1 / p2 / p3 / p4

ggsave("output/fig2.pdf", width = 10, height = 12, dpi = 320)
