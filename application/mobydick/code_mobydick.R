# devtools::install_github("njerezlillo/pldis")
# devtools::install_github("njerezlillo/pwpldis")
library(pldis)
library(dplyr)
library(scales)
library(ggplot2)
library(pwpldis)
library(poweRlaw)
library(textstem)
library(tidytext)
library(wordcloud)
source("clauset.R")

# Graphs ------------------------------------------------------------------

moby_dick <- readLines("application/mobydick/book.txt")
book <- data.frame(text = moby_dick, stringsAsFactors = FALSE)
x <- read.table("application/mobydick/frequency_words.txt", header = F)$V1
df <- data.frame(table(x))

word_counts <- book %>%
  mutate(text = tolower(text),
         text = gsub("[[:punct:]]", "", text),
         text = gsub("[[:digit:]]", "", text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(nchar(word) > 2) %>%
  mutate(word = lemmatize_words(word)) %>%
  count(word, sort = TRUE)

pdf("application/mobydick/fig8.pdf", width = 5, height = 3.5)
set.seed(2024)
wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  min.freq = 10,
  max.words = 150,
  scale = c(4, 0.5),
  colors = "black",
  random.order = FALSE
)
dev.off()

# Power Law ---------------------------------------------------------------

x_grid <- sort(unique(x))

fit_trad <- fit_pldis(x, bayesian = F)
plpva(x, xmin =  fit_trad$xmin, vec = seq(1.5, 3.5, .01))$p

fit <- fit_pldis(x, bayesian = T)
cdf_pl <- poweRlaw::ppldis(x_grid[x_grid >= fit$xmin], fit$xmin, fit$alpha)
step_aux <- stepfun(x_grid[x_grid >= fit$xmin], c(0, cdf_pl))
dgof::cvm.test(x[x >= fit$xmin], step_aux, type = "W2")$p.value
dgof::cvm.test(x[x >= fit$xmin], step_aux, type = "U2")$p.value
dgof::cvm.test(x[x >= fit$xmin], step_aux, type = "A2")$p.value

# Piecewise Power Law -----------------------------------------------------

fit_pwpldis(x, nbreak = 1, exclude_int = c(quantile(x, 0.90), Inf))
fit_pwpldis(x, nbreak = 2, exclude_int = c(quantile(x, 0.90), Inf))
fit_pwpldis(x, nbreak = 3, exclude_int = c(quantile(x, 0.90), Inf))

cdf_pwpl <- Vectorize(
  function(z) ppwpldis(z, c(1, 2, 6), c(1.70, 1.81, 1.94))
)

step_aux <- stepfun(x_grid, c(0, cdf_pwpl(x_grid)))
dgof::ks.test(x, step_aux)$p.value
dgof::cvm.test(x, step_aux, type = "W2")$p.value
dgof::cvm.test(x, step_aux, type = "U2")$p.value
dgof::cvm.test(x, step_aux, type = "A2")$p.value

den_pwpl <- Vectorize(
  function(t) dpwpldis(t, c(1, 2, 6), c(1.70, 1.81, 1.94))
)

UCL_pwpl <- function(t) {
  den_pwpl(t) - 1.96 * sqrt(den_pwpl(t) * (1 - den_pwpl(t))/length(x))
}

LCL_pwpl <- function(t) {
  den_pwpl(t) + 1.96 * sqrt(den_pwpl(t) * (1 - den_pwpl(t))/length(x))
}

ggplot(df, aes(x = as.numeric(x), y = Freq/sum(Freq))) +
  stat_function(fun = UCL_pwpl, linetype = 2, color = "gray30") +
  stat_function(fun = LCL_pwpl, linetype = 2, color = "gray30") +
  stat_function(fun = den_pwpl, size = 0.5, alpha = 1) +
  geom_point(size = 1.5, pch = 1) +
  scale_x_log10(labels = scales::trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "bl") +
  xlab("Word (log scale)") +
  ylab("Frequency (log scale)") +
  theme_classic() +
  theme(text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

ggsave("application/mobydick/fig9.pdf", height = 4, width = 7, dpi = 320)
