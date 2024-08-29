.libPaths("/share/sequence/lw/wwt/R/x86_64-pc-linux-gnu-library/4.3")
library(tidyverse)

input.length <- snakemake@input$length
output.distribution <- snakemake@output$distribution

length <- read_tsv(input.length) %>% group_by(length) %>% summarise(Count = n())
sum <- sum(length$Count)
length <- length[which(length$length<=1000),]
reads_distribution <- ggplot(length) +
  geom_col(aes(length, Count), width = 1, fill = "#90cbfb") +
  geom_line(aes(length, Count), group = 1, color = "#90cbfb") +
  #geom_point(aes(length, Count)) +
  scale_y_continuous(sec.axis = sec_axis(~.*100/sum, name = "% Relative Abundance")) +
  scale_x_continuous(breaks = c(0,167,250,320,500,750,1000), labels = c("0","167","250","320","500","750","1000")) +
  xlab("Length") +
  geom_vline(xintercept = 167, color = "red", size = 0.3) +
  geom_vline(xintercept = 320, color = "red", size = 0.3) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15))

ggsave(filename = output.distribution,height = 6, width = 10, plot = reads_distribution)

