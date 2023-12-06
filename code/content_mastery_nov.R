library(dplyr)
library(ggplot2)
library(stringr)

pre <- read.csv("data/Content Pre November.csv", as.is = TRUE)
post <-  read.csv("data/Content Post November.csv", as.is = TRUE)
pre <- pre |> 
  filter(GutGPT != "") |> 
  mutate(wave = "Pre")
pre <- pre[!is.na(pre[,8]), ]
post <- post |> 
  rename(GutGPT = X) |> 
  filter(GutGPT != "") |>
  mutate(wave = "Post")
post <- post[!is.na(post[,8]), ]

grade <- function(df){
  df$Q1 <- as.numeric(df[,8] == 3)
  df$Q2 <- as.numeric(df[,9] == 2)
  df$Q3 <- as.numeric(df[,10] == 4)
  df$Q4 <- as.numeric(df[,11] == 3)
  df$Q5 <- as.numeric(df[,12] == 4)
  df$Q6 <- as.numeric(df[,13] == 4)
  # df <- df |> mutate(pctcorrect = (Q1 + Q2 + Q3 + Q4 + Q5 + Q6) / 6) |> 
  df <- df |> mutate(pctcorrect = rowMeans(df |> select(Q1, Q2, Q3, Q4, Q5, Q6), na.rm = TRUE)) |>
                       select(GutGPT, wave, pctcorrect) 
  return(df)
}

pre <- grade(pre)
post <- grade(post)
df <- rbind(pre, post)

stats <- df |> 
  group_by(GutGPT, wave) |> 
  summarise(mean_pctcorrect = mean(pctcorrect), 
            sd_pctcorrect = sd(pctcorrect), 
            .groups = 'drop') |> 
  mutate(GutGPT = str_replace(GutGPT, "n", "Dashboard")) |> 
  mutate(GutGPT = str_replace(GutGPT, "y", "GutGPT")) |> 
  mutate(wave = factor(wave, levels = rev(unique(wave))))

# Define a color-blind friendly palette (Okabe-Ito palette)
cb_palette <- c("Pre" = "#E69F00", "Post" = "#56B4E9")

pdf("figures/content.pdf", width = 6, height = 3)
stats |> 
  ggplot(aes(x = GutGPT, y = 100 * mean_pctcorrect, fill = wave)) + 
  geom_col(position = "dodge", width=0.6) +
  geom_errorbar(aes(ymin = 100 * (mean_pctcorrect - sd_pctcorrect), 
                    ymax = 100 * (mean_pctcorrect + sd_pctcorrect)), 
                position = position_dodge(width=0.6), width = 0.2, color = "darkgrey") +  
  theme_bw() + 
  coord_flip() + 
  ylab("% Questions Answered Correctly") + 
  xlab("Group") +
  labs(fill = "") +
  theme(axis.title.y=element_text(size=16), 
        axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14),  
        axis.text.y=element_text(size=14),
        legend.position = "top",           
        legend.text = element_text(size=14)) + 
  scale_fill_manual(values = cb_palette) + ylim(c(0, 98))
dev.off()

