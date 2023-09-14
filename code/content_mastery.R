library(dplyr)
library(ggplot2)
library(stringr)

# Read in data
pre <- read.csv("data/Pre Education_Sep.csv", as.is = TRUE)
post <-  read.csv("data/Post Education_Sep.csv", as.is = TRUE)


# Preprocess
pre <- pre |> 
  mutate(arm = ifelse(Random == "y", "Gut-GPT", "Dashboard/Internet")) |> 
  mutate(wave = "before")
post <- post |> 
  mutate(arm = ifelse(Random == "y", "Gut-GPT", "Dashboard/Internet")) |> 
  mutate(wave = "after")

df <- rbind(pre, post) |> 
  mutate(pctcorrect = (Q1 + Q2 + Q3 + Q4 + Q5 + Q6) / 6) |> 
  select(arm, wave, pctcorrect) 

stats <- df |> 
  group_by(arm, wave) |> 
  summarise(mean_pctcorrect = mean(pctcorrect), 
            sd_pctcorrect = sd(pctcorrect), 
            .groups = 'drop') |> 
  mutate(arm = str_replace(arm, "Dashboard/Internet", "Dashboard/\nInternet")) |>
  mutate(arm = factor(arm, levels = rev(unique(arm))))


# Create Plot
pdf("figures/pctcorrect.pdf", width = 7, height = 3)
stats |> 
  ggplot(aes(x = arm, y = 100 * mean_pctcorrect, fill = wave)) + 
  geom_col(position = "dodge", width=0.5) +
  geom_errorbar(aes(ymin = 100 * (mean_pctcorrect - sd_pctcorrect), 
                    ymax = 100 * (mean_pctcorrect + sd_pctcorrect)), 
                position = position_dodge(width=0.5), width = 0.2) +  
  theme_bw() + 
  coord_flip() + 
  ylab("Percentage of Questions Answered Correctly") + 
  xlab("Group") +
  labs(fill = "") +
  theme(axis.title.y=element_text(size=16), 
        axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14),  
        axis.text.y=element_text(size=16),
        legend.position = "top",           
        legend.text = element_text(size=14))  + 
  scale_fill_manual(values = scales::hue_pal()(2), 
                    guide = guide_legend(reverse = TRUE))
dev.off()



library(dplyr)
library(ggplot2)
library(stringr)

pre <- read.csv("data/Pre Education_Sep.csv", as.is = TRUE)
post <-  read.csv("data/Post Education_Sep.csv", as.is = TRUE)
pre <- pre |> 
  mutate(arm = ifelse(Random == "y", "Gut-GPT", "Dashboard/Internet")) |> 
  mutate(wave = "before")
post <- post |> 
  mutate(arm = ifelse(Random == "y", "Gut-GPT", "Dashboard/Internet")) |> 
  mutate(wave = "after")

df <- rbind(pre, post) |> 
  mutate(pctcorrect = (Q1 + Q2 + Q3 + Q4 + Q5 + Q6) / 6) |> 
  select(arm, wave, pctcorrect) 

stats <- df |> 
  group_by(arm, wave) |> 
  summarise(mean_pctcorrect = mean(pctcorrect), 
            sd_pctcorrect = sd(pctcorrect), 
            .groups = 'drop') |> 
  mutate(arm = str_replace(arm, "Dashboard/Internet", "Dashboard/\nInternet"))
  
stats$arm <- factor(stats$arm, levels = rev(unique(stats$arm)))
#stats$wave <- factor(stats$wave, levels = rev(unique(stats$wave)))


# Plot
stats |> 
  ggplot(aes(x = arm, y = 100 * mean_pctcorrect, fill = wave)) + 
  geom_col(position = "dodge", width=0.5) +
  geom_errorbar(aes(ymin = 100 * (mean_pctcorrect - sd_pctcorrect), 
                    ymax = 100 * (mean_pctcorrect + sd_pctcorrect)), 
                position = position_dodge(width=0.5), width = 0.2) +  
  theme_bw() + 
  coord_flip() + 
  ylab("Percentage of Questions Answered Correctly") + 
  xlab("Group") +
  labs(fill = "") +
  theme(axis.title.y=element_text(size=16), 
        axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14),  
        axis.text.y=element_text(size=16),
        legend.position = "top",           
        legend.text = element_text(size=14))  + 
  scale_fill_manual(values = scales::hue_pal()(2), 
                    guide = guide_legend(reverse = TRUE))
# 7 in by 2 in 
