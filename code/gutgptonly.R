library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyr)

# UTAUT =======================================================================

# Read in data
pre <- read.csv("data/Pre UTAUT Nov.csv", as.is = TRUE)
post <-  read.csv("data/Post UTAUT Nov.csv", as.is = TRUE)

pre <- pre |> 
  filter(GutGPT != "") |> 
  mutate(wave = "before")
post <- post |> 
  filter(GutGPT != "") |> 
  mutate(wave = "after") |>
  select(!all_of(setdiff(names(post), names(pre)))) 
df <- rbind(pre, post)

# GutGPT only
df <- df |> 
  filter(GutGPT == "y") |>
  mutate(wave = factor(wave))

# Table 1 demographics
df |> 
  summarise(
    Age = mean(What.is.your.age.),
    Sex = mean(What.is.your.sex.),
    PGY = mean(What.PGY.year.are.you., na.rm = TRUE),
    MS = mean(What.year.are.you.in.medical.school., na.rm = TRUE),
    AI_familiar = mean(How.familiar.are.you.with.the.use.of.the.following.in.clinical.care....Artificial.Intelligence.and.Machine.Learning),
    AI_Course = mean(Have.you.ever.taken.a.course.on.machine.learning.or.artificial.intelligence.),
    CDSS_familiar = mean(How.familiar.are.you.with.the.use.of.the.following.in.clinical.care....Clinical.Decision.Support.Systems)
  )

df <- df |>
  mutate(
    `Effort Expectancy` = apply(df[, grep("^EE", names(df))], 1, mean),
    `Behavioral Intentions` = apply(df[, grep("^BI", names(df))], 1, mean),
    `Trust` = apply(df[, grep("^T", names(df))], 1, mean),
    `Facilitating Conditions` = apply(df[, grep("^FC", names(df))], 1, mean),
    `Performance Expectancy` = apply(df[, grep("^PE", names(df))], 1, mean),
    `Social Influence` = apply(df[, grep("^SI", names(df))], 1, mean)
  )

agg_mean <- aggregate(cbind(`Effort Expectancy`, `Behavioral Intentions`, 
                            `Trust`, `Facilitating Conditions`, 
                            `Performance Expectancy`, `Social Influence`)
                      ~ wave, 
                      data = df, FUN = mean)
agg_sd <- aggregate(cbind(`Effort Expectancy`, `Behavioral Intentions`, 
                          `Trust`, `Facilitating Conditions`, 
                          `Performance Expectancy`, `Social Influence`)
                    ~ wave,
                    data = df, FUN = sd)

res <- cbind(melt(agg_mean), melt(agg_sd))[-c(4,5)]
names(res) <- c("wave", "condition", "mean", "sd")


# dot / line plot
legend_order <- c("before", "after")
cb_palette <- c("before" = "#E69F00", "after" = "#56B4E9")

png("figures/utaut_gutgpt.png", width = 6, height = 4, units = "in", res = 300)
ggplot(res, aes(x = condition, y = mean, group = wave)) +   
  geom_point(aes(color = wave), position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, color = wave),
                position = position_dodge(width = 0.5), width = 0.25) +
  scale_color_manual(values = cb_palette, breaks = legend_order) +
  geom_hline(yintercept = 3, linetype = "dotted", color = "grey", size = 1) + 
  theme_bw() + ylim(1.1, 4.9) + ylab("Likert Scale Value") + xlab("") + 
  theme(#text = element_text(face = "bold"),  # Bold all text elements
    legend.title = element_blank(),  # Remove legend title
    legend.position = "top",
    axis.title = element_text(size = 12),  
    axis.text.x=element_text(size=10),  
    axis.text.y=element_text(size=12),
    legend.text = element_text(size = 10)) + 
  coord_flip()
dev.off()


# Content assessment ==========================================================

pre <- read.csv("data/Content Pre November.csv", as.is = TRUE)
post <-  read.csv("data/Content Post November.csv", as.is = TRUE)
pre <- pre |> 
  filter(GutGPT == "y") |> 
  mutate(wave = "before")
pre <- pre[!is.na(pre[,8]), ] # question values are all NA for obs 8
post <- post |> 
  rename(GutGPT = X) |> 
  filter(GutGPT == "y")  |> 
  mutate(wave = "after")
post <- post[!is.na(post[,8]), ]

grade <- function(df){
  df$Q1 <- as.numeric(df[,8] == 3)
  df$Q2 <- as.numeric(df[,9] == 2)
  df$Q3 <- as.numeric(df[,10] == 4)
  df$Q4 <- as.numeric(df[,11] == 3)
  df$Q5 <- as.numeric(df[,12] == 4)
  df$Q6 <- as.numeric(df[,13] == 4)
  df <- df |> mutate(pctcorrect = rowMeans(df |> select(Q1, Q2, Q3, Q4, Q5, Q6), na.rm = TRUE)) |>
    select(GutGPT, wave, pctcorrect)  
  return(df)
}

pre <- grade(pre)
post <- grade(post)
df <- rbind(pre, post)

stats <- df |> 
  group_by(wave) |> 
  summarise(mean_pctcorrect = mean(pctcorrect), 
            sd_pctcorrect = sd(pctcorrect), 
            .groups = 'drop') 

# Plot
png("figures/content_gutgpt.png", width = 4, height = 2, units = "in", res = 300)
stats |> 
  ggplot(aes(x = wave, y = 100 * mean_pctcorrect, fill = wave)) + 
  geom_col(position = "dodge", width=0.4) +
  geom_errorbar(aes(ymin = 100 * (mean_pctcorrect - sd_pctcorrect), 
                    ymax = 100 * (mean_pctcorrect + sd_pctcorrect)), 
                position = position_dodge(width=0.4), width = 0.2, color = "darkgrey") +  
  theme_bw() + 
  ylab("% Questions Answered Correctly") + 
  xlab("") +
  coord_flip() + 
  labs(fill = "") +
  theme(axis.title = element_text(size=10),  
        axis.text.x=element_text(size=10),  
        axis.text.y=element_text(size=12),
        legend.position = "none")+
  scale_fill_manual(values = cb_palette) + ylim(c(0, 98))
dev.off()



