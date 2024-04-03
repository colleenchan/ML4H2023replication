library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyr)

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
df <- rbind(pre, post) |> 
  mutate(wave = factor(wave))

df <- df |> 
  mutate(age = case_when(
    What.is.your.age. == 2 ~ "18-24",
    What.is.your.age. == 3 ~ "25-29",
    What.is.your.age. == 4 ~ "30-34",
    What.is.your.age. == 5 ~ "35-39",
    What.is.your.age. == 6 ~ "40-45",
    What.is.your.age. == 7 ~ "45-49",
    TRUE ~ as.factor(What.is.your.age.) 
  ))

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
                      ~ wave + GutGPT, 
                      data = df, FUN = mean)
agg_sd <- aggregate(cbind(`Effort Expectancy`, `Behavioral Intentions`, 
                          `Trust`, `Facilitating Conditions`, 
                          `Performance Expectancy`, `Social Influence`)
                    ~ wave + GutGPT,
                  data = df, FUN = sd)
res <- cbind(melt(agg_mean), melt(agg_sd))[-c(5:7)]
names(res) <- c("wave", "GutGPT", "condition", "mean", "sd")



res$group <- with(res, interaction(wave, GutGPT))
res$interaction <- factor(interaction(res$wave, res$GutGPT))
labels <- c("before.n" = "Pre Dashboard", "after.n" = "Post Dashboard", 
            "before.y" = "Pre GutGPT", "after.y" = "Post GutGPT")

# Define a color-blind friendly palette (Okabe-Ito palette)
cb_palette <- c("before.n" = "#E69F00", "after.n" = "#56B4E9", 
                "before.y" = "#E69F00", "after.y" = "#56B4E9")
legend_order <- c("before.y", "after.y", "before.n", "after.n")

# plot in ML4H camera ready paper
pdf("figures/utaut.pdf", width = 7, height = 5)
ggplot(res, aes(x = condition, y = mean, group = interaction)) +   
  geom_point(aes(color = interaction, fill = interaction), 
             position = position_dodge(width = 0.7), size = 2.5, shape = 21) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, color = interaction),
                position = position_dodge(width = 0.7), width = 0.25) +
  scale_color_manual(values = cb_palette, labels = labels, breaks = legend_order) +
  scale_fill_manual(values = c("after.n" = "#56B4E9", "before.n" = "#E69F00", 
                               "after.y" = "white", "before.y" = "white"), 
                    labels = labels, breaks = legend_order) +
  geom_hline(yintercept = 3, linetype = "dotted", color = "grey", size = 1) + 
  theme_bw() + ylim(1.1, 4.9) + ylab("Likert Scale Value") + xlab("") + 
  theme(#text = element_text(face = "bold"),  # Bold all text elements
        legend.title = element_blank(),  # Remove legend title
        legend.position = "top",
        axis.title = element_text(size = 16),  # Increase size of axis titles
        axis.text.x=element_text(size=14),  
        axis.text.y=element_text(size=16),
        legend.text = element_text(size = 14)) + 
  labs(color = "Wave and GutGPT", fill = "Wave and GutGPT") + 
  coord_flip() + 
  guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2))
dev.off()

