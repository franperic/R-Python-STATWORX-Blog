################################################################################
# R and Python, a comparison using kaggle survey (2018) responses
# 19.02.2019
# Fran Peric, STATWORX
################################################################################

# Setup
rm(list = ls())
options(scipen = 999)
library(tidyverse)
library(helfRlein)
library(VennDiagram)

# Read data
df_18 <- read_csv("../01_data/multipleChoiceResponses.csv")

# Construct dataframe
data <- data.frame("id" = 1:nrow(df_18))
data$age <- df_18$Q2
data$R <- df_18$Q16_Part_2
data$Python <- df_18$Q16_Part_1
data$Recommendation <- df_18$Q18
data$default_lang <- df_18$Q17
data$job_title <- df_18$Q6
# get rid of first observation (survey question)
data <- data[-1, ]

#### Speed R-Python ------------------------------------------------------------
# results from simulation
summary <- data.frame(n = c(1e2, 1e3, 1e4, 1e5, 1e6),
                      # r from sim_in_r$t
                      r = c(0.02332023, 0.03465166, 
                            0.20451117, 1.53275284, 11.68537079),
                      # python from results[:, 1]
                      python = c(0.04043785, 0.05187911, 
                                 0.12881283, 0.76886075, 6.9458098))

# transform to long layout for ggplot2
summary %>% 
  gather(key = "language", value = "time", 2:3) %>%
    ggplot(aes(x = n, y = time, col = language)) +
      geom_line(size = 1.2, alpha = 0.7) +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_manual(name = "Legend", 
                         values = c("#0085AF", "#00A378"),
                         labels = c("R", "Python")) +
      scale_x_log10() +
      labs(x = "Log Simulation Size",
           y = "Average Time in s") +
      theme_minimal(base_size = 16)


#### Survey in the community ---------------------------------------------------
### R & Python Comparison
# Calculate the number of users for each language
n_users <- data %>%
  drop_na(default_lang) %>%
  group_by(default_lang) %>%
  summarise(n = n()) %>%
  arrange(n)

order_lang <- n_users %>%
  select(default_lang) %>%
  as_vector()

ggplot(n_users,
       aes(x = default_lang, y = n)) +
  geom_segment(aes(xend = default_lang, 
                   y = 0, yend = n), show.legend = F) +
  geom_point(col = "steelblue", size = 3) +
  scale_x_discrete(limits = order_lang) +
  coord_flip() +
  labs(x = "",
       y = "Count") +
  theme_minimal(base_size = 16)

# Venndiagramm
# data prep
data$R[which(is.na(data$R))] <- 0 
data$Python[which(is.na(data$Python))] <- 0 
data <- data %>%
  mutate(both = ifelse(R == "R" & Python == "Python", 1, 0),
         onlyR = ifelse(R == "R" & Python == 0, 1, 0),
         onlyPython = ifelse(R == 0 & Python == "Python", 1, 0))


summary_venn <- data.frame(Python = sum(data$Python == "Python"),
                           R = sum(data$R == "R"),
                           both = sum(data$both))

grid.newpage()
areas <-  melt(summary_venn, measure.vars = c("Python", "R", "both"))

# Plot
draw.pairwise.venn(area1 = areas$value[1], 
                   area2 = areas$value[2],
                   cross.area = areas$value[3], 
                   category = c("Python", "R"),
                   fill = c("#00A378", "#0085AF"),
                   lty = rep("blank", 2),
                   cat.pos = c(150, 150),
                   scaled = T)

#### Relative Shares according to job title ------------------------------------
summary_relative_shares <- data %>%
  filter(!is.na(job_title)) %>%
  group_by(job_title) %>%
  summarise(R = sum(default_lang == "R", na.rm = T),
            Python = sum(default_lang == "Python", na.rm = T)) %>%
  gather(key = Language, value = Users, R:Python) %>%
  group_by(job_title) %>%
  mutate(n = sum(Users),
         share = Users/n,
         Language = factor(Language, levels = c("R", "Python")))

order_job <- summary_relative_shares %>%
  ungroup() %>%
  filter(Language == "Python" & n > 50) %>%
  arrange(share) %>% 
  select(job_title) %>%
  as_vector()

# Plot shares
ggplot(summary_relative_shares %>% 
         filter(n >= 50)) + 
  aes(x = job_title,
      y = share,
      fill = Language) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Legend", 
                    values = c("#0085AF", "#00A378"), 
                    labels = c("R", "Python")) +
  scale_x_discrete(limits = order_job) +
  coord_flip() +
  labs(x = "Job Title",
       y = "Share") +
  theme_minimal()

# Plot absolute values
ggplot(summary_relative_shares_melted %>% filter(n >= 50)) + 
  aes(x = job_title,
      y = value,
      fill = variable) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Legend", 
                    values = c("#0085AF", "#00A378"), 
                    labels = c("R", "Python")) +
  coord_flip() +
  labs(x = "Job Title",
       y = "Respondents") +
  theme_minimal()


#### Recommendations -----------------------------------------------------------
# Barplot for recommendations
ggplot(data %>% 
         filter(Recommendation %in% c("Python", "R"))) +
  geom_bar(aes(x = Recommendation, 
        fill = Recommendation), 
    show.legend = F,
    width = 0.6) +
  scale_fill_manual(values = c("#00A378", "#0085AF")) +
  labs(x = "",
       y = "Number of Recommendations") +
  theme_minimal(base_size = 16)

