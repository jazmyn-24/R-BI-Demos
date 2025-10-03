---
title: "Employee Attrition Analysis"
author: "Jazmyn Singh"
output: html_document
---

## Introduction
This report explores attrition patterns in HR data.  

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

# Install pacman if you don’t have it
if (!require("pacman")) install.packages("pacman")  

# Use pacman to install and load everything in one step
pacman::p_load(
  tidyverse, skimr, GGally, plotly, viridis, caret, randomForest, e1071, rpart,
  xgboost, h2o, ggcorrplot, rpart.plot, corrgram, lightgbm, ggplot2, highcharter,
  ggthemes, psych, scales, treemap, treemapify, repr, cowplot, magrittr, ggpubr,
  RColorBrewer, plotrix, ggrepel, forcats, reshape2, caTools, tree, rattle
)
# Import Libraries
# if (!require("pacman")) install.packages("pacman") 
# pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, caret, randomForest, e1071, rpart, 
#                xgboost, h2o, ggcorrplot, rpart.plot, corrgram, lightgbm, ggplot2, highcharter, 
#                ggthemes, psych, scales, treemap, treemapify, repr, cowplot, magrittr, ggpubr,
#                RColorBrewer, plotrix, ggrepel, forcats, reshape2, caTools, tree, rattle)


suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(h2o))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(rpart.plot))
suppressPackageStartupMessages(library(corrgram))
suppressPackageStartupMessages(library(lightgbm))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(treemap))
suppressPackageStartupMessages(library(treemapify))
suppressPackageStartupMessages(library(repr))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(plotrix))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(tree))
suppressPackageStartupMessages(library(rattle))

options(repr.plot.width=8, repr.plot.height=6)
options(warn=-1)

df <- read.csv("/Users/jazmynsingh/Documents/R Projects/WA_Fn-UseC_-HR-Employee-Attrition.csv")
head(df)

# This will be used for training and testing.
original_df <- df

# Using an insightful summary with skim and kable
df %>% glimpse()

library(ggplot2)
library(dplyr)
library(cowplot)

options(repr.plot.width=10, repr.plot.height=5)

# Count plot
attritions_number <- df %>%
  group_by(Attrition) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Attrition, y = Count, fill = Attrition)) +
  geom_col(color = "white", width = 0.7) +
  geom_text(aes(label = Count), vjust = 0.5, hjust = -0.3, color = "black", size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Employee Attrition (Count)", x = "", y = "Number of Employees") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face="bold"))

# Percentage plot
attrition_percentage <- df %>%
  group_by(Attrition) %>%
  summarise(Count = n()) %>%
  mutate(pct = round(prop.table(Count), 2) * 100) %>%
  ggplot(aes(x = Attrition, y = pct, fill = Attrition)) +
  geom_col(color = "white", width = 0.7) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, color = "black", size = 4) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Employee Attrition (%)", x = "", y = "Percentage") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face="bold"))

# Combine plots
plot_grid(attritions_number, attrition_percentage, align = "h", ncol = 2)

avg.age <- df %>% select(Gender, Age) %>% group_by(Gender) %>% summarize(avg=mean(Age))

avg.age

library(ggplot2)
library(dplyr)
library(cowplot)

options(repr.plot.width=9, repr.plot.height=7) 

# Labels for gender means
dat_text <- data.frame(
  label = c("Mean = 37.3 yrs", "Mean = 36.7 yrs"),
  Gender   = c("Female", "Male")
)

# Age distribution by gender
gender.dist <- df %>%
  filter(Gender %in% c("Male","Female"), !is.na(Age)) %>%
  ggplot(aes(x = Age, fill = Gender)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~Gender, ncol=2) +
  geom_vline(aes(xintercept = mean(Age)), 
             color = "red", linetype = "dashed", size = 1) +
  geom_text(data = dat_text, 
            aes(x = 55, y = 0.025, label = label), 
            inherit.aes = FALSE, size=4.2) +
  scale_fill_manual(values = c("Female"="#E377C2", "Male"="#1F77B4")) +
  labs(title = "Age Distribution by Gender", x = "Age", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        strip.text = element_text(face="bold"))

# Overall age distribution
overall.dist <- df %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age)) +
  geom_density(fill="steelblue", alpha=0.6, color="darkblue") +
  geom_vline(aes(xintercept = mean(Age)), 
             color="red", linetype="dashed", size=1) +
  annotate("text", label="Mean = 36.9 yrs", x=55, y=0.025, size=4.2) +
  labs(title = "Overall Age Distribution", x="Age", y="Density") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

# Combine plots
plot_grid(gender.dist, overall.dist, nrow=2, align="v")

library(ggplot2)
library(dplyr)
library(cowplot)
library(ggthemes)

options(repr.plot.width=9, repr.plot.height=7) 

# Boxplot: Attrition vs Job Satisfaction by Gender
box.attrition <- df %>%
  select(Attrition, JobSatisfaction, Gender) %>%
  ggplot(aes(x = Attrition, y = JobSatisfaction, fill = Attrition)) +
  geom_boxplot(width=0.6, outlier.shape=21, outlier.color="black") +
  facet_wrap(~Gender, ncol=2) +
  scale_fill_manual(values=c("Yes"="#E74C3C", "No"="#2ECC71")) +
  scale_y_continuous(breaks=1:4, labels=c("Very Low","Low","High","Very High")) +
  labs(title="Job Satisfaction by Attrition & Gender",
       x="Attrition", y="Job Satisfaction") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position="none",
        strip.text = element_text(face="bold"))

# Density distribution of Job Satisfaction
dist.satisfaction <- df %>%
  select(JobSatisfaction) %>%
  ggplot(aes(x=JobSatisfaction)) +
  geom_density(color="#2C3E50", fill="#3498DB", alpha=0.6, adjust=1.2) +
  scale_x_continuous(breaks=1:4, labels=c("Very Low","Low","High","Very High")) +
  labs(title="Overall Distribution of Job Satisfaction", x="Job Satisfaction", y="Density") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# Combine both plots
plot_grid(box.attrition, dist.satisfaction, nrow=2, align="v")

library(ggplot2)

p <- ggplot(df, aes(x = Gender, y = MonthlyIncome, fill = Gender)) +
  geom_boxplot(color = "black", width = 0.6, outlier.shape = 21, outlier.size = 2,
               outlier.fill = "white") +
  coord_flip() +
  scale_fill_manual(values = c("Female" = "#E67E22", "Male" = "#2980B9")) +
  labs(title = "Gender Disparities in Monthly Income",
       x = "Gender", y = "Monthly Income") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        axis.text = element_text(color = "black"))

p

options(repr.plot.width=10, repr.plot.height=8) 

gender.income <- df %>% select(Gender, MonthlyIncome) %>% group_by(Gender) %>% summarise(avg_income=round(mean(MonthlyIncome), 2)) %>%
  ggplot(aes(x=Gender, y=avg_income)) + geom_bar(stat="identity", fill="#2E9AFE", width=0.5) + 
  geom_text(aes(x=Gender, y=0.01, label= paste0("$ ", avg_income)),
            hjust=-2, vjust=0, size=3, 
            colour="black", fontface="bold",
            angle=360) + labs(title="Average Salary by Gender", x="Gender",y="Salary") + coord_flip() + 
  theme_minimal() + theme(plot.title=element_text(size=14, hjust=0.5))

# # How many people work in each department by gender
gender.department <- df %>% group_by(Department, Gender) %>% summarise(amount=n()) %>%
  ggplot(aes(x=reorder(Department, -amount), y=amount, fill=Gender)) + geom_bar(stat="identity", position="dodge") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) + scale_fill_manual(values=c("pink", "lightblue")) + 
  labs(title="Number of Employees \n
by Department",x="Department", y="Number of employees")


departments <- df %>% group_by(Department, Gender) %>% summarise(amount=n()) %>%
  ggplot(aes(x="", y=amount, fill=Department), show.legend=FALSE, width=) + geom_bar(stat="identity", position="dodge") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5), aspect.ratio=1) + 
  labs(title="Number of Employees \n
by Department") + coord_polar() + scale_fill_manual(values=c("#FE642E", "#0080FF","#00FF40"))

plot_grid(gender.income, gender.department, departments, ncol=2, nrow=2)

library(ggplot2)
library(dplyr)
library(cowplot)
library(ggthemes)

options(repr.plot.width=8, repr.plot.height=7) 

# Create Generation groups
df$Generation <- ifelse(df$Age < 37, "Millennials",
                        ifelse(df$Age >= 38 & df$Age < 54, "Generation X",
                               ifelse(df$Age >= 54 & df$Age < 73, "Boomers", "Silent")))

# Boxplot by Generation & Attrition
generation.dist <- df %>%
  select(Generation, NumCompaniesWorked, Attrition) %>%
  ggplot() +
  geom_boxplot(aes(x=reorder(Generation, NumCompaniesWorked, FUN=median),
                   y=NumCompaniesWorked,
                   fill=Generation),
               color="black", alpha=0.85) +
  facet_wrap(~Attrition) +
  coord_flip() +
  scale_fill_brewer(palette="Dark2") +   # <-- changed palette
  labs(title="Job Mobility Across Generations",
       x="Generation", y="Number of Companies Worked") +
  theme_tufte() +
  theme(
    legend.position="bottom",
    legend.background=element_rect(fill="#FDFDFD", size=0.5, linetype="solid", colour="black"),
    strip.background=element_blank(),
    strip.text.x=element_text(face="bold", colour="white"),
    plot.title=element_text(hjust=0.5, colour="white", face="bold"),
    plot.background=element_rect(fill="#1A5276"),   # new dark blue background
    axis.text.x=element_text(colour="white"),
    axis.text.y=element_text(colour="white"),
    axis.title=element_text(colour="white")
  )

# Average points by Generation & Attrition
avg.comp <- df %>%
  group_by(Generation, Attrition) %>%
  summarise(avg = mean(NumCompaniesWorked), .groups="drop") %>%
  ggplot(aes(x=Generation, y=avg, color=Attrition)) +
  geom_point(size=3) +
  geom_segment(aes(x=Generation, xend=Generation,
                   y=min(avg), yend=max(avg)),
               linetype="dashed", size=0.2, color="white") +
  coord_flip() +
  scale_color_manual(values=c("Yes"="#E67E22", "No"="#1ABC9C")) +  # <-- switched to orange/teal
  labs(subtitle="Behavioral Differences Between Generations",
       y="Average Number of Companies Worked",
       x="Generation") +
  theme_tufte() +
  theme(
    legend.position="bottom",
    legend.background=element_rect(fill="#FDFDFD", size=0.5, linetype="solid", colour="black"),
    strip.background=element_blank(),
    plot.subtitle=element_text(color="white", face="italic"),
    plot.background=element_rect(fill="#1A5276"),   # matches first plot
    axis.text.x=element_text(colour="white"),
    axis.text.y=element_text(colour="white"),
    axis.title=element_text(colour="white")
  )

# Combine both
plot_grid(generation.dist, avg.comp, nrow=2)


library(dplyr)
library(ggplot2)
library(forcats)
library(ggthemes)

options(repr.plot.width=8, repr.plot.height=5) 

# Map education levels
df$Educational_Levels <- ifelse(df$Education == 1, "Without College",
                                ifelse(df$Education == 2, "College",
                                       ifelse(df$Education == 3, "Bachelors",
                                              ifelse(df$Education == 4, "Masters", "PhD"))))

# Attrition by education (proportion within each level)
edu.level <- df %>%
  group_by(Educational_Levels, Attrition) %>%
  summarise(n = n(), .groups="drop") %>%
  group_by(Educational_Levels) %>%
  mutate(prop = n / sum(n) * 100) %>%
  ggplot(aes(x=fct_reorder(Educational_Levels, prop), y=prop, fill=Attrition)) +
  geom_col(position="fill", width=0.6, color="white") +
  geom_text(aes(label=paste0(round(prop,1),"%")),
            position=position_fill(vjust=0.5), color="white", size=3.5, fontface="bold") +
  coord_flip() +
  scale_fill_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  labs(title="Attrition by Educational Level",
       x="", y="Proportion of Employees") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

edu.level

library(ggplot2)
library(dplyr)

options(repr.plot.width=9, repr.plot.height=5) 

avg.income <- df %>%
  group_by(Department, Attrition) %>%
  summarise(avg.inc = mean(MonthlyIncome), .groups="drop") %>%
  ggplot(aes(x=reorder(Department, avg.inc), y=avg.inc, fill=Attrition)) +
  geom_col(position=position_dodge(width=0.7), width=0.6) +
  geom_text(aes(label=paste0("$", round(avg.inc,0))),
            position=position_dodge(width=0.7),
            vjust=-0.5, size=3.5, color="black", fontface="bold") +
  scale_fill_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  labs(title="Average Monthly Income by Department and Attrition",
       x="Department", y="Average Income") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        axis.text.x=element_text(angle=30, hjust=1))

avg.income

library(ggplot2)
library(dplyr)
library(forcats)

options(repr.plot.width=9, repr.plot.height=5)

df$JobSatisfaction <- as.factor(df$JobSatisfaction)

high.inc <- df %>%
  group_by(JobSatisfaction, Attrition) %>%
  summarise(med = median(MonthlyIncome), .groups="drop") %>%
  ggplot(aes(x = fct_reorder(JobSatisfaction, -med), y = med, color = Attrition)) +
  geom_segment(aes(x = JobSatisfaction, xend = JobSatisfaction, y = 0, yend = med),
               color="grey70", size=0.8) +
  geom_point(size=4, aes(color=Attrition)) +
  geom_text(aes(label=paste0("$", round(med,0))),
            vjust=-0.6, size=3.5, color="black", fontface="bold") +
  coord_flip() +
  scale_color_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  labs(title="Is Income a Reason for Employees to Leave?",
       subtitle="Median Monthly Income by Job Satisfaction & Attrition",
       y="Median Monthly Income", x="Job Satisfaction Level") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        plot.subtitle=element_text(hjust=0.5),
        legend.position="bottom")

high.inc



options(repr.plot.width=9, repr.plot.height=8) 

# Scatter: Income vs Salary Hike
per.sal <- df %>%
  ggplot(aes(x=PercentSalaryHike, y=MonthlyIncome, color=Attrition)) +
  geom_jitter(alpha=0.5, size=2) +
  scale_color_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  labs(title="Monthly Income vs. Salary Hike by Attrition",
       x="Percent Salary Hike", y="Monthly Income") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position="bottom")

# Violin: Performance vs Income by Attrition
perf.inc <- df %>%
  ggplot(aes(x=factor(PerformanceRating), y=MonthlyIncome, fill=Attrition)) +
  geom_violin(alpha=0.7, trim=FALSE) +
  scale_fill_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  labs(title="Income Distribution by Performance Rating & Attrition",
       x="Performance Rating", y="Monthly Income") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position="bottom")

# Combine
plot_grid(per.sal, perf.inc, nrow=2, align="v")

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(cowplot)

options(repr.plot.width=10, repr.plot.height=8) 

# --- Daily Rate by Job Role & Attrition ---
daily_r <- df %>%
  ggplot(aes(x=JobRole, y=DailyRate, color=Attrition)) +
  stat_summary(fun=mean, fun.min=min, fun.max=max, geom="pointrange",
               position=position_dodge(width=0.5), size=0.7) +
  coord_flip() +
  scale_color_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  labs(title="Daily Rate by Job Role and Attrition",
       x="Job Role", y="Daily Rate (Min–Max with Mean)") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# --- Percent Difference Between Attrition Groups ---
attrition_daily <- df %>%
  filter(Attrition == "Yes") %>%
  group_by(JobRole) %>%
  summarise(avg_attrition=mean(DailyRate), .groups="drop")

noattrition_daily <- df %>%
  filter(Attrition == "No") %>%
  group_by(JobRole) %>%
  summarise(avg_noattrition=mean(DailyRate), .groups="drop")

combined_df <- merge(attrition_daily, noattrition_daily)

percent_diff <- combined_df %>%
  mutate(pct_diff = round(((avg_noattrition - avg_attrition) / avg_noattrition) * 100, 1)) %>%
  ggplot(aes(x=reorder(JobRole, pct_diff), y=pct_diff, fill=JobRole)) +
  geom_col(width=0.6) +
  coord_flip() +
  scale_fill_manual(values=colorRampPalette(brewer.pal(8, "Set2"))(n_distinct(combined_df$JobRole))) +
  labs(x="Job Role", y="Percent Difference (%)",
       title="Percentage Difference in Daily Rates (Stayed vs Left)") +
  geom_text(aes(label=paste0(pct_diff, "%")),
            hjust=-0.1, color="black", size=3.5, fontface="bold") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position="none")

# --- Combine ---
plot_grid(daily_r, percent_diff, nrow=2, align="v")

df %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% group_by(Attrition, OverTime) %>%
  summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100)

library(ggplot2)
library(dplyr)
library(cowplot)
library(ggthemes)

options(repr.plot.width=10, repr.plot.height=5) 

# Pie chart (Percent)
overtime_percent <- df %>%
  filter(Attrition == "Yes") %>%
  count(OverTime) %>%
  mutate(pct = round(prop.table(n)*100,1)) %>%
  ggplot(aes(x="", y=pct, fill=OverTime)) +
  geom_col(width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  geom_text(aes(label=paste0(pct,"%")), 
            position=position_stack(vjust=0.5), color="white", size=4.2, fontface="bold") +
  labs(title="Attrition by Overtime Status", subtitle="Percent of Employees") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        plot.subtitle=element_text(hjust=0.5),
        legend.position="bottom")

# Bar chart (Numbers)
overtime_number <- df %>%
  filter(Attrition == "Yes") %>%
  count(OverTime) %>%
  ggplot(aes(x=OverTime, y=n, fill=OverTime)) +
  geom_col(width=0.6, color="white") +
  geom_text(aes(label=n), vjust=-0.5, size=4, fontface="bold", color="black") +
  scale_fill_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  labs(title="Attrition by Overtime Status", subtitle="Number of Employees",
       x="Overtime Status", y="Count") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        plot.subtitle=element_text(hjust=0.5),
        legend.position="none")

# Combine plots
plot_grid(overtime_percent, overtime_number, ncol=2, align="h")

library(ggplot2)
library(dplyr)
library(treemapify)  # correct package for treemap

role.amount <- df %>%
  count(JobRole) %>%
  ggplot(aes(area=n, fill=JobRole, label=JobRole)) +
  geom_treemap(color="white") +
  geom_treemap_text(grow=TRUE, reflow=TRUE, colour="black", fontface="bold", place="centre") +
  scale_fill_brewer(palette="Set3") +
  theme(legend.position="none") +
  labs(
    title="Major Job Roles Inside the Organization",
    caption="Each tile area represents the number of employees by job role"
  )

role.amount

library(ggplot2)
library(dplyr)
library(cowplot)

options(repr.plot.width=10, repr.plot.height=6) 

# Median & Mean Salary by JobRole
job.sal <- df %>%
  group_by(JobRole) %>%
  summarise(med = median(MonthlyIncome),
            avg = mean(MonthlyIncome), .groups="drop")

# Median salary plot
p1 <- ggplot(job.sal, aes(x=reorder(JobRole, med), y=med)) +  
  geom_col(width=0.6, fill="#E67E22") +
  coord_flip() +
  labs(title="Median Salary by Job Role", x="Job Role", y="Median Income") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# Mean salary plot
p2 <- ggplot(job.sal, aes(x=reorder(JobRole, avg), y=avg)) +  
  geom_col(width=0.6, fill="#6C5CE7") +
  coord_flip() +
  labs(title="Mean Salary by Job Role", x="Job Role", y="Mean Income") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# Combine side by side
plot_grid(p1, p2, ncol=2, align="h")

# Required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

options(repr.plot.width=10, repr.plot.height=6) 

# Prepare attrition % data by JobRole
attr.job <- df %>%
  count(JobRole, Attrition) %>%
  group_by(JobRole) %>%
  mutate(pct = n/sum(n)*100) %>%
  ungroup() %>%
  pivot_wider(names_from=Attrition, values_from=pct, values_fill=0)

# Create mirrored funnel-style plot
funnel_plot <- attr.job %>%
  ggplot() +
  geom_col(aes(x = reorder(JobRole, Yes), y = No), 
           fill="#2E86C1", width=0.6) +
  geom_col(aes(x = reorder(JobRole, Yes), y = -Yes), 
           fill="#E74C3C", width=0.6) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
  labs(title="Attrition by Job Role",
       subtitle="Blue = Stayed, Red = Left",
       x="Job Role", y="Percentage of Employees") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        plot.subtitle=element_text(hjust=0.5))

# Display
funnel_plot

library(ggplot2)
library(dplyr)
library(cowplot)
library(forcats)

options(repr.plot.width=9, repr.plot.height=7)

# --- Categorical variable for Years with Manager ---
df$CatYearsManager <- ifelse(df$YearsWithCurrManager <= 1, "Recently Hired",
                             ifelse(df$YearsWithCurrManager <= 4, "2-4 Years Hired", 
                                    "Long Established Manager"))

# --- Avg Relationship Satisfaction by Years with Manager & Attrition ---
rel.sat <- df %>%
  group_by(CatYearsManager, Attrition) %>%
  summarise(avg.sat = mean(RelationshipSatisfaction), .groups="drop") %>%
  ggplot(aes(x=fct_reorder(CatYearsManager, -avg.sat), y=avg.sat, fill=Attrition)) +
  geom_col(position=position_dodge(width=0.7), width=0.6) +
  scale_fill_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  labs(title="Relationship Satisfaction by Years with Current Manager",
       x="Years with Current Manager", y="Average Satisfaction (1–4)") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        axis.text.x=element_text(angle=20, hjust=1))

# --- Distribution of Relationship Satisfaction by Attrition ---
rel.dist <- df %>%
  ggplot(aes(x=RelationshipSatisfaction, fill=Attrition)) +
  geom_density(alpha=0.6) +
  scale_fill_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  labs(title="Distribution of Relationship Satisfaction by Attrition",
       x="Satisfaction Score (1–4)", y="Density") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position="bottom")

# --- Combine both ---
plot_grid(rel.sat, rel.dist, nrow=2, align="v")

library(ggplot2)
library(dplyr)
library(tidyr)

options(repr.plot.width=10, repr.plot.height=6)

# --- Prepare data: average environment satisfaction by JobRole & Attrition ---
env.attr <- df %>%
  group_by(JobRole, Attrition) %>%
  summarise(avg.env = mean(EnvironmentSatisfaction), .groups="drop") %>%
  pivot_wider(names_from=Attrition, values_from=avg.env, values_fill=0) %>%
  rename(No = "No", Yes = "Yes")


# --- Dumbbell-style plot with ggplot2 only ---
env_plot <- ggplot(env.attr, aes(y = reorder(JobRole, Yes))) +
  # Line between stayed and left
  geom_segment(aes(x = No, xend = Yes, y = JobRole, yend = JobRole), 
               color = "grey70", size = 1) +
  # Stayed (blue point)
  geom_point(aes(x = No), color = "#2E86C1", size = 4) +
  # Left (red point)
  geom_point(aes(x = Yes), color = "#E74C3C", size = 4) +
  labs(title = "Environment Satisfaction by Job Role",
       subtitle = "Blue = Stayed, Red = Left",
       x = "Average Environment Satisfaction (1–4)",
       y = "Job Role") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# --- Display ---
env_plot

library(ggplot2)
library(dplyr)
library(forcats)

options(repr.plot.width=9, repr.plot.height=5)

attritions <- df %>%
  filter(Attrition == "Yes") %>%
  mutate(WorkLifeBalance = as.factor(WorkLifeBalance))

by.department <- attritions %>%
  count(Department, WorkLifeBalance) %>%
  ggplot(aes(x=fct_reorder(WorkLifeBalance, -n), y=n, fill=WorkLifeBalance)) +
  geom_col(width=0.6) +
  geom_text(aes(label=n), vjust=-0.5, size=3.5, fontface="bold", color="black") +
  facet_wrap(~Department) +
  scale_fill_brewer(palette="Set2") +
  labs(title="Work–Life Balance of Employees Who Left",
       x="Work–Life Balance (1=Bad → 4=Good)",
       y="Number of Employees") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position="none")

by.department

r.d <- df %>%
  filter(Department == "Research & Development" & 
           (WorkLifeBalance == 1 | WorkLifeBalance == 2)) %>%
  group_by(Attrition) %>%
  summarise(count = n())

library(ggplot2)

library(dplyr)

options(repr.plot.width=8, repr.plot.height=4)

# Employees who did not quit
no.attritions <- df %>% filter(Attrition == "No")

# Median distance of employees that stayed
med.distance <- no.attritions %>%
  summarise(med.dist = round(median(DistanceFromHome), 2)) %>%
  pull(med.dist)

# Employees who quit
attritions <- df %>% filter(Attrition == "Yes")

# Classify based on median distance
attritions <- attritions %>%
  mutate(Median_Distance = ifelse(DistanceFromHome < med.distance,
                                  "Below Average", "Above Average"))

# Plot
ggplot(attritions, aes(x=Median_Distance, fill=Median_Distance)) +
  geom_bar(width=0.6, color="white") +
  scale_fill_manual(values=c("Below Average"="#2E86C1", "Above Average"="#E74C3C")) +
  labs(title=paste0("Distance from Home for Quitters (Median = ", med.distance, ")"),
       x="Distance Category", y="Number of Employees") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position="none")

library(ggplot2)
library(dplyr)
library(cowplot)
library(forcats)

options(repr.plot.width=10, repr.plot.height=7)

# --- Number of employees by stock option level ---
stockoption <- df %>%
  count(StockOptionLevel, Attrition) %>%
  ggplot(aes(x=fct_reorder(as.factor(StockOptionLevel), n), y=n, fill=Attrition)) +
  geom_col(position="dodge", width=0.6) +
  geom_text(aes(label=n), position=position_dodge(width=0.6), vjust=-0.3, size=3.5) +
  scale_fill_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  coord_flip() +
  labs(title="Employees by Stock Option Level and Attrition",
       x="Stock Option Level", y="Count") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# --- Monthly income by stock option level ---
income_stockoption <- df %>%
  ggplot(aes(x=as.factor(StockOptionLevel), y=MonthlyIncome, fill=Attrition)) +
  geom_boxplot(alpha=0.8, outlier.size=1) +
  scale_fill_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  labs(title="Monthly Income by Stock Option Level & Attrition",
       x="Stock Option Level", y="Monthly Income") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position="bottom")

# --- Combine plots ---
plot_grid(stockoption, income_stockoption, nrow=2, align="v")

library(ggplot2)
library(dplyr)

library(cowplot)


options(repr.plot.width=10, repr.plot.height=7)
# --- Count plot ---
work_bal_cnt <- df %>%
  count(Attrition, BusinessTravel) %>%
  ggplot(aes(x=BusinessTravel, y=n, fill=Attrition)) +
  geom_col(position="dodge", width=0.6) +
  geom_text(aes(label=n), 
            position=position_dodge(width=0.6), vjust=-0.4, size=3.5, fontface="bold") +
  scale_fill_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  labs(title="Attrition by Business Travel (Counts)",
       x="Business Travel", y="Number of Employees", fill="Attrition") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# --- Percentage plot ---
work_bal_pct <- df %>%
  count(Attrition, BusinessTravel) %>%
  group_by(BusinessTravel) %>%
  mutate(pct = round(n/sum(n)*100, 1)) %>%
  ggplot(aes(x=BusinessTravel, y=pct, fill=Attrition)) +
  geom_col(position="dodge", width=0.6) +
  geom_text(aes(label=paste0(pct, "%")), 
            position=position_dodge(width=0.6), vjust=-0.4, size=3.5, fontface="bold") +
  scale_fill_manual(values=c("No"="#2E86C1", "Yes"="#E74C3C")) +
  labs(title="Attrition by Business Travel (Percentages)",
       x="Business Travel", y="Percentage (%)", fill="Attrition") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# --- Combine ---
plot_grid(work_bal_cnt, work_bal_pct, nrow=2, align="v")

df$JobSatisfaction <- as.integer(df$JobSatisfaction)


library(ggcorrplot)

options(repr.plot.width=10, repr.plot.height=7)

# Select numeric variables only
nums <- df %>% select_if(is.numeric)

# Correlation matrix
corr <- cor(nums, use="pairwise.complete.obs")

# Plot
ggcorrplot(corr,
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method = "square", 
           colors = c("#E74C3C", "white", "#2980B9"),  # red to blue
           title = "Correlation Matrix: Employee Dataset",
           outline.col = "white",
           ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust=0.5, face="bold"))


library(ggplot2)
library(cowplot)

options(repr.plot.width=10, repr.plot.height=8)

# TotalWorkingYears vs MonthlyIncome
p1 <- ggplot(df, aes(x=TotalWorkingYears, y=MonthlyIncome)) +
  geom_point(color="#F2DFCE", alpha=0.5) +
  geom_smooth(method="loess", color="#EE4037") +
  labs(title="Positive Correlation", subtitle="Monthly Income vs Total Working Years") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# PerformanceRating vs PercentSalaryHike
p2 <- ggplot(df, aes(x=factor(PerformanceRating), y=PercentSalaryHike)) +
  geom_boxplot(color="#FE642E", fill="#A9D0F5") +
  geom_jitter(color="#F2DFCE", alpha=0.4, width=0.2) +
  labs(title="Positive Correlation", subtitle="Percent Salary Hike vs Performance Rating", x="Performance Rating") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# YearsWithCurrManager vs YearsSinceLastPromotion
p3 <- ggplot(df, aes(x=YearsWithCurrManager, y=YearsSinceLastPromotion)) +
  geom_jitter(color="#F2DFCE", alpha=0.4) +
  geom_smooth(method="loess", color="#EE4037", lty=2, size=0.8) +
  labs(title="Positive Correlation", subtitle="Years Since Last Promotion vs Years with Current Manager", x="Years with Current Manager") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# Age vs MonthlyIncome
p4 <- ggplot(df, aes(x=Age, y=MonthlyIncome)) +
  geom_point(color="#F2DFCE", alpha=0.5) +
  geom_smooth(method="loess", color="#EE4037") +
  labs(title="Positive Correlation", subtitle="Monthly Income vs Age") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# Combine
plot_grid(p1, p2, p3, p4, ncol=2, nrow=2)

set.seed(142)
# # I personally prefer to shuffle my data before splitting.
original_df <- original_df[sample(nrow(original_df)),]

# Let's encode the ordinal variables
original_df$BusinessTravel = factor(original_df$BusinessTravel,
                                    levels = c('Travel_Frequently', 'Travel_Rarely', 'Non-Travel'),
                                    labels = c(1, 2, 3))




# Changing the datatype from integer to factors from the ordinal variables.
cols <- c("Education", "EnvironmentSatisfaction", "JobInvolvement", "JobLevel",
          "JobSatisfaction", "PerformanceRating", "RelationshipSatisfaction", 
          "StockOptionLevel", "TrainingTimesLastYear", "WorkLifeBalance")

original_df[cols] <- lapply(original_df[cols], factor)

# Delete unecessary columns
cols <- c("Over18", "EmployeeNumber", "EmployeeCount")

original_df[cols] <- NULL


# Splitting our data
trainIndex <- createDataPartition(original_df$Attrition, p=0.8, 
                                  list=FALSE, times=1)

train <- original_df[trainIndex,]
test <- original_df[-trainIndex,]



# Checking that both the training and testing sets have the same label proportions.
prop_train <- train %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

prop_test <- test %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

prop_train
prop_test

options(repr.plot.width=10, repr.plot.height=8) 

rpart.tree <- rpart(Attrition ~ ., data=train)
plot(rpart.tree, uniform=TRUE, branch=0.6, margin=0.05)
text(rpart.tree, all=TRUE, use.n=TRUE)
title("Training Set's Classification Tree")


library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tibble)

# Extract variable importance from rpart tree
var_imp <- rpart.tree$variable.importance %>%
  as.data.frame() %>%
  rownames_to_column("features") %>%
  rename(raw_importance = 2)   # the single unnamed column

# Convert to percentages
var_imp <- var_imp %>%
  mutate(importance = round(100 * raw_importance / sum(raw_importance), 2))

# Number of features for colors
colorCount <- n_distinct(var_imp$features)

# Plot feature importance
feature_importance <- ggplot(var_imp, aes(x=reorder(features, importance),
                                          y=importance, fill=features)) +
  geom_col(width=0.6) +
  coord_flip() +
  geom_text(aes(label=paste0(importance, "%")),
            hjust=-0.1, color="black", size=3.5, fontface="bold") +
  scale_fill_manual(values=colorRampPalette(brewer.pal(8, "Set2"))(colorCount)) +
  labs(title="Feature Importance (Decision Tree)",
       x="Features", y="Relative Importance (%)") +
  theme_minimal(base_size=13) +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5, face="bold"))

feature_importance

library(ggplot2)
library(dplyr)

options(repr.plot.width=8, repr.plot.height=6) 

# Predictions
predictions <- predict(rpart.tree, test, type="class")

# Confusion matrix as df
conf_df <- as.data.frame(table(Actual = test$Attrition, Predicted = predictions))

# Plot
ggplot(conf_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color="black", fontface="bold", size=5) +
  scale_fill_gradient(low = "#FADBD8", high = "#196F3D") +
  labs(title="Confusion Matrix",
       x="Predicted", y="Actual") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# Pruning reduces the size of decision trees by removing parts of the tree that do not provide power to classify instances

prune.rpart.tree <- prune(rpart.tree, cp=0.02) # pruning the tree
plot(prune.rpart.tree, uniform=TRUE, branch=0.6)
text(prune.rpart.tree, all=TRUE, use.n=TRUE)

install.packages("partykit")

library(partykit)

rparty.tree <- as.party(rpart.tree)
rparty.tree

options(repr.plot.width=12, repr.plot.height=12) 

fancyRpartPlot(rpart.tree)

colnames(df)

# Delete the following columns
delete <- c("Generation", "Educational_Levels", "CatYearsManager")
df[delete] <- NULL
head(df)

library(caret)
library(pROC)
set.seed(123)

# --- Split again (train/test) ---
trainIndex <- createDataPartition(df$Attrition, p = 0.8, list = FALSE)
train <- df[trainIndex, ]
test  <- df[-trainIndex, ]

# --- Train a logistic regression ---
logit_model <- train(
  Attrition ~ ., data = train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(
    method = "cv", number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  ),
  metric = "ROC"
)

# --- Predictions ---
pred_probs <- predict(logit_model, test, type = "prob")[,2]
pred_class <- predict(logit_model, test)

# --- Confusion Matrix ---
cm <- confusionMatrix(pred_class, test$Attrition, positive = "Yes")
print(cm)

# --- ROC / AUC ---
roc_curve <- roc(response = test$Attrition, predictor = pred_probs, levels = rev(levels(test$Attrition)))
auc_value <- auc(roc_curve)
cat("Test AUC:", auc_value, "\n")

# --- Plot ROC ---
plot(roc_curve, col = "#2E86C1", lwd = 2, main = "ROC Curve (Logistic Regression)")
abline(a = 0, b = 1, lty = 2, col = "gray")


