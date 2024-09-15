# Written by Abdulla Alameri
# Last Edit : 12-June-2023

# Load Packages
library(readxl)
library(psych)
library(ggplot2)
library(dplyr)
library(corrplot)

# Set Working Directory
setwd("D:/side project")

# Import the CSV file from the working directory
df <- read_excel("Choosen Data.xlsx")

# Clean Data from missing values
df_clean <- subset(df, !apply(df == 99 | df == 98, 1, any))

# Generate a Descriptive stats table
describe(df_clean)


# Create Histogram for WLLT_SAT
ggplot(df_clean, aes(x=WLTH_SAT)) +
  geom_histogram(binwidth=1, fill="#606060", color="white") +
  theme_minimal() +
  labs(title="Histogram of Wealth Satisfaction", 
       x="Wealth Satisfaction Score", 
       y="Frequency")

# Create Box and Whisker plot WLTH_SAT
ggplot(df_clean, aes(y=WLTH_SAT)) +
  geom_boxplot(fill="#606060", coef=0) +
  theme_minimal() +
  labs(title="Boxplot of Wealth Satisfaction", 
       y="Wealth Satisfaction Score")

# Create a bar chart for EDU
ggplot(df_clean, aes(x=factor(EDU))) +
  geom_bar(fill="#606060") +
  theme_minimal() +
  labs(title="Bar Chart of Education Levels",
       x="Education Level",
       y="Count")


# Create An Area plot of EDU
ggplot(df_clean, aes(x=EDU)) +
  stat_bin(binwidth=1, geom="area", fill="#606060", color="#606060", size=1.5, alpha=0.5) +
  geom_vline(xintercept=1:6, linetype="dotted", color="#606060") +
  scale_x_continuous(breaks=1:6, limits=c(1,6)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title="Area Plot of Education Levels",
       x="Education Level",
       y="Count")

# Create a bar chart for Gender
ggplot(df_clean, aes(x=factor(GENDER))) +
  geom_bar(fill="#606060") +
  scale_x_discrete(labels=c("1" = "Male", "2" = "Female")) +
  theme_minimal() +
  labs(title="Bar Chart of Gender",
       x="Gender",
       y="Count")

# Create a pie chart for Gender
df_clean %>%
  group_by(GENDER) %>%
  summarise(n=n()) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x="", y=prop, fill=factor(GENDER))) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_grey(start = 0.8, end = 0.2) +
  theme_void() +
  labs(title="Pie Chart of Gender Distribution",
       fill="Gender")

# Create a bar chart for AGE_GR
ggplot(df_clean, aes(x=factor(AGE_GR))) +
  geom_bar(fill="#606060") +
  theme_minimal() +
  labs(title="Bar Chart of Age Groups",
       x="Age Group",
       y="Count")

# Create a Area plot for AGE_GR
ggplot(df_clean, aes(x=AGE_GR)) +
  stat_bin(binwidth=1, geom="area", fill="#606060", color="#606060", size=1.5, alpha=0.5) +
  geom_vline(xintercept=1:6, linetype="dotted", color="#606060") +
  scale_x_continuous(breaks=1:6, limits=c(1,6)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title="Area Plot of Age Groups",
       x="Age Group",
       y="Count")

# Create a histogram for RISK_TOL
ggplot(df_clean, aes(x=RISK_TOL)) +
  geom_histogram(binwidth=1, fill="#606060", color="white") +
  theme_minimal() +
  labs(title="Histogram of Risk Tolerance", 
       x="Risk Tolerance Score", 
       y="Frequency")

# Create a box-and-whisker for RISK_TOL 
ggplot(df_clean, aes(y=RISK_TOL)) +
  geom_boxplot(fill="#606060") +
  theme_minimal() +
  labs(title="Box-and-Whisker Plot of Risk Tolerance", 
       y="Risk Tolerance Score")


# Create a bar chart for BUDGET 
ggplot(df_clean, aes(x=factor(BUDGET))) +
  geom_bar(fill="#606060") +
  scale_x_discrete(labels=c("1" = "Has a budget", "2" = "No budget")) +
  theme_minimal() +
  labs(title="Bar Chart of Budget Status", 
       x="Budget Status", 
       y="Count")

# Create a pie chart for BUDGET
budget_df <- data.frame(Category = factor(df_clean$BUDGET, levels = c(1, 2), labels = c("Has a budget", "No budget")))

ggplot(budget_df, aes(x="", y=..count.., fill=Category)) +
  geom_bar(stat="count", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#A9A9A9", "#606060")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title="Pie Chart of Budget Status",
       fill="Budget Status")

# Create correlation matrix Table
cor_matrix <- cor(df_clean[, c("WLTH_SAT", "EDU", "GENDER", "AGE_GR", "RISK_TOL", "BUDGET")])
print(cor_matrix)

# Create Multiple Regression Analysis table

model <- lm(WLTH_SAT ~ EDU + GENDER + AGE_GR + RISK_TOL + BUDGET, data=df_clean)
summary(model)
