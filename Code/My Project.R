#ASSESSMENT 
library(tidyverse)
(melanomafor <- read_csv("/Users/aia/Desktop/Data Science Class C1/melanomafor_stat1.csv"))
view(melanomafor)

# This changes the sex vector into factor with labels "male" and "female" for "1" and "0" respectively 
melanomafor$sex <- factor(melanomafor$sex, 
                          levels = c("1","0"), 
                          labels = c("male","female"))

# This changes the ulcer vector into factor with labels "present" and "absent" for "1" and "0" respectively 
melanomafor$ulcer <- factor(melanomafor$ulcer, 
                   levels = c("1","0"), 
                   labels = c("present","absent"))

# This changes the status vector into factor with labels as seen below 
melanomafor$status <- factor(melanomafor$status, 
                      levels = c("1", "2", "3"), 
                      labels = c("Died from melanomafor", "Alive", "Died from other causes"))
attach(melanomafor)
str(melanomafor)

#Question 1 starts here
#time summary statistics starts here
time_summary <- c(
  Mean = mean(time),
  Median = median(time),
  Maximum = max(time),
  Minimum = min(time),
  Quantile = quantile(time),
  σ = sd(time)
)
print(time_summary)

# Calculate summary statistics for 'age'
age_summary <-
  c(
    Mean = mean(age),
    Median = median(age),
    Maximum = max(age),
    Minimum = min(age),
    Quantile = quantile(age),
    σ = sd(age)
  )
print(age_summary)

year_summary <- c(
  Mean = mean(year),
  Median = median(year),
  Maximum = max(year),
  Minimum = min(year),
  Quantile = quantile(year),
  σ = sd(year)
)
print(year_summary)

# Calculate summary statistics for 'thickness'
thickness_summary <- c(
    Mean = mean(thickness),
    Median = median(thickness),
    Min = min(thickness),
    Max = max(thickness),
    Quantile = quantile(thickness),
    σ = sd(thickness)
  )
print(thickness_summary)

summary_statistics <- cbind(time_summary, age_summary, year_summary, thickness_summary)
view(summary_statistics)

summary_statistics <- as.data.frame(summary_statistics)

# Sex data analysis
ggplot(melanomafor, aes(x = as.factor(sex), fill = sex)) +
  geom_bar(color = "black", width = 0.5) +
  theme_bw() +
  labs(
    title = "Gender Distribution",
    x = "Sex",
    y = "Percentage"
  )+


#QUESTION TWO STARTS HERE
  
# survival time distribution histogram chart code starts here  
ggplot(melanomafor, aes(x=time))+
  geom_histogram(bins = 25, fill = "yellow", color = "black")+
  theme_bw()+
  labs(title = "Survival Time",
  x = "Days",
  y = "Count"
  )

#Distribution of status analysis starts here
# Convert status to a factor
melanomafor %>%
  mutate(status = as.factor(status))
# Create the bar plot
  ggplot(melanomafor, aes(x = status), fill = sex) +
  geom_bar(fill = "blue", color = "black", width = 0.5) + 
  theme_bw() +
  labs(
    title = "Distribution of Patient Status",
    x = "Status",
    y = "Count"
  )

#Sex analysis starts here
# Convert sex to a factor
melanomafor %>%
  mutate(sex = as.factor(sex))
# Create the bar plot for 'sex'
  ggplot(melanomafor, aes(x = sex)) +
  geom_bar(fill = "red", color = "black", width = 0.5) +  
  theme_bw() +
  labs(
    title = "Distribution of Patient Sex",
    x = "Sex",
    y = "Count"
  )

#Age analysis starts here
# Create a histogram for age
  ggplot(melanomafor, aes(x = age))+
  geom_histogram(bins = 25, fill = "lightgreen", color = "black")+
  theme_bw()+
  labs(
    title = "Distribution of Patient Age",
    x = "Age (Years)",
    y = "Count"
  )+
    # adding lines to represent meann and median
  geom_vline(aes(xintercept = mean(age)), color = "red", linetype = "dashed", size = 0.3)+
  geom_vline(aes(xintercept = median(age)), color = "blue", linetype = "dashed", size = 0.3)  

#Tumor Thickness analysis starts here
# Create a histogram for thickness
  ggplot(melanomafor, aes(x = thickness)) +
  geom_histogram(bins = 30, fill = "yellow", color = "black")+
  theme_bw() +
  labs(
    title = "Distribution of Tumor Thickness",
    x = "Tumor Thickness (mm)",
    y = "Count"
  ) +
    # adding lines to represent meann and median
  geom_vline(aes(xintercept = mean(thickness)), color = "red", linetype = "dashed", size = 0.3) + 
  geom_vline(aes(xintercept = median(thickness)), color = "blue", linetype = "dashed", size = 0.3) 

  #Ulcer analysis starts here
# Convert 'ulcer' to a factor
melanomafor %>%
  mutate(melanomafor, ulcer = as.factor(ulcer))
# Create a bar plot for 'ulcer'
  ggplot(melanomafor, aes(x = ulcer)) +
  geom_bar(fill = "#CBC3E3", color = "black", width = 0.5) +
  theme_bw() +
  labs(
    title = "Distribution of Ulceration Presence",
    x = "Ulceration",
    y = "Count"
  )
  
  ggplot(melanomafor, aes(x = as.factor(year), fill = as.factor(year))) +
    geom_bar(color = "black") +
    theme_bw() +
    labs(
      title = "Distribution of Year of Operation",
      x = "Year of Operation",
      y = "Count"
    )


#DATA RELATIONSHIPS STARTS HERE
# Bar chat for 'year and Sex'
ggplot(melanomafor, aes(x = as.factor(year), fill = as.factor(sex))) +
  geom_bar(color = "black") +
  theme_bw() +
  labs(
    title = "Distribution of Year of Operation in Relation to Sex",
    x = "Year of Operation",
    y = "Count",
    fill = "Year"
  )
# Box plot relating ulceration to survival time 
ggplot(melanomafor, aes(x = as.factor(ulcer), y = time, fill = as.factor(ulcer))) +
  geom_boxplot() +
  theme_bw()+
  labs(
    title = "Survival Time by Ulceration Status",
    x = "Ulceration",
    y = "Survival Time (Days)",
    fill = "Ulceration"
  )

# Bar chat for ulcer presence filled with sex
ggplot(melanomafor, aes(x = as.factor(sex), fill = ulcer)) +
  geom_bar(width = 0.5) +
  labs(title = "Ulcer Presence by Sex",
  x = "Sex",
  y = "Count",
) +
  scale_fill_manual(values = c("present" = "#4daf4a", "absent" = "#ff7f00"))+
 theme_bw()
  t.test(male$ulcer, female$ulcer, var.equal = FALSE)

#Box plot showing difference in status as related to sex and age 
melanomafor %>%
  mutate(status = as.factor(status)) %>%
  ggplot()+
  geom_boxplot(aes(x=sex, y=age, fill = status))+
  labs(title = "Status in relationship with Sex-Age",
       x = "Sex",
       y = "Age",
  )+
  theme_bw()


# QUESTION THREE STARTS HERE
#time ~ thickness
ggplot(melanomafor, aes(x = thickness, y = time)) +
  geom_point(color = "blue") +
  geom_smooth(se = FALSE,method = "lm", color = "red", formula = "y ~ x", linewidth = 0.5) +
  theme_bw() +
  labs(
    title = "Scatter Plot Showing Relationship Between Survival Time and Tumor Thickness",
    x = "Tumor Thickness (mm)",
    y = "Survival Time (Days)"
  )
cor(time, thickness, method = "spearman")

#time ~ age
ggplot(melanomafor, aes(x = age, y = time)) +
  geom_point(color = "darkgreen") +
  geom_smooth(se = FALSE, method = "lm", color = "red", linewidth = 0.5) +
  theme_bw() +
  labs(
    title = "Scatter Plot: Survival Time vs Age",
    x = "Age (Years)",
    y = "Survival Time (Days)"
  )
cor(age, time, method = "pearson")

# thickness ~ age
ggplot(melanomafor, aes(x = age, y = thickness)) +
  geom_point(color = "purple") +
  geom_smooth(se = FALSE, method = "lm", color = "red", linewidth = 0.5) +
  theme_bw() +
  labs(
    title = "Scatter Plot: Tumor Thickness vs Age",
    x = "Age (Years)",
    y = "Tumor Thickness (mm)"
  )
cor(age, thickness, method = "spearman")


#QUESTION FIVE STARTS HERE
library(tidyverse)
(melanomafor <- read_csv("/Users/aia/Desktop/Data Science Class C1/melanomafor_stat1.csv"))
male <- melanomafor %>% 
  filter(sex == "1")
female <- melanomafor %>% 
  filter(sex == "0")
t.test(male$time, female$time, var.equal = FALSE)
t.test(male$age, female$age, var.equal = FALSE)
t.test(male$thickness, female$thickness, var.equal = FALSE)


#QUESTION SIX STARTS HERE
qqnorm(male$time, main = "Male Survival Time Q-Q PLot")
qqline(male$time)
qqnorm(female$time, main = "Female Survival Time Q-Q PLot")
qqline(female$time)

qqnorm(male$age, main = "Male Age Q-Q PLot")
qqline(male$age)
qqnorm(female$age, main = "Female Age Q-Q PLot")
qqline(female$age)

qqnorm(male$thickness, main = "Male Tumor Thickness Q-Q PLot")
qqline(male$thickness)
qqnorm(female$thickness, main = "Female Tumor Thickness Q-Q PLot")
qqline(female$thickness)