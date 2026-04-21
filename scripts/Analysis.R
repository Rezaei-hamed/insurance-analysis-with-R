install.packages("tidyverse")

library(tidyverse)

df <- read_csv("data/insurance_costs.csv")


head(df)
str(df)
summary(df)

glimpse(df)


colSums(is.na(df))



df <- df %>%
  mutate(customer_id = sub("^C", "", customer_id))


df$age <- as.integer((df$age))
df$children <- as.integer((df$children))

df $sex <- as.factor(df $sex)
df $region <- as.factor(df $region)
df $smoker <- as.factor(df $smoker)



glimpse(df)


df <-df %>% 
  mutate(
    bmi_cat = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi < 25 ~ "Normal",
      bmi < 30 ~ "Overweight",
      TRUE ~ "Obese"          # alla resterande fall 
    ),
    # att göra ordning och blir logisk så la jag level 
    bmi_cat =factor(
      bmi_cat,
      levels = c("Underweight", "Normal", "Overweight", "Obese")
    )
  )


df <- df %>% 
  mutate(
    age_group =case_when(
      age < 20 ~ "Young",
      age < 35 ~ "Adult",
      age < 50 ~ "Middle Aged",
      TRUE  ~ "Senior"
    ),
    age_group =factor(
      age_group,
      levels =c("Young", "Adult", "Middle Aged", "Senior")
    )
  )



summary(df)

df %>% 
  summarise(
    mean_cost =mean (charges),
    medain_cost =median(charges),
    max_cost = max(charges)
  )


#group analysis

df %>% 
  group_by(smoker) %>% 
  summarise(mean_cost =mean(charges))

#bmi kategori 
df %>% 
  group_by(bmi_kategori) %>% 
  summarise(mean_cost =mean(charges))

#Ålders group
df %>% 
  group_by(age_group) %>% 
  summarise(mean_cost =mean(charges))

#Visualisering
ggplot(df, aes(x =charges))+
  geom_histogram(bins =30, fill ="skyblue", color ="white")+
  labs(
    title = "Distribution of insurance Charges ",
    x = "Charge",
    y = "Count"
  )+
theme_linedraw()


ggplot(df, aes(x = smoker, y = charges))+
  geom_boxplot(alpha = 0.5)+
  labs(
    title = "charges by smoking status ",
    x = "smoking status",
    y = "charges "
  )+
  theme_minimal()


ggplot(df, aes(x = age, y =charges))+
  geom_point(alpha = 0.6)+
  geom_smooth(method = "lm",se =FALSE, color="red")+
  labs(
    title = "Charges vs Age",
    x = "Age",
    y = "Charges"
  )+
  theme_minimal()


ggplot(df, aes(x = bmi_cat, y = charges, fill = bmi_cat))+
  geom_boxplot(alpha = 0.6)+
  labs(
    title = "Charges by bmi categori",
    x = "bmi categori",
    y = "Charges"
  )+
  theme_minimal()


#Regression
model_1 <- lm(charges ~ age + bmi + smoker + children, data = df)
summary(model_1)


model_2 <- lm(charges ~ age+ bmi+ smoker+ children + bmi_cat + age_group, data =df)
summary(model_2)



### Behöver jobba vidare med detta -----

model_comparison <- tibble(
  model =c(
    "Model 1: age + bmi + smoker + children",
    "Model 2 :bmi_cat + age_group"
  ),
  r_squared =c(
    summary(model_1)$r.squared,
    summary(model_2)$r.squared
  ),
  adjusted_r_squared =c(
    summary(model_1)$adj.r.squared,
    summary(model_2)$adj.r.squared
  ),
  residual_se =c(
    summary(model_1)$sigma,
    summary(model_2)$sigma
  )
)

model_comparison







AIC(model_1,model_2)


