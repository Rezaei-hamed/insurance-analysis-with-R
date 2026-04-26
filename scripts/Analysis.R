library(tidyverse)


#läser in data och får en översikt 
df <- read_csv("../data/insurance_costs.csv")


#Att se datatyper ,sammanfattning och saknade värde
head(df)
str(df)
summary(df)

#översikt och datatyper och variabler 
glimpse(df)

#Att se om det finns saknade värde 
colSums(is.na(df))


# Datastädning och omvandlar variabler till rätt datatyper 
df <- df %>%
  mutate(
    customer_id = as.integer(sub("^C", "", customer_id)),
    age = as.integer(age),
    children = as.integer(children),
    sex = as.factor(sex),
    region = as.factor(region),
    smoker = tolower(smoker),
    smoker = as.factor(smoker)
  )
    

# Variabler som exercise_level, plan_type och chronic_condition
# finns i datasetet men inkluderas inte i modellen 
#detta är en begränsning och kan påverka resultatet.





#Kontrollerar att ändringar är korrekta
glimpse(df)

#skapar nya variabler (motivering: förenklar analys av hur hälsa påverkar kostnader )
df <- df %>% 
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

# Skapar åldersgrupper(motivering: det gör lättare att jämföra olika livsfaser)
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


#samanfattning efter datastädning
summary(df)


#Grundläggande statistik över kostnader 
summary_stats <-df %>% 
  summarise(
    mean_cost = mean (charges, na.rm = TRUE),
    median_cost = median(charges, na.rm =TRUE),
    max_cost = max(charges, na.rm = TRUE)
  )


#group analysis
#jämför kostnader mellan rökare och ej rökare 
df %>% 
  group_by(smoker) %>% 
  summarise(mean_cost = mean(charges, na.rm = TRUE), .groups = "drop")

#jämför kostnader mellan bmi kategori 
df %>% 
  group_by(bmi_cat) %>% 
  summarise(mean_cost = mean(charges, na.rm = TRUE), .groups ="drop")

#jämför kostnader mellan ålders group
df %>% 
  group_by(age_group) %>% 
  summarise(mean_cost = mean(charges, na.rm = TRUE), .groups = "drop")

#Visualisering

#visar hur kostnaderna är fördelade 
charges_hist_plot <- ggplot(df, aes(x =charges))+
  geom_histogram(bins =30, fill ="skyblue", color ="white")+
  labs(
    title = "Distribution of insurance Charges ",
    x = "Charge",
    y = "Count"
  )+
theme_linedraw()

#histogram tolkning:
#kostnaderna är snedfördelade där de flesta har låga kostnader 
# men några har mycket höga värden



#visar skillnad i kostnad mellan rökare och ej rökare 
charges_smoker_plot <- ggplot(df, aes(x = smoker, y = charges))+
  geom_boxplot(alpha = 0.5)+
  labs(
    title = "Charges by smoking status ",
    x = "Smoking status",
    y = "Charges "
  )+
  theme_minimal()
#Tolkning:Rökare har betydligt högre kostnader än ej rökare 





#visar samband mellan ålder och kostnader 
age_charges_scatter_plot <- ggplot(df, aes(x = age, y =charges))+
  geom_point(alpha = 0.6)+
  geom_smooth(method = "lm",se =FALSE, color="red")+
  labs(
    title = "Charges vs Age",
    x = "Age",
    y = "Charges"
  )+
  theme_minimal()
# Tolkning:kostnader ökar med ålder ,vilket tyder på ett positivt samband 



#visar skillnad i kostnader mellan bmi-kategorier 
charges_bmi_box_plot <- ggplot(df, aes(x = bmi_cat, y = charges, fill = bmi_cat))+
  geom_boxplot(alpha = 0.6)+
  labs(
    title = "Charges by bmi categori",
    x = "bmi categori",
    y = "Charges"
  )+
  theme_minimal()
#Tolkning: högre BMI är kopplat till högre kostnader,särskilt i "Obese"




#tar bort saknade värden inför modellering
df_model <- df %>%  drop_na()


#Regression

#model_1 :undersöker direkt samband mellan variabler och kostnader 
model_1 <- lm(charges ~ age + bmi + smoker + children, data = df_model)
summary(model_1)
# Tolkning:smoker har störst påverkan på kostnader,följt av ålder och BMI



#model_2 :undersöka skillnader mellan grupper 
model_2 <- lm(charges ~ smoker+ children + bmi_cat + age_group, data =df_model)
summary(model_2)
#Tolkning:Modellen visar skillnader mellan grupper,för rökare och BMI






# Modellkontroll (diagnostics)

# 1. Normalfördelning av residualer (Q-Q plot)
qqnorm(resid(model_2))
qqline(resid(model_2), col = "red")

# 2. Alla diagnostiska grafer
par(mfrow = c(2,2))   # visar 4 plots i samma fönster
plot(model_2)

# Återställ layout
par(mfrow = c(1,1))
# qqnorm + qqline → kontrollerar om residualerna är normalfördelade
# plot(model_2) → visar 4 viktiga tester:
# 1. Residuals vs Fitted → linjäritet
# 2. Normal Q-Q → normalfördelning
# 3. Scale-Location → konstant varians
# 4. Residuals vs Leverage → outliers






#jämför modeller 

model_comparison <- tibble(
  model =c(
    "Model 1: age + bmi + smoker + children",
    "Model 2: smoker + children + bmi_cat + age_group"
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



#samlar alla grafer
plots <- list(
  hist = charges_hist_plot,
  smoker = charges_smoker_plot,
  age = age_charges_scatter_plot,
  bmi = charges_bmi_box_plot
)


#slutsats:
# Rökning,ålder och BMI är viktigaste faktorer som påverkar kostnader 
#Modellen visar tydliga samband men fångar inte alla faktorer.



#Reflektion:
#Jag tycker att jag lyckades bra med att strukturera analysen och använda båda
#visualisering och regression.Det svårare var att tolka modellerna.
#Jag anser att arbetet motsvarar VG eftersom jag uppfyller alla krav.




