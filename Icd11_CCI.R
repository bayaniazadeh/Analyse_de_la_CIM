#install.packages("writexl")
library(stats)
library(MASS)
library(writexl)


data <- read.csv("/Users/azadehbayani/Documents/Azadé/UdeM/Master/Projet/Regression/code/icd9_EliX.csv")

full.model <- glm(death ~ Chf + Carit+Valv+Pcd+Pvd+Para+
Ond+Cpd+Hypohthy+Rf+Ld+Aids+
Lymph+Solidtum+Rheumd+Coag+Wloss+Fed+Blane+
Dane+Alcohol+Drug+Psycho+Depre, family = binomial(link = "logit"), data = data)

summary(full.model)
backward.model <- stepAIC(full.model, direction = "backward")

coef <- coef(backward.model)

summary(backward.model)
p_values <- summary(backward.model)$coefficients[, "Pr(>|z|)"]

# Print the p-values
print(p_values)

#backward.model$
print(coef)
min_c = min(abs(coef))
weighted_coeffs = abs(coef)/min_c
print(weighted_coeffs)


# Create a new data frame to store the summary statistics and coefficients
result_df <- data.frame(
  #Summary = c("Coefficients", summary(backward.model)),
  Coefficients = coef,
  P_Values = p_values,
  weighted_coeffs = weighted_coeffs
)



