#libraries
library(stats)
library(MASS)


# read data
data <- read.csv("/Users/azadehbayani/Documents/Azadé/UdeM/Master/Projet/Regression/code/CCI10.csv")

#logistic regression model
full.model <- glm(death ~ Mi+Chf+Pvd+Dementia+Cpd+Rheumd+
                    Mld+Hp+Rend+Canc+Msld+Aids,
                  family = binomial(link = "logit"), data = data)

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
  Coefficients = coef,
  P_Values = p_values,
  weighted_coeffs = weighted_coeffs
)



