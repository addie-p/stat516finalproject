# Load dataset
heart.2 <- read.csv("~/Downloads/heart 2.csv")

# For the code, assume:
# (1) Sex column has values "M" and "F"
# (2) HeartDisease column has values 1 (disease) and 0 (no disease)

# Count number of male and female patients with and without heart disease
table(heart.2$Sex, heart.2$HeartDisease)

# Extract counts
x_male <- sum(heart.2$Sex == "M" & heart.2$HeartDisease == 1)
n_male <- sum(heart.2$Sex == "M")

x_female <- sum(heart.2$Sex == "F" & heart.2$HeartDisease == 1)
n_female <- sum(heart.2$Sex == "F")

# Sample proportions
p_hat_m <- x_male / n_male
p_hat_f <- x_female / n_female
diff_hat <- p_hat_m - p_hat_f

# Standard error
se <- sqrt(
  p_hat_m * (1 - p_hat_m) / n_male +
    p_hat_f * (1 - p_hat_f) / n_female
)

# 95% confidence interval
z_star <- qnorm(0.975)
ci_lower <- diff_hat - z_star * se
ci_upper <- diff_hat + z_star * se


# Print results
cat("Estimated diff. in proportions (p_male - p_female):", round(diff_hat, 3), "\n")
cat("Standard error:", round(se, 3), "\n")
cat("95% CI:", round(ci_lower, 3), "to", round(ci_upper, 3), "\n")