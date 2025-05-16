# Load data
heart_data <- read.csv("~/Downloads/heart 2.csv")

# Get counts
n_male <- sum(heart_data$Sex == "M")
n_female <- sum(heart_data$Sex == "F")
x_male <- sum(heart_data$Sex == "M" & heart_data$HeartDisease == 1)
x_female <- sum(heart_data$Sex == "F" & heart_data$HeartDisease == 1)

# Sample proportions
p_hat_male <- x_male / n_male
p_hat_female <- x_female / n_female

# Pooled proportion under null
p_pooled <- (x_male + x_female) / (n_male + n_female)

# Standard error
se <- sqrt(p_pooled * (1 - p_pooled) * (1 / n_male + 1 / n_female))

# Test statistic (Z-score)
z_score <- (p_hat_male - p_hat_female) / se

# Two-sided p-value
p_value <- 2 * pnorm(abs(z_score), lower.tail = FALSE)

# Output
cat("Z-Statistic:", z_score, "\n")
cat("P-Value:", p_value, "\n")