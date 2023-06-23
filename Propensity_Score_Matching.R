
# Step 1: Install and load the required packages
install.packages("MatchIt")
install.packages("CausalImpact")
#install.packages("causal")

library(MatchIt)
library(CausalImpact)
library(stringr)


df <- read.csv('predictions.csv')

df$probability <- str_extract(df$probability, "(?<=^.{1}).{6}" )

data <- df[,-c(23,24,26)]


data$probability <- as.numeric(data$probability)

data$probability <- 1 - data$probability



## Taking CholCheck as the treatment



# Step 3: Perform matching
match_obj <- matchit(CholCheck ~ probability, data = data, method = "nearest")

# Step 4: Assess the quality of the match
summary(match_obj)


treatment_effect <- with(data, t.test(probability[CholCheck == 1], probability[CholCheck == 0]))

# Step 6: Interpret the results
print(treatment_effect)

############ Cholcheck has a significant impact on diabetes ############



### Using HvyAlcoholConsump as treatent


# Step 3: Perform matching
match_obj <- matchit(HvyAlcoholConsump ~ probability, data = data, method = "nearest")

# Step 4: Assess the quality of the match
summary(match_obj)


treatment_effect <- with(data, t.test(probability[HvyAlcoholConsump == 1], probability[HvyAlcoholConsump == 0]))

# Step 6: Interpret the results
print(treatment_effect)



### Using HighBP as treatent

match_obj <- matchit(HighBP ~ probability, data = data, method = "nearest")

# Step 4: Assess the quality of the match
summary(match_obj)


treatment_effect <- with(data, t.test(probability[HighBP == 1], probability[HighBP == 0]))

# Step 6: Interpret the results
print(treatment_effect)




