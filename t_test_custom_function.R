# Install necessary packages for the custom t-test function to run
install.packages("stats", "MBESS", "psych")

# Be sure to define your two groups beforehand. See a general example below
## Group 1 Data
# dataset %>%
  # filter(as.factor([your grouping column]) == "[your first group]")

# Group 2 Data
# dataset %>%
# filter(as.factor([your grouping column]) == "[your second group]")

# Define custom t-test function
t_test_custom_function <- function(group_1_data, group_1_name,
                                   group_2_data, group_2_name,
                                   var.equal.argument, alternative.argument, paired.argument) {
  
  # Group 1 
  print(paste(group_1_name, "Descriptives:"))
  descriptives_group_1_data <- psych::describeBy(group_1_data)
  print(descriptives_group_1_data)
  
  # Group 2
  print(paste(group_2_name, "Descriptives:"))
  descriptives_group_2_data <- psych::describeBy(group_2_data)
  print(descriptives_group_2_data)
  
  # Define t-test object
  rating_t <- t.test(x = group_1_data,
                     y = group_2_data,
                     paired = paired.argument, # TRUE or FALSE
                     var.equal = var.equal.argument, # TRUE or FALSE
                     alternative = alternative.argument) #"two.sided", "greater", or "less"
  # Print the results of the t-test
  print(rating_t)
  
  # Print mean difference
  # Mean of Group 1:
  mean_group_1 <- as.numeric(rating_t$estimate[1])
  
  # Mean of Group 2:
  mean_group_2 <- as.numeric(rating_t$estimate[2])
  
  mean_difference <- mean_group_1 - mean_group_2
  

  # Mean difference 
  ifelse(mean_difference < 0, print(paste(group_1_name, " is less than ", group_2_name)),
         print(paste(group_2_name, " is larger than ", group_1_name)))
  
  # Print the d-value
  d_value <- smd(Group.1 = group_1_data, Group.2 = group_2_data)
  print(paste("d-value:", d_value, sep = " "))
  
  # Get the confidence intervals on the d-value
  ci_d_value <- ci.smd(smd = d_value, n.1 = length(group_1_data), n.2 = length(group_2_data))
  print(ci_d_value)
}

# General Example 
# t_test_custom_function(group_1_data = group_1$dependent_variable, group_1_name = "Your Group 1",
                       # group_2_data = group_2$dependent_variable, group_2_name = "Your Group 2",
                       # var.equal.argument = TRUE, alternative.argument = "two.sided", paired.argument = FALSE)

