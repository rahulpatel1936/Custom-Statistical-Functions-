# This function is best used to compute t-tests for multiple outcome variables. 
# You will get two outputs: one with full results and one with concise results. 

t_test_results <- function(dataframe, grouping_column_number,
                           grouping_column, group1, group2,
                           starting_column = variable_1_name, ending_column = variable_2_name, df_name,
                           var.equal.argument = TRUE, alternative.argument = "two.sided", pooled.argument = TRUE, hedges.correction.argument = TRUE) {
  
  # Group 1
  dataframe_group_1 <- subset(dataframe, dataframe[grouping_column_number] == group1)
  dataframe_group1_sd <- sapply(dataframe_group_1[starting_column:ending_column], sd)
  dataframe_group1_mean <- sapply(dataframe_group_1[starting_column:ending_column], mean)
  dataframe_group1_n <- sapply(dataframe_group_1[starting_column:ending_column], function(x) sum(!is.na(x)))
  
  # Group 2
  dataframe_group_2 <- subset(dataframe, dataframe[grouping_column_number] == group2)
  dataframe_group2_sd <- sapply(dataframe_group_2[starting_column:ending_column], sd)
  dataframe_group2_mean <- sapply(dataframe_group_2[starting_column:ending_column], mean)
  dataframe_group2_n <- sapply(dataframe_group_2[starting_column:ending_column], function(x) sum(!is.na(x)))
  
  # Get statistics and related values
  p_values <- numeric(length = length(starting_column:ending_column))
  t_statistic <- numeric(length = length(starting_column:ending_column))
  df <- numeric(length = length(starting_column:ending_column))
  m_diff_lower <- numeric(length = length(starting_column:ending_column))
  m_diff_upper <- numeric(length = length(starting_column:ending_column))
  m_diff_ci <- numeric(length = length(starting_column:ending_column))
  cohen_d <- numeric(length = length(starting_column:ending_column))
  cohen_d_lower <- numeric(length = length(starting_column:ending_column))
  cohen_d_upper <- numeric(length = length(starting_column:ending_column))
  power <- numeric(length = length(starting_column:ending_column))
  
  # A for loop that will get the above statistics and related values for each item/outcome
  
  for (i in starting_column:ending_column) {
    p_values[i] <- t.test(dataframe[, i] ~ grouping_column,
                          var.equal = var.equal.argument,
                          alternative = alternative.argument)$p.value
    t_statistic[i] <- t.test(dataframe[, i] ~ grouping_column,
                             var.equal = var.equal.argument,
                             alternative = alternative.argument)$statistic 
    df[i] <- t.test(dataframe[, i] ~ grouping_column,
                    var.equal = var.equal.argument,
                    alternative = alternative.argument)$parameter
    m_diff_lower[i] <- t.test(dataframe[, i] ~ grouping_column,
                              var.equal = var.equal.argument,
                              alternative = alternative.argument)$conf.int[1]
    m_diff_upper[i] <- t.test(dataframe[, i] ~ grouping_column,
                              var.equal = var.equal.argument,
                              alternative = alternative.argument)$conf.int[2]
    m_diff_ci[i] <- paste("[", format(round(t.test(dataframe[, i] ~ grouping_column,
                                                   var.equal = var.equal.argument,
                                                   alternative = alternative.argument)$conf.int[1],2), nsmall = 2), ", ", format(round(t.test(dataframe[, i] ~ grouping_column,
                                                                                                                                              var.equal = F,
                                                                                                                                              alternative = "two.sided")$conf.int[2],2), nsmall = 2), "]", sep = "")
    cohen_d[i] <- effsize::cohen.d(dataframe[, i] ~ grouping_column,
                                   pooled = pooled.argument,
                                   hedges.correction = hedges.correction.argument,
                                   na.rm = T)$estimate
    cohen_d_lower[i] <- as.numeric(effsize::cohen.d(dataframe[, i] ~ grouping_column,
                                                    pooled = pooled.argument,
                                                    hedges.correction = hedges.correction.argument,
                                                    na.rm = T)$conf.int[1])
    cohen_d_upper[i] <- as.numeric(effsize::cohen.d(dataframe[, i] ~ grouping_column,
                                                    pooled = pooled.argument,
                                                    hedges.correction = hedges.correction.argument,
                                                    na.rm = T)$conf.int[2])
    power[i] <- as.numeric(pwr.t2n.test(n1 = dataframe_group1_n, n2 = dataframe_group2_n, d = cohen_d[i],
                                        sig.level = 0.05,
                                        power = NULL,
                                        alternative = alternative.argument)$power)
  }
  
  # Bind results to a new dataframe
  new_df_results <- data.frame(
    variables = colnames(dataframe[starting_column:ending_column]),
    mean_group1 = format(round(dataframe_group1_mean,2), nsmall = 2),
    sd_group1 = format(round(dataframe_group1_sd, 2), nsmall = 2),
    n_group1 = dataframe_group1_n,
    mean_group2 = format(round(dataframe_group2_mean,2), nsmall = 2),
    sd_group2 = format(round(dataframe_group2_sd, 2), nsmall = 2),
    n_group2 = dataframe_group2_n,
    m_diff = format(round(dataframe_group1_mean - dataframe_group2_mean, 2), nsmall = 2),
    m_diff_lower = format(round(m_diff_lower, 2), nsmall = 2)[starting_column:ending_column],
    m_diff_upper = format(round(m_diff_upper, 2), nsmall = 2)[starting_column:ending_column],
    m_diff_ci = m_diff_ci[starting_column:ending_column],
    df = format(round(df, 2), nsmall = 2)[starting_column:ending_column],
    t_statistic = format(round(t_statistic,2), nsmall = 2)[starting_column:ending_column],
    p_value = format(round(p_values, 3), nsmall = 2)[starting_column:ending_column],
    cohen_d = format(round(cohen_d, 2), nsmall = 2)[starting_column:ending_column],
    cohen_d_lower = format(round(cohen_d_lower, 2), nsmall = 2)[starting_column:ending_column],
    cohen_d_upper = format(round(cohen_d_upper, 2), nsmall = 2)[starting_column:ending_column],
    power = format(round(power*100, 2), nsmall = 2)[starting_column:ending_column]
  )
  
  new_df_results <- new_df_results %>%
    arrange(variables)
  
  new_df_results_concise <- data.frame(
    variables = colnames(dataframe[starting_column:ending_column]),
    mean_group1 = format(round(dataframe_group1_mean,2), nsmall = 2),
    sd_group1 = format(round(dataframe_group1_sd, 2), nsmall = 2),
    mean_group2 = format(round(dataframe_group2_mean,2), nsmall = 2),
    sd_group2 = format(round(dataframe_group2_sd, 2), nsmall = 2),
    m_diff = format(round(dataframe_group1_mean - dataframe_group2_mean, 2), nsmall = 2),
    m_diff_ci = m_diff_ci[starting_column:ending_column]
  )
  
  new_df_results_concise <- new_df_results_concise %>%
    arrange(variables)
  
  # Rename columns
  colnames(new_df_results) <- c("variables",
                                paste("mean", group1, sep = "_"),
                                paste("sd", group1, sep = "_"),
                                paste("n", group1, sep = "_"),
                                paste("mean", group2, sep = "_"),
                                paste("sd", group2, sep = "_"),
                                paste("n", group2, sep = "_"),
                                "m_diff",
                                "m_diff_lower",
                                "m_diff_upper",
                                "95% CI [LL, UL]",
                                "df",
                                "t_statistic",
                                "p_value",
                                "d_value",
                                "d_value_lower",
                                "d_value_upper",
                                "power")
  # Rename columns
  colnames(new_df_results_concise) <- c("variables",
                                        paste("mean", group1, sep = "_"),
                                        paste("sd", group1, sep = "_"),
                                        paste("mean", group2, sep = "_"),
                                        paste("sd", group2, sep = "_"),
                                        "m_diff",
                                        "95% CI [LL, UL]"
  )
  
  # Export as dataframe to global environment
  assign(paste(df_name), new_df_results, envir = globalenv())
  # Export as dataframe to global environment
  assign(paste(df_name, "concise", sep = "_"), new_df_results_concise, envir = globalenv())
  
}

# General example use 
# t_test_results_object <- (dataframe, 
  #grouping_column_number = which(colnames(prolific_elra_subset) == "grouping_variable_name"), 
  #grouping_column = your_data$grouping_variable_name,
  #group1 = "variable_1", group2 = "variable_2, # specify the two levels in your grouping variable, surround with quotes
  #starting_column = which(colnames(prolific_elra_subset) == "outcome_1"),
  #ending_column = which(colnames(prolific_elra_subset) == "outcome_2"),
  #df_name, # specify the name of your new dataframes containing your results (e.g., "my_t_test_results")
  #var.equal.argument = TRUE, alternative.argument = "two.sided", pooled.argument = TRUE, hedges.correction.argument = TRUE) {
  
