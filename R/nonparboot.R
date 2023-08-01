#' Nonparametric Bootstrap Test with Pooled Resampling for Small Sample Sizes
#'
#' This function performs a nonparametric bootstrap test with pooled resampling
#' for small sample sizes, as described in Dwivedi et al. (2017). It supports
#' t-tests (independent and paired) and F-tests (one-way ANOVA), with a
#' user-specified number of bootstrap resamples.
#'
#' @param data A data frame containing the variables to be analyzed.
#' @param x A character string specifying the column in 'data' to be used as the
#'   primary variable.
#' @param y An optional character string specifying the column in 'data' to be
#'   used as the second variable for paired t-tests. Default is NULL.
#' @param grp An optional character string specifying the column in 'data' to be
#'   used as the grouping variable for independent t-tests and F-tests. Default
#'   is NULL.
#' @param nboot An integer specifying the number of bootstrap resamples to
#'   perform.
#' @param test A character string specifying the type of test to perform. Must
#'   be one of "t", "pt", or "F" for independent t-test, paired t-test, or F-test,
#'   respectively. Default is "t".
#' @param conf.level A numeric value between 0 and 1 specifying the confidence
#'   level for confidence intervals. Default is 0.95.
#' @param seed An optional integer value to set the seed for the random number
#'   generator, for reproducibility. Default is NULL (no seed).
#'
#' @return A list with the following components:
#' \itemize{
#'   \item \code{p.value}: The p-value of the test.
#'   \item \code{orig.stat}: The test statistic calculated from the original data.
#'   \item \code{ci.stat}: The confidence interval for the test statistic from the bootstrap distribution.
#'   \item \code{bootstrap.stat.dist}: The distribution of the test statistic values from the bootstrap resamples.
#'   \item \code{effect.size}: The effect size (mean difference or eta-squared) calculated from the original data.
#'   \item \code{ci.effect.size}: The confidence interval for the effect size from the bootstrap distribution.
#'   \item \code{bootstrap.effect.dist}: The distribution of effect size values from the bootstrap resamples.
#' }
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA (2017). "Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method." Statistics
#' in Medicine, 36 (14), 2187-2205. https://doi.org/10.1002/sim.7263
#'
#' @examples
#' # Example usage of nonparboot
#' np_res <- nonparboot(iris, x = "Sepal.Length", grp = "Species", nboot = 1000, test = "F")
#' print(np_res$p.value)
#' @export
nonparboot <- function (data, x, y = NULL, grp = NULL, nboot,
                        test = c("t", "pt", "F"), conf.level = 0.95,
                        seed = NULL) {
  # Check parameters
  stopifnot(is.data.frame(data))
  stopifnot(is.character(x))
  stopifnot(is.null(y) || is.character(y))
  stopifnot(is.null(grp) || is.character(grp))
  stopifnot(is.numeric(nboot) && length(nboot) == 1L && nboot > 0 && nboot == as.integer(nboot))
  stopifnot(is.character(test) && length(test) == 1L && test %in% c("t", "pt", "F"))
  stopifnot(is.numeric(conf.level) && length(conf.level) == 1L && conf.level > 0 && conf.level < 1)
  stopifnot(is.null(seed) || (is.numeric(seed) && length(seed) == 1L && seed == as.integer(seed)))

  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # test argument
  test <- match.arg(test)

  # Select appropriate bootstrap sampling function based on the test type
  if (test == "t") {
    sample_fun <- bootstrap_t_sample
    x_val <- data[[x]]
    grp_val <- data[[grp]]
    unique_grp <- unique(grp_val)
    grp_sizes <- table(grp_val)
    pre_calc <- list(orig_stat = stats::t.test(x_val ~ grp_val)$statistic,
                     orig_diff = mean(x_val[grp_val == unique_grp[1]]) - mean(x_val[grp_val == unique_grp[2]]))
    } else if (test == "pt") {
    sample_fun <- bootstrap_pt_sample
    x_val <- data[[x]]
    y_val <- data[[y]]
    n <- length(x_val)
    pre_calc <- list(orig_stat = stats::t.test(x_val, y_val, paired = TRUE)$statistic,
                     orig_diff = mean(x_val - y_val))
    } else {  # test == "F"
      sample_fun <- bootstrap_f_sample
      x_val <- data[[x]]
      grp_val <- data[[grp]]
      grp_sizes <- table(grp_val)
      anova_orig <- stats::anova(stats::lm(x_val ~ grp_val))
      pre_calc <- list(orig_stat = anova_orig$`F value`[1],
                       orig_diff = anova_orig$`F value`[1] * anova_orig$Df[1] / (anova_orig$`F value`[1] * anova_orig$Df[1] + anova_orig$Df[2]))
    }

  # Perform bootstrap resampling
  bootstrap_results <- replicate(nboot, sample_fun(x_val, y_val, grp_val, grp_sizes, pre_calc))

  # Extract the bootstrapped statistics and differences/effects
  stat_values <- bootstrap_results[1, ]
  diff_values <- bootstrap_results[2, ]

  # Calculate the p-value
  p_boot <- mean(abs(stat_values) >= abs(pre_calc$orig_stat), na.rm = TRUE)

  # Calculate the confidence interval for the test statistic
  ci_stat <- stats::quantile(stat_values, c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2))

  # Calculate the confidence interval for the difference/effect
  ci_diff <- stats::quantile(diff_values, c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2))

  return(list(p.value = p_boot, orig.stat = pre_calc$orig_stat,
              ci.stat = ci_stat, bootstrap.stat.dist = stat_values,
              effect.size = pre_calc$orig_diff, ci.effect.size = ci_diff,
              bootstrap.effect.dist = diff_values))
}

#' Internal Bootstrap Sampling Function for T-tests
#'
#' This is an internal function used by nonparboot() to perform bootstrap
#' resampling for independent t-tests.
#'
#' @param x_val A numeric vector of values from the primary variable.
#' @param y_val A numeric vector of values from the second variable
#'   (only used for paired t-tests).
#' @param grp_val A factor vector of group labels (only used for independent
#'   t-tests and F-test).
#' @param grp_sizes A table of group sizes (only used for independent t-tests and
#'   F-test).
#' @param pre_calc A list containing pre-calculated statistics from the original
#'   data.
#'
#' @return A numeric vector of length 2 containing the bootstrapped test statistic
#'   and mean difference.
#'
#' @keywords internal
bootstrap_t_sample <- function(x_val, y_val, grp_val, grp_sizes, pre_calc) {
  unique_grp <- unique(grp_val)
  group1 <- sample(x_val, size = grp_sizes[unique_grp[1]], replace = TRUE)
  group2 <- sample(x_val, size = grp_sizes[unique_grp[2]], replace = TRUE)
  if (stats::sd(group1)==0 & stats::sd(group2)==0){
    t_stat <- NA
    diff_mean <- NA
  } else {
    t_stat <- stats::t.test(group1, group2, var.equal = TRUE, na.rm = T)$statistic
    diff_mean <- mean(group1) - mean(group2)
  }
  return(c(t_stat, diff_mean))
}

#' Internal Bootstrap Sampling Function for Paired T-tests
#'
#' This is an internal function used by nonparboot() to perform bootstrap
#' resampling for paired t-tests.
#'
#' @param x_val A numeric vector of values from the primary variable.
#' @param y_val A numeric vector of values from the second variable.
#' @param grp_val A factor vector of group labels. This parameter is not used in
#'   this function but is included for consistency with other bootstrap sampling
#'   functions.
#' @param grp_sizes A table of group sizes. This parameter is not used in this
#'   function but is included for consistency with other bootstrap sampling
#'   functions.
#' @param pre_calc A list containing pre-calculated statistics from the original
#'   data.
#'
#' @return A numeric vector of length 2 containing the bootstrapped test statistic
#'   and mean difference.
#'
#' @keywords internal
bootstrap_pt_sample <- function(x_val, y_val, grp_val, grp_sizes, pre_calc) {
  all_vals <- c(x_val, y_val)
  group1 <- sample(all_vals, size = length(x_val), replace = TRUE)
  group2 <- sample(all_vals, size = length(x_val), replace = TRUE)
  if (stats::sd(group1)==0 & stats::sd(group2)==0){
    t_stat <- NA
    diff_mean <- NA
  } else {
    t_stat <- stats::t.test(group1, group2, paired = TRUE)$statistic
    diff_mean <- mean(group1 - group2)
  }
  return(c(t_stat, diff_mean))
}

#' Internal Bootstrap Sampling Function for F-tests
#'
#' This is an internal function used by nonparboot() to perform bootstrap
#' resampling for F-tests. It is not intended to be called directly by the user.
#'
#' @param x_val A numeric vector of values from the primary variable.
#' @param y_val A numeric vector of values from the second variable. This
#'   parameter is not used in this function but is included for consistency with
#'   other bootstrap sampling functions.
#' @param grp_val A factor vector of group labels.
#' @param grp_sizes A table of group sizes.
#' @param pre_calc A list containing pre-calculated statistics from the original
#'   data.
#'
#' @return A numeric vector of length 2 containing the bootstrapped test statistic
#'   and the effect size.
#'
#' @keywords internal
bootstrap_f_sample <- function(x_val, y_val, grp_val, grp_sizes, pre_calc) {
  grp_unique <- unique(grp_val)

  # Create bootstrap samples for each group
  bootstrap_samples <- lapply(grp_unique, function(g) {
    sample(x_val, size = grp_sizes[g], replace = TRUE)
  })

  # Check if the standard deviation of each group's bootstrap sample is zero
  all_sd_zero <- all(sapply(bootstrap_samples, stats::sd) == 0)

  if (all_sd_zero) {
    return(c(NA, NA))
  }

  val_boot <- unlist(bootstrap_samples)
  grp_boot <- rep(grp_unique, times = sapply(bootstrap_samples, length))

  anova_boot <- stats::anova(stats::lm(val_boot ~ factor(grp_boot)))
  f_boot <- anova_boot$`F value`[1]
  df1_boot <- anova_boot$Df[1]
  df2_boot <- anova_boot$Df[2]
  eta2_boot <- f_boot * df1_boot / (f_boot * df1_boot + df2_boot)

  return(c(f_boot, eta2_boot))
}

