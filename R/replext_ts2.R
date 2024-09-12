#' Replicate and Extend Type I Error Rates for ANOVA in a Three-Sample Setting
#'
#' This function aims to replicate and extend the Type I error rate analysis for
#' ANOVA (Analysis of Variance) from the supplemental tables of the paper by
#' Dwivedi et al. (2017). It allows for the simulation of three-sample scenarios
#' with the option to use either normal or skew normal distributions, and
#' performs various statistical tests to assess the Type I error rates.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param M3 Mean for the third group, default is 5.
#' @param S3 Standard deviation for the third group, default is 1.
#' @param Sk1 Skewness parameter for the first group, NULL implies normal distribution.
#' @param Sk2 Skewness parameter for the second group, NULL implies normal distribution.
#' @param Sk3 Skewness parameter for the third group, NULL implies normal distribution.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group.
#' @param n3 Vector of sample sizes for the third group, must be the same length as n1 and n2.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples for the nonparametric bootstrap test, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size combination (n1, n2, n3) and
#'         the proportions of significant p-values for each test (ANOVA, Kruskal-Wallis, Nonparametric Bootstrap F-test, Permutation F-test).
#'
#' @examples
#' replext_ts2_c1.1(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @importFrom lmPerm lmp
#' @export
replext_ts2_c1.1 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, M3 = 5, S3 = 1,
                             Sk1 = NULL, Sk2 = NULL, Sk3 = NULL,
                            n1 = c(2,3,4,5,6,7,8,9,10,15),
                            n2 = c(2,3,4,5,6,7,8,9,10,15),
                            n3 = c(2,3,4,5,6,7,8,9,10,15),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  # Ensure n1 and n2 and n3 are of equal length
  if (!(length(n1) == length(n2) & length(n2) == length(n3))) {
    stop("n1 and n2 and n3 must be of equal length")
  }

  # Get results for one iteration of the simulation or one pair of sample sizes
  get_result <- function(n1, n2, n3) {

    # Generate normal or skew normal data
    generate_data <- function(n, mean, sd, skew) {
      if (is.null(skew) || skew == 0) {
        if(!is.null(skew)){
          warning("Using stats::rnorm() instead of fGarch::rsnorm() since skew = 0")
        }
        stats::rnorm(n, mean, sd)
      } else {
        fGarch::rsnorm(n, mean, sd, xi = skew)
      }
    }

    F1 <- generate_data(n1, M1, S1, Sk1)
    F2 <- generate_data(n2, M2, S2, Sk2)
    F3 <- generate_data(n3, M3, S3, Sk3)

    df <- data.frame(x = c(F1, F2, F3), grp = rep(c("F1", "F2", "F3"), c(n1, n2, n3)))

    # Test p-value for each method
    npbft <- tryCatch({
      nonparboot(df, x = "x", grp = "grp", nboot = nboot, test = "F", conf.level = conf.level)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    anov <- tryCatch({
      stats::anova(stats::lm(x ~ factor(grp), df))$`Pr(>F)`[1] <= 1 - conf.level
    }, error = function(e) NA)

    kw <- tryCatch({
      stats::kruskal.test(x ~ factor(grp), df)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    pft <- tryCatch({
      lmPerm::aovp(x ~ factor(grp),df)$perm$P[2,1] <= 1 - conf.level
    }, error = function(e) NA)

    # Returning a named vector
    return(c(NPBFT = npbft, ANOV = anov, KW = kw, PFT = pft))
  }

  # Run n_simulations at each sample size pair
  results <- lapply(seq_along(n1), function(i) {
    replicate(n_simulations, get_result(n1[i], n2[i], n3[i]))
  })

  # Use lapply to calculate proportions and create a data frame for each sample size combination
  proportions_list <- lapply(seq_along(results), function(i) {
    proportions <- rowMeans(results[[i]], na.rm = TRUE)
    data.frame(
      n1 = n1[i],
      n2 = n2[i],
      n3 = n3[i],
      ANOV = proportions["ANOV"],
      KW = proportions["KW"],
      NPBFT = proportions["NPBFT"],
      PFT = proportions["PFT"]
    )
  })

  # Combine the list of data frames into a single data frame
  proportions_df <- do.call(rbind, proportions_list)

  return(proportions_df)
}

#' Replicate and Extend Type I Error Rates for ANOVA in a Different Setting
#'
#' This wrapper function is designed to reproduce or extend the Type I error rate analysis for
#' ANOVA (Analysis of Variance) in a different setting as compared to `replext_ts2_c1.1`. It utilizes
#' different default values for the standard deviations of the second and third groups, allowing
#' for a different simulation setup. It is part of the analysis extending the supplemental tables
#' of the paper by Dwivedi et al. (2017).
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 2.
#' @param M3 Mean for the third group, default is 5.
#' @param S3 Standard deviation for the third group, default is 4.
#' @param Sk1 Skewness parameter for the first group, NULL implies normal distribution.
#' @param Sk2 Skewness parameter for the second group, NULL implies normal distribution.
#' @param Sk3 Skewness parameter for the third group, NULL implies normal distribution.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group.
#' @param n3 Vector of sample sizes for the third group, must be the same length as n1 and n2.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples for the nonparametric bootstrap test, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame similar to `replext_ts2_c1.1` with columns for each sample size combination
#'         (n1, n2, n3) and the proportions of significant p-values for each test (ANOVA,
#'         Kruskal-Wallis, Nonparametric Bootstrap F-test, Permutation F-test), but with the modified
#'         default parameters.
#'
#' @examples
#' replext_ts2_c1.2(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts2_c1.2 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 2, M3 = 5, S3 = 4,
                             Sk1 = NULL, Sk2 = NULL, Sk3 = NULL,
                             n1 = c(2,3,4,5,6,7,8,9,10,15),
                             n2 = c(2,3,4,5,6,7,8,9,10,15),
                             n3 = c(2,3,4,5,6,7,8,9,10,15),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}

#' Replicate and Extend Type I Error Rates for ANOVA with Skewness
#'
#' This function, `replext_ts2_c2.1`, extends the `replext_ts2_c1.1` function to specifically
#' simulate scenarios under skew normal distributions. It is tailored to explore the impact of
#' skewness on the Type I error rates in ANOVA (Analysis of Variance), contributing to the
#' comprehensive analysis in the context of the study by Dwivedi et al. (2017). The function allows
#' for simulations under the assumption of skewness in all three groups.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param M3 Mean for the third group, default is 5.
#' @param S3 Standard deviation for the third group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param Sk3 Skewness parameter for the third group, default is 0.8.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group.
#' @param n3 Vector of sample sizes for the third group, must be the same length as n1 and n2.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples for the nonparametric bootstrap test, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with results similar to those from `replext_ts2_c1.1`, but with the added
#'         dimension of skewness. The data frame includes columns for each sample size combination
#'         (n1, n2, n3) and the proportions of significant p-values for each test (ANOVA,
#'         Kruskal-Wallis, Nonparametric Bootstrap F-test, Permutation F-test) under skew normal distribution.
#'
#' @examples
#' replext_ts2_c2.1(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts2_c2.1 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, M3 = 5, S3 = 1,
                             Sk1 = 0.8, Sk2 = 0.8, Sk3 = 0.8,
                             n1 = c(2,3,4,5,6,7,8,9,10,15),
                             n2 = c(2,3,4,5,6,7,8,9,10,15),
                             n3 = c(2,3,4,5,6,7,8,9,10,15),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}

#' Replicate and Extend Type I Error Rates for ANOVA with Skewness and Varied Standard Deviations
#'
#' The `replext_ts2_c2.2` function extends the `replext_ts2_c1.1` function by incorporating skewness
#' in data distributions and utilizing different default values for standard deviations in the
#' second and third groups. This function is specifically designed to investigate the influence of
#' skewness combined with varying standard deviations on the Type I error rates in ANOVA (Analysis of Variance).
#' It aligns with the broader analytical goals set in the study by Dwivedi et al. (2017), offering
#' insights into the behavior of statistical tests under these conditions.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 2.
#' @param M3 Mean for the third group, default is 5.
#' @param S3 Standard deviation for the third group, default is 4.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param Sk3 Skewness parameter for the third group, default is 0.8.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group.
#' @param n3 Vector of sample sizes for the third group, must be the same length as n1 and n2.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples for the nonparametric bootstrap test, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with results that extend those from `replext_ts2_c1.1`, focusing on the impact
#'         of skewness and varying standard deviations. The data frame includes columns for each
#'         sample size combination (n1, n2, n3) and the proportions of significant p-values for
#'         each test (ANOVA, Kruskal-Wallis, Nonparametric Bootstrap F-test, Permutation F-test)
#'         under these specific conditions.
#'
#' @examples
#' replext_ts2_c2.2(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}, \code{\link{replext_ts2_c2.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts2_c2.2 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 2, M3 = 5, S3 = 4,
                             Sk1 = 0.8, Sk2 = 0.8, Sk3 = 0.8,
                             n1 = c(2,3,4,5,6,7,8,9,10,15),
                             n2 = c(2,3,4,5,6,7,8,9,10,15),
                             n3 = c(2,3,4,5,6,7,8,9,10,15),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}

#' Replicate and Extend Type I Error Rates for ANOVA with Diverse Skewness Parameters
#'
#' The `replext_ts2_c3.1` function is designed to replicate and extend Type I error rate analysis
#' for ANOVA (Analysis of Variance) with a specific focus on the impact of different skewness parameters
#' across the three groups. This function is a variation of `replext_ts2_c1.1`, providing an
#' opportunity to explore how varying degrees of skewness in each group affect the statistical
#' inferences in ANOVA, as part of the extended analysis in the context of the study by Dwivedi et al. (2017).
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param M3 Mean for the third group, default is 5.
#' @param S3 Standard deviation for the third group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param Sk3 Skewness parameter for the third group, default is 1.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group.
#' @param n3 Vector of sample sizes for the third group, must be the same length as n1 and n2.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples for the nonparametric bootstrap test, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with results that build upon those from `replext_ts2_c1.1`. The data frame
#'         includes columns for each sample size combination (n1, n2, n3) and the proportions of
#'         significant p-values for each test (ANOVA, Kruskal-Wallis, Nonparametric Bootstrap F-test,
#'         Permutation F-test) under the specified skewness conditions for each group.
#'
#' @examples
#' replext_ts2_c3.1(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts2_c3.1 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, M3 = 5, S3 = 1,
                             Sk1 = 0.8, Sk2 = 0.8, Sk3 = 1,
                             n1 = c(2,3,4,5,6,7,8,9,10,15),
                             n2 = c(2,3,4,5,6,7,8,9,10,15),
                             n3 = c(2,3,4,5,6,7,8,9,10,15),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}

#' Replicate and Extend Type I Error Rates for ANOVA with Varied Skewness and Standard Deviations
#'
#' The `replext_ts2_c3.2` function is a modification of the `replext_ts2_c1.1` function, designed to
#' explore the impact of both skewness and different standard deviations in a three-sample ANOVA setting.
#' This variant maintains skewness in all groups but changes the default standard deviations for the
#' second and third groups. It contributes to a more comprehensive understanding of Type I error rates
#' in the context of the study by Dwivedi et al. (2017), especially under conditions of non-normality.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 2.
#' @param M3 Mean for the third group, default is 5.
#' @param S3 Standard deviation for the third group, default is 4.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param Sk3 Skewness parameter for the third group, default is 1.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group.
#' @param n3 Vector of sample sizes for the third group, must be the same length as n1 and n2.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples for the nonparametric bootstrap test, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with results that extend those from `replext_ts2_c1.1`. This data frame
#'         includes columns for each sample size combination (n1, n2, n3) and the proportions of
#'         significant p-values for each test (ANOVA, Kruskal-Wallis, Nonparametric Bootstrap F-test,
#'         Permutation F-test), under the specific conditions of varying skewness and standard deviations.
#'
#' @examples
#' replext_ts2_c3.2(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}, \code{\link{replext_ts2_c3.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts2_c3.2 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 2, M3 = 5, S3 = 4,
                             Sk1 = 0.8, Sk2 = 0.8, Sk3 = 1,
                             n1 = c(2,3,4,5,6,7,8,9,10,15),
                             n2 = c(2,3,4,5,6,7,8,9,10,15),
                             n3 = c(2,3,4,5,6,7,8,9,10,15),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}

#' Replicate and Extend Type I Error Rates for ANOVA with Skewness in Specific Sample Size Combinations
#'
#' The `replext_ts2_c4.1` function is a specialized version of `replext_ts2_c1.1`, designed to analyze
#' Type I error rates in ANOVA settings with skewness in data distributions and tailored combinations
#' of sample sizes for each group. This function explores the impact of non-normality (skewness) and
#' varying group sizes, thereby extending the analysis framework of the study by Dwivedi et al. (2017).
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param M3 Mean for the third group, default is 5.
#' @param S3 Standard deviation for the third group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param Sk3 Skewness parameter for the third group, default is 0.8.
#' @param n1 Vector of specific sample sizes for the first group.
#' @param n2 Vector of specific sample sizes for the second group.
#' @param n3 Vector of specific sample sizes for the third group, not necessarily the same length as n1 and n2.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples for the nonparametric bootstrap test, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with results extending those from `replext_ts2_c1.1`, focusing on the
#'         combined effects of skewness and specific sample size configurations. The data frame
#'         includes columns for each unique sample size combination (n1, n2, n3) and the proportions
#'         of significant p-values for each test (ANOVA, Kruskal-Wallis, Nonparametric Bootstrap F-test,
#'         Permutation F-test) in these particular scenarios.
#'
#' @examples
#' replext_ts2_c4.1(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts2_c4.1 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, M3 = 5, S3 = 1,
                             Sk1 = 0.8, Sk2 = 0.8, Sk3 = 0.8,
                             n1 = c(2,2,2,3,2,2,3,2,3,2),
                             n2 = c(2,3,3,4,2,3,4,2,4,2),
                             n3 = c(3,3,4,3,6,6,4,7,5,8),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}

#' Replicate and Extend Type I Error Rates for ANOVA with Specific Sample Size Combinations
#'
#' `replext_ts2_c4.2` is designed to explore the impact of specific combinations of sample sizes
#' on the Type I error rates in ANOVA (Analysis of Variance) under conditions of skewness and
#' varying standard deviations. This function extends `replext_ts2_c1.1` by utilizing unique sample
#' size combinations along with altered default standard deviations and skewness parameters. It is
#' part of a broader analysis aimed at understanding statistical behavior in skewed and heteroscedastic
#' scenarios, aligning with the research context provided by Dwivedi et al. (2017).
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 2.
#' @param M3 Mean for the third group, default is 5.
#' @param S3 Standard deviation for the third group, default is 4.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param Sk3 Skewness parameter for the third group, default is 0.8.
#' @param n1 Vector of sample sizes for the first group, with specific combinations.
#' @param n2 Vector of sample sizes for the second group, with specific combinations.
#' @param n3 Vector of sample sizes for the third group, with specific combinations.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples for the nonparametric bootstrap test, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with results extending those from `replext_ts2_c1.1`. This data frame
#'         provides insights into the Type I error rates for each test (ANOVA, Kruskal-Wallis,
#'         Nonparametric Bootstrap F-test, Permutation F-test) under the conditions of skewness,
#'         varying sample sizes, and varying standrd deviations.
#'
#' @examples
#' replext_ts2_c4.2(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts2_c4.2 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 2, M3 = 5, S3 = 4,
                             Sk1 = 0.8, Sk2 = 0.8, Sk3 = 0.8,
                             n1 = c(2,2,2,3,2,2,3,2,3,2),
                             n2 = c(2,3,3,4,2,3,4,2,4,2),
                             n3 = c(3,3,4,3,6,6,4,7,5,8),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}
