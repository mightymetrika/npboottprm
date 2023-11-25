#' Replicate and Extend Statistical Power Analysis for ANOVA in a Three-Sample Setting
#'
#' This function aims to replicate and extend the statistical power analysis for
#' ANOVA (Analysis of Variance) from the supplemental tables of the paper by
#' Dwivedi et al. (2017). It allows for the simulation of three-sample scenarios
#' with the option to use either normal or skew normal distributions, and
#' performs various statistical tests to assess the statistical power. The function
#' is a wrapper around `replext_ts2_c1.1`.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 6.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param M3 Mean for the third group, default is 7.
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
#' @return A data frame with columns for each sample size combination (n1, n2, n3)
#'         and the proportions of significant p-values for each test (ANOVA,
#'         Kruskal-Wallis, Nonparametric Bootstrap F-test, Permutation F-test).
#'
#' @examples
#' replext_ts3_c1.1(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @importFrom lmPerm lmp
#' @export
replext_ts3_c1.1 <- function(M1 = 5, S1 = 1, M2 = 6, S2 = 1, M3 = 7, S3 = 1,
                             Sk1 = NULL, Sk2 = NULL, Sk3 = NULL,
                             n1 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n2 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n3 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)
}

#' Replicate and Extend Statistical Power Analysis for ANOVA
#'
#' This function is a wrapper around `replext_ts2_c1.1`. The function is designed
#' to reproduce or extend the statistical power analysis for ANOVA (Analysis of
#' Variance) from Dwivedi et al. (2017) supplemental table 3, cell 1.2.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 6.
#' @param S2 Standard deviation for the second group, default is 2.
#' @param M3 Mean for the third group, default is 7.
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
#' @return A data frame with columns for each sample size combination (n1, n2, n3)
#'         and the proportions of significant p-values for each test (ANOVA,
#'         Kruskal-Wallis, Nonparametric Bootstrap F-test, Permutation F-test).
#'
#' @examples
#' replext_ts3_c1.2(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts3_c1.2 <- function(M1 = 5, S1 = 1, M2 = 6, S2 = 2, M3 = 7, S3 = 4,
                             Sk1 = NULL, Sk2 = NULL, Sk3 = NULL,
                             n1 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n2 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n3 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}

#' Replicate and Extend Statistical Power Analysis for ANOVA with Skewness
#'
#' This function is a wrapper around `replext_ts2_c1.1`. The function is designed
#' to reproduce or extend the statistical power analysis for ANOVA (Analysis of
#' Variance) from Dwivedi et al. (2017) supplemental table 3, cell 2.1.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 6.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param M3 Mean for the third group, default is 7.
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
#' @return A data frame with columns for each sample size combination (n1, n2, n3)
#'         and the proportions of significant p-values for each test (ANOVA,
#'         Kruskal-Wallis, Nonparametric Bootstrap F-test, Permutation F-test).
#'
#' @examples
#' replext_ts3_c2.1(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts3_c2.1 <- function(M1 = 5, S1 = 1, M2 = 6, S2 = 1, M3 = 7, S3 = 1,
                             Sk1 = 0.8, Sk2 = 0.8, Sk3 = 0.8,
                             n1 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n2 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n3 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}

#' Replicate and Extend Statistical Power Analysis for ANOVA with Skewness and
#' Varied Standard Deviations
#'
#' This function is a wrapper around `replext_ts2_c1.1`. The function is designed
#' to reproduce or extend the statistical power analysis for ANOVA (Analysis of
#' Variance) from Dwivedi et al. (2017) supplemental table 3, cell 2.2.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 6.
#' @param S2 Standard deviation for the second group, default is 2.
#' @param M3 Mean for the third group, default is 7.
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
#' @return A data frame with columns for each sample size combination (n1, n2, n3)
#'         and the proportions of significant p-values for each test (ANOVA,
#'         Kruskal-Wallis, Nonparametric Bootstrap F-test, Permutation F-test).
#'
#' @examples
#' replext_ts3_c2.2(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts3_c2.2 <- function(M1 = 5, S1 = 1, M2 = 6, S2 = 2, M3 = 7, S3 = 4,
                             Sk1 = 0.8, Sk2 = 0.8, Sk3 = 0.8,
                             n1 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n2 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n3 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}

#' Replicate and Extend Statistical Power Analysis for ANOVA with Diverse Skewness
#' Parameters
#'
#' This function is a wrapper around `replext_ts2_c1.1`. The function is designed
#' to reproduce or extend the statistical power analysis for ANOVA (Analysis of
#' Variance) from Dwivedi et al. (2017) supplemental table 3, cell 3.1.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 6.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param M3 Mean for the third group, default is 7.
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
#' @return A data frame with columns for each sample size combination (n1, n2, n3)
#'         and the proportions of significant p-values for each test (ANOVA,
#'         Kruskal-Wallis, Nonparametric Bootstrap F-test, Permutation F-test).
#'
#' @examples
#' replext_ts3_c3.1(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts3_c3.1 <- function(M1 = 5, S1 = 1, M2 = 6, S2 = 1, M3 = 7, S3 = 1,
                             Sk1 = 0.8, Sk2 = 0.8, Sk3 = 1,
                             n1 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n2 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n3 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}

#' Replicate and Extend Statistical Power Analysis for ANOVA with Varied Skewness
#' and Standard Deviations
#'
#' This function is a wrapper around `replext_ts2_c1.1`. The function is designed
#' to reproduce or extend the statistical power analysis for ANOVA (Analysis of
#' Variance) from Dwivedi et al. (2017) supplemental table 3, cell 3.2.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 6.
#' @param S2 Standard deviation for the second group, default is 2.
#' @param M3 Mean for the third group, default is 7.
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
#' @return A data frame with columns for each sample size combination (n1, n2, n3)
#'         and the proportions of significant p-values for each test (ANOVA,
#'         Kruskal-Wallis, Nonparametric Bootstrap F-test, Permutation F-test).
#'
#' @examples
#' replext_ts3_c3.2(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts3_c3.2 <- function(M1 = 5, S1 = 1, M2 = 6, S2 = 2, M3 = 7, S3 = 4,
                             Sk1 = 0.8, Sk2 = 0.8, Sk3 = 1,
                             n1 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n2 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n3 = c(2,3,4,5,6,7,8,9,10,15,25,50,100),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}

#' Replicate and Extend Statistical Power Analysis for ANOVA with Skewness in
#' Specific Sample Size Combinations
#'
#' This function is a wrapper around `replext_ts2_c1.1`. The function is designed
#' to reproduce or extend the statistical power analysis for ANOVA (Analysis of
#' Variance) from Dwivedi et al. (2017) supplemental table 3, cell 4.1.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 6.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param M3 Mean for the third group, default is 7.
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
#' @return A data frame with columns for each sample size combination (n1, n2, n3)
#'         and the proportions of significant p-values for each test (ANOVA,
#'         Kruskal-Wallis, Nonparametric Bootstrap F-test, Permutation F-test).
#'
#' @examples
#' replext_ts3_c4.1(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts3_c4.1 <- function(M1 = 5, S1 = 1, M2 = 6, S2 = 1, M3 = 7, S3 = 1,
                             Sk1 = 0.8, Sk2 = 0.8, Sk3 = 0.8,
                             n1 = c(2,2,2,3,2,2,3,2,3,2),
                             n2 = c(2,3,3,4,2,3,4,2,4,2),
                             n3 = c(3,3,4,3,6,6,4,7,5,8),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}

#' Replicate and Extend Statistical Power Analysis for ANOVA with Specific Sample
#' Size Combinations
#'
#' This function is a wrapper around `replext_ts2_c1.1`. The function is designed
#' to reproduce or extend the statistical power analysis for ANOVA (Analysis of
#' Variance) from Dwivedi et al. (2017) supplemental table 3, cell 4.2.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 6.
#' @param S2 Standard deviation for the second group, default is 2.
#' @param M3 Mean for the third group, default is 7.
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
#' @return A data frame with columns for each sample size combination (n1, n2, n3)
#'         and the proportions of significant p-values for each test (ANOVA,
#'         Kruskal-Wallis, Nonparametric Bootstrap F-test, Permutation F-test).
#'
#' @examples
#' replext_ts3_c4.2(n1 = c(10), n2 = c(10), n3 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_ts2_c1.1}}
#' @importFrom lmPerm lmp
#' @export
replext_ts3_c4.2 <- function(M1 = 5, S1 = 1, M2 = 6, S2 = 2, M3 = 7, S3 = 4,
                             Sk1 = 0.8, Sk2 = 0.8, Sk3 = 0.8,
                             n1 = c(2,2,2,3,2,2,3,2,3,2),
                             n2 = c(2,3,3,4,2,3,4,2,4,2),
                             n3 = c(3,3,4,3,6,6,4,7,5,8),
                             n_simulations = 10000, nboot = 1000,
                             conf.level = 0.95){

  replext_ts2_c1.1(M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n1, n2, n3,
                   n_simulations, nboot, conf.level)

}
