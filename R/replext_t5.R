#' Replicate and Extend Simulation Results for Paired Distributions
#'
#' This function aims to replicate and extend the simulation results from
#' Table 5 cell 1.1 of the paper by Dwivedi et al. (2017) for paired distributions
#' with the option to use either normal or skew normal distributions. It allows
#' specifying means, standard deviations, skewness, and correlation for two
#' paired distributions.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is 0 (normal distribution).
#' @param Sk2 Skewness parameter for the second group, default is 0 (normal distribution).
#' @param correl Correlation between the two groups, default is 0.8.
#' @param n Vector of sample sizes for the paired groups.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size and
#'         the proportions of significant p-values for each test (PT, NPBTT, WRST, PTT).
#'
#' @examples
#' replext_t5_c1.1(n = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t5_c1.1 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, Sk1 = 0, Sk2 = 0,
                            correl = 0.8,
                            n = c(3,4,5,6,7,8,9,10,15,25),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  # Get results for one iteration of the simulation or one pair of sample sizes
  get_result <- function(n) {

    # Generate normal or skew normal data
    generate_data <- function(n, M1, S1, M2, S2, Sk1, Sk2, correl) {
      Sigma <- matrix(c(S1^2, S1*S2*correl, S1*S2*correl, S2^2), 2, 2)
      if ((Sk1 == 0 && Sk2 == 0) ||
          (is.null(Sk1) && is.null(Sk2)) ||
          (Sk1 == 0 && is.null(Sk2)) ||
          (is.null(Sk1) && Sk2 == 0)
           ) {
        # Use MASS::mvrnorm for normal distribution
        return(MASS::mvrnorm(n, mu = c(M1, M2), Sigma = Sigma))
      } else {
        # Use sn::rmsn for skew normal distribution
        cpM <- list(mean=c(M1,M2), var.cov=Sigma, gamma1=c(Sk1, Sk2))
        dpM <- sn::cp2dp(cpM, family="SN")
        return(sn::rmsn(n, dp=dpM))
      }
    }

    FP <- generate_data(n, M1, S1, M2, S2, Sk1, Sk2, correl)

    df <- data.frame("F1" = FP[,1], "F2" = FP[,2])

    # Test p-value for each method
    npbtt <- tryCatch({
      nonparboot(df, x = "F1", y = "F2", nboot = nboot, test = "pt", conf.level = conf.level)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    pt <- tryCatch({
      stats::t.test(x = df$F1, y = df$F2, paired = TRUE, var.equal = TRUE)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    wrst <- tryCatch({
      stats::wilcox.test(x = df$F1, y = df$F2, paired = TRUE, exact = TRUE)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    ptt <- tryCatch({
      MKinfer::perm.t.test(x = df$F1, y = df$F2, paired = TRUE)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    # Returning a named vector
    return(c(NPBTT = npbtt, PT = pt, WRST = wrst, PTT = ptt))
  }

  # Run n_simulations at each sample size pair
  results <- lapply(seq_along(n), function(i) {
    replicate(n_simulations, get_result(n[i]))
  })

  # Use lapply to calculate proportions and create a data frame for each sample size combination
  proportions_list <- lapply(seq_along(results), function(i) {
    proportions <- rowMeans(results[[i]], na.rm = TRUE)
    data.frame(
      n = n[i],
      PT = proportions["PT"],
      NPBTT = proportions["NPBTT"],
      WRST = proportions["WRST"],
      PTT = proportions["PTT"]
    )
  })

  # Combine the list of data frames into a single data frame
  proportions_df <- do.call(rbind, proportions_list)

  return(proportions_df)
}

#' Replicate and Extend Simulation Results for Paired Distributions with Different Variances
#'
#' This function is a wrapper around 'replext_t5_c1.1' and is specifically aimed
#' at replicating and extending simulation results from Table 5 cell 1.2 of the
#' paper by Dwivedi et al. (2017). It is tailored for paired distributions with
#' the option to use either normal or skew normal distributions, differing in
#' standard deviations between the two groups.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 3.
#' @param Sk1 Skewness parameter for the first group, default is 0 (normal distribution).
#' @param Sk2 Skewness parameter for the second group, default is 0 (normal distribution).
#' @param correl Correlation between the two groups, default is 0.8.
#' @param n Vector of sample sizes for the paired groups.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size and
#'         the proportions of significant p-values for each test (PT, NPBTT, WRST, PTT),
#'         similar to 'replext_t5_c1.1' but with differing standard deviations for the groups.
#'
#' @examples
#' replext_t5_c1.2(n = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t5_c1.2 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 3, Sk1 = 0, Sk2 = 0,
                            correl = 0.8,
                            n = c(3,4,5,6,7,8,9,10,15,25),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t5_c1.1(M1, S1, M2, S2, Sk1, Sk2, correl, n, n_simulations, nboot,
                  conf.level)

}

#' Replicate and Extend Simulation Results for Paired Distributions with Skewness
#'
#' This function, serving as a wrapper around 'replext_t5_c1.1', is designed to
#' replicate and extend the simulation results from Table 5 cell 1.3 of the paper
#' by Dwivedi et al. (2017). It focuses on paired distributions featuring both
#' normal and skew normal distributions, with specified skewness parameters for each group.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is 0.5.
#' @param Sk2 Skewness parameter for the second group, default is 0.5.
#' @param correl Correlation between the two groups, default is 0.8.
#' @param n Vector of sample sizes for the paired groups.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size and
#'         the proportions of significant p-values for each test (PT, NPBTT, WRST, PTT),
#'         similar to 'replext_t5_c1.1' but with skewness parameters for the groups.
#'
#' @examples
#' replext_t5_c1.3(n = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t5_c1.3 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, Sk1 = 0.5, Sk2 = 0.5,
                            correl = 0.8,
                            n = c(3,4,5,6,7,8,9,10,15,25),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t5_c1.1(M1, S1, M2, S2, Sk1, Sk2, correl, n, n_simulations, nboot,
                  conf.level)

}

#' Replicate and Extend Simulation Results for Paired Distributions with Skewness and Different Variances
#'
#' This function is a wrapper around 'replext_t5_c1.1', targeting the replication
#' and extension of simulation results from Table 5 cell 2.1 of the paper by
#' Dwivedi et al. (2017). It focuses on paired distributions that combine normal
#' and skew normal behaviors with different standard deviations and specified skewness
#' parameters for each group.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 3.
#' @param Sk1 Skewness parameter for the first group, default is 0.5 (indicating skew normal distribution).
#' @param Sk2 Skewness parameter for the second group, default is 0.5 (indicating skew normal distribution).
#' @param correl Correlation between the two groups, default is 0.8.
#' @param n Vector of sample sizes for the paired groups.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size and
#'         the proportions of significant p-values for each test (PT, NPBTT, WRST, PTT),
#'         similar to 'replext_t5_c1.1' but with variations in standard deviations and skewness.
#'
#' @examples
#' replext_t5_c2.1(n = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t5_c2.1 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 3, Sk1 = 0.5, Sk2 = 0.5,
                            correl = 0.8,
                            n = c(3,4,5,6,7,8,9,10,15,25),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t5_c1.1(M1, S1, M2, S2, Sk1, Sk2, correl, n, n_simulations, nboot,
                  conf.level)

}

#' Replicate and Extend Simulation Results for Paired Distributions with Different Skewness Levels
#'
#' This function is a specialized version of 'replext_t5_c1.1', tailored to
#' replicate and extend the simulation results from Table 5 cell 2.2 of the paper
#' by Dwivedi et al. (2017). It is designed for paired distributions that exhibit
#' both normal and skew normal characteristics, with different skewness parameters for
#' each group while maintaining the same means and standard deviations.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is 0.2.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param correl Correlation between the two groups, default is 0.8.
#' @param n Vector of sample sizes for the paired groups.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size and
#'         the proportions of significant p-values for each test (PT, NPBTT, WRST, PTT),
#'         with a focus on differing skewness levels between the two groups.
#'
#' @examples
#' replext_t5_c2.2(n = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t5_c2.2 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, Sk1 = 0.2, Sk2 = 0.8,
                            correl = 0.8,
                            n = c(3,4,5,6,7,8,9,10,15,25),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t5_c1.1(M1, S1, M2, S2, Sk1, Sk2, correl, n, n_simulations, nboot,
                  conf.level)

}

#' Replicate and Extend Simulation Results for Paired Distributions with Varied Skewness and Standard Deviations
#'
#' `replext_t5_c2.3` is a wrapper function around `replext_t5_c1.1`, specifically
#' designed to replicate and extend the simulation results from Table 5 cell 2.3 of
#' the paper by Dwivedi et al. (2017). It focuses on paired distributions that exhibit
#' skew normal characteristics with differing skewness parameters and standard deviations
#' for each group.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 3.
#' @param Sk1 Skewness parameter for the first group, default is 0.2.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param correl Correlation between the two groups, default is 0.8.
#' @param n Vector of sample sizes for the paired groups.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size and
#'         the proportions of significant p-values for each test (PT, NPBTT, WRST, PTT),
#'         focusing on variations in skewness and standard deviations between the groups.
#'
#' @examples
#' replext_t5_c2.3(n = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t5_c2.3 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 3, Sk1 = 0.2, Sk2 = 0.8,
                            correl = 0.8,
                            n = c(3,4,5,6,7,8,9,10,15,25),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t5_c1.1(M1, S1, M2, S2, Sk1, Sk2, correl, n, n_simulations, nboot,
                  conf.level)

}

