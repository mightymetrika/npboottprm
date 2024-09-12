#' Replicate and Extend Simulation Results from Table 2 Cell 1.1
#'
#' This function attempts to replicate and extend the simulation results
#' from Table 2 cell block 1.1 of the paper by Dwivedi et al. (2017).
#' The default parameter values aim to replicate the results from the paper,
#' while modifying the parameter values allows for an extension of the results.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is NULL (normal distribution).
#' @param Sk2 Skewness parameter for the second group, default is NULL (normal distribution).
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions of significant p-values for each test (ST, WT, NPBTT, WRST, PTT).
#'
#' @examples
#' replext_t2_c1.1(n1 = c(4), n2 = c(4), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t2_c1.1 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, Sk1 = NULL, Sk2 = NULL,
                            n1 = c(3,4,5,6,7,8,9,10,15),
                            n2 = c(3,4,5,6,7,8,9,10,15),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  # Ensure n1 and n2 are of equal length
  if (length(n1) != length(n2)) {
    stop("n1 and n2 must be of equal length")
  }

  # Get results for one iteration of the simulation or one pair of sample sizes
  get_result <- function(n1, n2) {

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

    df <- data.frame(x = c(F1, F2), grp = rep(c("F1", "F2"), c(n1, n2)))

    # Test p-value for each method
    npbtt <- tryCatch({
      nonparboot(df, x = "x", grp = "grp", nboot = nboot, test = "t", conf.level = conf.level)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    st <- tryCatch({
      stats::t.test(x ~ grp, df, var.equal = TRUE)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    wt <- tryCatch({
      stats::t.test(x ~ grp, df)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    wrst <- tryCatch({
      stats::wilcox.test(x ~ grp, df, exact = TRUE)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    ptt <- tryCatch({
      MKinfer::perm.t.test(x ~ grp, df, paired = FALSE)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    # Returning a named vector
    return(c(NPBTT = npbtt, ST = st, WT = wt, WRST = wrst, PTT = ptt))
  }

  # Run n_simulations at each sample size pair
  results <- lapply(seq_along(n1), function(i) {
    replicate(n_simulations, get_result(n1[i], n2[i]))
  })

  # Use lapply to calculate proportions and create a data frame for each sample size combination
  proportions_list <- lapply(seq_along(results), function(i) {
    proportions <- rowMeans(results[[i]], na.rm = TRUE)
    data.frame(
      n1 = n1[i],
      n2 = n2[i],
      ST = proportions["ST"],
      WT = proportions["WT"],
      NPBTT = proportions["NPBTT"],
      WRST = proportions["WRST"],
      PTT = proportions["PTT"]
    )
  })

  # Combine the list of data frames into a single data frame
  proportions_df <- do.call(rbind, proportions_list)

  return(proportions_df)
}

#' Replicate and Extend Simulation Results from Table 2 Cell 1.2
#'
#' This function is a wrapper around `replext_t2_c1.1` and is specifically used
#' for replicating and extending the simulation results from Table 2 cell block 1.2
#' of the paper by Dwivedi et al. (2017). It sets the standard deviation of the
#' second group (`S2`) to 3 by default.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 3.
#' @param Sk1 Skewness parameter for the first group, default is NULL (normal distribution).
#' @param Sk2 Skewness parameter for the second group, default is NULL (normal distribution).
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions of significant p-values for each test (ST, WT, NPBTT, WRST, PTT).
#'
#' @examples
#' replext_t2_c1.2(n1 = c(4), n2 = c(4), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t2_c1.2 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 3, Sk1 = NULL, Sk2 = NULL,
                            n1 = c(3,4,5,6,7,8,9,10,15),
                            n2 = c(3,4,5,6,7,8,9,10,15),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  # Call the replext_t2_c1.1 function with modified S2 default
  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table 2 Cell 2.1
#'
#' This function is intended to replicate and extend the simulation results
#' from Table 2 cell block 2.1 in the paper by Dwivedi et al. (2017). It is designed
#' for scenarios with the same skewed distribution and equal variance in
#' both groups. The function acts as a wrapper around `replext_t2_c1.1`, applying
#' specific skewness parameters as required for the cell.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT).
#'
#' @examples
#' replext_t2_c2.1(n1 = c(4), n2 = c(4), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t2_c2.1 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, Sk1 = 0.8, Sk2 = 0.8,
                            n1 = c(3,4,5,6,7,8,9,10,15),
                            n2 = c(3,4,5,6,7,8,9,10,15),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table 2 Cell 2.2
#'
#' This function is designed to replicate and extend the simulation results
#' from Table 2 cell block 2.2 of the paper by Dwivedi et al. (2017). It handles
#' scenarios with same skewed distribution but with different variances in
#' the two groups. The function is a wrapper around `replext_t2_c1.1`, setting
#' specific skewness and variance parameters as per the cell's requirements.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 3.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions of significant p-values for each test (ST, WT, NPBTT, WRST, PTT).
#'
#' @examples
#' replext_t2_c2.2(n1 = c(4), n2 = c(4), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t2_c2.2 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 3, Sk1 = 0.8, Sk2 = 0.8,
                            n1 = c(3,4,5,6,7,8,9,10,15),
                            n2 = c(3,4,5,6,7,8,9,10,15),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table 2 Cell 3.1
#'
#' This function is designed to replicate and extend the simulation results
#' from Table 2 cell block 3.1 of the paper by Dwivedi et al. (2017). It handles
#' scenarios with different skewed distributions but equal variance in the
#' two groups. The function is a wrapper around `replext_t2_c1.1`, setting
#' specific skewness parameters as per the cell's requirements.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 1.0.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions of significant p-values for each test (ST, WT, NPBTT, WRST, PTT).
#'
#' @examples
#' replext_t2_c3.1(n1 = c(4), n2 = c(4), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t2_c3.1 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, Sk1 = 0.8, Sk2 = 1.0,
                            n1 = c(3,4,5,6,7,8,9,10,15),
                            n2 = c(3,4,5,6,7,8,9,10,15),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table 2 Cell 3.2
#'
#' This function aims to replicate and extend the simulation results from Table 2
#' cell block 3.2 in the paper by Dwivedi et al. (2017). It is tailored for scenarios
#' with different skewed distributions and unequal variance between the two groups.
#' The function serves as a wrapper around `replext_t2_c1.1`, utilizing specific
#' skewness parameters and variances as described in the cell.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 3.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 1.0.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT).
#'
#' @examples
#' replext_t2_c3.2(n1 = c(4), n2 = c(4), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t2_c3.2 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 3, Sk1 = 0.8, Sk2 = 1.0,
                            n1 = c(3,4,5,6,7,8,9,10,15),
                            n2 = c(3,4,5,6,7,8,9,10,15),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table 2 Cell 4.1
#'
#' This function is designed to replicate and extend the simulation results
#' from Table 2 cell block 4.1 of the paper by Dwivedi et al. (2017). It addresses
#' scenarios with unequal sample sizes but the same skewed distribution and equal
#' variance in both groups. The function acts as a wrapper around `replext_t2_c1.1`,
#' setting specific skewness parameters and sample sizes as per the cell's requirements.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, designed for unequal sample sizes.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT).
#'
#' @examples
#' replext_t2_c4.1(n1 = c(4), n2 = c(2), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t2_c4.1 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, Sk1 = 0.8, Sk2 = 0.8,
                            n1 = c(4,3,5,4,6,4,3,4,5,6),
                            n2 = c(2,4,3,5,3,6,7,11,10,9),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table 2 Cell 4.2
#'
#' This function is designed to replicate and extend the simulation results
#' from Table 2 cell block 4.2 in the paper by Dwivedi et al. (2017). It is tailored
#' for scenarios with unequal sample sizes, same skewed distribution, but
#' different variances between the two groups. The function acts as a wrapper
#' around `replext_t2_c1.1`, setting specific skewness parameters, variances,
#' and sample sizes as described in the cell.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 5.
#' @param S2 Standard deviation for the second group, default is 3.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, designed for unequal sample sizes.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT).
#'
#' @examples
#' replext_t2_c4.2(n1 = c(4), n2 = c(2), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t2_c4.2 <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 3, Sk1 = 0.8, Sk2 = 0.8,
                            n1 = c(4,3,5,4,6,4,3,4,5,6),
                            n2 = c(2,4,3,5,3,6,7,11,10,9),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}
