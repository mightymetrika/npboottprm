t2_repext_c1.1 <- function(M = 5, S = 1, sample_size = c(3,4,5,6,7,8,9,10,15), n_simulations = 10000,
                        nboot = 1000, conf.level = 0.95){

  get_result <- function(sample_size) {
    F1 <- stats::rnorm(n = sample_size, mean = M, sd = S)
    F2 <- stats::rnorm(n = sample_size, mean = M, sd = S)

    df <- data.frame(x = c(F1, F2), grp = c(rep("F1", length(F1)), rep("F2", length(F2))))

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
      stats::wilcox.test(x ~ grp, df, paired = FALSE)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    ptt <- tryCatch({
      MKinfer::perm.t.test(x ~ grp, df, paired = FALSE)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    # Returning a named vector
    return(c(NPBTT = npbtt, ST = st, WT = wt, WRST = wrst, PTT = ptt))
  }

  results <- lapply(sample_size, function(size) {
    replicate(n_simulations, get_result(size))
  })

  # Use lapply to calculate proportions and create a data frame for each sample size
  proportions_list <- lapply(seq_along(results), function(i) {
    proportions <- rowMeans(results[[i]], na.rm = TRUE)
    data.frame(
      sample_size = sample_size[i],
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
