calculate_acceleration <- function(
    bootstrap_samples_df,
    data_cube,
    fun,
    ...,
    grouping_var,
    ref_group = NA,
    jackknife = "usual",
    progress = FALSE) {
  ### Start checks
  # Check data_cube input
  cube_message <- paste("`data_cube` must be a data cube object (class",
                        "'processed_cube' or 'sim_cube') or a dataframe.")
  do.call(stopifnot,
          stats::setNames(list(
            rlang::inherits_any(data_cube,
                                c("processed_cube", "sim_cube", "data.frame"))),
            cube_message)
  )

  # Check fun input
  stopifnot("`fun` must be a function." = is.function(fun))

  # Check if ref_group is NA or a number or a string
  stopifnot(
    "`ref_group` must be a numeric/character vector of length 1 or NA." =
      (assertthat::is.number(ref_group) | assertthat::is.string(ref_group) |
         is.na(ref_group)) &
      length(ref_group) == 1)

  # Check if jackknife is 'usual' or 'pos'
  jackknife <- tryCatch({
    match.arg(jackknife, c("usual", "pos"))
  }, error = function(e) {
    stop("`jackknife` must be one of 'usual', 'pos'.",
         call. = FALSE)
  })

  # Check if progress is a logical vector of length 1
  stopifnot("`progress` must be a logical vector of length 1." =
              assertthat::is.flag(progress))
  ### End checks

  # Perform jackknifing
  jackknife_df <- perform_jackknifing(
    data_cube = data_cube,
    fun = fun,
    ...,
    grouping_var = grouping_var,
    ref_group = ref_group,
    progress = progress)

  # Calculate influence values
  influence_df <- jackknife_df %>%
    dplyr::left_join(bootstrap_samples_df %>%
                       dplyr::distinct(!!!dplyr::syms(grouping_var),
                                       .data$est_original),
                     by = grouping_var) %>%
    dplyr::mutate(n = dplyr::n(),
                  .by = dplyr::all_of(grouping_var)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(influence = ifelse(
      jackknife == "usual",
      (.data$n - 1) * (.data$est_original - .data$jack_rep),
      (.data$n + 1) * (.data$jack_rep - .data$est_original)
    )
    ) %>%
    dplyr::ungroup()

  # Calculate acceleration
  out_df <- influence_df %>%
    dplyr::summarise(
      numerator = sum(.data$influence^3),
      denominator = 6 * sum(.data$influence^2)^1.5,
      acceleration = .data$numerator / .data$denominator,
      .by = dplyr::all_of(grouping_var)
    )

  return(out_df)
}
