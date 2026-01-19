#' Resolve bootstrap method including use of the boot package
#'
#' Resolves the effective bootstrap method to be used by
#' [bootstrap_cube()], combining:
#'
#' - the *scope* of the indicator (group-specific vs whole-cube), and
#' - whether a reference group is used.
#'
#' When `method = "smart"`, the scope of the indicator is inferred using
#' [derive_bootstrap_method()]. If no reference group is specified
#' (`ref_group = NA`), the corresponding `boot_*` method is selected.
#'
#' @param df A dataframe.
#' @param fun A function which, when applied to `df`, returns the
#' statistic(s) of interest. This function must return a dataframe with a
#' column `diversity_val`.
#' @param ... Additional arguments passed to `fun`.
#' @param cat_var A character vector specifying the grouping variable(s)
#' used by `fun`.
#' @param ref_group A value indicating the reference group. If `NA`,
#' bootstrapping may be delegated to the \pkg{boot} package.
#' @param method Character string specifying the bootstrap method.
#' One of `"whole_cube"`, `"group_specific"`, `"boot_whole_cube"`,
#' `"boot_group_specific"`, or `"smart"`.
#'
#' @return
#' A single character string giving the resolved bootstrap method:
#'
#' - `"whole_cube"`
#' - `"group_specific"`
#' - `"boot_whole_cube"`
#' - `"boot_group_specific"`
#'
#' @details
#' The resolution follows these rules:
#'
#' 1. If `method` is not `"smart"`, it is returned unchanged.
#' 2. If `method = "smart"`, the indicator scope is inferred using
#'    [derive_bootstrap_method()].
#' 3. If `ref_group = NA`, the resolved method is prefixed with `"boot_"`,
#'    resulting in `"boot_group_specific"` or `"boot_whole_cube"`.
#' 4. If a reference group is specified, the non-boot variants
#'    `"group_specific"` or `"whole_cube"` are returned.
#'
#' @export
#' @family indicator_uncertainty
#'
#' @examples
#' # Example 1: Group-specific indicator without a reference group
#' # Mean sepal length per species (calculated independently per group)
#' mean_sepal_length <- function(x) {
#'   out_df <- aggregate(Sepal.Length ~ Species, x, mean)
#'   names(out_df) <- c("Species", "diversity_val")
#'   out_df
#' }
#'
#' resolve_bootstrap_method(
#'   df = iris,
#'   fun = mean_sepal_length,
#'   cat_var = "Species",
#'   ref_group = NA,
#'   method = "smart"
#' )
#'
#' # Example 2: Group-specific indicator with a reference group
#' resolve_bootstrap_method(
#'   df = iris,
#'   fun = mean_sepal_length,
#'   cat_var = "Species",
#'   ref_group = "setosa",
#'   method = "smart"
#' )
#'
#' # Example 3: Indicator that depends on the whole cube
#' # The statistic per species depends on all species together
#' scaled_sepal_length <- function(x) {
#'   out_df <- aggregate(Sepal.Length ~ Species, x, mean)
#'   out_df$Sepal.Length <- out_df$Sepal.Length / nrow(out_df)
#'   names(out_df) <- c("Species", "diversity_val")
#'   out_df
#' }
#'
#' resolve_bootstrap_method(
#'   df = iris,
#'   fun = scaled_sepal_length,
#'   cat_var = "Species",
#'   ref_group = NA,
#'   method = "smart"
#' )
resolve_bootstrap_method <- function(
    df,
    fun,
    ...,
    cat_var,
    ref_group,
    method) {
  ### Start checks ---------------------------------------------------------
  stopifnot("`df` must be a dataframe." = inherits(df, "data.frame"))
  stopifnot("`fun` must be a function." = is.function(fun))
  stopifnot("`cat_var` must be a character vector." =
              is.character(cat_var))
  stopifnot(
    "`method` must be a character string." =
      is.character(method) && length(method) == 1
  )
  ### End checks -----------------------------------------------------------

  if (method != "smart") {
    return(method)
  }

  scope_method <- derive_bootstrap_method(
    df = df,
    fun = fun,
    ...,
    cat_var = cat_var
  )

  if (is.na(ref_group)) {
    paste0("boot_", scope_method)
  } else {
    scope_method
  }
}
