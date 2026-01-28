# Changelog

## dubicube 0.10.0

- Fix chunk option in article
  [\#73](https://github.com/b-cubed-eu/dubicube/issues/73)
- Detect group-specific or whole-cube bootstrapping automatically
  [\#74](https://github.com/b-cubed-eu/dubicube/issues/74)
- Improve funder and rights holder descriptions
  [\#82](https://github.com/b-cubed-eu/dubicube/issues/82)
- Allow users to use **boot** dependency for bootstrapping and CI
  calculation

## dubicube 0.9.5

- Drop levels that do not occur in `calc_bootstrap_ci()`
  [\#69](https://github.com/b-cubed-eu/dubicube/issues/69)
- Clarify whole-cube bootstrap versus group-specific bootstrap
  [\#70](https://github.com/b-cubed-eu/dubicube/issues/70)

## dubicube 0.9.4

- Do not use internal functions from **boot** package
  [\#56](https://github.com/b-cubed-eu/dubicube/issues/56)

## dubicube 0.9.3

- Review of tutorials

## dubicube 0.9.2

- Update README [\#63](https://github.com/b-cubed-eu/dubicube/issues/63)

## dubicube 0.9.1

- Fix mistake in package versioning

## dubicube 0.8.3

- Fix mistake in installation guidelines

## dubicube 0.8.2

- Create tutorial for visualising uncertainty in case of temporal
  indicators
- Create tutorial for visualising uncertainty in case of spatial
  indicators

## dubicube 0.8.1

- Silence package warnings in vignettes
- Set repo status to active

## dubicube 0.8.0

- Simplify compatibility with **b3gbi** package
  [\#48](https://github.com/b-cubed-eu/dubicube/issues/48)
- Add links to tutorials in README
  [\#49](https://github.com/b-cubed-eu/dubicube/issues/49)
- Make README more attractive
  [\#50](https://github.com/b-cubed-eu/dubicube/issues/50)
- Improve table layout tutorial effect classification
  [\#51](https://github.com/b-cubed-eu/dubicube/issues/51)
- Include bootstrap figure in tutorial
  [\#37](https://github.com/b-cubed-eu/dubicube/issues/37)

## dubicube 0.7.3

- Update README [\#39](https://github.com/b-cubed-eu/dubicube/issues/39)
  and [\#45](https://github.com/b-cubed-eu/dubicube/issues/45)
- Create tutorial for effect classification

## dubicube 0.7.2

- Use ROR for copyright holder, drop email
  [\#40](https://github.com/b-cubed-eu/dubicube/issues/40)
- Use DOI for funding
  [\#41](https://github.com/b-cubed-eu/dubicube/issues/41)
- Redo spelling check with **checklist** v0.4.2
  [\#42](https://github.com/b-cubed-eu/dubicube/issues/42)

## dubicube 0.7.1

- Fix bug pkgdown site

## dubicube 0.7.0

- Add tutorials [\#30](https://github.com/b-cubed-eu/dubicube/issues/30)
- Simplify function examples
- Check package with
  [`lintr::indentation_linter()`](https://lintr.r-lib.org/reference/indentation_linter.html)

## dubicube 0.6.0

- Add transformation and no bias options to
  [`calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.md)

## dubicube 0.5.0

- Move acceleration calculation to separate function
  [`calculate_acceleration()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_acceleration.md)
- Improve structure of
  [`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md)
  function

## dubicube 0.4.0

- Allow `grouping_var` argument to have length \> 1
- Implement ellipsis argument `...` in
  [`cross_validate_cube()`](https://b-cubed-eu.github.io/dubicube/reference/cross_validate_cube.md)

## dubicube 0.3.1

- Fix issues: [\#19](https://github.com/b-cubed-eu/dubicube/issues/19),
  [\#22](https://github.com/b-cubed-eu/dubicube/issues/22),
  [\#23](https://github.com/b-cubed-eu/dubicube/issues/23)

## dubicube 0.3.0

- Add cross-validation function

## dubicube 0.2.1

- Join the [R-universe](https://b-cubed-eu.r-universe.dev/)!

## dubicube 0.2.0

- Add effect classification function
- Add `...` argument to
  [`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md)
  and
  [`calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.md)

## dubicube 0.1.0

- Add bootstrapping functions

## dubicube 0.0.2

- Add pkgdown website.
- Add package description in `README.Rmd`.

## dubicube 0.0.1

- Set up basic package structure.

## dubicube 0.0.0

- Added a `NEWS.md` file to track changes to the package.
- Add [`checklist`](https://inbo.github.io/checklist/) infrastructure.
