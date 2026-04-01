# Print a list of cube diagnostic rules

Displays a compact overview of a collection of diagnostic rules used by
[`diagnose_cube()`](https://b-cubed-eu.github.io/dubicube/reference/diagnose_cube.md).
Each rule is shown with its identifier and the cube dimension it
evaluates.

## Usage

``` r
# S3 method for class 'cube_rule_list'
print(x, ...)
```

## Arguments

- x:

  An object of class `cube_rule_list`.

- ...:

  Additional arguments passed to other methods (currently unused).

## Value

The input object `x`, returned invisibly.
