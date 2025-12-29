# Joining `JAGS` Models

**\[stable\]**

This helper function joins two JAGS models in the way that the body of
the second model is appended to the body of the first model (in this
order). After that, the first, body-extended model is returned. The
arguments of `model1`, `model2` model functions (if any) are not
combined in any way.

## Usage

``` r
h_jags_join_models(model1, model2)
```

## Arguments

- model1:

  (`function`)  
  the first model to join.

- model2:

  (`function`)  
  the second model to join.

## Value

joined models.

## Note

`model1` and `model2` functions must have a multi-expression body, i.e.
braced expression(s). Environments or any attributes of the function
bodies are not preserved in any way after joining.
