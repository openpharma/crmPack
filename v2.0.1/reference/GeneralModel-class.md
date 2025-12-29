# `GeneralModel`

**\[stable\]**

`GeneralModel` is a general model class, from which all other specific
model-like classes inherit.

## Usage

``` r
.DefaultGeneralModel()
```

## Slots

- `datamodel`:

  (`function`)  
  a function representing the `JAGS` data model specification.

- `priormodel`:

  (`function`)  
  a function representing the `JAGS` prior specification.

- `modelspecs`:

  (`function`)  
  a function computing the list of the data model and prior model
  specifications that are required to be specified completely (e.g.
  prior parameters, reference dose, etc.), based on the data slots that
  are required as arguments of this function. Apart of data arguments,
  this function can be specified with one additional (optional) argument
  `from_prior` of type `logical` and length one. This `from_prior` flag
  can be used to differentiate the output of the `modelspecs`, as its
  value is taken directly from the `from_prior` argument of the `mcmc`
  method that invokes `modelspecs` function. That is, when `from_prior`
  is `TRUE`, then only `priormodel` JAGS model is used (`datamodel` is
  not used) by the `mcmc`, and hence `modelspecs` function should return
  all the parameters that are required by the `priormodel` only. If the
  value of `from_prior` is `FALSE`, then both JAGS models `datamodel`
  and `priormodel` are used in the MCMC sampler, and hence `modelspecs`
  function should return all the parameters required by both `datamodel`
  and `priormodel`.

- `init`:

  (`function`)  
  a function computing the list of starting values for parameters
  required to be initialized in the MCMC sampler, based on the data
  slots that are required as arguments of this function.

- `datanames`:

  (`character`)  
  the names of all data slots that are used by `datamodel` JAGS
  function. No other names should be specified here.

- `datanames_prior`:

  (`character`)  
  the names of all data slots that are used by `priormodel` JAGS
  function. No other names should be specified here.

- `sample`:

  (`character`)  
  names of all parameters from which you would like to save the MCMC
  samples.

## Note

The `datamodel` must obey the convention that the data input is called
exactly in the same way as in the corresponding data class. All prior
distributions for parameters should be contained in the model function
`priormodel`. The background is that this can be used to simulate from
the prior distribution, before obtaining any data.

Typically, end users will not use the `.DefaultGeneralModel()` function.

## See also

[`ModelPseudo`](https://openpharma.github.io/crmPack/reference/ModelPseudo-class.md).
