# Helper Function to Obtain Simulation Results List

The function `fun` can use variables that are visible to itself. The
names of these variables have to be given in the vector `vars`.

## Usage

``` r
get_result_list(fun, nsim, vars, parallel, n_cores)
```

## Arguments

- fun:

  (`function`)  
  the simulation function for a single iteration, which takes as single
  parameter the iteration index.

- nsim:

  number of simulations to be conducted.

- vars:

  names of the variables.

- parallel:

  should the simulation runs be parallelized across the clusters of the
  computer?

- n_cores:

  how many cores should be used for parallel computing?

## Value

The list with all simulation results (one iteration corresponds to one
list element).
