Statistical Rethinking Chapter 4
================

## Medium Section

##### 4M1. For the model definition below, simulate observed heights from the prior (not the posterior).

\(y_i\) Normal(\(\mu\); \(\sigma\)) \(\mu \sim\) Normal(0; 10)
\(\sigma \sim\) Uniform(0; 10)

``` r
p_grid <- seq(from = 0 , to = 1 , length.out = 1000)
prior <- rep(1 , 1000)
likelihood <- dbinom(6 , size=9 , prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
```

``` r
sum(samples < 0.2 )/10000
```

    ## [1] 0.0005

``` r
sum( samples > 0.8)/10000
```

    ## [1] 0.1117
