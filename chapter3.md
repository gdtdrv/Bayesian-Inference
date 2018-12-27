Statistical Rethinking Chapter 3
================

## Easy Section

Based on the code below answer the questions:

``` r
p_grid <- seq(from = 0 , to = 1 , length.out = 1000)
prior <- rep(1 , 1000)
likelihood <- dbinom(6 , size=9 , prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
```

##### 3E1. How much posterior probability lies below p = 0.2?

``` r
sum(samples < 0.2 )/10000
```

    ## [1] 0.0005

##### 3E2. How much posterior probability lies above p = 0.8?

``` r
sum( samples > 0.8)/10000
```

    ## [1] 0.1117

##### 3E3. How much posterior probability lies between p = 0:2 and p = 0:8?

``` r
sum( samples > 0.2 & samples < 0.8 ) / 1e4
```

    ## [1] 0.8878

##### 3E4. 20% of the posterior probability lies below which value of p?

``` r
quantile( samples , 0.2 )
```

    ##       20% 
    ## 0.5195195

##### 3E5. 20% of the posterior probability lies above which value of p?

``` r
quantile( samples , 0.8 )
```

    ##       80% 
    ## 0.7567568

##### 3E6. Which values of p contain the narrowest interval equal to 66% of the posterior probability?

``` r
HPDI( samples , prob=0.66 )
```

    ##     |0.66     0.66| 
    ## 0.5205205 0.7847848

##### 3E7. Which values of p contain 66% of the posterior probability, assuming equal posterior probability both below and above the interval?

``` r
PI(samples , prob=0.66 )
```

    ##       17%       83% 
    ## 0.5005005 0.7687688

## Medium Section

##### 3M1. Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Construct the posterior distribution, using grid approximation. Use the same flat prior as before.

``` r
p_grid <- seq(from = 0 , to = 1 , length.out = 1000)
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

plot(posterior)
```

![](chapter3_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
plot(samples)
```

![](chapter3_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
dens(samples)
```

![](chapter3_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

##### 3M2. Draw 10,000 samples from the grid approximation from above. Then use the samples to calculate the 90% HPDI for p.

``` r
HPDI(samples , prob=0.9 )
```

    ##      |0.9      0.9| 
    ## 0.3243243 0.7157157

##### 3M3. Construct a posterior predictive check for this model and data. This means simulate the distribution of samples, averaging over the posterior uncertainty in p. What is the probability of observing 8 water in 15 tosses?

  - simulate predicted observations for single value of p:

As we have observed 8 out of 15, then the probability is 8/15=0.53:

``` r
sim_samples <-rbinom( 10000, size = 15 , prob = 0.53 )
dens(sim_samples)
```

![](chapter3_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

  - include parameter uncertainty in this(replace the single value of p
    with waht we have computed already for the distribution of p):

<!-- end list -->

``` r
dist_p <-rbinom(10000, size = 15 , prob = samples)
mean(dist_p==8)
```

    ## [1] 0.1392

``` r
table(dist_p)/10000
```

    ## dist_p
    ##      0      1      2      3      4      5      6      7      8      9 
    ## 0.0006 0.0039 0.0142 0.0299 0.0535 0.0829 0.1161 0.1413 0.1392 0.1356 
    ##     10     11     12     13     14     15 
    ## 0.1149 0.0828 0.0515 0.0240 0.0081 0.0015

##### 3M4. Using the posterior distribution constructed from the new (8/15) data, now calculate the probability of observing 6 water in 9 tosses.

``` r
dist_p <-rbinom(10000, size = 9 , prob = samples)
mean(dist_p==6)
```

    ## [1] 0.178

##### 3M5. Start over at 3M1, but now use a prior that is zero below p = 0:5 and a constant above p = 0:5. This corresponds to prior information that a majority of the Earth’s surface is water. Repeat each problem above and compare the inferences. What difference does the better prior make? If it helps, compare inferences (using both priors) to the true value p = 0:7.

  - M1

<!-- end list -->

``` r
p_grid <- seq(from = 0 , to = 1 , length.out = 1000)
prior <- ifelse(p_grid < 0.5, 0, 0.5)
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
dens(samples)
```

![](chapter3_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

  - M2

<!-- end list -->

``` r
HPDI( samples , prob=0.9 )
```

    ##      |0.9      0.9| 
    ## 0.5005005 0.7077077

  - M3

<!-- end list -->

``` r
sim_samples <-rbinom(10000, size = 15 , prob = samples)
dens(sim_samples)
```

![](chapter3_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
mean(sim_samples==8)
```

    ## [1] 0.1617

  - M4

<!-- end list -->

``` r
dist_p_6_out_9 <-rbinom( 10000, size = 9 , prob = samples )
dens(dist_p_6_out_9)
```

![](chapter3_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
mean(dist_p_6_out_9==6)
```

    ## [1] 0.2376

## Hard Section

###### 3H1. Using grid approximation, compute the posterior distribution for the probability of a birth being a boy. Assume a uniform prior probability. Which parameter value maximizes the posterior probability?

``` r
births<-data(homeworkch3)

total_boys <- sum(birth1) + sum(birth2)

p_grid <- seq(from = 0 , to = 1 , length.out = 10000)
prior <- rep( 1 , 10000 )
likelihood <- dbinom( total_boys , size=200 , prob=p_grid )
posterior_non_std <- likelihood * prior
posterior <- posterior_non_std / sum(posterior_non_std)

maximum_a_posteriori<-p_grid[which.max(posterior)]

maximum_a_posteriori
```

    ## [1] 0.5549555

###### 3H2. Using the sample function, draw 10,000 random parameter values from the posterior distribution you calculated above. Use these samples to estimate the 50%, 89%, and 97% highest posterior density intervals.

``` r
samples <- sample(p_grid , prob=posterior , size=1e4 , replace=TRUE)

hpdi_50_initial<-HPDI( samples , prob=0.5 )
HPDI( samples , prob=0.89 )
```

    ##     |0.89     0.89| 
    ## 0.4982498 0.6095610

``` r
HPDI( samples , prob=0.97 )
```

    ##     |0.97     0.97| 
    ## 0.4736474 0.6253625

``` r
dens(samples)
abline(v = maximum_a_posteriori, col = "red" )
```

![](chapter3_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

###### 3H3. Use rbinom to simulate 10,000 replicates of 200 births. You should end up with 10,000 numbers, each one a count of boys out of 200 births. Compare the distribution of predicted numbers of boys to the actual count in the data (111 boys out of 200 births). There are many good ways to visualize the simulations, but the dens command (part of the rethinking package) is probably the easiest way in this case. Does it look like the model fits the data well? That is, does the distribution of predictions include the actual observation as a central, likely outcome?

``` r
initial_preditive_posterior_dist_sample200 <-rbinom(10000, size = 200 , prob = samples)

dens(initial_preditive_posterior_dist_sample200)
abline(v = total_boys, col = "red" )
```

![](chapter3_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

The distribution of predictions includes the actual observation as a
central, likely
outcome.

##### 3H4. Now compare 10,000 counts of boys from 100 simulated first borns only to the number of boys in the first births, birth1. How does the model look in this light?

``` r
boys1 <- sum(birth1)
boys2 <- sum(birth2)

initial_preditive_posterior_dist_sample100 <-rbinom(10000, size = 100 , prob = samples)

dens(initial_preditive_posterior_dist_sample100)
abline(v = boys1, col = "red" )
abline(v = total_boys, col = "red" )
```

![](chapter3_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

The distribution of predictions doesn’t include the actual observation
of first born boys as a central, likely
outcome.

##### 3H5. The model assumes that sex of first and second births are independent. To check this assumption, focus now on second births that followed female first borns. Compare 10,000 simulated counts of boys to only those second births that followed girls. To do this correctly, you need to count the number of first borns who were girls and simulate that many births, 10,000 times. Compare the counts of boys in your simulations to the actual observed count of boys following girls. How does the model look in this light? Any guesses what is going on in these data?

  - vector with only those families, which had a first child girl and a
    second child boy

<!-- end list -->

``` r
girl_then_boy_families <- birth2[birth1 == 0]
gb_boys <- sum(girl_then_boy_families)
gb_girls <- length(girl_then_boy_families) - gb_boys
```

  - count number of first borns who were girls and simulate that many
    births, 10,000
times.

<!-- end list -->

``` r
posterior_predictive_gb <- rbinom(10000, size = length(girl_then_boy_families), prob = samples)

dens(posterior_predictive_gb)
abline(v = gb_boys, col = "red")
```

![](chapter3_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

We have simulated 10 000 samples of 49 births (the number of families,
which had a girl the first time), using the probability that we have
calculated at the begining, where the analysis disregarded the fact half
of the sample are first borns and secound half are secound born. If the
birth of a boy or a girl is independent, then we would expect the
observed number of boys, which are born after a girl to be central
number in this simulation (somewhere between 25 and 30). However, the
plot shows that the number of boys is at the top end - almost 40, then
it can be concluded that it is more likely to have a baby boy after
having a baby girl.

Lets look at the other proportion - possibility of having a girl after
having a boy.

  - vector with only those families, which had a first child boy and a
    second child girl

<!-- end list -->

``` r
boy_then_girl_families <-birth2[birth1 == 1]
bg_boy <- sum(boy_then_girl_families)
bg_girls <- length(boy_then_girl_families) - bg_boy
```

  - count number of first borns who were girls and simulate that many
    births, 10,000
times.

<!-- end list -->

``` r
predictive_posterior_bg <- rbinom(n = 10000, size = length(boy_then_girl_families), prob = samples)

dens(predictive_posterior_bg)
abline(v = bg_boy, col = "red")
```

![](chapter3_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

This picture also shows that having a boy or a girl is not indepedent.
Having simulated 10 000 samples of 51 families (those, who had a boy
first), we would expect the number of born boys to be somewhere between
25 and 30. However, the actual count is near 20, which indicates that
the probability of having a girl as a secound baby (after having a boy)
is higher then having two boys.
