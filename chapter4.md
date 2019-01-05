Statistical Rethinking Chapter 4
================

## Medium Section

##### 4M1. For the model definition below, simulate observed heights from the prior (not the posterior).

<!-- $'y_i \sim'$ Normal ($'\mu$; $\sigma'$) -->

<!-- $\mu \sim$ Normal (0; 10) -->

<!-- $\sigma \sim$ Uniform (0; 10) -->

``` r
number_samples <- 100

prior_sim_mu <-rnorm(n=number_samples, 0 , 10 )
prior_sim_sigma <-runif(n=number_samples, 0, 10)
prior_sim_heigth<- rnorm(n=number_samples,prior_sim_mu,prior_sim_sigma)

plot(prior_sim_heigth)
```

![](chapter4_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

##### 4M2. Translate the model just above into a map formula.

    m2 <- map(alist(height ~ dnorm( mu , sigma ), mu ~ dnorm( 0 , 10 ), sigma ~ dunif( 0 , 10 )), data=d )

##### 4M4. A sample of students is measured for height each year for 3 years. After the third year, you want to fit a linear regression predicting height using year as a predictor. Write down the mathematical model definition for this regression, using any variable names and priors you choose. Be prepared to defend your choice of priors.

``` r
m4 <- map(alist(height ~ dnorm( mu , sigma ) ,
                mu <- a + b*age ,
                a ~ dnorm( 100 , 50) ,
                b ~ dnorm( 0 , 10) ,
                sigma ~ dunif( 0 , 50)) , data=d )
```

##### 4M5. Now suppose I tell you that the average height in the first year was 120 cm and that every student got taller each year. Does this information lead you to change your choice of priors? How?

``` r
m5 <- map(alist(height ~ dnorm( mu , sigma ) ,
                mu <- a + b*age ,
                a ~ dnorm( 120 , 50) ,
                b ~ dnorm( 1 , 10) ,
                sigma ~ dunif( 0 , 50)) , data=d )
```

##### 4M6. Now suppose I tell you that the variance among heights for students of the same age is never more than 64cm. How does this lead you to revise your priors?

If variance is 64, then standart deviation is 8. Therefore the uniform
distribution will have values untill 8.

    m5 <- map(alist(height ~ dnorm( mu , sigma ) ,
                    mu <- a + b*age ,
                    a ~ dnorm( 120 , 50) ,
                    b ~ dnorm( 1 , 10) ,
                    sigma ~ dunif( 0 , 8)) , data=d )

## Hard Section

##### 4H1 The weights listed below were recorded in the \!Kung census, but heights were not recorded for these individuals. Provide predicted heights and 89% intervals (either HPDI or PI) for each of these individuals. That is, fill in the table below, using model-based predictions.

``` r
weight_new <- c(46.95, 43.72, 64.78, 32.59, 54.63)

# select appropriate data for the analysis. Adults below 18 have diffent temp for gaining weigth, therefore, 
# data below 30 (the lowest value for the desired prediction is 32) should be ignored. 
d_over_30kg <- d[d$weight>=30,]


mh1 <- map(alist(height ~ dnorm( mu , sigma ) ,
                 mu <- a + b*weight ,
                 a ~ dnorm( 150 , 40) ,
                 b ~ dnorm( 0 , 10) ,
                 sigma ~ dunif( 0 , 8)) , data=d_over_30kg )

# get the coeffients
precis( mh1 , corr=TRUE )
```

    ##         Mean StdDev   5.5%  94.5%     a     b sigma
    ## a     115.39   1.78 112.55 118.24  1.00 -0.99  0.03
    ## b       0.87   0.04   0.81   0.94 -0.99  1.00 -0.03
    ## sigma   5.00   0.18   4.71   5.30  0.03 -0.03  1.00

``` r
post <- extract.samples( mh1 , n=1e4 )


# plot the maximum a posteriori line 
plot( height ~ weight , data=d_over_30kg )
abline( a=coef(mh1)["a"] , b=coef(mh1)["b"] )
```

![](chapter4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#get MAP and HPDI for MAP
mu.link <- function(weight) post$a + post$b*weight
mu <- sapply( weight_new , mu.link )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )


# plot raw data
# fading out points to make line and interval more visible
plot( height ~ weight , data=d_over_30kg , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight_new , mu.mean )
# plot a shaded region for 89% HPDI
shade(mu.HPDI , weight_new)
```

![](chapter4_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
mu.mean
```

    ## [1] 156.3604 153.5414 171.9216 143.8276 163.0632

##### 4H2. Select out all the rows in the Howell1 data with ages below 18 years of age. If you do it right,you should end up with a new data frame with 192 rows in it.

1)  Fit a linear regression to these data, using map. Present and
    interpret the estimates. For every 10 units of increase in weight,
    how much taller does the model predict a child gets?

<!-- end list -->

``` r
# select only those above 18
d_18 <- d[d$age<18,]
summary(d_18)
```

    ##      height           weight            age              male       
    ##  Min.   : 53.98   Min.   : 4.252   Min.   : 0.000   Min.   :0.0000  
    ##  1st Qu.: 89.13   1st Qu.:11.708   1st Qu.: 3.000   1st Qu.:0.0000  
    ##  Median :111.12   Median :16.981   Median : 7.000   Median :0.0000  
    ##  Mean   :108.32   Mean   :18.414   Mean   : 7.722   Mean   :0.4792  
    ##  3rd Qu.:127.72   3rd Qu.:23.417   3rd Qu.:12.000   3rd Qu.:1.0000  
    ##  Max.   :158.12   Max.   :44.736   Max.   :17.000   Max.   :1.0000

``` r
# fit a model
mh2 <- map(alist(height ~ dnorm( mu , sigma ) ,
                 mu <- a + b*weight ,
                 a ~ dnorm( 80 , 70) ,
                 b ~ dnorm( 0 , 20) ,
                 sigma ~ dunif( 0 , 20)) , data=d_18 )

coeff_values<-precis(mh2)
coeff_values
```

    ##        Mean StdDev  5.5% 94.5%
    ## a     58.24   1.40 56.01 60.47
    ## b      2.72   0.07  2.61  2.83
    ## sigma  8.44   0.43  7.75  9.13

``` r
10*coef(mh2)["b"]
```

    ##        b 
    ## 27.19679

Child gets 27 cm taller with each 10 units increase in weigth.

2)  Plot the raw data, with height on the vertical axis and weight on
    the horizontal axis. Superimpose the MAP regression line and 89%
    HPDI for the mean.

<!-- end list -->

``` r
d_18$hdpi_5.5 <- 56.01 + d_18$weight*2.61
d_18$hdpi_94.5 <- 60.47 + d_18$weight*2.83

# plot the maximum a posteriori line 
plot( height ~ weight , data=d_18 )
abline( a=coef(mh2)["a"] , b=coef(mh2)["b"])
```

![](chapter4_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Also superimpose the 89% HPDI for predicted heights.

``` r
weight.seq_18 <- seq( from=0 , to=70 , by=1 )

# sample from the posterior predictive
post <- extract.samples(mh2)

sim.height_18 <- sapply( weight.seq_18 ,
                         function(weight) rnorm(n=nrow(post) ,
                                          mean=post$a + post$b*weight ,
                                          sd=post$sigma ) )

height.HDPI_18 <- apply( sim.height_18 , 2 , HPDI , prob=0.89 )
mu.mean_18 <- apply( sim.height_18 , 2 , mean )

# plot raw data
# fading out points to make line and interval more visible
plot( height ~ weight , data=d_18 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq_18 , mu.mean_18 )
# plot a shaded region for 89% HPDI
shade( height.HDPI_18 , weight.seq_18 )
```

![](chapter4_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

3)  What aspects of the model fit concern you? Describe the kinds of
    assumptions you would change, if any, to improve the model. You
    don’t have to write any new code. Just explain what the model
    appears to be doing a bad job of, and what you hypothesize would be
    a better model.

<!-- end list -->

``` 
Looking at the plot we can see that the relationship is not linear. 
In order to  include this in the model we can add the variable weigth squared or cubed. 
```

##### 4H3. Suppose a colleague of yours, who works on allometry, glances at the practice problems just above. Your colleague exclaims, “That’s silly. Everyone knows that it’s only the logarithm of body weight that scales with height\!” Let’s take your colleague’s advice and see what happens.

1)  Model the relationship between height (cm) and the natural logarithm
    of weight (log-kg). Use the entire Howell1 data frame, all 544 rows,
    adults and non-adults. Fit this model, using quadratic
    approximation:

<!-- end list -->

``` r
mh3 <- map(alist(height ~ dnorm( mu, sigma ) ,
                 mu <- a + b*log(weight),
                 a ~ dnorm( 178 , 100) ,
                 b ~ dnorm( 0 , 100) ,
                 sigma ~ dunif( 0 , 50)) , data=d )

precis(mh3)
```

    ##         Mean StdDev   5.5%  94.5%
    ## a     -23.79   1.34 -25.93 -21.66
    ## b      47.08   0.38  46.47  47.69
    ## sigma   5.13   0.16   4.89   5.38

2)  Then use samples from the quadratic approximate posterior of the
    model in (a) to superimpose on the plot: (1) the predicted mean
    height as a function of weight, (2) the 97% HPDI for the mean, and
    (3) the 97% HPDI for predicted heights.

<!-- end list -->

``` r
# sample from the posterior
posterior <- extract.samples( mh3 , n=1e4 )

#get MAP and HPDI for MAP
mu.link <- function(weight) posterior$a + posterior$b*log(weight)
weight.seq <- seq( from=0 , to=70 , by=1 )
mu <- sapply( weight.seq , mu.link )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

# sample from the posterior predictive
postеrior <- extract.samples(mh3)

sim.height_log <- sapply( weight.seq ,    function(weight.seq) rnorm(n=nrow(post) ,
                                          mean=posterior$a + posterior$b*log(weight.seq),
                                          sd=posterior$sigma ) )



height.HDPI_log <- apply( sim.height_log , 2 , HPDI , prob=0.89 )
mu.mean_log <- apply( sim.height_log , 2 , mean )


# plot raw data
plot( height ~ weight , d , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( weight.seq , mu.mean_log )
# draw HPDI region for line
shade( mu.HPDI , weight.seq )
# draw PI region for simulated heights
shade( height.HDPI_log , weight.seq )
```

![](chapter4_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
