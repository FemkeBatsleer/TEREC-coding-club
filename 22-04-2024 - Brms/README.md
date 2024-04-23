# Bayesian analyses with brms session 22-04-2024

In this folder, you find the scripts, data and explanation on the brms session, given by Frederik Mortier.
The R-script `Brms_intro_unfilled.R` has some left out parts of code for you to exercise, the `Brms_intro_filled.R` contains the solution.

## Installing rstan & brms

Install the R package of BRMS (instructions: https://paul-buerkner.github.io/brms/), which may also ask you to install rstan (instructions, choose latest released version: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started). Heads up: this is not the easiest installation...
The internet is a big help for issues, but you can also ask Frederik Mortier to help with the installation. If everything is installed you should be able to run these few lines:

```
library(brms)
library(tidyverse)
mod <- brm(formula = y ~ x,data = tibble(x = 1:2000/500-2, y = rnorm(2000, mean = x, sd = 0.1)),chains = 2, iter = 1000)
predict(mod)%>%as_tibble()%>%mutate(E2 = Estimate^2, x = 1:2000/500-2, eye_x = 1, eye_y = 6)%>%ggplot(aes(x = x, y = E2))+geom_line()+ylim(c(0, 7))+geom_point(aes(eye_x*-1,eye_y),size = 5)+geom_point(aes(eye_x,eye_y), size = 5)
```

## Extra background

* https://www.youtube.com/watch?v=FdnMWdICdRs&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus Statistical Rethinking youtube course
* https://bookdown.org/content/4857/ the brms 'translation' to Statistical Rethinking
* https://paul-buerkner.github.io/brms/articles/index.html BRMS vignettes on some of the more advance features of BRMS
