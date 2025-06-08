<h1>Likelihood ratio test for sphericity under a multivariate normal assumption</h1>

<h2>Description</h2>

This repository contains my disseratation which I wrote in my last year of my undergraduate 2024/2025, in partial fulfillment of the requirements for the degree of Bachelor of Science in Mathematics at the University of St Andrews.


<h2>Abstract</h2>

In multivariate statistics, it is often important to assume independence and homoscedasticity. Under the multivariate normal assumption, testing for these conditions corresponds to test sphericity. While, in general, likelihood ratio tests have great properties, the sphericity test is known to be ineffective in detecting departures from sphericity, and to lose accuracy when the assumption of multivariate normality is violated. The exact distribution of the statistic is difficult to compute and consequently, several approximations have been proposed, the most common of which relies on Wilks’ theorem to model the negative logarithm of the likelihood ratio test statistic $U$, using a chi-squared distribution. Alternatively, a second statistic, $U'$, may be used by scaling $U$ with an appropriate constant. This study investigates the properties of the sphericity test using these two statistics. Through extensive simulations, we assess the accuracy and limitations of the sphericity test, providing insight into when the test can be reliably applied in practice. We conclude suggesting on the use of the statistic $U'$, instead of $U$, since its distribution shows a better approximation to the expected asymptotic behaviour and it allows to achieve a higher power, without increasing the complexity.

<br />

<h2>Repository Content</h2>

- <b>Dissertation written in LaTeX (source and pdf)</b>
- <b>A folder containg the code written in `R` used in the simulations and to produce plots</b>
