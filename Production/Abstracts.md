
# Introduction

In this chapter I will loosely define the terms of the title: design research, New Statistics and tidy R. Then I give some possible routes through the book, as well as ideas how to use it in the classroom.


# Getting started with R

In this book, we will be using the statistical computing environment R.
R at its core is a programming language that specializes on statistics
and data analysis. R comes with a complete set of standard packages that cover common routines in statistical analysis. However, the
standard packages are known to be incomplete and inconsistent, which makes it hard to learn. In the past few years, Hadley Wickham has  started an initiative known as the Tidyverse. Under this new dogma a fast growing collection of tidy packages emerged, which share a set of data engineering  principles. This makes Tidyverse packages inter-operable and easy to learn. This chapter introduces two tidy packages: Dplyr for tidy data processing and Ggplot for plotting.


# Elements of Bayesian statistics

The aim of scientific research is to avoid the pitfalls of our minds and act as rational as possible by translating our theory into a formal model of reality, gathering evidence in an unbiased way and weigh evidence against noise in a controlled way. This chapter introduces the Bayesian approach to statistical modeling from ground up and illustrates its use for rational decision making. Probability is derived from set theory and relative frequencies, before we dive into Bayes Bayesian thinking. The elements and practical details of statistical modeling is introduced by its two components: the structural part, which typically carries the research question or theory, followed by a rather deep account of the second component of statistical models: the random part.



# Basic Linear models

Linear models answer the question of how one quantitative outcome, say ToT, decreases or increases, when a condition changes. This chapter introduces the basic three basic ways how such conditions can enter a linear model. 
The most basic LM, the grand mean model does not account for any conditions and produces just a single estimate: the grand mean in the population.
In linear regression models, a metric predictor (e.g. age) is linked to the outcome by a linear function, such as: $f(x) = \beta_0 + \beta_1x$. A very common type of question is how an outcome changes by a set of discrete conditions, such as two different designs. Factorial model use a mechanism called dummy variables to make categorical variables fit into the linear term. With ordinal factor models, dummy variables are put to a good use. 



# Multi-predictor models

Design researchers are often collecting data under a variety of conditions, each of which qualifies as a predictor in its own right. The advantage of the linear terms is that they can be combined, which allows estimating the simultaneous influence of multiple predictors in one model. We will start with models, where all predictors act mutually independent on the outcome. In reality it often happens that predictors are not independent, but act conditional upon each other. Conditional effects models can adjust a model for common violations of linearity, saturation and amplification, but can also uncover relevant structures, or test theories. The chapter ends with polynomial models, which allow to fit non-linear relationships between a metric predictors and outcome.




# Multilevel models

Multi-level linear models introduce a special type of categorial variable, the random factor, which applies when the factor levels can be seen as members of a population, such as participants in a sample. Multi-level models allows to simultaneously produce estimates on population level and on participant level. That makes multi-level models interesting for a variety of applications, three of which are covered in this chapter. First, we will see how multi-level models render diversity of users, which is expressed as the random factor variance.  Taken this to an extreme, participant-level coefficients can answer the question, whether a design-related impact factor is universal, in that it can be observed for every participant. Finally, multi-level models are very well-suited to handle psychometric, or design-o-metric, situations, where some of the populations are non-human.


# Generalized Linear Models


In the preceding chapters were all about specifying an appropriate (and often sophisticated) predictor term. In this chapter, we will give the outcome variables their due respect. First, the assumptions of Gaussian linear model are reviewed and generally discarded. Next, the framework of Generlized LIner Models is explained from ground up. After that is established, I will introduce a good dozen  of model families, organized by types of measures. Next to some commonly known families, such as Poisson or Logistic regression models, this chapter will cover outcome variables for which good defaults have been lacking, such as rating scale responses and ToT measures. For RT and ToT data, I will suggest exponentially-modified Gaussian models and for (quasi)continuous rating scales I will introduce a rather novel approach, Beta regression. The chapter closes with a look beyond GLMs. Distributional models allow to link predictors to the mean, but also other properties of the outcome distribution. I will show how a distributional model covers differences in individual answer styles to rating scales.


# Working with models

This chapter introduces techniques to evaluate models and to compare multiple models. The first part applies only to (Gaussian) linear models and introduces visual techniques to discover violations of assumptions,  linearity and residual distribution. Then, a general framework for comparing models of any kind is introduced. This ground on the idea of predictive accuracy, which leads to the technique of leave-one-out cross validation. As this method is too inefficient in most situations, computationally more efficient approximations known as information criteria are introduced. By revisiting cases from preceding chapters, I demonstrate how model selection 
by information criteria can be used to make a justified choice on a set of models. Finally, the same technique also applies  for explicit hypothesis testing, although this is not my general tone.
