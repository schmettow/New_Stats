---
title: "New statistics for the design researcher"
author: "Martin Schmettow"
date: "2018-10-30"
output: pdf_document
github-repo: schmettow/NewStats
documentclass: book
site: bookdown::bookdown_site
subtitle: A Bayesian course in tidy R
description: A statistics book for designers, human factors specialists, UX researchers,  applied psychologists and everyone else who works hard to make this world a better  place.
---


# Introduction

This book makes the following assumptions:

1.	Design research is for decision making, where one accounts for expected utility of design options. This requires
   a.	quantitative statements
   b.	statements of (un)certainty
2.	Bayesian statistics is intuitive. Everybody has a good intuition about probability, value, decision making and gambling. It requires little effort to get to a more formal level of understanding.
3.	Data arrives through data generating processes. Premise of statistical modelling is to neatly align to the generating process’ anatomy.
4.	Applied research data is multivariate and correlated. There is nothing such as a nuisance variable. Everything that helps understanding advantages and drawbacks of a design matters.
5.	Average? Neverage! People differ, and diversity matters in design.
6.	The universe is endless, but everything in it is finite, which strictly is at odds with linearity assumptions. 
7.	The human eye is a highly capable pattern evaluator. It would be a waste to not use visuals for exploring data and communicating results.
8.	The best way to anticipate and plan data analysis is to simulate data upfront.
9.	R is the statisticians preferred toolbox. 80% of statistical analysis can be done with 20% of R’s full capabilities.



Chapter \@ref(design_research) introduces a framework for quantitative design research. It carves out the basic elements of empirical design research, such as users, designs and performance and links them to typical research problems. Then the idea of design as decision making under uncertainty is developed at the example of two case studies.

Chapter \@ref(bayesian_statistics) introduces the basic idea of Bayesian statistics, which boils down to three remarkably intuitive conjectures:

1. uncertainty is the rule
2. you gain more certainty by observations
3. your present knowledge about the world is composed of what you learned from data and what you knew before.

The chapter goes on with introducing basic terms of statistical thinking. Finally, an overview on common statistical distributions serve as a vehicle to make the reader familiar with data generazing processes. I am confident that this chapter serves newbies and classically trained researchers with all tools they need to understand Bayesian statistics. The more formally trained reader may want to take this as a bedtime reading.

Chapter \@ref(getting_started_r) is a minimal introduction to this marvelous programming language. Readers with some programming experience can work through this in just one day and they will get everything they need to get started with the book. Readers with prior R experience may get a lot out of this chapter, too, as it introduces the *tidy* paradigm of R programming. Tidy R is best be thought of as a set standard libraries 2.0. New tidy packages are arriving at an accelerating pace in the R world, and coding tidy is usually much briefer, easier to read and less error prone. It has at least the same significance as turning from procedural programming to object orientation. While this chapter serves as a stepping stone, the reader will encounter countless code examples throughout the book, working through these examples is a 

The second part of the book consists of three chapters that strictly built on each other. The first chapter \@ref(linear_models) introduces basic elements of linear models, which are factors, covariates and interaction effects. A number of additional sections cover under-the-hood concepts, such as dummy variables or contrasts, as well as basic model criticism (residual analysis). Working through this chapter fully is essential as develops the jargon to a good deal. 

However, for most readers, the first chapter is not sufficient to get to work, as it does not cover models for repeated measures. This is added extensively in chapter \@ref(multilevel_models). It proceeds from simple repeated measures designs to complex designs, where human and multiple non-human populations encounter each other. After working through this chapter, the reader is prepared to design highly effective studies. As long as the patterns of randomness are not off by to much from the linear model assumptions, it also suffices to get to work, seriously.

The third chapter \@ref(generalized_linear:models)  opens up a plethora of possibilities to analyze all kinds of performance variables. Standard models are introduces to deal with counts, chances, temporal variables and rating scales. It is also meant as an eye opener for reseachers who routinely resorted to non-parametric procedures. After working through this chapter, I would consider anyone a sincere data analyst.   

The fourth chapter \@ref(nonlinear_models) re-iterates on the truth that in an endless universe everything is finite. We leave our fond of high-level regression engine alone and take a closer look at the nuts and bolts it is based upon. By the example of a logistic growth process and efefcts of training, the reader gains a preview on the almighty model specification language Stan.

The third part of the book is primarily dedicated to researchers who deal with complex and high-stake problems, routinely. Elememts of the statistical workflow are spread out in much detail, covering data manipulation, advanced graphics, simulation and model evaluation.

<!--
re-organize the reading guide into three learning routes:

+ learning R
+ learning statistical modelling
+ learning doing effective research

for all routes, distinguish between reader types, perhaps make a cross table
-->

