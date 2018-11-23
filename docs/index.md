---
title: "New statistics for the design researcher"
subtitle: A Bayesian course in tidy R
author: "Martin Schmettow"
date: "2018-11-20"
github-repo: schmettow/New_Stats
site: bookdown::bookdown_site
description: A statistics book for designers, human factors specialists, UX researchers,  applied psychologists and everyone else who works hard to make this world a better  place.
documentclass: svmono
---
# (PART) Preparations {-}

# Introduction


## Whom this book is for

### The empirical design researcher

If you are not a designer, chances are good that you are a design researcher very well. Are you doing studies with people and is your work about ways to achieve or improve products, services, trainings or business? Welcome to the not-so-special  club of people I call design researchers. 

Of course, I have no idea who you are, personally, but I figure you as one of the following personas:

You are an industrial design researcher, thirty-three years old, leading a small team in the center for research of a car maker. Your task is to evaluate emerging technologies, like augmented reality, car-to-car communication and smart light. Downstream research relies on your competent assessment of what is feasible. For example, you have just been asked to evaluate the following: 
Blue light makes people wake up easier, could it also be used to let car drivers not fall asleep? Several engineers and one industrial engineering student are involved in putting a light color stimulator for a premium car of the brand and stuff two workstations in its back, hooked to the recording systems and driving performance and physiological measures. Yes, this is as expensive as it sounds, and this why you have skin in the game, when you do a study.

You are a young academic and just got a cum laude master’s degree in computer science. For your thesis you developed and implemented an animated avatar for a digital assistant. A professor from the social psychology department has read your thesis. He just got a funding for research on mass emergency communication using projected virtual characters. He fears that a young psychologist would not be up to the technical part of the project and found you. But, you ask yourself, am I up to the task of running experimental studies and do the statistics?


<!-- 42 -->



### The experimentalist

If you are doing research with people, but the answers you seek are far from being applicable to anything in sight, don't put this book away. Chances are good that you will be able to read through the ephemeral details of the cases I present and recognize your own research situations, for example that you are comparing two groups. As you can see in the table of contents, you don't even have to read more than half the book to get there. 

It may be true that strictly experimental data does not require more than group comparison. Still, I please you: also read the chapter on multilevel models. So much contemporary experimental research mistakens *average* for *universal*. It makes a difference to ask:

>"Are responses in the Stroop task responses delayed in the incongruent condition?"

or to ask:

>"Every person responds delayed in the incongruent condition?"

If your work is about revealing universal laws of behaviour, you have to search for the answer on an individual level. Technically, that means a rather simple multi-level model and a spaghetti plot will do the rest. But note that such a model is composed of many little participant-level models and all of them need their data. For multi-level models you need a within-subject design and repeated measures. On the second thought that makes full sense, as what really ask is:

>"Do all people shift into a slower mode in the incongruent condition?"

This is not about groups of people, but mode transitions in individual brains. These transitions can only be observed by putting one-and-the-same person into all the conditions. If you fear that the order of modes makes a difference, why not put order under statistical control? Record what you cannot control and check the interaction effects. Maybe it is not so bad.

Just another candy for you: Response times! Have you been struggling with them forever, because they are not Normal distributed? Have you resorted to non-parametric tests so many times that what you say became "response times are non-parametric"? You shouldn't say that. Furthermore, your non-parametric sacrifices can be history with Generalized Linear Models.

Then again, you may soon notice a disturbing lack of stars. Is this book like a very dark night? Consider the opposite possibility: most times when you don't see the stars is at daylight. But let me hand you a torch with a chapter on model selection. The following paragraph may help you pushing your work through the review process:

>In order to confirm that the Stroop effect exists, we compared the predictive power of two models by information crieria. M_0 is an intercept-only model that denies the Stroop effect, M_1 allows for it. In order to evaluate universality of the Stroop effect, the models got participant-level random effects. As response times tend to have an offset and are left-skewed we chose exponential-Gaussian error distributions.


### The applied researcher

Tongue-in-cheek, applied researchers take real problems to get their questions, but rarely solve them. Why not? It is legitimate, almost natural, to ask what causes the Uncanny Valley effect, for instance. You do a series of experimental study and also throw personality scales into the  game. Maybe, the effect is not unversal. Why? Just that.

<!-- 43 -->




## Assumptions

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


## How to read this book

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
