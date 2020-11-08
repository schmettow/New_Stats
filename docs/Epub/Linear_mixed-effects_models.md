# Multilevel models {#mlm}



In the previous chapters we have seen several examples of conditional effects: groups of users responding differently to design conditions, such as font size, noise and emerging technology. Dealing with differential design effects seems straight forward: identify the relevant property, record it and add an conditional effect to the model. 

<!-- Identifying the relevant property is, in fact, a catch-22: how would you know what is relevant before you actually conducted the research. Researchers routinely record basic demographic properties such as age and gender, but these frequently show little effects, or the effects are obvious, i.e. not interesting. In addition, such predictors are rarely more than approximations of the properties that make the real difference. Older people have weaker vision *by tendency*, but the individual differences in any age group are immense. Boys tend to be more technophile, but there are some real geeky girls, too. -->

Identifying user properties that matter requires careful review of past research or deep theorizing, and even then it remains guesswork. Presumably, hundreds of studies attempted to explain differences in usage patterns or performance by all sorts of psychological predictors, with often limited results. That is a big problem in design research, as variation in performance can be huge and good predictors are urgently needed. Identifying the mental origins of being fast versus slow, or motivated versus bored, is extremely useful to improve the design of systems to be more inclusive or engaging.

As we will see in this chapter, individual differences can be accounted for and measured accurately without any theory of individual differences. For researchers trained in experimental social sciences it may require a bit of getting used to theory-free reasoning about effects, as it is always tempting to ask for the *why*. But in applied evaluation  studies, what we often really need to know is by *how much* users vary. The key to measuring variation in a population is to create models that operate on the level of participants, in addition to the population level, for example.

+ on population level, users prefer design B over A on average ($\beta_1 = 20$)
+ on the participant-level, participant $i$ preferred B over A ($\beta_{1i} = 20$), $j$ preferred A over B ($\beta_{1j} = -5$), +

When adding a participant-level effects, we still operate with coefficients, but in contrast to single-level linear models, every participant gets their own coefficient  ($\beta_{1\cdot}$). The key to estimating individual parameters is simply to regard participant (`Part`) a grouping variable on its own, and introduce it as a factor. 

<!-- Multi-level analysis is not limited to estimating models, that is using a regression engine. Throughout this chapter you will see exploratory plots that are on  in this introduction we will use the participant factor for producing multi-level exploratory plots. That should get you started. -->

The subsequent two sections introduce the basics of estimating multi-level linear models, first introducing intercept-only participant-level effects \@ref(intercept-re) and then slope (or group difference) effects \@ref(slope-re). Typically, fixed and random effects appear together in a linear multi-level model. It depends on the research question whether the researcher capitalizes on the average outcome, the variation in the population or participant-level effects. 

<!-- Throughout the chapter you will see two kinds of interpretingIn section \@ref(reporting_re) we will see how to report multi-level results depending on the type of research question.  -->

The participant-level is really just the factor and once it is regarded alongside the population level, a model is multi-level.  However, in multi-level linear modelling we usually  use a different type of factor, for the particpant level. The additional idea is that the levels of the factor, hence the individuals, are part of a *population*. The consequences of this perspective, will be discussed in \@ref(pool-shrink): a population is a set of entities that vary to some extent but also clump around a typical value. And that is precisely what *random effects* do: levels are drawn from an overarching distribution, usually the Gaussian. This distribution is estimated simultaneously to the individual parameters ($\beta_{1\cdot}$), which has advantages.  We will return to a more fundamental research case, the Uncanny Valley, and examine the *universality* of this strange effect \@ref(universality).

Once it is clear what the concept of random effects means for studying participant behaviour, we will see that it transfers with grace to *non-human populations*, such as designs, teams or questionnaire items. Three sections introduce multi-population multi-level models: In \@ref(non-human-populations) we will use a random effects model with four populations and compare their relative contribution to overall variance in performance. Section \@ref(re_nested) will show how multiple levels can form a  hierarchy and in \@ref(psychometrics) we will see that multi-level models can be employed the development of  *psychometrics tests*, that apply for people. Finally, we will see how to treat tests to compare designs, for which I will coin the term *design-o-metrics* \@ref(designometrix). 




## The Human Factor: Intercept random effects {#intercept-re}

Design science fundamentally deals with interaction between  systems and humans. Every measure we take in a design study is an encounter of an individual with a system. As people differ in many aspects, it is likely that people differ in how they use and perform with a system. In the previous chapter we have already dealt with differences between users: in the BrowsingAB case, we compared two designs in how inclusive they are with respect to elderly users. Such a research question seeks for a definitive answer on what truly causes variation in performance. Years of age is a standard demographic variable and in experimental studies it can be collected without hassle. If we start from deeper theoretical considerations than that, for example, we suspect a certain personality trait to play a significant role, this can become more effort. Perhaps, you need a 24-item scale to measure the construct, perhaps you first have to translate this particular questionnaire into three different languages, and perhaps you have to first invent and evaluate a scale. In my experience, personality scales rarely explain much of the variation we see in performance. It may be interesting to catch some small signals for the purpose of testing theories, but for applied design research it is more important to quantify the performance variation within a population, rather than explaining it.


<!-- That is already quite fancy, as it is seeks to explain variation. In applied design research, we often observe massive variation in performance betwee. based on idea that age may be related to performance in such a convoluted way Imagine, you had a different look at the BrowsingAB data, but had gathered no predictors at all and observed a massive amount of variation in performance. It is reasonable to assume that much of this variation stems from individual differences. That, in turn would mean that a fraction of users perform extremely well, whereas others fail miserably.  If that were true,  you would give the advice to invest into redesign that is more inclusive, ironing out the differences, would you not? But, to really drive it home, you have to prove that individual differences are the main source of variation. -->

At first, one might incorrectly think that a grand mean model would do, take $\beta_0$ as the population mean and $\sigma_\epsilon$ as a measure for individual variation. The mistake is that the residuals collect all random variations sources, not just variance between individuals, in particular residuals are themselves composed of:

+ inter-individual variation
+ intra-individual variation, e.g. by different levels of energy over the day
+ variations in situations, e.g. responsiveness of the website
+ inaccuracy of measures, e.g. misunderstanding a questionnaire item

What is needed, is a way to separate the variation of participants from the rest? Reconsider the principles of model formulations: the structural part captures what is repeatable, what does not repeat goes to the random term. This principle can be turned around: If you want to pull a factor from the random part to the structural part, you need repetition. For estimating users' individual performance level, all that is needed is repeated measures.

In the IPump study we have collected performance data of 25 nurses, operating a novel interface for a  syringe infusion pump. Altogether, every nurse completed a set of eight tasks three times. Medical devices are high-risk systems where a single fault can cost a life, which makes it a requirement that user performance is on a *uniformly* high level. We start the investigation with the global question:

> What is the average ToT in the population?


```r
attach(IPump)
```




```r
D_Novel %>% 
summarize(mean_Pop = mean(ToT))
```



| mean_Pop|
|--------:|
|       16|

The answer is just one number and does not refer to any individuals in the population. This is called the population-level estimate or fixed effect estimate. The following question is similar, but here one average is taken for every participant. We call such a summary *participant-level*. 

> What is the average ToT of individual participants?


```r
D_Novel %>% 
group_by(Part) %>% 
summarize(mean_Part = mean(ToT)) %>% 
  sample_n(5)
```



|Part | mean_Part|
|:----|---------:|
|4    |      22.4|
|5    |      16.0|
|19   |      16.6|
|17   |      13.3|
|9    |      19.4|

Such a grouped summary can be useful for situations where we want  to directly compare individuals, like in performance tests. In experimental research, individual participants are of lesser interest, as they are exchangeable entities. What matters is the total variation within the sample, representing the population of users. Once we have participant-level effects, the amount of variation can be summarized by the standard deviation:


```r
D_Novel %>% 
  group_by(Part) %>% 
  summarize(mean_Part = mean(ToT)) %>% 
  ungroup() %>% 
  summarize(sd_Part   = var(mean_Part))
```



| sd_Part|
|-------:|
|    13.8|


Generally, these are the three types of parameters in multi-level models: the population-level estimate (commonly called *fixed effects*), the participant-level estimates (*random effects*) and the *participant-level variation*.

<!-- Now, that we have the individual means, we can plot the variation in task performance. For comparison, the overall variation is added to the plot. It is apparent that individual differences make only part of the overall variance. -->

<!-- #### IMPROVE FIG -->

<!-- ```{r re_variation} -->
<!-- D_Novel %>%  -->
<!--   group_by(Part) %>%  -->
<!--   summarize(mean_Part = mean(ToT)) %>%  -->
<!--   ungroup() %>%  -->
<!--   ggplot(aes(x = mean_Part)) + -->
<!--   geom_density(data = D_Novel, aes(x = ToT, y=..scaled..,  -->
<!--                                fill = "total"),alpha = 1/2) + -->
<!--   geom_density(aes(fill = "user", y=..scaled..),alpha = 1/2) + -->
<!--   labs(fill = "Variation") -->
<!-- ``` -->


```r
detach(IPump)
```


Obviously, the variable `Part` is key to build such a model. This variable  groups observations by participant identity and, formally, is a plain factor. A naive approach to multi-level modeling would be to estimate an AGM, like `ToT ~ 0 + Part`, grab the center estimates and compute the standard deviation. What sets a truly multi-level apart is that population-level effects, participant-level effects and variation are contained in one model and are estimated *simultaneously*. Random effects are really just factors with one level per participant. The only difference to a fixed effects factor is that the levels are assumed to follow a Gaussian distribution. This will further be explained in section \@ref(pool-shrink).

For the IPump study we can formulate a GMM model with participant-level random effect $\beta_{p0}$ as follows:
<!-- #84 -->

$$
\begin{aligned}
\mu_i &= \beta_0 + x_p\beta_{0p}\\
\beta_{p0} &\sim \textrm{Gaus}(0, \sigma_{p0})\\
y_i &\sim \textrm{Gaus}(\mu_i, \sigma_\epsilon)
\end{aligned}
$$

There will be as many parameters $\beta_{0p}$, as there were participants in the sample, and they have all become part of the structural part. The second term describes the distribution of the participant-level group means. And finally, there is the usual random term. Before we examine further features of the model, let's run it. In the package `rstanarm`, the command `stan_glmer` is dedicated to estimating multi-level models with the extended formula syntax. 

However, I will now introduce another Bayesian engine and use it from here on. The Brms package provides the Brm engine, which is invoked by the command `brm()`. This engine covers all models that can be estimated with `stan_glm` or `stan_glmer` and it uses the precise same syntax. All models estimated in this chapter, should also work with `stan_glmer`. However, Brms supports an even broader set of models, some of which we will encounter in chapter \@ref(glm).

<!-- For multi-level models, the reason to switch Brms is a selfish one. It became to tedious, to support both Rstanarm and Brms with the Bayr package.  -->
The only downside of Brms is that it has to compile the model, preceding the estimation. For simple models, as in the previous chapter, the chains are running  very quickly, and the extra step of compilation creates much overhead. For the models in this chapter, the chains run much slower, such that compilation time becomes almost negligible.

Both engines Brms and Rstanarm differ a lot in how they present the results. The Bayr package provides a consistent interface to extract information from model objects of both engines. 


```r
attach(IPump)
```



```r
M_hf <- brm(ToT ~ 1 + (1|Part), data = D_Novel)
P_hf <- posterior(M_hf)
```




The posterior of a multi-level  model contains three types of variables (and the standard error)

1. the *fixed effect* captures the population average (Intercept)
1. *random effects* capture how individual participants deviate from the population mean
1. *random factor variation* (or group effects) captures the overall variation in the population.

With the `bayr` package these parameters can be extracted using the respective commands:


```r
fixef(P_hf)
```



Table: (\#tab:unnamed-chunk-7)Estimates with 95% credibility limits

|model |type  |fixef     | center| lower| upper|
|:-----|:-----|:---------|------:|-----:|-----:|
|M_hf  |fixef |Intercept |     16|  14.4|  17.5|

```r
ranef(P_hf) %>% sample_n(5)
```



Table: (\#tab:unnamed-chunk-7)Estimates with 95% credibility limits

|re_entity | center| lower| upper|
|:---------|------:|-----:|-----:|
|10        | -0.160| -3.86|  2.77|
|1         |  1.091| -1.10|  6.38|
|4         |  0.782| -1.38|  5.89|
|18        | -0.269| -4.17|  2.32|
|6         | -0.377| -4.79|  2.06|

```r
grpef(P_hf)
```



Table: (\#tab:unnamed-chunk-7)Estimates with 95% credibility limits

|model |type  |fixef     |re_factor | center| lower| upper|
|:-----|:-----|:---------|:---------|------:|-----:|-----:|
|M_hf  |grpef |Intercept |Part      |   1.53| 0.079|  3.73|


Random effects are factors and enter the model formula just as linear terms. To indicate that to the regression engine, a dedicated syntax is used in the model formula (recall that `1` represents the intercept parameter):

`(1|Part)`

In probability theory expressions, such as the famous Bayes theorem, the `|` symbol means that something to the left is conditional on something to the right. Random effects can be read as such conditional effects. Left of the `|` is the fixed effect that is conditional on (i.e. varies by) the factor to the right. In the simplest form the varying effect is the intercept and in the case here could be spoken of as:

> Average ToT, conditional on the participant

Speaking of factors: So far, we have used *treatment contrasts* as lot for population-level factors, which represent the difference towards a reference level. If random effects were coded as treatment effects, we would have one absolute score for the first participants (reference group). All other average scores, we would express as differences to the reference participant. This seems odd and, indeed, has two disadvantages: first, whom are we to select as the reference participant? The choice would be arbitrary, unless we wanted to compare brain sizes against the grey matter of Albert Einstein, perhaps. Second, most of the time the researcher is after the factor variation rather than differences between any two individuals, which is inconvenient to compute from treatment contrasts. 

The solution is to use a different contrast coding for random factors: *deviation contrasts* represent the individual effects as *difference ($\delta$) towards the population mean*. As the  population mean is represented by the respective fixed effect, we can compute the absolute individual predictions by adding the fixef effect to the random effect:


```r
tibble(mu_i = ranef(P_hf)$center + 
             fixef(P_hf)$center) %>% 
  ggplot(aes(x = mu_i)) +
  geom_histogram()
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-8-1.png" width="90%" />

Finally, we can assess the initial question: are individual differences a significant component of all variation in the experiment? Assessing the impact of variation is not as straight-forward as with fixed effects. Two useful heuristics are to compare group-level variation to the fixed effects estimate (Intercept) and against the standard error:


```r
P_hf %>% 
  filter(type %in% c("grpef", "disp", "fixef")) %>% 
  clu()
```



Table: (\#tab:unnamed-chunk-9)Estimates with 95% credibility limits

|parameter          |type  |fixef     |re_factor | center|  lower| upper|
|:------------------|:-----|:---------|:---------|------:|------:|-----:|
|b_Intercept        |fixef |Intercept |NA        |  15.96| 14.423| 17.46|
|sd_Part__Intercept |grpef |Intercept |Part      |   1.53|  0.079|  3.73|
|sigma              |disp  |NA        |NA        |  16.39| 15.488| 17.39|

```r
detach(IPump)
```

The variation due to individual differences is an order of magnitude smaller than the Intercept, as well as the standard error. This lets us conclude that the novel interface works pretty much the same for every participant. If we are looking for relevant sources of variation, we have to look elsewhere. (As we have seen in \@ref(ofm), the main source of variation is learning.)

<!-- #### EATME -->

<!-- The variable `Part` is central for the model aboveA participant is just a group of observations, and fomrally, they there is one apparent, one practical and one subtle difference compared to factors as we know them so far. The apparent difference is that before we had just very few levels and many observations. With participants we would have to estimate dozens or hundreds of coefficients. In consequence, the posterior distribution will become spread out like butter on a toast and certainty would become will be abysmal. The practical difference is that, while we are interested in the overall variation, the ability of individual users is rather uninteresting. We actually have no use for dozens of user ability scores. The subtle difference is that users form a population. That sounds rather obviuous than subtle, but is key for the solution once we understand what being *member of a population* means. I will give a brief account here and return to the topic in section \@ref(random_effects). -->

<!-- Imagine the following situation: you are seated in a university bistro and you get the task to guess the intelligence quotient of every person entering the bistro. After every trial you are disclosed  the real IQ of the person. You know that the average IQ is 100 and you give a bonus of 5 owing to the fact it is at a university. The first five persons have the IQs: -->

<!-- ```{r} -->
<!-- IQ <- c(106, 108, 103, 115, 110) -->
<!-- ``` -->

<!-- It seems the bonus of five is an understatement and the average is closer to 110. You adjust your best guess accordingly. That sounds trivial, but for the specification of a regression model it is not. The crucial point is that any guess $k$ depends on the information you received on trials $1...(k-1)$. Review the model formulation for comparison of means models. There is nothing in the linear term  that transfers information between levels of factors. The group means are estimated in complete independence.   -->

<!-- The key is is that participants are *members of a population*. In a reasonably large population, extremes such as an IQ of 160 will eventually happen, but they are very unlikely. By far most people, roughly two thirds, are clumped together in the range 85 to 115. Imagine you are travelling to an African country you have never heard of before. Africa  has the richest gene pool of all and ethnicies differ a lot in tallness and skin tone. But, after you have seen a few people, you get an impression of how tall and dark people are in this place, and there is no more surprise. The same principle holds for human attributes or capabilities in design research. Having seen a bunch of participants completing a task between two and five minutes gives a best guess for the next particiant. It is not excluded that this person will need 25 minutes or 20 seconds  for the task, but it is less likely. -->

<!-- Factors where the individual levels vary, but are more or less clumped around a population mean are best modelled as *random factors* and the individual levels are called *random effects*. Random effects enter the likelihood of the linear model specification in a similar additive way as population-level linear effects. The difference is that the levels are assumed to stem from a normal distribution, which represents the clumping. The mean of this distribution is zero, similar to the distribution of residuals, but the standard deviation of this distribution, representing the amount of variation in the population, is *estimated alongside* the individual levels.  -->



## Slope random effects: variance in change {#slope-re}

So far, we have dealt with Intercept random effects that capture the gross differences between participants of a sample. We introduced these random effects as conditional effects like: "average performance depends on what person you are looking at". However, most research questions rather regard differences between conditions. 

With *slope random effects* we can represent individual change in performance. 
For an illustration of slope random effects, we take a look at a data set that ships with package Lme4 (which provides a non-Bayesian engine for multi-level models). 18 participants underwent sleep deprivation on ten successive days and the average reaction time on a set of tests has been recorded per day and participant. The research question is: what is the effect of sleep deprivation on reaction time and, again, this question can be asked on population level and participant level.

The participant-level plot below shows the individual relationships between days of deprivation and reaction time. For most participants an upwards-pointing straight line seems to be a good approximation, so we can go with a straight linear regression model, rather than an ordered factor model. One noticeable exception is the curve of participant 352, which is fairly linear, but reaction times get shorter with sleep deprivation. (What would be the most likely explanation? Probably, 352 is a cheater, who slept secretly slept and improved by gaining experience with the task).


```r
attach(Sleepstudy)
```



```r
D_slpstd %>% 
  ggplot(aes(x = days, y = RT)) +
  facet_wrap(~Part) +
  geom_point() +
  geom_smooth(se = F, aes(color = "LOESS")) +
  geom_smooth(se = F, method = "lm", aes(color = "lm")) +
  labs(color = "Smoothing function")
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-12-1.png" width="90%" />

A more compact way of plotting multi-level slopes is the spaghetti plot below. By superimposing the population level effect, we can clearly see that participants vary in how sleep deprivation delays the reactions.


```r
D_slpstd %>% 
  ggplot(aes(x = days,
             y = RT,
             group = Part)) +
  geom_smooth(aes(color = "participant effects"), 
              size = .5, se = F, method = "lm")+
  geom_smooth(aes(group = 1, color = "population effect"), 
              size = 2, se = F, method = "lm") +
  labs(color = NULL)
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-13-1.png" width="90%" />

For a single level model, the formula would be `RT ~ 1 + days`, with the intercept being RT at day Zero and the coefficient `days` representing the change per day of sleep deprivation. The multi-level formula retains the population level and adds the participant-level term as a conditional statement: again, the effect depends on whom you are looking at.

`RT ~ 1 + days + (1 + days|Part)`

Remember to always put complex random effects into brackets, because the `+` operator has higher precedence than `|`. We estimate the multi-level model using the Rstanarm engine. 



```r
M_slpsty_1 <- brm(RT ~ 1 + days + (1 + days|Part),
                       data = D_slpstd,
                       iter = 2000)
```







Again, we could use the commands `fixef`, `ranef` and `grpef` to extract the parameters, but Bayr also provides a specialized command for multi-level tables. `fixef_ml` extracts the population-level estimates in CLU form and adds the participant-level standard deviation. The overall penalty for sleep deprivation is around ten milliseconds per day, with a 95% CI ranging from 7ms to 14ms. At the same time, the participant-level standard deviation is around 6.5ms, which is considerable. Based on the assumption that the central two standard deviations of a Gaussian distribution contain two-thirds of the total mass, we can expect that roughly one third of the population has a penalty of smaller than 4ms *or* larger than 17ms.



```r
fixef_ml(M_slpsty_1)
```



|fixef     | center|  lower| upper| SD_Part|
|:---------|------:|------:|-----:|-------:|
|Intercept |  251.2| 237.17| 265.8|    26.1|
|days      |   10.5|   7.01|  13.8|     6.4|

The following plot shows the slope random effects, ordered by the center estimate.


```r
ranef(M_slpsty_1) %>% 
  filter(fixef == "days") %>% 
  mutate(Part_ord = rank(center)) %>% 
  ggplot(aes(x = Part_ord, ymin = lower, y = center, ymax = upper)) +
  geom_crossbar()
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-17-1.png" width="90%" />

The multi-level regression model is mathematically specified as follows. Note how random coefficients $\beta_{.(Part)}$ are drawn from a Gaussian distribution with their own standard deviation, very similar to the errors $\epsilon_i$.


$$
\begin{aligned}
y_i &= \mu_i + \epsilon_i\\
\mu_i &= \beta_0 + \beta_{0(Part)} + x_1 \beta_1 + x_{1}\beta_{1(Part)}\\
\beta_{0(Part))} &\sim \textrm{Gaus}(0,\sigma_{0(Part)})\\
\beta_{1(Part))} &\sim \textrm{Gaus}(0,\sigma_{1(Part)})\\
\epsilon_i &= \textrm{Gaus}(0, \sigma_\epsilon)
\end{aligned}
$$

 The second line can also be written as:
 
 $$
 \mu_i = \beta_0 + \beta_{0(Part)} + x_1 (\beta_1 + \beta_{1(Part)})
 $$
 
which underlines that random coefficients are additive correction terms to the population-level effect. Whereas the `ranef` command reports only these corrections, it is sometimes useful to look at the total scores per participant. In package Bayr, the command `re_scores` computes total scores on the level of the posterior distribution. The following plot uses this command and plots the distribution.


```r
posterior(M_slpsty_1) %>% 
  re_scores() %>% 
  clu() %>% 
  ggplot(aes(x = center)) +
  facet_grid(~fixef, scales = "free") +
  geom_density()
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-18-1.png" width="90%" />






## Thinking multi-level {#thinking-multi-level}

There is a lot of confusion about the type of models that we deal with in this chapter. They have also been called hierarchical models or mixed effects models. The "mixed" stands for a mixture of so called fixed effects and random effects. The problem is: if you start by understanding what fixed effects and random effects are, confusion is programmed, not only because there exist several very different definitions. In fact, it does not matter so much whether an estimate is a fixed effect or random effect. As we will see, you can construct a multi-level model by using just plain descriptive summaries. What matters is that a model contains estimates on population level and on participant level. The benefit is, that a multi-level model can answer the same question for the population  as a whole and for every single participant.

For entering the world of multi-level modelling, we do not need fancy tools. More important is to start thinking multi-level on a familiar example: the IPump case. A novel syringe infusion pump design has been tested against a legacy design by letting trained nurses complete a series of eight tasks. Every nurse repeated the series three times on both designs. Time-on-task was measured  and the primary research question is:

>> Does the novel design lead to faster execution of tasks?



```r
attach(IPump)
```

To answer this question, we can compare the two group means using a basic CGM:



```r
M_cgm <-
  D_pumps %>% 
  stan_glm(ToT ~ 1 + Design, data = .)
```




```r
fixef(M_cgm)
```



Table: (\#tab:unnamed-chunk-23)Estimates with 95% credibility limits

|fixef       | center| lower| upper|
|:-----------|------:|-----:|-----:|
|Intercept   |   27.9|  25.9| 29.76|
|DesignNovel |  -11.8| -14.5| -9.17|


This model is a single-level model. It takes all observations as "from the population" and estimates the means in both groups. It further predicts that with this population of users, the novel design is faster *on average*, that means taking the whole population into account, (and forgetting about individuals).

An average benefit  sounds promising, but we should be clear what it precisely means, or better what it does not mean: That there is a benefit for the population does not imply, that every individual user has precisely that benefit. It does not even imply that every user has a benefit at all. In extreme case,  a small subgroup could be negatively affected by the novel design, but this could still result in a positive result on  average. In the evaluation of high-risk devices like infusion pumps concerns about individual performance are real and this is why we designed the study with within-subject conditions, which allows to estimate the same model on population level and participant level. The following code produces a *multi-level descriptive model*. First, a summary on participant level is calculated, then it is summarized to obtain the population level. By putting  both summaries into one figure, we are doing a multi-level analysis. 



```r
T_Part <-
  D_pumps %>% 
  group_by(Part, Design) %>% 
  summarize(mean_Part = mean(ToT))

T_Pop  <-
  T_Part %>% 
  group_by(Design) %>% 
  summarize(mean_Pop = mean(mean_Part))
```



```r
gridExtra::grid.arrange(nrow = 2,
             T_Pop %>% 
  ggplot(aes(x = Design, group = NA,
             y = mean_Pop)) +
  geom_point() +
  geom_line() +
  ggtitle("Population-level model (average benefits)") +
  ylim(0, 60),
 T_Part %>% 
  ggplot(aes(x = Design,
             y = mean_Part,
             group = Part, label = Part)) +
  geom_line() +
  ggrepel::geom_label_repel(size = 3, alpha = .5) +
  ggtitle("Participant-level model (individual benefits)") +
  ylim(0, 60))
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-25-1.png" width="90%" />

Note

* how with `gridExtra::grid.arrange()` we can multiple plots into a grid, which is more flexible than using facetting
* that `ggrepel::geom_label_repel` produces non-overlapping labels in plots


This is a full multi-level analysis, as it shows the same effect on two different levels *alongside*. In this case, the participant-level part removes all worries about the novel design. With the one small exception of participant 3, all users had net benefit from using the novel design and we can call the novel design  universally better. In addition, some users (4, 8 and 9) seem to have experienced catastrophes with the legacy design, but their difficulties disappear when they switch to the novel design.

If you look again at the participant-level *spaghetti plot* and find it similar to what you have seen before, you are right: This is an design-by-participant *conditional plot*. Recall, that conditional effects represent the change of outcome, depending on another factor. In this multi-level model, this second factor simply Part(icipant). That suggests that it is  well within reach of plain linear models to estimate design-by-participant conditional effects. Just for the purpose of demonstration, we can estimate a population level model, conditioning the design effect on participants. Ideally, we would use a parametrization giving us separate Intercept and DesignNovel effects per participant, but the formula interface is not flexible enough and we would have to work with dummy variable expansion. Since this is just a demonstration before we move on to the multi-level formula extensions, I use an AMM instead. A plain linear model can only hold one level at a time, which is why we have to estimate the two separate models for population and participant levels. Then we merge the posterior objects, produce a combined CLU table for plotting.




```r
M_amm_pop <-
  D_pumps %>% 
  stan_glm(ToT ~ 0 + Design, data = .)

M_amm_part <-
  D_pumps %>% 
  stan_glm(ToT ~ (0 + Design):Part, data = .)
```



```r
T_amm <-
  bind_rows(posterior(M_amm_pop),
            posterior(M_amm_part)) %>% 
  fixef() %>% 
  separate(fixef, into = c("Design", "Part"))


T_amm %>% 
  ggplot(aes(x = Design, y = center, group = Part, color = model)) +
  geom_line()
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-27-1.png" width="90%" />

In the first place, the convenience of (true) multi-level models is that  both (or more) levels are specified and estimated as one model. For the multi-level models that follow, we will use a specialized engine, `brm()` (generalized multi-level regression) that estimates both levels simultaneously and produce multi-level coefficients. The multi-level CGM we desire is written like this:


```r
M_mlcgm <-
  D_pumps %>% 
  brm(ToT ~ 1 + Design + (1 + Design|Part), data = .)
```

In the formula of this multi-level CGM the predictor term (`1 + Design`) is just copied. The first instance is the usual population-level averages, but the second is on participant-level. The `|` operator in probability theory means "conditional upon" and here this can be read as *effect of Design conditional on participant*.

For linear models we have been using the `coef()` command to extract all coefficients. Here it would extract all coefficients on both levels. With multi-level models, two specialized command exist to separate the levels:  we can extract population-level effects using the `fixef()` command (for "fixef effects"). All lower level effects can be accessed with the `ranef` command, which stands for *random effects*. Here, the population level coefficients are absolute means, whereas random effects are *not*. Usually, random effects are *differences towards the population-level*. This is why random effects are always *centered at zero*. In the following histogram, the distribution of the DesignNovel random effects are shown. This is how much users deviate from the average effect in the population.


```r
fixef(M_mlcgm)
```



Table: (\#tab:unnamed-chunk-29)Estimates with 95% credibility limits

|fixef       | center| lower| upper|
|:-----------|------:|-----:|-----:|
|Intercept   |   27.6|  23.7| 31.54|
|DesignNovel |  -11.7| -15.3| -8.05|

```r
ranef(M_mlcgm) %>%
  rename(Part = re_entity, `deviation` = center) %>% 
  ggplot(aes(x = deviation)) +
  facet_grid(~fixef) +
  geom_histogram()
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-29-1.png" width="90%" />

The distribution of random effects should resemble a *Gaussian distribution*. It is usually hard to tell with such small sample sizes, but it seems that the Intercept effects have a left skew. As we will see in chapter \@ref(glm), this problem is not surprising and can be resolved. The distributions are also centered at zero, which is not a coincidence, but the way random effects are designed: deviations from the population mean. That opens up two interesting perspectives: first, random effects look a lot like residuals \@ref(likelihood-random-term), and like those we can summarize a random effects vector by its *standard deviation*, using the `grpef`  command from package Bayr.


```r
bayr::fixef_ml(M_mlcgm)
```



|fixef       | center| lower| upper| SD_Part|
|:-----------|------:|-----:|-----:|-------:|
|Intercept   |   27.6|  23.7| 31.54|    8.71|
|DesignNovel |  -11.7| -15.3| -8.05|    6.02|

Most design research is located on the population level. We want to know how a design works, broadly. Sometimes, stratified samples are used to look for conditional effects in  (still broad) subgroups. Reporting individual differences makes little sense in such situations. The standard deviation summarizes individual differences and can be interpreted the *degree of diversity*. The command `bayr::fixef_ml` is implementing this by simply attaching the standard deviation center estimates to the respective population-level effect. As coefficients and standard deviations are on the same scale, they can be compared. Roughly speaking, a two-thirds of the population is contained in an interval *twice as large* as the SD.


```r
fixef_ml(M_mlcgm)
```



|fixef       | center| lower| upper| SD_Part|
|:-----------|------:|-----:|-----:|-------:|
|Intercept   |   27.6|  23.7| 31.54|    8.71|
|DesignNovel |  -11.7| -15.3| -8.05|    6.02|


That having said, I believe that more researchers should watch their participant levels more closely.  Later, we will look at two specific situations: psychometric models have the purpose of measuring individuals \@ref(psychometrics) and those who propose universal theories (i.e., about people *per se*) must also show that their predictions hold for each and everyone \@ref(universality).





```r
detach(IPump)
```



















<!-- The population-level coefficients represent the  -->



<!-- The random factor variation is extracted from the posterior distribution using the `grpef` command. The coefficients are standard deviations of the two random factors, Intercept and DesignB. The results confirm the earlier fixed effects analysis on the data set: the benefits of Design B varies a lot between users. When comparing the standard deviation with the the population-level effect, it seems that some users are experiencing a disadvantage, even. That matches the earlier results, where design B caused problems for elderly users. In fact, we can phrase the slope random effect in much the same way as conditional effect `age:Design` in \@ref(differential_design_effects), namely as a conditional statement of the form: -->

<!-- > the effect of Design depends on the participant.  -->


<!-- ```{r results = "tab"} -->

<!-- attach(BrowsingAB) -->

<!-- P_slope_1 %>%  -->
<!--   filter(fixef %in% c("Intercept", "DesignB"), -->
<!--          type %in% c("grpef", "fixef")) %>%  -->
<!--   group_by(re_factor, fixef) %>%  -->
<!--   clu() -->


<!-- detach(BrowsingAB) -->

<!-- ``` -->


<!-- Intercept random effects capture gross variations between users. Independent of designs, tasks or situation, some users can be assumed to always be faster, more accurate or pleased. That is certainly better than sticking with the image of the average user, but it remains static. With slope random effects can we render *individual differences in the process*. How differently do individual users react when proceeding from design A to B? That is a completely different question than the average difference between designs. Review the spaghetti plot \@ref(fig:BAB_RE_age) once again. Adding a slope random effect to the model really means that the effect is estimated as many times as there are participants in the sample (run `ranef(M_slope_1)` to see it). And these coefficients tell a story of *universality*. The more the random effects are dispersed, the less typical is the average (the fixed effect). Whether you are doing practical design research or  experimental design studies, universality matters.  -->

<!-- Assume an A/B study tested five participants with one task each.   -->

<!-- #### MAKE Sleep_study -->

<!-- #### REWORK ALL FORMULAS -->

<!-- The following simulation function creates a multi-level data set by sampling the Intercept effect and the Design effect from a Gaussian distribution. Per default arguments, participants differ a lot ($\sigma_{0P}$) The figure below illustrates the model on a sample of five participants. As the effect is the same, we see five parallel lines. While the height where a line starts differs between users, they all benefit to the precisely same amount from design B.Is that realistic? If we expect that differential design effects exist, it is not. There can be plenty of reasons why two users differ in how much the benefit from a certain design. For example, the BrowsingAB case features a strong conditional effect on age. When thinking of random effects, it helps to imagine the situation where an influencing factor exists, but was not recorded. The inter-individual variance will remain, and the participant identifier is the only predictor at hand. -->

<!-- ```{r} -->
<!-- sim <- function(n_Part = 5, -->
<!--                 beta_0P = rnorm(n_Part,0, sigma_0P), -->
<!--                 sigma_0P = 20, -->
<!--                 beta_1P = rnorm(n_Part,0, sigma_1P), -->
<!--                 sigma_1P = 0, -->
<!--                 sigma_eps = 0) { -->
<!--   Part = tibble(Part = 1:n_Part,  -->
<!--                      beta_0P = beta_0P,  -->
<!--                      beta_1P = beta_1P) -->
<!--   Design = tibble(Design = c("A", "B"), -->
<!--                        beta_0 = 120, -->
<!--                        beta_1 = -30) -->
<!--   mascutils::expand_grid(Part = Part$Part, Design = Design$Design) %>%  -->
<!--     left_join(Part) %>%  -->
<!--     left_join(Design) %>%  -->
<!--     mutate(mu = beta_0 + beta_0P + (beta_1 + beta_1P) * (Design == "B"), -->
<!--            ToT = rnorm(nrow(.), mu, 0)) %>%  -->
<!--     as_tbl_obs() -->
<!-- } -->

<!-- sim() %>%  -->
<!--   ggplot(aes(x = Design,  -->
<!--              y = ToT, -->
<!--              group = Part)) + -->
<!--   geom_line() -->


<!-- ``` -->



<!-- A more realistic figure of the effects is the spaghetti plot below. All but one of the participants improve with design B, but to rather different degrees. The lines have different starting points, and they differ in slopes. This is called a *slope random effect*. Similar to intercept random effects, slope random effects are factors that represent individual deviations from the population-level effect.  -->

<!-- ```{r} -->
<!-- sim(sigma_1P = 15) %>%  -->
<!--   ggplot(aes(x = Design,  -->
<!--              y = ToT, -->
<!--              group = Part)) + -->
<!--   geom_line() -->

<!-- ``` -->

<!-- For the design effect in BrowsingAB, a model with intercept and slope random effects is formally specified as: -->

<!-- $$ -->
<!-- \mu_i = \beta_0 + \beta_{1P} + x_1\beta_1 + x_{1}\beta_{1p}\\ -->
<!-- \beta_{1P} \sim \textrm{Gaus}(0,\sigma_{1P})\\ -->
<!-- \beta_{1p} \sim \textrm{Gaus}(0,\sigma_{1P}) -->
<!-- $$ -->

<!-- Previously, we have seen that repeated measures are the key to pull a random effect out of the residual term. The same holds for slope random effects. For estimating individual slopes, the same participant (or any non-human entity) must encounter multiple conditions. For demonstration of slope random effects, we examine another instantiation of the BrowsingAB data set `BAB5`, which differs from `BAB1` in two aspects: all participants encounter both designs (within-entity design) and ToT was measured on five given tasks (repeated measures). We use the simulation function as provided by the BrowsingAB case environment. As always, we start with some exploratory analysis.  -->


<!-- ```{r} -->
<!-- attach(BrowsingAB) -->
<!-- ``` -->


<!-- The spaghetti plot (the pasta is dry, not cooked) is excellent for visualizing participant-level effects. For the BAB5 data set the following figure shows the individual design effects. While on average design B is favoured, there is a wild bunch of slopes. That should not surprise us, as in the previous chapter we discovered a strong conditional effect by age. Here we ignore this predictor (or pretend not to have recorded it) and exclusively work with the participant factor. -->

<!-- ```{r BAB_RE_age, opts.label = "fig.wide"} -->
<!-- G_slope_RE_1 <- -->
<!--   BAB5 %>%  -->
<!--   group_by(Part, Design) %>%  -->
<!--   summarize(ToT = mean(ToT)) %>%  -->
<!--   ggplot(aes(x = Design,  -->
<!--               y = ToT)) + -->
<!--   geom_line(aes(color = "participant effects", group = Part), alpha = .7) + -->
<!--   geom_line(data = group_by(BAB5, Design) %>% summarize(ToT = mean(ToT)),  -->
<!--             aes(X = Design, y = ToT, group = 1, color = "population effect"), size = 1) + -->
<!--   labs(color = NULL) -->

<!-- G_slope_RE_1 -->

<!-- ``` -->




## Testing universality of theories {#universality}

Often, the applied researcher is primarily interested in a population-level effect, as this shows the *average* expected benefit. If you run a webshop, your returns are exchangeable. One customer lost can be compensated by gaining a new one. In such cases, it suffices to report the random effects standard deviation. If user performance varies strongly, this can readily be seen in this one number. 

In at least two research situations, going for the average is just not enough: when testing hazardous equipment and when testing theories. In safety critical research, such as a medical infusion pump, the rules are different than for a webshop. The rules are non-compensatory, as the benefit of extreme high performance on one patient cannot compensate the costs associated with a single fatal error on another patient. For this asymmetry, the design of such a system must enforce a *robust* performance, with  no catastrophes. The multi-level analysis of the infusion pumps in \@ref(thinking-multi-level) is an example. It demonstrated that practically all nurses will have a benefit from the novel design.

The other area where on-average is not enough, is theory-driven experimental research. Fundamental behavioural researchers are routinely putting together theories on The Human Mind and try to challenge these theories. For example the Uncanny Valley effect \@ref(prm): one social psychologist's theory could be that the Uncanny Valley effect is caused by religious belief, whereas a cognitive psychologist could suggest that the effect is caused by a category confusion on a fundamental processing level (seeing faces). Both theories make universal statements, about all human beings. *Universal statements* can never be proven, but can only be hardened. However, once a counter-example is found, the theory needs to be abandonded. If there is one participant who is provenly non-religious, but falls into the  Uncanny Valley, our social psychologist would be proven wrong. If there is a single participant in the world, who is robvust against the Uncanny Valley, the cognitive psychologist was wrong.

Obviously, this counter-evidence can only be found on participant level. In some way, the situation is analog to robustness. The logic of universal statements is that they are false if there is one participant who breaks the pattern, and there is no compensation possible. Unfortunately, the majority of fundamental behavioural researchers, have ignored this simple logic and still report population-level estimates when testing universal theories. In my opinion, all these studies should not be trusted, before a multi-level analysis shows that  the pattern exists on participant level.

In \@ref(prm), the Uncanny Valley effect has been demonstrated on population level. This is good enough, if we just want to confirm the Uncanny Valley effect as an observation, something that frequently 
happens, but not necessarily for everyone. The sample in our study consisted of mainly students and their closer social network. It is almost certain, that many of the tested persons were religious and others were atheists. If the religious-attitude theory is correct, we would expect to see the Uncanny Valley in several participants, but not in all. If the category confusion theory is correct, we would expect all participants to fall into the valley. The following model performs the polynomial analysis as before \@ref(prm), but multi-level:


```r
attach(Uncanny)
```



```r
M_poly_3_ml <-
  RK_1 %>% 
  brm(response ~ 1 + huMech1 + huMech2 + huMech3 + 
               (1 + huMech1 + huMech2 + huMech3|Part),
             data = ., iter = 2500)

P_poly_3_ml <- posterior(M_poly_3_ml)

PP_poly_3_ml <- post_pred(M_poly_3_ml, thin = 5)
```



One method for testing universality is to extract the fitted responses (`predict`)  and perform a visual examination: can we see a valley for every participant? 




```r
T_pred <- 
  RK_1 %>% 
  mutate(M_poly_3_ml = predict(PP_poly_3_ml)$center)

T_pred %>% 
  ggplot(aes(x = huMech, y = M_poly_3_ml, group = Part)) +
  geom_smooth(se = F, size = .5)
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-37-1.png" width="90%" />

This spaghetti plot broadly confirms, that all participants experience the Uncanny Valley. For a more detailed analysis, a facetted plot would be better suited, allowing to inspect the curves case-by-case. 

We proceed directly to a more formal method of testing universality: In \@ref(test-stat) we have seen how the posterior distributions of shoulder and trough can be first derived and then used to give a more definitive answer on the shape of the polynomial. It was argued that the unique pattern of the Uncanny Valley is to have a shoulder left of a trough. These two properties can be checked by identifying the stationary points. The proportion of MCMC iterations that fulfill these properties can is evidence that the effect exists. 

For testing universality of the effect, we just have to run the same analysis on participant-level. Since the participant-level effects are deviations from the population-level effect, we first have to add the population level effect to the random effects (using the Bayr command `re_scores`), which creates absolute polynomial coefficients. The two command `trough` and `shoulder` from package Uncanny require a matrix of coefficients, which is done by spreading out the posterior distribution table.




```r
devtools::install_github("schmettow/uncanny")

library(uncanny)

T_univ_uncanny <- 
  P_poly_3_ml %>% 
  re_scores() %>% 
  select(iter, Part = re_entity, fixef, value) %>% 
  tidyr::spread(key = "fixef", value = "value") %>% 
  select(iter, Part, huMech0 = Intercept, huMech1:huMech3) %>% 
  mutate(trough = trough(select(.,huMech0:huMech3)),
         shoulder = shoulder(select(.,huMech0:huMech3)),
         has_trough = !is.na(trough),
         has_shoulder = !is.na(shoulder), 
         shoulder_left = trough > shoulder,
         is_uncanny = has_trough & has_shoulder & shoulder_left) 
```






```r
T_univ_uncanny %>% 
  group_by(Part) %>% 
  summarize(prob_uncanny = mean(is_uncanny),
            prob_trough = mean(has_trough),
            prob_shoulder = mean(has_shoulder)) %>% 
  mutate(label = str_c(100 * round(prob_uncanny, 4), "%")) %>%
  ggplot(aes(x = Part, y = prob_uncanny)) +
  geom_col() +
  geom_label(aes(label = label)) +
  theme(axis.text.x = element_text(angle = 45))
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-40-1.png" width="90%" />

The above plot shows the probability that a participant experiences the Uncanny Valley, as defined by polynomial stationary points. Every single MCMC step is has a shoulder and a trough. This complete absence of counter-evidence may raise suspicions. If this is all based on a random walk, we should at least see a few deviations. The reason for that is simply that the number of MCMC runs puts a limit on the resolution. If we increase the number of iterations enough, we would eventually see few "deviant" samples appear and measure the tiny chance that a participant does not fall for the Uncanny Valley.



```r
detach(Uncanny)
```


This is great news for all scientists who believe that the Uncanny Valley effect is an innate cognitive phenomenon (rather than cultural), it does not demonstrate the identification of deviant participants. We briefly re-visit case Sleepstudy, for which we have estimated a multi-level linear regression model to render individual increase of reaction time as result of sleep deprivation (\@ref(slope-re)). By visual inspection, we identified a single deviant participant who showed an improvement over time. However, the fitted lines a based on point estimates, only (the median of the posterior). Using the same technique as above, it is possible to calculate the participant-level probabilities for the slope being positive, as expected.


```r
attach(Sleepstudy)
```



```r
P_scores <- 
  posterior(M_slpsty_1) %>% 
  re_scores() %>% 
  mutate(Part = re_entity)

P_scores %>% 
  filter(fixef == "days") %>% 
  group_by(Part) %>% 
  summarize(prob_positive = mean(value >= 0)) %>% 
  mutate(label = str_c(100 * round(prob_positive, 4), "%")) %>% 
  ggplot(aes(x = Part, y = prob_positive)) +
  geom_col() +
  geom_label(aes(label = label), vjust = 1) +
  theme(axis.text.x = element_text(angle = 45))
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-43-1.png" width="90%" />

All, but participants 309 and 335, almost certainly have positive slopes. Participant 335 we had identified earlier by visual inspection. Now, that we account for the full posterior distribution, it seems a little less suspicious. Basically, the model is almost completely indecisive whether this is a positive and negative effect. The following plot shows  all the possible slopes the MCMC random walk has explored. While participant 335 clearly is an outlier, there is no reason to get too excited and call  him or her a true counter-example from the rule that sleep deprivation reduces performance.


```r
P_scores %>%
  filter(Part == 335, fixef == "days") %>% 
  ggplot() +
  xlim(0.5, 9) +
  ylim(-50, 50) +
  geom_abline(aes(intercept = 0, slope = value), 
              alpha = .01)
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-44-1.png" width="90%" />


```r
detach(Sleepstudy)
```


In turn, the method of posterior-based test statistics can also be used for *analysis of existence*. In the Sleepstudy case a hypothetical question of existence would be that there exist persons who are completely insensitive to sleep deprivation. Why not? Recently, I saw a documentary about a guy who could touch charged electric wires, because due to a rare genetic deviation, his skin had no sweat glands. Universal statements can only be falsified by a counter-example. Statements of existence can be proven by just a single case. For example, in the 1980  dyslexia became more widely recognized as a defined condition. Many parents finally got an explanation for the problems their kids experienced in school. Many teachers complained that many parents would just seek cheap excuses for their lesser gifted offspring. And some people argued that dyslexia does not exist and that the disability to read is just a manifestation of lower intelligence. According to the logic of existence, a single person with an otherwise well functioning intellect, but slow reading suffices to proof the existence of  dyslexia. These people have been found in the meantime.




<!-- Robustness and universality can be examined by looking at participant-level estimates. In \@ref(thinking-multilevel) we compared two infusion pump designs using a multi-level model. The participant-level results showed that  -->


<!-- #### HERE -->

<!-- The question of universality is also relevant for safety-critical systems, because they have a non-linear loss function:  In  section [thinking-multi-level] we already encountered such a case: a novel infusion pump interface was compared with a legacy design. In this study, all participants encountered both designs, so we can estimate all effects on participant level and check universality of direction. Is the novel design better for everyone? -->

<!-- At the same time, we studied learnability in this experiment by tracking performance across three sessions with the same set of tasks. For learnability, it makes not so much sense to examine unversality. Learning by repetition is so common, that no one would be surprised in learning that every single participant got better with practice. -->

<!-- The following model specifies the effects of design and practice on cognitive workload. This is simultaneously estimated on population and participant level. In order to make the interpretation of coefficients and their variance easier to interpret, we are estimating a multi-level model with absolute group means for the first session per design and . The factor Session, we leave as successive difference coding [contrasts]. -->

<!-- #### HERE -->


<!-- ```{r} -->
<!-- attach(IPump) -->
<!-- ``` -->

<!-- ```{r opts.label = "mcmc"} -->
<!-- M_wkld <-    -->
<!--   D_pumps %>%   -->
<!--   brm(workload ~  -->
<!--                 0 + Design:Session + Design +  -->
<!--                (0 + Design:Session + Design|Part), -->
<!--              data = ., -->
<!--              iter = 200) -->
<!-- ``` -->

<!-- ```{r opts.label = "mcsync"} -->
<!-- sync_CE(IPump,M_wkld) -->
<!-- ``` -->

<!-- In  order to help the reader interpreting the magnitude of effects and inspect variation of that effect, a useful form of presenting multi-level results is to attach the random factor standard deviations to the coefficient table. The following function creates such a table. It first extracts the SD center estimates and puts them into as many columns as there are factor levels (Part, Task). The result is joint with the population-level coefficients. -->


<!-- ```{r} -->
<!-- fixef_ml <- function(model, ...){ -->
<!--   T_grpef <-  -->
<!--     grpef(model, ...) %>%  -->
<!--     select(fixef, re_factor, SD = center) %>%  -->
<!--     mutate(re_factor = str_c("SD_", re_factor)) %>%  -->
<!--     spread(re_factor, SD) -->

<!--   fixef(model, ...)  %>% -->
<!--     left_join(T_grpef, by = "fixef") %>%  -->
<!--     discard_redundant() -->
<!-- } -->

<!-- ``` -->


<!-- ```{r} -->
<!-- T_fixef_ml <- fixef_ml(M_wkld) -->
<!-- T_fixef_ml -->

<!-- T_fixef_ml %>%  -->
<!--   ggplot(aes(x = Session, -->
<!--              y = center, -->
<!--              color = Design)) -->
<!-- ``` -->

<!-- This shows that participants vary strongly in the levels of mental workload they reported. The Part-level SD for the Intercept is   -->



<!-- ```{r} -->
<!-- detach(IPump) -->
<!-- ``` -->



<!-- #### Exercises -->

<!-- 1. Review the scientific literature in your field. Find examples of design experiments where general conclusions were drawn from between-subject predictors. If that was easy, visit one of the leading social psychology journals and repeat. -->



## Non-human populations and cross-overs {#non-human-populations}

With multi-level models design researchers can examine how a design affects the population of users as a whole, as well as on individual level. If there is little variation between users, it can be concluded that the effect is uniform in the population of users. In this section we will generalize the term *population* and extend the application of multi-level modeling to other types of research entities, such as designs, questionnaire items and tasks.

<!-- In the  BrowsingAB case, this gives valuable information on how well this particular design serves the population of users and whether one should be satisfied with it or not. This is a typical research question in applied design research where one seeks to choose one from a small set of designs. Generally, A/B studies seek to answer the question: *is one design better for the user population?*. This question has two entities: the user and the design. However, it is not symmetric, as users are provided as a sample from which one wishes to generalize, whereas the two designs are exhaustive. There is the choice between the two and they are both present. -->

Many studies in, what one could call *fundamental design research* seek to uncover general laws of design that may guide future system development. Fundamental design research is not concerned with choosing between individual designs, whether they are good enough or not, but with separating the population of possible designs into good ones and bad ones by universal statements, such as "For informational websites, broad navigation structures are better than deep ones".  Note how this statement speaks of designs (not users) in an unspecified plural. It is framed as a universal law for the population of designs.

Comparative evaluation studies, such as the IPump case, are not adequate to answer such questions, simply because you cannot generalize to the population from a sample of two. This is only possible under strict constraints, namely that the two designs under investigation only differ in one design property. For example two versions of a website present the same web content in a deep versus a wide hierarchy, but layout, functionality are held constant. And even then, we should be very careful with conclusions, because there can be interaction effects. For example, the rules could be different for a website used by expert users.

If the research question is universal, i.e. aiming at general conclusions on all designs (of a class), it is inevitable to see designs as a population from which we collect a sample. The term population suggests a larger set of entities, and in fact many application domains have an abundance of existing designs and a universe of possible designs. Just to name a few: there exist dozens of note taking apps for mobile devices and hundreds of different jump'n run games. Several classes of websites count in the ten thousands, such as webshops, municipal websites or intranets.

We can define classes of objects in any way we want, but the term population, has a stronger meaning than just a collection. A population contains individuals of the same kind and these individuals vary, but only to some extent. At the same time, it is implied that we can identify some sort of typical value for a population, such that most individuals are clumped around this typical value. Essentially, if it looks similar to one of the basic statistical distributions \@ref(distributions), we can call it a population. 

To illustrate that not every class is a population, take vehicles. Vehicles are a class of objects that transport people or goods. This broad definition covers many types of vehicles, including bicycles, rikshas, cars, buses, trucks and container vessels. If the attribut under question is the weight,  we will see a distribution spreading from a 10 kilograms up to 100 tons (without freight). That is a a scale of 1:10.000 and the distribution would spread out like butter on a warm toast. Formally, we can calculate the average weight of a all vehicles, but that would in no way represent a typical value.



<!-- Even in highly standardized domains such as automotive cockpits the differences between generations, manufacturers and models are noticable to be called individuals from a population. That is precisely the idea of a random effect: entities vary around a clump of commonalities. In consequence, when testing a fundamental design rule, like *large fonts are better to read*, the proposed research strategy is to sample from the population of websites, classify sample members by font size, and test every design with a sample of users. Given our definition of random effects it is easy to see that the regression model will contain two random effects (at least), one across users, the other across websites. More on that later. -->

In design research the most compelling populations are *users* and *designs*. Besides that research objects exist that we can also call members of a population, such as *tasks*, *situations* and *questionnaire items*.

*Tasks:* Modern informational websites contain thousands of information pieces and finding every one of those can be considered a task. At the same time, it is impossible to cover them all in one study, such that sampling a few is inevitable.  We will never be able to tell the performance of every task, but using random effects it is possible to estimate the variance of tasks. Is that valuable information? Probably it is in many cases, as we would not easily accept a design that prioritizes on a few items at the expense of many others, which are extremely hard to find.

*Situations:* With the emerge of the web, practically everyone started using computers to find information. With the more recent breakthrough in mobile computing, everyone is doing everything using a computer in almost every situation. People chat, listen to music and play games during meals, classes and while watching television. They let themselves being lulled into sleep, which is tracked and interrupted by smart alarms. Smartphones are being used on trains, while driving cars and bikes, however dangerous that might be, and not even the most private situations are spared. If you want to evaluate the usability of any mobile messaging, navigation and e-commerce app, you should test it in all these situations. A study to examine performance across situations needs a sample from a population of situations.

*Questionnaire items:* Most design researchers cannot refrain from using questionnaires to evaluate certain elucive aspects of their designs. A well constructed rating scale consists of a set of items that trigger similar responses. At the same time, it is desireable that items are unsimilar to a degree, as that establishes good discrimination across a wide range. In ability tests, for example to assess people's intelligence or math skills, test items are constructed to vary in difficulty. The more ability a person has, the more likely will a very difficult item be solved correctly. In design research, rating scales cover concepts such as perceived mental workload, perceived usability, beauty or trustworthiness. Items of such scales differ in how extreme the proposition is, like the following three items that could belong to a scale for aesthetic perception:

1. The design is not particularly ugly.
2. The design is pretty.
3. The design is a piece of art.

For any design it is rather difficult to get a positive response on item 3, whereas item 1 is a small hurdle. So, if one thinks of all possible propositions about beauty, any scale is composed of a sample from the population of beauty propositions.




If we look beyond design research, an abundance of non-human populations can be found in other research disciplines, such as:

+ products in consumer research
+ phonemes in psycholinguistics
+ test items in psychometrics
+ pictures of faces in face recognition research
+ patches of land in agricultural studies
+ households in socio-economic studies

In all these cases it is useful (if not inevitable) to ask multi-level questions not just on the human population, but on the encounter of multiple populations. In research on a single or few designs, such as in A/B testing, designs are usually thought of (and modeled) as common fixed-effects factors. However, when the research question is more fundamental, such that it regards a whole class of designs, it is more useful to think of designs as a population and draw a sample. In the next section we will see an example, where a sample of users encounters a sample of designs and tasks.

In experimental design research, the research question often regards a whole class of designs and it is almost inevitableto view designs as a population. As we usually want to generalize across users, that is another sample. A basic experimental setup would be to have every user rate (or do a task) on every design, which is called a complete (experimental) design, but I prefer to think of it as a complete *encounter*. 

Every measure is an encounter of one participant and one design. But, if a multi-item rating scale is used,  measures are an encounter between three populations. Every measure combines the impact from three members from these populations. With a single measure, the impact factors are inseparable. But if we have repeated measures on all populations, we can apply a *cross-classified multi-level model* (CRMM). An intercept-only CRMM just adds intercept random effects for every population. 

In the following case Egan, the question is a comparison of diversity across populations. Three decades ago, [@Egan1988h] published one of the first papers on individual differences in computer systems design and made the following claim:

> ‘differences among people usually account for much more variability in performance than differences in system designs’

What is interesting about this research question is that it does not speak about effects, but about *variability of effects* and seeks to compare variability of two totally different populations. In the following we will see how this claim can be tested by measuring multiple encounters between populations

Egan's claim has been cited in many papers that regarded individual differences and we were wondering how it would turn out in the third millennium, with the probably most abundant of all application domains: informational websites. For the convenience, we chose the user population of student users, with ten university websites as our design sample. Furthermore, ten representative tasks on such websites were selected, which is another population. During the experiment, all 41 participants completed 10 information search items such as:

> On website [utwente.nl] find the [program schedule Biology].

<!-- #### REDUCE -->

<!-- As this is a real study, there is some inconvenience ahead. Recall that the distribution of residuals is assumed to be Gaussian in linear regression models. Unfortunately, that is not how the noise of time-on-task measures and other "temporal" variables is frequently shaped. The cheap trick of *log transformation* often serves the purpose when not much is at stake. A more disciplined approach to work with time variables is given in \@ref(duration-measures).  -->



```r
attach(Egan)
```



```r
D_egan$logToT <- log(D_egan$ToT)
```

*Note* that ToT is log-transformed for compliance with the assumptions of Linear Models. Generally, the advice is to use a Generalized Linear Model instead \@ref(exgauss-reg).

Egan's claim is a two-way encounter to which we added the tasks, which makes it three-way. However, our data seemed to require a fourth random effect, which essentially is a conditional effect between tasks and websites, which we call an item. The following grid of histogram shows the marginal distributions of human and non-human populations. The individual plots were created using the following code template:


```r
  D_egan %>% 
    group_by(Part) %>%
    summarize(avg_logToT = mean(logToT)) %>% 
    ggplot(aes(x = avg_logToT)) +
    geom_histogram() +
    labs(title = "participant-level average log-times") +
    xlim(1.5,6.5)
```



<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-50-1.png" width="90%" />


There seems to be substantial variation between participants, tasks and items, but very little variation in designs. We build a GMM for the encounter of the four populations.

<!-- #### FORMULAS -->


<!-- $$ -->
<!-- \mu_i = \beta_0 + x_{Pi}\beta_{P} + x_{Di}\beta_{D} + x_{Ti}\beta_{T} + x_{D:Ti}\beta_{D:T}\\ -->
<!-- \beta_{P} \sim \textrm{Gaus}(0, \sigma_{P}) \\ -->
<!-- \beta_{D} \sim \textrm{Gaus}(0, \sigma_{D}) \\ -->
<!-- \beta_{T} \sim \textrm{Gaus}(0, \sigma_{T}) \\ -->
<!-- \beta_{DxT} \sim \textrm{Gaus}(0, \sigma_{DxT})\\ -->
<!-- y_i \sim \textrm{Gaus}(\mu, \sigma_\epsilon) -->
<!-- $$ -->




```r
M_1 <-  
  brm(logToT ~ 1 + 
        (1|Part) + (1|Design) + (1|Task) + (1|Design:Task),
             data = D_egan)

P_1 <-  posterior(M_1)
```




A Bayesian multi-level model estimates the standard deviation alongside with  coefficients, such that we can compare magnitude and certainty of variability. In addition, we can always compare a random factor standard deviation to the standard error.



```r
library(mascutils) # reorder-levels

P_1 %>% 
  filter(type == "grpef"  | type == "disp") %>% 
  mutate(re_factor = if_else(type == "disp", 
                             "Obs", re_factor),
         re_factor = reorder_levels(re_factor, 
                                    c(4, 2, 5, 3, 1))) %>% 
  ggplot(aes(x = value)) +
  geom_density() +
  labs(x = "random effect standard deviation") +
  facet_grid(re_factor~.)
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-52-1.png" width="90%" />

<!-- Before we formally evaluate Egan's claim, there are a few noteworthy remarks on how to read the posteriors, when the parameter is a group-level standard deviation. First, standard deviation is on the same scale as the measured variable. Usually, this makes standard deviations  rather intuitive to interpret. This is, *unless* you have obscured your variable by log transformation, which is the major disadvantage of this procedure.  -->

<!-- Second, the posterior graph is too easily misinterpreted by confusing the posterior distribution for the group-level distribution. Instead, the amount of variation in a random factor is reflected by the location on the x-axis, while the spread is uncertainty. The population of designs seems to have the lowest variation of all, with users being close second. The observation-level variation (Obs) is nothing but the standard error, $\sigma_\epsilon$. The observation-level variation sometimes serves well as a reference point to make quantitative statements on variation.  -->


The outcomes of our study are indecisive regarding  Egan's claim. Variance of participants is stronger than variance of designs, but the posterior distributions overlap a good deal. Both factors also produce much less variability in measures than does the remaining noise. Tasks seem to have the overall strongest effect, but this comes with huge uncertainty. The strongest variability is found in the sample of items (Design x Task), which is an interesting observation. How easy a task is, largely depends on the website where it is carried out. That makes sense, as all pieces of information somehow compete for space. For example, one university website could present the library  on the homepage, whereas another websites hides it deep in its navigation structure.

A secondary observation on the posterior plot is that some effects are rather certain, such as Obs and Design:Task, whereas others are extremely uncertain, especially Task. There is a partial explaination for this: the variation is estimated from the "heads" in thehuman or non-human population. It therefore strongly depends on respective sample size. Design and Task have a meager $N = 10$, which is why the estimate are so uncertain. With $N = 41$ the participant level estimate has more data and reaches better certainty, same for the pairs ($N = 100$). The observations level can employ to all 410 observations, resulting in a highly certain estimate.


<!-- All conditional effects are potential assaults on generalizability and so are items (`Design:Task`). Without this conditional effect, the model would assume that all ten tasks had the same relative difficulty regardless of the website. And the same goes for website. Given the countless possibilities to structure information on a website, this is a bold assumption. It were as if all ten information architects had previously agreed on how well to support any of the tasks. And, as the results show, there is substantial conditional variation, information architects do not fully agree. Regarding the interpretation, the question is: are we willing to add the *conditional* design effects under what Egan called "designs", or can only complete websites be called designs, with everything underneath being singular forces that amplify or erase each other? This depends on whether you are an optimist or realist designer. The realist thinks that design is a wicked problem science, where a good design is a bunch of reasonable compromises, but it is never the maximum. You may use large fonts to improve reading speed, but that comes at more scrolling time. You may choose to place an awesome video presentation of your university on its homepage, but you are using costly space you could use for other frequently used information. In such a view, a design is a set of carefully calibrated trade-offs and must therefore be taken as a whole.  -->

Still, Egan's claim is out in the world and requires an answer. To reduce the quantitative findings to a Yes/No variable, we use the same technique as in \@ref(test-stat) and \@ref(universality). What is the probability, that Egan is right? We create a Boolean variable and summarize the proportion of MCMC draws, where $\sigma_ \textrm{Part} > \sigma_ \textrm{Design}$ holds.


```r
P_1 %>% 
  filter(type == "grpef", re_factor %in% c("Part", "Design")) %>% 
  select(chain, iter, re_factor, value) %>% 
  spread(re_factor, value) %>% 
  summarize(prob_Egan_is_right = mean(Part > Design))
```



| prob_Egan_is_right|
|------------------:|
|              0.928|


Such a chance can count as good evidence in favor of Egan's claim, although it certainly does not match the "much more" in the original quote. However, if we take the strong and certain Item effect into account, the claim could even be reversed. Apparently, the difficulty of a task depends on the design, that means, it depends on where this particular designer has placed an item in the navigation structure. This is a clear design choice, having nothing to do with the user. Seen this way, our study provides strong counter-evidence for Egan's claim:


```r
P_1 %>% 
  filter(type == "grpef", re_factor %in% c("Part", "Design", "Design:Task")) %>% 
  select(chain, iter, re_factor, value) %>% 
  spread(re_factor, value) %>% 
  mutate(Egan_is_wrong = Part < Design | Part < `Design:Task`) %>% 
  summarize(prob_Egan_is_wrong = mean(Egan_is_wrong))
```



| prob_Egan_is_wrong|
|------------------:|
|                  1|






In this section we have seen that measures in design research happen in encounters between users, designs and several other non-human populations. Cross-classified random effects models capture these structures. When testing Egan's claim, we saw how an exotic hypothesis such as the difference in variance, can be answered probabilistically, because with Bayesian models, we get posterior distributions for all parameters in a model, not just coefficients.






## Nested random effects {#nested-re}

In some research designs, we have populations, where every member is itself a population. A classic example is from educational research: a sample of schools is drawn and inside every school a sample of students is selected. Like cross-classified models, nested models consist of multiple levels. The difference is that if one knows the lowest (or: a lower) level of an observation, the next higher level is unambiguous, like:

+ every student is in exactly one class
+ every participant is from exactly one professional group

*Nested random effects* (NRE) represent nested sampling schemes. As we have seen above, cross-classified models play an important role in design research, due to the user/task/design encounter. NREs are more common in research disciplines where organisation structures or geography plays a role, such as education science (think of the international school comparison studies PISA and TIMMS), organisational psychology or political science. 

One examples of a nested sampling structure in design research is the CUE8 study, which is the eighth instance of  Comparative Usability Evaluation (CUE) studies by Rolf Molich [@Molich2010d]. Different to what the name might suggest, not designs are under investigation in CUE, but usability professionals. The over-arching question in the CUE series is the performance and reliability of usability professionals when evaluating designs. Earlier studies sometimes came to devastating results regarding consistency across professional groups when it comes to identifying and reporting usability problems. The CUE8 study lowered the bar, by asking if professionals can at least measure time in a comparable way. 

The CUE8 study measured time-on-task in usability tests, which had been conducted by 14 different teams. The original research question was: How reliable are time-on-task measures across teams? All teams used the same website (a car rental company) and the same set of tasks. All teams did moderated or remote testing (or both) and recruited their own sample of participants. 

Hence, the analysis can performed on three levels: the population level would tell us the average performance on this website. That could be interesting for the company running it. Below that are the teams and their variation is what the research question is about.  Participants  make the third level for a nested multi-level model. It is nested, because every participant is assigned to exactly one team. If that weren't the case, say there is one sample of participants shared by the teams, that would be cross-classification.

As the original research question is on the consistency across teams, we can readily take the random effect variance as a measure for the opposite: when variance is high, consistency is low. But, how low is low? It is difficult to come up with an absolute standard for inter-team reliability. Because we also have the participant-level, we can resort to a relative standard: how does the variation between teams compare to variation between individual participants?

Under this perspective, we examine the data. This time, we have real time-on-task data and as so often, it is highly skewed. Again, we use the trick of logarithmic transformation to obtain a more symmetric distribution of residuals. The downside is that the outcome variable may not be zero. For time-on-task data this is not an issue. Before proceeding to the model, we explore the original variable `ToT` on the two levels (Participant and Team): In the following code the mean ToT is computed for the two levels of analysis, participants and teams and shown in ascending order.



```r
attach(CUE8)

D_cue8

D_part_mean <-
  D_cue8 %>% 
  group_by(Part, Condition) %>% 
  summarize(mean_ToT = mean(ToT), na.rm = T,
            n_Obs = n()) %>%   
  ungroup() %>% 
  rename(Unit = Part) %>% 
  mutate(percentile = percent_rank(mean_ToT),
         Level = "Part")

D_team_mean <-
  D_cue8 %>% 
  group_by(Team, Condition) %>% 
  summarize(mean_ToT = mean(ToT, na.rm = T),
            n_Obs = n()) %>%
  ungroup() %>% 
  rename(Unit = Team) %>% 
  mutate(percentile = percent_rank(mean_ToT),
         Level = "Team")

D_task_mean <-
  D_cue8 %>% 
  group_by(Task, Condition) %>% 
  summarize(mean_ToT = mean(ToT, na.rm = T),
            n_Obs = n()) %>%
  ungroup() %>% 
  rename(Unit = Task) %>% 
  mutate(percentile = percent_rank(mean_ToT),
         Level = "Task")


bind_rows(D_team_mean,
          D_part_mean,
          D_task_mean) %>%
  ggplot(aes(x = percentile, 
             y = mean_ToT,
             col = Level,
             shape = Level)) + 
  geom_point()
```

<img src="Linear_mixed-effects_models_files/figure-epub3/CUE8_eda-1.png" width="90%" />

It seems there is ample variation in ToT for participants, with mean ToT ranging from below 100 to almost 500 seconds. There also is considerable variation on team level, but the overall range seems to be a little smaller. Note, however, that the participant level contains all the variation that is due to teams. A model with nested random effects can separate the sources of variation. When two (or more) levels are nested, a special syntax applies for specifying nested random effects.  `1|Team/Part`.




```r
M_1 <- 
  D_cue8 %>% 
  brm(logToT ~ Condition + (1|Team/Part),
       data = .)
# logToT ~ Condition + 1|Part/Team

P_1 <- posterior(M_1)
```



Note that the model contains another feature of the CUE8 study the effect of the testing condition, moderated or remote. Why does this not have a participant-level effect. As participants are either moderated or remote, we simply don't get any data, on how the same participant behaved in the other condition. 


```r
P_1
```



** tbl_post: 4000 samples in 4 chains


Table: (\#tab:tab:CUE8_sov)Coefficients

|model |parameter               |type  |fixef              |re_factor | entities|
|:-----|:-----------------------|:-----|:------------------|:---------|--------:|
|M_1   |                        |ranef |Intercept          |Team      |       14|
|M_1   |                        |ranef |Intercept          |Team:Part |      523|
|M_1   |b_Conditionmoderated    |fixef |Conditionmoderated |NA        |        1|
|M_1   |b_Intercept             |fixef |Intercept          |NA        |        1|
|M_1   |sd_Team:Part__Intercept |grpef |Intercept          |Team:Part |        1|
|M_1   |sd_Team__Intercept      |grpef |Intercept          |Team      |        1|

The posterior object reveals two random factors, one for teams and one for participants. The interpretation is in no way different than  cross-classified random effects. In both cases, the absolute group mean for a certain participant is obtained by adding up all two coefficients. 

The syntax really is just a safe way to deal with nested samples, where participant identifiers could be re-used. If participant identifiers are truly unique, the following would produce the exact same results:


```r
D_cue8 %>% 
  mutate(Part = str_c(Team, Part, sep = "_")) %>% 
  brm(logToT ~ Condition + (1|Part) + (1|Team), data = .)
```

Let's take a closer look at the results regarding consistency of ToT measures across teams. We would always expect participants to show variation, but if team averages show strong variation, then we can suspect that there are biases. It turns out that the variation by team is  by a factor of 1.5 larger than individual differences. And it is on par with the measurement error (sigma).


```r
P_1 %>% 
  filter(type %in% c("fixef", "grpef")) %>% 
  clu()
```



Table: (\#tab:unnamed-chunk-58)Estimates with 95% credibility limits

|parameter               |type  |fixef              |re_factor | center|  lower| upper|
|:-----------------------|:-----|:------------------|:---------|------:|------:|-----:|
|b_Intercept             |fixef |Intercept          |NA        |  4.624|  4.079| 5.154|
|b_Conditionmoderated    |fixef |Conditionmoderated |NA        |  0.329| -0.367| 1.065|
|sd_Team__Intercept      |grpef |Intercept          |Team      |  0.611|  0.412| 0.992|
|sd_Team:Part__Intercept |grpef |Intercept          |Team:Part |  0.427|  0.389| 0.468|


It is not surprising to see the test users vary greatly in performance. In contrast, the discordance between professional teams is concerning. And it even arises while controlling for the condition, remote or moderated. Surprisingly, evidence for a difference between conditions is rather low and highly uncertain ($0.33 [-0.37, 1.07]_{CI95}$).


```r
T_fixef <- fixef(P_1)
T_fixef
```



Table: (\#tab:tab:CUE8_fixef)Estimates with 95% credibility limits

|fixef              | center|  lower| upper|
|:------------------|------:|------:|-----:|
|Intercept          |  4.624|  4.079|  5.15|
|Conditionmoderated |  0.329| -0.367|  1.06|


```r
detach(CUE8)
```


<!-- Actually, it is not even necessary to specify that participants are nested within teams. If we make sure that participant identifiers are unique across the whole study (not just within a team unit), we can also just use `1|Part + 1|Team`. If the participant identifier is only unique within a team, it is either to be recoded, e.g. `mutate(Part = str_c(Team, Part))`. -->

In this section we introduced a new perspective on multi-level models. Here, the question was to quantify and compare samples (rather than conditions) as sources of variation. With multi-level models, we can separate sources of variation. This builds on how random effects are constructed, as factor levels drawn from a Gaussian distribution.  In the following section, we will delve deeper into the matter of random effects.




## What are random effects? On pooling and shrinkage {#pool-shrink}

At least half a dozen of definitions exist for the term random effect. This is so confusing that some authors refrain to use the term altogether. 
In addition, the very terms *random effect* and *random factor* are highly misleading, as there is nothing more or less random in a random factors as compared to fixed factors. Here, the definition of a random effects is conceptually based on the idea of a population, and implemented as a factor, where levels are assumed to follow a Gaussian distribution. 

A Gaussian distribution extends in both directions infinitely, but the extreme tails are  becoming very thin. The low assigned probability of extreme events acts on the estimate, by correcting extreme estimates towards the mean, which is called shrinkage.

<!-- The opposite is the case: as we have seen above, a random effects model pulls a variance component from the random term and explains it by assigning coefficients to entities (teams or users). The best advice is  to not contemplate over what makes a factor random. It is just a name and because random factors are so amazingly useful, they should be called fonzy <!-- #87 factors, instead. -->

When a data set contains a factor that we may wish to add to the model, the question is: fixed effect or random effect? In \@ref(non-human-populations), I have introduced the heuristic of populations. If one can conceive tasks, designs, or whatever set of items as a population, there is clumping to some degree, but also  variation. The more clumping there is, the better is the guess for unobserved members by observing some members.

Obviously, we would never speak of a population, when the objects of interest are from different classes. Entities gathering on super market parking lots, like persons, cars, baskets and and dropped brochures, we would never see as a population. People, we would generally see as a population, as long as what we want to observe is somewhat comparable between members. When the question is, how fast persons can do a 2000 meter run at the Olympic games, we would certainly want one population per discipline (swimming, running, etc). Why is that so? It is because we expect members of a population to have some similarity, with the consequence that, if you already have observed some members of the population, this tells you something about any unobserved members.

Reconsider the Bayesian principle of prior knowledge by an experiment of thought: Consider, a UX expert with experience in e-commerce is asked to estimate how long it takes users to do the checkout, but without showing the expert the actual system.  The expert will probably hesitate briefly, and then come up with an estimate of, let's say, 45 seconds. Without any data, the expert made some reasonable assumptions, e.g. that a  disciplined design process has been followed, and then relies on experience. The experts personal experience has formed prior to the study by observing many other cases. Now, we confront the expert with an even more bizzare situation: guess the time-on-task for an unknown task with an unseen system of unknown type! The expert will probably refuse to give an answer, arguing that some systems have tasks in the second range (e.g. starting a car), whereas other processes easily run for hours or days (e.g. writing a report). This is a good point and the expert is provided, not with knoeledge of the system, but with average ToT of four other tasks within the same system:

$ToT_{1-4} = {23, 45, 66, 54}$

Now, the expert is confident that ToT be around 50 seconds and that is probably a good guess. What has happened is that prior belief about the unkown task parameter has been formed not externally, but *by data* as it arrived. The likely value of one unit has been learned from the other units and this appears pretty reasonable. The same principle applies when visually identifying outliers in a boxplot or scatterplot. First, the mind of the observer forms a gestalt that covers the salient features of data, for example: almost all points are located in the range 100 - 500. Once this pattern has formed, deviant points stand out.

However, the salience of the gestalt may vary. Consider a situation where ToT has been measured by the same procedure, but using five different stop watches. Stop watches are so incredibly accurate that if you know one measure, you basically know them all. What many researchers do with repeated measures data, is take the average. This is the one extreme called *total pooling*.  In the stopwatch case the average of the five measures would be so highly representative, that total pooling is a reasonable thing to do.

In other cases, the levels of a factor are more or less independent, for example tasks in a complex system, where procedure duration ranges from seconds to hours. Guessing the duration of one task from a set of others is highly susceptible and the average duration across tasks is not representative at all. The best choice then is to see tasks as factor levels, that are independent. This extreme of *no pooling* is exactly represented by fixed effect factors as they have been introduced in \@ref(lm).

Random effects sit right between these two extremes of no and total pooling and implement *partial pooling*: the more the group mean is representative for the units of the group, the more it is taken into account. By this we can also see, why a multi-level model must estimate alle levels simultaneously. The best thing about partial pooling is that, unlike real priors, there is not even the need to determine the amount of pooling in advance. The variation of entities has been observed. The stronger the enities vary, the less can be learned from the group level. The variation is precisely the group-level standard deviation of the random effect. 

<!-- Because the population-level learns from the participant-level, and vice vers, , we can think of random factors as factors where there is a certain amount of cross-talk between levels. The random effect estimate then draws on two sources of evidence: all data from the overarching population and data that belongs just to this one entity. As that is the case, the exploratory analysis of individual performance in SOV does not resemble a true random effect, as group means were calculated independently.  -->

How are random effects implemented to draw on both sources? Obviously, the procedure must be more refined than just adding participant-level dummy variables into the structural part of the model. In the Bayesian framework a remarkably simple trick suffices, and it is even a familiar one. By the concept of prior distributions, we already know a way to restrict the range of an effect based on prior knowledge. For example, intelligence test results have the prior distribution $IQ ~ \textrm{Gaus}(100, 15)$, just because they have been empirically calibrated this way. In most other cases, we do have rough ideas about the expected magnitude and range in the population, say: healthy human adults will finish a 2000m run in the range of 5-12 minutes. 

As prior knowledge is external to the data, it often lacks systematic evidence, with the exception of a meta analyses. This is why we tend to use weak informative priors. Like priors, random effects take into account knowledge external to the entity under question. But, they draw this knowledge from the data, which is more convincing after all. The basic trick to establish the cross-talk between random factor levels, is to *simultaneously estimate factor levels and random factor variation*. This has several consequences:

All random effects get a more or less subtle trend towards the population mean. As a side effect, the random factor variance is usually smaller than variance between fixed factors, or naive group means. This effect is called *shrinkage*. When the random factor variation is small, extreme factor levels are pulled stronger towards the population mean, resulting in stronger shrinkage. Or vice versa: When random variation is large, the factor levels stand more on their own.

The random factor variation is an estimate and as such it is certain only to a degree. As we have seen in \@ref(non-human-populations), the more levels a random factor comprises, the more precise is the estimate of random factor variation. The strongest shrinkage occurs with few observations per factor levels and highly certain random factor variation.

Previously, I have stressed how important repeated measures design is, as the number of observations per entity plays a role, too. The more observations there are, the less is the group mean overruled by the population mean. Less shrinkage occurs. This is why multi-level models gracefully deal with imbalanced designs. Groups with more observations are just gradually more self-determined. Taking this to the opposite extreme: when a factor level contains no data at all, it will just be replaced by the population mean. This principle offers a very elegant solution to the problem of missing data. If you know nothing about a person, the best guess is the population mean.

Under the perspective of populations as a more or less similar set of entities, these principles seem to make sense. Within this framework, we can even define what fixed effects are:

> a fixed effect is a factor where levels are regarded so unsimilar, that the factor-level variance can be practically considered infinite.

<!-- We routinely approximate such a situation when using non-informative prior distributions, like $\beta_0 ~ \textrm{Gaus}(0, 10000)$. In the extreme case, a uniform distribution with an infinite upper boundary truly has an infinite variance: $\beta_0 ~ U(0, \infty)$. A finite population mean doesn't even exist with such a distribution. -->

<!-- So, when a design researcher has observed that with design A, ToT is approximately distributed as $ToT_A ~ \textrm{Gaus}(120, 40)$, is it realistic to assume that design B has ToT in the range of several hours? Would a cognitive psychologist see it equally likely that the difference in reaction time on two primitive tasks is 200ms or 2h? Probably not. Still, using fixed effects for factors with very few levels is a justifyable approximation. First of all, priors can be used at any time to factor in reasonable assumptions about the range. Second, with very few estimates, the random factor variation cannot be estimated with any useful certainty. Very small shrinkage would occur and the results would practically not differ. -->

The CUE8 study makes a case for seeing shrinkage in action: Teams of researchers were asked to conduct a performance evaluation on a website. Tasks and website were the same, but the teams followed their own routines. Some teams tested a few handful of participants, whereas others tested dozens remotely. Teams, as another non-human population (sic!) differ vastly in the number of observations they collected. We can expect differences in shrinkage. To see the effect, we compare the team-level group means as fixed factor versus random factor. All teams have enough participants tested to estimate their mean with some certainty. At the same time, the group sizes vary so dramatically that we should see clear differences in tendency towards the mean.

We estimate two models, a random effects (RE) model and a fixed effects (FE) model. For the RE model, the absolute group means are calculated on the posterior. Figure XY shows the comparison of FE and RE estimates. 


```r
attach(CUE8)
```



```r
M_2 <- 
  D_cue8 %>% 
  stan_glm(logToT ~ Team - 1, data = .)

M_3 <- 
  D_cue8 %>% 
  brm(logToT ~ 1 + (1|Team), data = .)
```






```r
P_2 <- posterior(M_2, type = "fixef")
P_3_fixef <-  posterior(M_3, type = "fixef")
P_3_ranef <-  posterior(M_3, type = "ranef")

## Creating a derived posterior with 
## absolute team-level random effects

P_3_abs <-
  left_join(P_3_ranef, P_3_fixef,
    by = c("chain", "iter", "fixef"),
    suffix = c("","_fixef"))

P_3_abs$value <-  P_3_abs$value + P_3_abs$value_fixef


T_shrinkage <- 
  D_cue8 %>% 
  group_by(Team) %>% 
  summarize(N = n()) %>% 
  mutate(fixef = fixef(P_2)$center,
         ranef = ranef(P_3_abs)$center,
         diff = fixef - ranef) 


sd_fixef <- sd(T_shrinkage$fixef)
sd_ranef <- sd(T_shrinkage$ranef)


T_shrinkage %>% 
  ggplot(aes(x = Team, y = fixef, size = N, col = "fixef")) +
  geom_point() +
  geom_point(aes(y = ranef, col = "ranef"), alpha = .5)
```

<img src="Linear_mixed-effects_models_files/figure-epub3/CUE8_fe_vs_re-1.png" width="90%" />

```r
detach(CUE8)
```


Team H is far off the population average, but almost no shrinkage occurs due to the large number of observations. Again, no shrinkage occurs for Team L, as it is close to the population mean, and has more than enough data to speak for itself. Team B with the fewest observation (a genereous $N = 45$, still), gets noticable  shrinkage, although it is quite close to the population mean. Overall, the pattern resembles the above properties of random effects: groups that are far off the population mean and have comparably small sample size get a shrinkage correction. In the case of CUE8, these correction are overall negligible, which is due to the fact that all teams gathered ample data. Recall the SOV simulation above, where the set of tasks every user did was beyond control of the researcher. In situations with quite heterogeneous amount of missing data per participant, shrinkage is more pronounced and more information is drawn from the population mean. <!-- #88 -->

<!-- At the same time, shrinkage adjusts the estimates for variation, with $sd_{RE} = 0.583 < sd_{FE} = 0.59$. The random effects estimate is an unbiased estimate for the population variance, whereas fixed effects variation would be overestimating. -->

In conclusion, random effects are factors with the additional assumption of Gaussian distribution. When a multi-level model is estimated, the population level effect, the random effects levels and the variance of the distribution are estimated simultaneously. This creates two particular advantages of multi-level models with random effects: 

1. In unbalanced research designs (with unequal number of observations per subject) small groups are corrected towards the population mean.
2. Strong outliers are corrected towards the population mean.

Classical techniques for repeated measures often require additional tweaks to work well with unbalanced designs and outliers. Multi-level models with random effects handle those situations gracefully.








## Psychometrics and design-o-metric models {#psychometrics}

Up to to this point, we have characterized random factors mainly by their variance. However, a random effect is just like a factor, where the levels are typically entities in a sample. In a multi-level model, these levels are represented as coefficients. Every entity gets their own estimate which represents the level of functioning of the entity. These values can either be compared against an external benchmark, or they are compared to each other. When human individuals are being compared this is called *psychometrics*. 

Traditionally, psychometrics deals with the valid and reliable measurement of personal characteristics, such as individual levels of performance, motivation, socio-cognitive attitude and the like. Advanced statistical models have been devised, such as confirmatory factor analysis or item response models. Formally, these applications establish an order among population entities, from low to high. The least one is aiming for is that for any pair of entities $A$ and $B$, we can tell which entity has the more pronounced property. This is called an *ordering structure*. Fewer applications go beyond mere ranking and establish metric interpretations. For example, in an *interval-scaled* test, the researcher may compare differences between participants.

In design research, multi-item validated scales are routinely used for one of two purposes:

1. A design-related research questions involve traits or abilities of users. For example: Do social network users with high Openness have more connections? A six-item test for Openness is used on every individual in the sample and the scores are compared the number of connections. This is the basic *psychometric situation*, which is an *encounter of persons and items*.
2. In design research one frequently assesses properties of a design by using multi-item questionnaires. One example would be the comparison of user experience among a set of e-commerce homepages using scales such as the AttrakDiff (the hedonic dimension). Another example would be to map the uncanny valley effect by letting participants judge a set of artificial faces with a multi-item scale to measure eeriness. Apparently, when the aim is to measure a design, the study is an *encounter of users, items and designs*. I  call this the *designometric* situation.

We begin with the first case, standard psychometrics to assess user characteristics. For example, one could ask how a persons visual-spatial abilities are related to performance in navigating a complex hypertext environment, exploring body cavities during surgical procedures or monitoring the scattered displays in air traffic control centers. 

In the case of visual-spatial ability, the researcher could administer a test for visual-spatial abilities: for example, participants solve a set of mental rotation tasks and reaction times are collected as a score for spatial processing speed. This would later be use to predict performance with a real task, for example, how quickly someone learns to perform a minimally invasive surgical procedure. The most straight-forward approach  for obtaining test scores would be to  take the average measure per person. 


```r
n_Part <- 20
n_Trial <- 10
n_Obs <- n_Part * n_Trial

D_Part <- tibble(Part = 1:n_Part,
                 true_score = rnorm(n_Part, 900, 80))

D_Trial <- tibble(Trial = 1:n_Trial)

D_CTT <- 
  mascutils::expand_grid(Part = D_Part$Part,
                                Trial = D_Trial$Trial) %>% 
  left_join(D_Part) %>% 
  mutate(RT = rnorm(n_Obs, 
                    mean = true_score,
                    sd = 50)) %>% 
  mascutils::as_tbl_obs()

D_CTT %>% 
  group_by(Part) %>% 
  summarize(score = mean(RT)) %>% 
  sample_n(8)
```



| Part| score|
|----:|-----:|
|    8|   875|
|   16|  1074|
|   11|   909|
|   10|   827|
|    5|   892|
|   14|   828|
|   19|   951|
|   12|   873|

This is the approach of Classical Test Theory. CTT models are very simple, because they assume items to be completely exchangeable. The observed test score $y_i$ for participant $i$ is composed of the true score of participant $i$, $\mu_i$, and a Gaussian measurement error $\epsilon_{ij}$.

$$
y_{ij} = \mu_i + \epsilon_{ij}
$$

The following model implements CTT as an absolute group means model \@ref(amm), with the only difference that the person factor is a random effect, i.e. it assumes a Gaussian distribution of person scores.




CTT assumes that all items function precisely the same way and item information can therefore be ignored, which is a bold claim. For a set of experimental trials it may (or may not) be true that they are exchangeable. For self-report rating scales item equality is highly questionnable. Two items from the same scale can differ in several aspects, one of which is how hard (or strong) an item is. Consider the following two items from a fictional user experience scale; most likely, the second item would get lower ratings on average, because it is stronger:

1. The interface is nice.
1. The interface is really cool.

One problem with CTT is that by averaging scores, the CTT swallows any information on item functioning. In contrast, the families of *Item response models (IRM)*, as well as *factor analysis models (FAM)*, do not take for granted that all items act the same. As diverse and elaborated these models can be today, they all have in common, that items are modeled explicitly and get their own estimates. Discussing these models in more depth would require a separate book. Still, a simple item response model is nothing but an encounter of persons and test items, a simple tow-way cross over (\@ref(non-human-populations)). 

Some years ago, I proposed a novel personality construct *geekism*, which states that users differ in how enthusiastic they are about tinkering with technology. The hope was that we could explain differences in user behavior by using this scale, such as how they react when having to learn a new software application. A qualitative study with self-proclaimed geeks and several psychometric studies resulted in rating scale with 32 items. The Hugme case is one of the quantitative follow-up studies, where the Geekism scale was used together with the Need for Cognition scale (NCS), which assesses the tendency to enjoy intellectual puzzles in general. We were interested in 

1. how the items function, 
2. how reliable the scale is and 
3. how Geekism correlates with Need-for-cognition.



```r
attach(Hugme)

D_quest <- D_quest %>% mutate(Session = as.factor(session))
```

One important thing to note at this point is that psychometricians like to put things in matrices. An item response matrix is squared, whereas we need the long format for the regression engine. As is shown below, the long form can be transformed into a matrix and vice versa.


```r
D_long <- expand_grid(Part = 1:8, 
                      Item = 1:5) %>% 
  mutate(rating = rnorm(40)) %>% 
  mascutils::as_tbl_obs()
D_long

D_long %>%  
  select(Part, Item, rating) %>% 
  spread(key = Item, value = rating)
```



| Part|      1|      2|      3|      4|      5|
|----:|------:|------:|------:|------:|------:|
|    1|  0.278|  2.022|  1.175| -0.759| -1.554|
|    2|  0.801| -0.167|  0.006|  2.033| -1.737|
|    3|  0.298|  0.178| -0.056| -0.614|  1.327|
|    4|  1.463| -1.253| -0.620| -0.312|  1.686|
|    5| -0.519|  1.170| -1.137|  0.967| -1.014|
|    6|  0.084| -0.971|  1.336|  0.121| -1.014|
|    7|  0.672| -2.107| -0.463|  0.196|  0.061|
|    8|  0.297| -0.827|  0.496|  0.604|  2.441|

Psychometric programs often require matrix data, but for a multi-level models we need the long format. IRM models regard items as  populations, too, and the basic IRT model is a cross-classified intercept-only model \@ref(non-human-populations).


```r
D_psymx_1 <- 
  D_quest %>%   
  filter(Scale == "Geek", Session == 1)
  

M_psymx_1 <- 
  D_psymx_1 %>% 
  brm(rating ~ 1 + (1|Part) + (1|Item), data = .)
```

Once a rating scale instrument is ready to use, the resercaher will be mainly interested in the person scores. However, during the process of psychometric evaluation of an instrument, items scores play an important role. In the following I will demonstrate three psychometric evaluations, using multi-level models: 

1. *Test coverage* of a scale can be assessed by comparing the distribution of item scores with the distribution of person scores
2. *Test reliability* can be estimated by comparing scores across two sessions of testing.
3. *Test validity* can be estimated as person score correlations between scales.

### Coverage {#coverage}

Geekism was assumed to vary widely in the population of users and we wanted to be able to cover the whole range with good precision. In IRT psychometrics, items and persons are actually scored on the same scale. The person-level coefficients represent the persons' level of geekism. The item-level effects can best be called *item sensitivity*. A rule in the development of good psychometric instruments is that the range of interest has to be covered by items with a matching sensitivity.  An item with consistently high ratings gets a high score, and is able to distinguish low levels of geekism. But, that makes it barely useful for discriminating between geeks on high levels. Just think of how poorly a very simple arithmetic question, like "Which of the following numbers is divisible by 3? [2, 3, 5, 7, 9]"  would be able to diagnose the math skills of you, the readers of this book. The inverse is also true: an item with a very strong proposition, like

>> I always build my own computers

may be great to distinguish between amateur and pro level geekism, but most average and below average persons will just say No.

We have a linear model, where the rating is weighted sums of person tendency and item sensitivity. A high rating can mean two things (or both): coming from a very geek person, indeed, or it was a very sensitive item. For a good test coverage we need sensitive items for levels of low geekism and strong, i.e. *less* sensitive, items for the pros. Because  random effects are centered at zero, we can simply reverse the scale with *item strength* being the negative sensitivity. Now we can compare the distributions of person and item scores side-by-side and check how the person tendencies are covered by item strength. *Note* that for obtaining the absolute scores, we can use the Bayr function `re_scores`, but for psychometric analysis, the deviation from the population average is sufficient, hence `ranef`.



```r
P_psymx_1 <- posterior(M_psymx_1)

T_ranef <- 
  ranef(P_psymx_1) %>% 
  rename(geekism = center) %>% 
  mutate(geekism = if_else(re_factor == "Item", -geekism, geekism)) # reversing
  

T_ranef %>% 
  ggplot(aes(x = re_factor,
             y = geekism,
             label = re_entity)) +
  geom_violin() +
  geom_jitter(width = .2) +
  ylim(-2, 2)
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-68-1.png" width="90%" />

It turns out that the 32 items of the test cover the range of very low to moderately high geekism quite well. The upper 20 percent are not represented so well, as it seems. If we were to use the scale to discriminate between geeks and super-geeks, more strong item had to be added.


### Reliability {#reliability}

Next, we examine the reliability of the Geekism scale. Reliability is originally a CTT concept and means that the measurement error is small. For example, a reliable personality scale produces almost exactly the same score when applied to a person on different occasions. Is the Geekism score reliable? In our study we asked participants to fill out the questionnaire twice, with an experimental session in-between. If reliability of Geekism is good, the correlation of scores between sessions should be very strong.

In order to obtain the scores per session, we add an effect to the model. For reliability we are interested in correlation between person scores, so it suffices to add the Session random effect to the participant level, only.  However, the same model can be used to do assess *stability* of item scores, too. This is rarely practiced, but as we will see, there is an interesting pattern.


```r
D_psymx_2 <- 
  D_quest %>% filter(Scale == "Geek")

M_psymx_2 <- 
  D_psymx_2 %>% 
  brm(rating ~ 0 + Session + (0 + Session|Part) + (0 + Session|Item),
      data = .)
```



We extract the random effects and plot test-retest scores for participants and items. The red line in the plots indicates perfect stability for comparison.



```r
T_ranef <- 
  ranef(M_psymx_2) %>% 
  select(re_factor, re_entity, Session = fixef, score = center) %>% 
  spread(key = Session, value = score)

sample_n(T_ranef, 5)
```



|re_factor |re_entity | Session1| Session2|
|:---------|:---------|--------:|--------:|
|Part      |77        |    0.489|    0.431|
|Item      |Geek19    |    0.234|    0.229|
|Item      |Geek29    |   -0.323|   -0.272|
|Item      |Geek03    |    0.755|    0.523|
|Item      |Geek10    |   -0.242|   -0.128|




```r
plot_stability <- 
  function(ranef) ranef %>% 
  ggplot(aes(x = Session1, y = Session2)) +
  facet_grid(.~re_factor) +
  geom_point() +
  geom_smooth(aes(color = "observed stability"), se = F) +
  geom_abline(aes(intercept = 0, slope = 1, 
                  color = "perfect stability"))

T_ranef %>% plot_stability()
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-72-1.png" width="90%" />

The participant scores are highly reliable. If you measure the score of a person, you almost precisely know the result of another measure a few hours later. At least in short terms, the Geekism construct - whatever it truly is - can be measured with almost no error. Only ever so slightly is there a trend that lower scores get higher the second time and higher get lower, which could be called a trend towards the average. Perhaps, some experience during the experiment has led participants to report a more mediocre image of themselves.

In psychometric analysis it is common to assess participant-level test-retest reliability, but rarely is that done on items. This is, in fact, easy because the psychometric model contains intercept and slope random effects for items, and we can examine test-retest patterns in the same way. We see the same trend towards the average, but much stronger. In the present case, we see that this can be a mistake. Here it seems that the trend towards mediocrity does not produce a bias on the population mean, because it is bi-directional and the item and participant scores are nicely symmetric around the center of the scale. Not every test may have these properties and any asymmetric wear-off effect of items would produce more serious biases.

Another situation where item stability matters is when a person doing the test is actually learning from it. Usually, it is not desired that a test can be learned, because that means people can train for it. This is unlikely to occur in a regular math or intelligence test, but when the items are real-world tasks, like operating medical infusion pumps or driving a car, participants get a lot of feedback and will learn.

The example of test-retest stability shows one more time, how useful plots are for discovering patterns in data. More formally, test-retest stability is reported as a correlation. We can produce a correlation estimate by using the standard `cor` command on the participant-level random effects:


```r
T_ranef %>% 
  group_by(re_factor) %>% 
  summarize(cor = cor(Session1, Session2))
```



|re_factor |   cor|
|:---------|-----:|
|Item      | 0.998|
|Part      | 1.000|

Unfortunately, this lacks information about the degree of certainty. The better way is to let the regression engine estimate all correlations between random factors that are on the same level (Part, Item). The regression engine `brm` fromn package Brms package does that by default (which can be switched off). The following code extracts the posterior distributions of all correlations in the model, creates an estimates table and a plot of 95% certainties.


```r
clu_cor <- 
  function(model){
    model %>% 
    posterior() %>% 
    filter(type == "cor") %>% 
    mutate(parameter = str_remove_all(parameter, "cor_")) %>% 
    group_by(parameter) %>% 
    summarize(center = median(value),
              lower = quantile(value, .025),
              upper = quantile(value, .975)) %>%
    separate(parameter, into = c("re_factor", "between", "and"), 
             sep = "__")
}


M_psymx_2 %>% 
  clu_cor()
```



|re_factor |between  |and      | center| lower| upper|
|:---------|:--------|:--------|------:|-----:|-----:|
|Item      |Session1 |Session2 |  0.984| 0.916| 0.999|
|Part      |Session1 |Session2 |  0.994| 0.967| 1.000|

With random effects correlations assessing test-retest-stability is straight-forward. If test and retest random effects correlate strongly, we can be sure that the error of measurement is low and we can call it a reliable scale. Good  reliability is necessary, but not sufficient to also call a scale valid. 




### Validity {#validity}

Reliability doesn't say anything about what the scale actually measures. In psychometric studies, *validity* of a scale is routinely evaluated by comparing the scores to external criteria. In a perfect world, it would be assessed how scores are related to relevant real-world behavior, such as:

1. Are high-Geek persons more enthusiastic to learn a programming language?
1. Do high-Geek persons perform better in computer jobs?
1. Are high-Geek persons more likely to buy robot toys for their offsprings?

In the real world, researchers in the field of personality are often content with relating  a new rating scales to another, already validated personality scale. In the Hugme study, participants were also asked to rate themselves on the Need-for-Cognition scale (NCS). In very brief NCS measures how much a person enjoys intellectual puzzles. Since computers are intellectual puzzles, sometimes in a good way, often not, we thought that high-Geek persons must also score high on NCS. At the same time, a very strong correlation between Geek and NCS would indicate that the two scales render the same property, which would make one of them redundant, probably the newcomer. The following model estimates the person scores per scale and we can extract the correlation.



```r
M_psymx_3 <- 
  D_quest %>% 
  brm(rating ~ 0 + Scale + (0 + Scale|Part),  data = .)
```





```r
M_psymx_3 %>% clu_cor()
```



|re_factor |between   |and      | center| lower| upper|
|:---------|:---------|:--------|------:|-----:|-----:|
|Part      |ScaleGeek |ScaleNCS |  0.389| 0.119| 0.604|

We observe a weakly positive  association between Geek and NCS, just as was hoped for. It is related, but not quite the same.


```r
detach(Hugme)
```





### Towards Design-o-metrix {#designometrix}

Psychometrics, as it was introduced above, deals with comparing human individuals. In Design Research, this may be of interest sometimes, but the real stake is to *compare designs*. As we will see in this section, psychometric concepts can well be extended to *design-o-metric problems*. However, there is one twist, which has up til now been overlooked in most of Design Research: in  design-o-metric  studies the target population is designs, not humans. In a typical psychometric study, measurements are an encounter of humans with items, with the ultimate goal of measuring humans. A design-o-metric measurement is the encounter of three popluations, humans, items and, ultimately,  designs. Classic psychometric tools use a 2-dimensional matrix as input and cannot deal with a third dimension. Multi-level models have no such limits. All we have to do, is crossing in another non-human population \@ref(non-human-populations).

We revisit the Uncanny Valley data set \@ref(prm). The experiment used eight items from the Eeriness scale [@Ho2017a] to  ask the judgment of participants on 82 stimuli showing robot faces. In one of our experiments (RK_1), participants simply rated all robots face in three separate session. Here are a few example observations: 


```r
attach(Uncanny)
```


```r
RK_1 %>%  
  select(Part, Item, Stimulus, Session, response)
```

With this data we seem to be standing on familiar psychometric grounds: Persons encounter items and we have three measures over time. We can calculate test-retest stability of items and persons using a multi-level model. Voila! Here are your correlations, person and item stability - with credibility limits. But, wait a second! What is being measured here? Persons? No, robot faces. The original question was, how human-likeness of robot faces is related to perceived eeriness of robot faces and the Eeriness scale intended purpose is the comparison of designs, not persons. For example, it could be used by robot designers to check that a design does not trigger undesirable emotional responses. Without knowing the human-likeness scores, robot faces become just a naked *sample of designs* \@ref(non-human-populations):


```r
UV_dsgmx <- 
  RK_1 %>% 
  rename(Design = Stimulus) %>% 
  select(Part, Item, Design, Session, response) %>% 
  as_tbl_obs()
UV_dsgmx
```

Measures in the Uncanny experiment are an encounter of three samples: Part, Item and Design, and designs is what we ultimately want to compare. That means we need a model that produces design-level scores. For the user of multi-level models that just means to add a Design random effect to the psychometric model (Part, Item). Models, where a design random factor sits on top of a psychometric model, I call from here on a *design-o-metric models*. The most basic design-o-metric model is a three-way cross-classified intercept-only model, from which design scores can be extracted. By extending the test-retest psychometric model `M_psymx_2`, we can estimate test-retest stability.


```r
M_dsgmx_1 <-
  UV_dsgmx %>% 
  brm(response ~ 0 + Session + 
        (0 + Session|Design) + 
        (1 + Item) + 
        (0 + Session|Part), data = .)
```


Like in the psychometric situation, we extract the correlations.  Since we have three sessions, we get two stability scores per level.


```r
M_dsgmx_1 %>% 
  posterior() %>% 
  clu_cor() %>% 
  ggplot(aes(x = re_factor,
             y = center,
             ymin = lower,
             ymax = upper)) +
  facet_grid(and  ~ between) +
  geom_crossbar() +
  ylim(0,1)
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-83-1.png" width="90%" />


```r
detach(Uncanny)
```




The test-retest stability for designs is very reassuring. Ratings on the Eeriness scale are highly reproducible and the error will be very small. To a lesser, but still sufficient degree are person scores stable. 

But, what does the person score (and its stability) actually mean? It describes the tendency of a person to give high ratings on Eeriness. Should a researcher want to assess how vulnerable a person is to the Uncanny Valley effect, the Eeriness scale is also reliable for measuring persons. Many scales in design research lend themselves to be looked at from a design-o-metric and psychometric perspective. For example, a hypothetical scale to measure comfort of sitting can be used to evaluate seats, but can also be used to measure how comfortable a person is with sitting.

No seat fits every person, or put differently: the comfort  of a seat depends  on the person sitting in it. This points us at one of many possible extensions to carry out deeper design-o-metric analysis. If the difficulty of an item in a psychometric test depends on who is being tested, this is called *differential item functioning*. For example, the large international student evaluations PISA and TIMMS routinely check their test items for cultural differences. The aim is to formulate test questions in such a way that they are equally comprehensible with all cultural backgrounds. This is a desirable property for design-o-metric scales, too. In a multi-level design-o-metric model, this could be incorporated as an interaction effect between cultural background and item-level coefficients.

That all being said about design-o-metric models, my observation is that practically all published rating scales in design research have been validated under a psychometric perspective, rather than a design-o-metric. This is a mistake! If the purpose of the scale is to compare designs, the scale's reliability and validity must examined on the design level. In the worst case, a design-o-metric scale is evaluated by a study, where a sample of participants and a sample of items do only encounter a single design, rather than a sample. How can a scale's capability to discern between designs be assessed on a single subject?  
It worries me that practically all purportedly design-o-metric scales out there have been validated under the wrong perspective, and I call this the *psychometric fallacy* in design research.



## Further readings

1. In psychometric models the population level is of lesser interest. One could build a single-level model, using fixed-effects factors for participants and items. [@Gelman2012a] show that random-effects models are superior, because they allow multiple pairwise comparisons, which is problematic with fixed effects. For psychometric situations, comparison of individuals is essential.
1. An introduction to planned missing data designs, as we used it in cases Egan and Uncanny, is given by [@Graham2006].
1. Multi-level models for psycholinguistic studies have been used by [@Baayen2008b].
1. In [@Schmettow2016], we evaluated whether we can predict by the method of card sorting, how quickly users find a specific information on a website. A multi-level model showed that this is *not* so. This serves as an example, that you can prove the (practical) absence of effects with New Statistics, and publish.
1. [@Ho2017a] is one of many examples of falling for the psychometric fallacy.


<!--[semantic Stroop]


Measuring participant performance is a useful application in some design research fields, such as human performance in  critical systems, e.g. identify persons with the best talent for being an airspace controller. However, in design research, it is frequently designs that are to be compared. For example, in the Egan case, one might be interested to select the best of the ten website designs, in order to use it as a blueprint. Such a research question is no longer psychometric, literally, but formally the only difference is that participants can be considered test items, rather than test objects. At least, when using ramdom effects for psychometric purposes, there is nothing special about the participant random effects compared to design random effects. Let's pretend we had conducted the study in order to identify a good template for university websites. The following code extracts design random effects posteriors, summarizes them in the usual way and makes a plot.


```r
attach(Egan)

ranef(P_1) %>%
  filter(re_factor == "Design") %>%
  rename(Design = re_entity) %>%
  ggplot(aes(x = Design, y = center, ymax = upper, ymin = lower)) +
  geom_point(size = 1) +
  geom_crossbar() +
  ylab("log ToT") +
  theme(axis.text.x = element_text(angle = 45))
```

<img src="Linear_mixed-effects_models_files/figure-epub3/unnamed-chunk-86-1.png" width="90%" />


The most simple form is the Rasch model in item response theory (IRT), where participants respond to a set of items and the response is either correct or incorrect. The outcome variable response is usually coded as 0 = incorrect and 1 = correct. Apparently, such a variable does nowhere near satisfy the assumption of linear models. It turns out that the Rasch model can be interpreted as cross-classified random effects in a *logistic regression* \@ref(logistic_regression). Logistic regression is a member of the General*ized* Linear family of models, which will be introduced in the next chapter.

As innocent as the Egan study seems, there is some psychometrics involved.  First, cognitive workload was measured using the NASA TLX questionnaire which has four items. With four items, we would hardly want to speak of a population. The trick is to view the four items as instantiations from all a virtual set of possible questions (on that matter). A typical question in psychometric research is that of *item consistency*, which for a small set of items can be measured by the correlation between items. High inter-item correlations indicate that a scale is reliable, which basically means it predicts well. Item correlations are mainly used during scale development, as a criterion to sort out renegade items, those that do not move in accordance with the others.





```r
detach(Egan)
```
-->








<!-- ## Average? Neverage! A pledoyer for within-subject research -->

<!-- Many studies in design research pretend that all users are alike. This idea is borrowed from experimental social science research. Seminal studies in cognitive psychology and social psychology claim mental structures or processes that hold for all humans. That is the very paradigm of fundamental psychology and many researchers ground their theorizing and experimentation on it. And indeed, several properties of human information processing have been replicated under such diverse conditions that one can assume that they are general. For example, the Stroop observation has been observed hundred of times in various populations subgroups [REF], situations [REF] and variations. -->

<!-- The treatments in such experiments can be simplistic to an extreme. Often, participants are asked to respond to colored geometric shapes that light up on a black background by simple key presses. This is called *reductionism* and it has led to at least some insights into how the human mind works. As much as  design researchers should draw upon psychological theory, as useless is the reductionist paradigm in most applied research. Designs must work in real world situations and often in many different ones, highly controlled lab experiments just bear too little resemblance to the real world. In particular, by no means can we expect all users to respond alike to one or more designs. Their minds are complex, obscure and unpredictable besides. -->

<!-- > Our minds are not run as top - down dictatorships; they are rambunctious parliaments, populated by squabbling factions and caucuses, with much more going on beneath the surface than our conscious awareness ever accesses. -->
<!-- (Carroll, Sean. The Big Picture (Kindle Locations 5029-5031). Oneworld Publications. Kindle Edition.) -->


<!-- Let me get it straight: People differ! If you plan to move from design A to B, or any other design choice, it is a good idea to also regard how much people vary. If your research question refers to any mental process or its change, the only solid way is to observe how one-and-the-same individuals behave on all conditions. That means within-subject designs should be the rule, not the exception. In some branches of experimental reseach it is still common to use between-subject designs, at the expense of statistical power and without the possibility to see "change of mind" actually happen. In contrast, between-subject experimental designs  is a potential mistake and requires good justification. The following case illustrates how things can go terribly wrong, when within-subject processes are examined in a between-subject manner: -->

<!-- Imagine you set out to determine the association between typing speed (as number of ke) and errors. It is common sense that quality decreases with speed, so a positive correlation is expected. During the experiment, participants get a number of texts to type. With the first trial they are instructed to type at a relaxed speed. With every further trial they were asked to type slightly faster than at the trial before. The expectation is that -->

<!-- > The faster a person types, the more errors occur -->

<!-- In terms of statistical skills, the researcher is familiar with linear models, and is aware of the fact, that these may not be applied for repeated measures. Therefore our researcher first averages the scores across participants and receives a data set with a neat 30 data points, one per trial. A first exploratory plot reveals a bizarre relation: it seems that the faster participants type, the less errors they make. That is completely against expectations, as there almost always is a trade-off between speed and accuracy. -->

<!-- ```{r Typing_average, opts.label = "fig.small"} -->
<!-- attach(Typing) -->

<!-- Type_1_avg %>%  -->
<!--   ggplot(aes(x = speed, -->
<!--              y = error)) + -->
<!--   geom_point() + -->
<!--   geom_smooth(se = F) -->

<!-- ``` -->

<!-- Could it be true that the faster a person types, the less errors happen? Of course not. The problem is how precisely we ask the question. When averaging over persons, we actually answer a different question, which is on a *population-level*: Do persons who can type faster make fewer errors? This question sees every person as one data point with two performance measures, speed and errors, both representing the ability to type. Now, the above results make more sense: people, who are trained in typing are at the same time fast and more accurate. But, what the experiment was out for is the trade-off between speed and accuracy *within a person*. The experiment provokes this trade-off by asking the same person to accelerate in typing. And, when we visualize the results on an individual level, the speed-accuracy trade-off becomes immediately visible. Now, every individual curve has an upward: with increased speed, the error goes up as well.  -->

<!-- ```{r Typing_individual, ops.label = "fig.small"} -->
<!-- Type_1 %>%  -->
<!--   ggplot(aes(x = speed, -->
<!--              y = error, -->
<!--              group = Part)) + -->
<!--   #geom_point() + -->
<!--   geom_smooth(se = F, method = "lm", aes(col = "within participant")) + -->
<!--   geom_smooth(data = Type_1_avg,  -->
<!--               aes(col = "between participant", x = speed, y = error, group = NA),  -->
<!--               se = F, method = "lm") -->

<!-- ``` -->

<!-- For the matter of simplicity let' us's ignore that the process is not linear, but takes a curved form. What is relevant is that there is a downwards trend on the population level (averaging over participants) that turns into a uniform upwards trend on participant level. Only a multi-level model will be able to uncover such a bizzare situation: -->

<!-- ```{r} -->
<!-- M_1 <- Type_1 %>%  -->
<!--   stan_glmer(error ~ 1 + speed + (1 + speed|Part), data = .) -->
<!-- ``` -->

<!-- ```{r} -->
<!-- M_1 -->
<!-- ``` -->



<!-- ```{r} -->
<!-- detach(Typing) -->
<!-- ``` -->


<!-- ## Random effects correlations {#re_correlations} -->

<!-- Another feature of the Typing data is discernable in the figure above: Participants with initial high error countstend to have much steeper curves  -->



<!-- ###  -->

<!-- The example here is constructed to be extreme: the population-level effect has the opposite direction of individual level effects. However, the situation is fully plausible and points us to an important principle: Whenever a research question capitalizes on a change in mental state or behaviour, the only solid way to investigate is a within-subject design. Therefore again: treating averaged data as if it represents within-persons changes is a severe mistake. In the A/B case, for example, one must always be clear about the level. Is it true that  -->

<!-- + on average, users prefer B over A? -->
<!-- + or all users individually prefer B over A? -->

<!-- The bizzare situation with the Typing study will have to wait, because we will need all elements of multilevel modelling to resolve it. However, the figure above features the core elements of mutlilevel models: -->

<!-- + persons have different levels for errors at relaxed typing (varying intercepts) -->
<!-- + persons differ in how much the error frequency goes up with speed (varying slopes) -->
<!-- + the better a person performs at relaxed typing, the less the number of errors goes up when the person types faster (correlated random effects) -->



<!-- ## Exercises -->

<!-- 1. In the Egan study, cross-classified random effects capture the encounter of samples. Actually, we have not even taken this to an extreme, as the original study also measured mental workload with a scale of four items. Create and run a model that captures all samples, including the scale items. Interpret the results. -->






