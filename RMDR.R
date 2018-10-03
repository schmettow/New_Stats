
#source("~/.cran/MYLIBDIR.R")
## various output purposes, is primarily used for chunk options

if(!exists("purp.book")) # Book
  purp.book = T
if(!exists("purp.mcmc")) # updating MCMC models
  purp.mcmc = F
if(!exists("purp.debg")) # debugging, show hidden code
  purp.debg = F
if(!exists("purp.prnt")) # print (for ioslides, todo)
  purp.prnt = T
if(!exists("purp.rtut")) # show R tutorial code
  purp.rtut = T

# if(!exists("thisdir")) thisdir = getwd()
# #setwd("/..") # manually
# datadir = paste0(thisdir, "/data/")
# mcmcdir = paste0(thisdir, "/mcmc/")
# 
# 
# 
# if(!exists("LOAD_DATA_SETS")) 
#   LOAD_DATA_SETS = F ## a hack for using RMDR.R by external programs
# RELOAD_DATA_SETS = F
# 
# if(LOAD_DATA_SETS){ 
#   if(RELOAD_DATA_SETS) {
#     source("load_dat.R")
#   }else{
#     load("Book.Rda")
#   }
# }

## Regression models
# library(lme4)
# library(MCMCglmm)
library(brms)
library(rstanarm)

## Simulation etc
#library(modeest)
#library(retimes, quietly = !purp.debg)
#library(MCMCpack)
library(polynom)
# library(wakefield)

## Plotting and setting
library(knitr)
library(printr)
#library(knitcitations)
library(gridExtra)
library(GGally)
library(DiagrammeR)

## Data management (go last to prevent overwriting dplyr/tidyr namespace)
library(foreign)
library(plyr)
library(pipeR)
library(tidyverse)
library(stringr)
library(haven)
library(readr)
library(openxlsx)

## ETC
#library(beepr)


## own libraries
library(mascutils)
library(bayr)
library(syncenv)



## literature
#cleanbib()
#options("citation_format" = "pandoc")

## cross referencing system

# storing anchors 
# Anchors = data.frame(type=character(), 
#                      label=character(), 
#                      no = integer())
# # registering anchor types
# AnchorTypes = data.frame(type = c("fig", "tab", "sim", "model"), 
#                          prefix = c("Figure", "Table", "Simulation", "Model"))
# 
# knit_hooks$set(anchor = function(before, options, envir) {
#   if(!before){
#     ## cross referencing system
#     label = opts_current$get("label")
#     message("label",label,"found")
#     if(label %in% Anchors$label) warning(paste("anchor hook: label", label, "exists"))
#     type = stringr::str_split(label, ":", 2)[[1]][1]
#     if(!(type %in% AnchorTypes$type)) warning(paste("Anchor type", type, "not defined"))
#     message(paste("Found caption type ", type, " with label ", label))
#     no = (filter(Anchors, type == type) %>% nrow() + 1)
#     Anchors <<- bind_rows(Anchors, data.frame(label = label, type = type, no = no))
#     message(paste("created anchor", type, no, label))
#     #return(paste0("{#",label,"}"))
#   }
# })

# figr <- function(ref){
#   #ref = quote(ref)
#   if(ref %in% Anchors$label){
#     entry = (Anchors  %>% inner_join(AnchorTypes, by = "type") %>% filter(label == ref))
#     out = paste0("[",entry$prefix," ",entry$no,"](#",entry$label,")")
#     message(out)
#     return(out)
#   }else{
#     warning(paste("label ", ref, "does not exist"))
#   }
# }


#opts_knit$set(kfigr.prefix = T)

opts_chunk$set(echo = purp.rtut, 
               warning = purp.debg, 
               message = purp.debg,
               eval = purp.book, results = "asis")

options(digits=3)

## chunk templates

opts_template$set( 
  tab = list(anchor = 'Table', echo = T, eval = T, results = 'markup' ),
  #fig.full = list(fig.width = 8, fig.height = 12, anchor = 'Figure'),
  #fig.large = list(fig.width = 8, fig.height = 8, anchor = 'Figure'), 
  #fig.small = list(fig.width = 4, fig.height = 4, anchor = 'Figure'),
  #fig.wide = list(fig.width = 8, fig.height = 4,  anchor = 'Figure'),
  #fig.slide = list(fig.width = 8, fig.height = 4, dpi = 120, dev = "svg"),
  #fig.half = list(fig.width = 3.8, fig.height = 4, dpi = 120, dev = "svg"),
  #fig.half = list(fig.width = 4, fig.height = 4, dpi = 120, dev = "svg", echo = purp.rtut),
  invisible = list(eval = purp.book, echo = purp.debg, message=purp.debg, warning=purp.debg),
  inv = list(eval = purp.book, echo = purp.debg, message=purp.debg, warning=purp.debg),
  #sim = list(eval = purp.book, echo = purp.debg),
  mcmc = list(eval = purp.mcmc, echo = purp.rtut),
  mcsync = list(eval = purp.mcmc, echo = purp.debg),
  #rtut = list(eval = purp.rtut, echo = purp.rtut, warnings = purp.rtut, results = "markup"),
  #rtut.slide = list(eval = purp.rtut, echo = purp.rtut, warnings = purp.rtut,
  #                  fig.width = 8, fig.height = 4, dpi = 120, dev = "svg"),
  rtut.nr = list(eval = F, echo = purp.rtut),
  future = list(echo = F, eval = F),
  deprecated = list(eval = F, echo = F, eval = F),
  scratch = list(eval = F, echo = F, eval = F)
)


## ggplot
theme_set(theme_minimal())
theme_mulifacet <- function() {
  theme_minimal() + 
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank())
}
        

# distribution functions, link functions 
logit = Vectorize(function(mu, upper = 1) log(mu/(upper-mu)))
inv.logit = function(eta) plogis(eta)


## setting defaults for brm
# rstan_options(auto_write = TRUE)
# rstan_options(mc.cores = parallel::detectCores())
# formals(brm)$algorithm <- "sampling"
# formals(brm)$chains <- 3
# formals(brm)$iter <- 1000
# formals(brm)$cluster <- 3
# formals(brm)$family <- gaussian("identity")

## setting defaults for rstanarm
# rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# formals(stan_lm)$chains <- 1
# formals(stan_lm)$iter <- 1000
# formals(stan_glm)$chains <- 2
# formals(stan_glm)$iter <- 3000
# formals(MCMCglmm)$verbose <- F




## setting defaults for MCMCglmm runs (switch off debugging)
# formals(MCMCglmm)$verbose <- F
# formals(MCMCglmm)$nitt <- 5000
# formals(MCMCglmm)$burnin <- 1000
# formals(MCMCglmm)$thin <- 1
## downward compatibility
# MG = function(...) MCMCglmm(...)

## extending MCMCglmm


## p-values don't work
# pMCMC = function(object) 2 * pmax(0.5/dim(object$Sol)[1], 
#                                   pmin(colSums(object$Sol[,1:object$Fixed$nfl, drop = FALSE] > 0)/dim(object$Sol)[1], 
#                                        1 - colSums(object$Sol[, object$Fixed$nfl, drop = FALSE] > 
#                                                      0)/dim(object$Sol)[1]))


## extending ggplot2
ggcurve = function(from = 0, to = 1, steps = 100, dfunc = dnorm, ...){
  X = seq(from = from, to = to, length.out = steps)
  data_frame(X = X, 
             density = dfunc(X, ...)) %>% 
    ggplot(aes(x = X, y = density)) +
    geom_path()
}


gtable <- function(d, plot = T, ...) {
  if(plot) {
    return(grid.table(d, rows = NULL))
  }else
  {
    return(tableGrob(d, rows = NULL))
  }
}


## displaying interaction effects

ia_table <- function(M, parameters){
  fixef(M) %>% 
    mutate(parameter = str_replace_all(parameter, 
                                       paste(parameters, collapse = "|"), "")) %>% 
    separate(parameter, parameters, sep = ":")
}

ia_plot <- function(M, parameters) {
  ia_table(M, parameters) %>% 
    ggplot(aes_string(y = "location", x = parameters[1], 
                      col = parameters[2], group = parameters[2])) +
    geom_point() +
    geom_line()
}

## Dealing with Case Environments

# knit_hooks$set(CE = function(before, options, envir) {
#   CE = opts_current$get("CE")
#   if(before){
#     attach(as.symbol(CE))
#     message("Entering CE ", CE)
#   } else {
#     detach(CE)
#     message("Leaving CE ", CE)
#   }
# })

# syncenv::new_syncenv("ce")
# ls(ce)

# nse
rip_CE <-
  function(Env){
    try(rm("<-", envir = eval(Env)), silent = T)
    try(rm("environment", envir = eval(Env)), silent = T)
  }

# rip_CE(ce)
# ls(ce)

#nse
register_CE <-
  function(Env, ...){
    varnames <- names(pryr::named_dots(...))
    print(varnames)
    varvalues <- rlang::dots_list(...)
    for (v in 1:length(varnames)){
        assign(varnames[[v]], varvalues[[v]], envir = Env)
    }
  }

# a <- "test"
# register_CE(ce, a)
# ls(ce)

#nse
sync_CE <-
  function(Env, ...){
    register_CE(Env, ...)
    save_CE_(as.character(substitute(Env)))
  }

# ls(ce)
# b <- "toast"
# sync_CE(ce, b)
# ls(ce)
# 

#se
load_CE_ <- 
  function(cases) {
    for(c in cases){
      load(paste0("Cases/",c,".Rda"), envir = globalenv())
      message(paste0("Loading case environment ", c))
    }
  }

#nse
load_CE <- 
  function(...) {
    cases <- names(pryr::named_dots(...))
    for(c in cases){
      load(paste0("Cases/",c,".Rda"), envir = globalenv())
      message(paste0("Loading case environment ", c))
    }
  }

#load_CE(Sec99, Overdisp)

#se
save_CE_ <-
  function(cases) {
    for(c in cases){
      fname = paste0("Cases/",c,".Rda")
      save(list = c, file = fname)
      message(paste0("Saving case environment ", c, " to: ", fname))
    }
  }

#nse
save_CE <-
  function(...) {
    cases <- names(pryr::named_dots(...))
    for(c in cases){
      fname = paste0("Cases/",c,".Rda")
      save(list = c, file = fname)
      message(paste0("Saving case environment ", c, " to: ", fname))
    }
  }

#save_CE(Sec99, Overdisp)


deploy_CE <-
  function(cases, path){
    for(c in cases){
      fname = paste0(path,"/",c,".Rda")
      load(paste0("Cases/",c,".Rda"), envir = globalenv())
      rm("<-", envir = eval(parse(text = c)))
      rm("environment", envir = eval(parse(text = c)))
      save(list = c, file = fname)
      message(paste0("Deploying case environment ", c, " to: ", fname))
    }
}

