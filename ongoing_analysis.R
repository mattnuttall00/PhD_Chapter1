#' ---
#' title: Mixed models - socioeconomic predictors of forest cover
#' author: Matt Nuttall
#' date: 17/06/20
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---


#' This document is a summary of the ongoing analysis into predictors of forest cover/loss at the commune level across Cambodia.
#' 
#' Currently the response variable is raw count of forest pixels, and there are 9 predictor variable sets with varying number of predictors in each set. All predictor variables have been scaled and centered prior to analysis. A GLMM framework has been used, with a Poisson distribution and the following random effects structure:
#' 
#' (year|Province/Commune)
#' 
#' The logged area of each commune has been used as an offset term in each model.
#' 
#' The variable sets are as follow:
#' 
#' * POPULATION DEMOGRAPHICS - population density, total population, proportion indigenous
#' * EDUCATION -  proportion of males aged 6-24 in school
#' * EMPLOYMENT - proportion of people employed in the Primary sector, proportion of people emplyed in the Secondary sector
#' * ECONOMIC SECURITY - proportion of population with less than 1ha of farming land, proportion of families with pigs
#' * ACCESS TO SERVICES - median distance to nearest school, median distance to the Commune Office, proportion of families with access to waste collection services
#' * SOCIAL JUSTICE - total number of land conflict cases, number of criminal cases per capita
#' * MIGRATION - total number of in-migrants, total number of out-migrants
#' * ENVIRONMENTAL (CONTROL) - mean elevation, habitat
#' * HUMAN (CONTROL) - distance to international border, distance to Provincial Capital, PA presence, PA category, economic land concession presence
#' 
#' ## Brief summary
#' 
#' Unfortunately, none of the socioeconomic predictors appear to have any predictive power!  The only exception is population density.  The "control" variables however, do have some predictive power. Below I will show the results of the initial models for each set.
#' 
#' My general conclusion thus far however is that with this response, I don't think I am asking the question I want to. I think the models below are telling us which variables can predict forest cover (i.e. where is the forest? Which communes have lots of forest and which ones have little forest?).  I don't think the below models can really tell me anything about forest LOSS, or which variables are able to predict forest cover loss/change.  I feel like I need to go further and look at forest cover change as a response (e.g. rate of change / difference in forest pixels).  That's not to say that what I have done so far is not useful - I think in combination with further analysis we can build up a wider picture of forest cover and forest cover dynamics.  For example, the overall analysis and our results could be made up of the following:
#' 
#' * Which variables predict where forest cover is greatest / lowest across the country? (this is what I have done. Although at the moment the question is really "...where forest cover is greatest/lowest given there is SOME forest. This is because I removed all communes with 0 forest cover right at the start, as at that stage I was planning on useing rate of change as a response and so communes with no forest were false 0's)
#' * Which variables predict where forest is likely to be lost? (Presumably I could use binomial logistic model here: forest lost/not lost)
#' * Then subset the communes to only those that have lost some forest, and then model rate of change / magnitude of change. (Initially this is what I was going to do, either manually i.e. two steps, or via a hurdle model)
#' 
#' I keep having to remind myself of what the purpose of this analysis is.  The purpose is to use the resulting model(s) to predict where in the country forest is likely to be lost under various potential scenarios. I want to be able to highlight communes where forest cover is likely to be lost if, say, population density in rural areas goes up, or if the number of land conflicts in communes with land concessions spike.  The aim WAS then to use these predictions to assess the robustness of the national wildlife corridor network to future changes.  But in theory I could do a number of things, e.g. identifiy wich PAs are most vulnerable to certain future scenarios etc. 
#' The main point being though, is that I don't think I can use the below models to do that.  
#' 
#' Unless I am mis-understanding the usefulness of the models below.  The only way I can think to use these models to do what I want, is using the below approach:
#' * Ignore the "global" model (i.e. the model with RE's set to means)
#' * Pull out the individual communes of interest, e.g. the ones within the wildlife corridor, or the ones in/around the PAs
#' * run the commune-specific models (i.e. with RE's specific to that commune) with varying ranges of the predictors, e.g. increasing population density.  Do you think this does what I want?
#' 
#' The obvious problem with the above approach is that I don't really have any socioeconomic predictors (apart from population density) to play with as they're all a bit shit!  The other predictor variables that have some predictive power are mostly unchangeable i.e. elevation, distance to Provincial capital etc.
#' 
#' So I am currently unsure how best to proceed, and I would very much appreciate some guidance!  Below I will summarise the models, the diagnostics, and the predictions.
#' 
#' ## Model results
#' 
#' # Population demographics   


rmarkdown::render('ongoing_analysis.html')


