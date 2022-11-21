##########################################################
# installing and loading missing libs
##########################################################
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(RSelenium)) install.packages("RSelenium", repos = "http://cran.us.r-project.org")
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
if(!require(owidR)) install.packages("owidR", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(latex2exp)) install.packages("latex2exp", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(pdftools)) install.packages("pdftools", repos = "http://cran.us.r-project.org")
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(fpc)) install.packages("fpc", repos = "http://cran.us.r-project.org")
if(!require(TSclust)) install.packages("TSclust", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(hopkins)) install.packages("hopkins", repos = "http://cran.us.r-project.org") 
if(!require(clValid)) install.packages("clValid", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(compiler)) install.packages("compiler", repos = "http://cran.us.r-project.org")

###############################################################
# for simplicity's sake: loading the necessary data from github
###############################################################
if (!file.exists("./suicide-data-analysis.zip")) {
download.file("https://github.com/drnjha/suicidal-world/raw/main/suicide-data-analysis.zip", "./suicide-data-analysis.zip")
}
unzip("./suicide-data-analysis.zip")

##########################################################
# loading libraries
##########################################################
library(knitr)
library(data.table)
library(tidyverse)
library(RSelenium)
library(stringi)
library(ggthemes)
library(gridExtra)
library(viridis)
library(owidR)
library(lubridate)
library(kableExtra)
library(psych)
library(latex2exp)
library(rvest)
library(pdftools)
library(openxlsx)
library(readxl)
library(fpc)
library(TSclust)
library(factoextra)
library(hopkins)
library(clValid)
library(ggpubr)
library(compiler)

##########################################################
# a useful function for changing the formatting on ggplots' axes from github.com/tinyheero/tinutils/tree/master/R
##########################################################
fancy_scientific <- function(l) {
 # turn in to character string in scientific notation
 l <- format(l, scientific = TRUE)
 # quote the part before the exponent to keep all the digits
 l <- gsub("^(.*)e", "'\\1'e", l)
 # turn the 'e+' into plotmath format
 l <- gsub("e", "%*%10^", l)
 # return this as an expression
 parse(text=l)
}

##########################################################
# other useful functions and settings
##########################################################
setCompilerOptions(optimize=3)
enableJIT(3)

getOutputFormat <- function() {
  output <- rmarkdown:::parse_yaml_front_matter(
    readLines(knitr::current_input())
    )$output
  if (is.list(output)){
    return(names(output)[1])
  } else {
    return(output[1])
  }
}

minmax <- function(df) {c(min(df), max(df))}

Mode <- function(x) {           # https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

##########################################################
# a quite interesting function to translate numbers into (english) words
##########################################################
#Fork of https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
#https://gist.githubusercontent.com/hack-r/22104543e2151519c41a8f0ce042b31c/raw/01731f3176b4ee471785639533d38eda4790ab77/numbers2words.r
numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  if(x==0){
     print( "zero")
    } else{
    helper <- function(x){
      
      digits <- rev(strsplit(as.character(x), "")[[1]])
      nDigits <- length(digits)
      if (nDigits == 1) as.vector(ones[digits])
      else if (nDigits == 2)
        if (x <= 19) as.vector(teens[digits[1]])
      else trim(paste(tens[digits[2]],
                      Recall(as.numeric(digits[1]))))
      else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                        Recall(makeNumber(digits[2:1]))))
      else {
        nSuffix <- ((nDigits + 2) %/% 3) - 1
        if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
        trim(paste(Recall(makeNumber(digits[
          nDigits:(3*nSuffix + 1)])),
          suffixes[nSuffix],"," ,
          Recall(makeNumber(digits[(3*nSuffix):1]))))
      }
    }
    trim <- function(text){
      #Tidy leading/trailing whitespace, space before comma
      text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
      #Clear any trailing " and"
      text=gsub(" and$","",text)
      #Clear any trailing comma
      gsub("\ *,$","",text)
    }  
    makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
    #Disable scientific notation
    opts <- options(scipen=100) 
    on.exit(options(opts)) 
    ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
              "eight", "nine") 
    names(ones) <- 0:9 
    teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
               "sixteen", " seventeen", "eighteen", "nineteen")
    names(teens) <- 0:9 
    tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
              "ninety") 
    names(tens) <- 2:9 
    x <- round(x)
    suffixes <- c("thousand", "million", "billion", "trillion")     
    if (length(x) > 1) return(trim(sapply(x, helper)))
    helper(x)    
  }

}

##########################################################
# ompute clusters within cluster sum of squares
##########################################################

# from: https://rdrr.io/github/sarafrr/basicClEval/man/wcss.html
#' Compute centroids
#'
#' This function computes the centroids for each cluster.
#' It takes in input a data.frame or a matrix of data and a vector
#' containing the classification of each sample.
#'
#' @param data Dataset of class type data.frame or matrix
#' @param clClassification Vector of samples' clustering labels
#' @param ... Other arguments
#' @return A list with:\cr
#' * clCentres - data frame of means nClustersxncol(data)
#' * nClusters - number of clusters
#' * sizeCl - number of elements by cluster
#' @export
clMeans <- function(data,clClassification,verbose=TRUE,... ) {
  if(is.matrix(data)){
    mat <-data
    df <- as.data.frame(mat)
  }else if(is.data.frame(data)){
    df <- data
    mat<-as.matrix(data)
  }else{
    if(verbose){
      cat("The data has to be a matrix or a dataframe\n",sep="")
    }
    stop()
  }
  if(nrow(mat)!=length(clClassification)){
    if(verbose){
      cat("The classification vector length (",length(clClassification),
          ") differs from the number of observation in the dataset (",nrow(mat),") \n",sep="")
    }
    stop()
  }else{
    sizeCl <- summary(as.factor(clClassification))
    clCenters <- tapply(mat, list(rep(clClassification,ncol(mat)),col(mat)),
                      function(x) mean(x, na.rm=TRUE))
    clCenters <- as.data.frame(clCenters)
    clCenters <- setNames(clCenters, names(df))
  }
  nClusters = nlevels(as.factor(clClassification))
  list(
    clCenters=clCenters,
    nClusters=nClusters,
    sizeCl=sizeCl
  )
}

#' Compute clusters' variances
#'
#' This function computes the inner variance for each cluster.
#' It takes in input a data.frame or a matrix of data and a vector
#' containing the classification of each sample.
#'
#' @param data Dataset of class type data.frame or matrix
#' @param clClassification Vector of samples' clustering labels
#' @param ... Other arguments
#' @return A list with:\cr
#' * clVariances - data frame of variances nClustersxncol(data)
#' * nClusters - number of clusters
#' * sizeCl - number of elements by cluster
#' @export
clInnerVariances <- function(data,clClassification,verbose=TRUE,... ) {
  if(is.matrix(data)){
    mat <-data
    df <- as.data.frame(mat)
  }else if(is.data.frame(data)){
    df <- data
    mat<-as.matrix(data)
  }else{
    if(verbose){
      cat("The data has to be a matrix or a dataframe\n",sep="")
    }
    stop()
  }
  if(nrow(mat)!=length(clClassification)){
    if(verbose){
      cat("The classification vector length (",length(clClassification),
          ") differs from the number of observation in the dataset (",nrow(mat),") \n",sep="")
    }
    stop()
  }else{
    sizeCl <- summary(as.factor(clClassification))
    clVariances <- tapply(mat, list(rep(clClassification,ncol(mat)),col(mat)),
                        function(x) var(x, na.rm=TRUE))
    clVariances <- as.data.frame(clVariances)
    clVariances <- setNames(clVariances, names(df))
  }
  nClusters = nlevels(as.factor(clClassification))
  list(
    clVariances=clVariances,
    nClusters=nClusters,
    sizeCl=sizeCl
  )
}

#' Compute clusters' Within Cluster Sum of Squares
#'
#' This function computes the Within Cluster Sum of Squares WCSS for each cluster.
#' It takes in input a data.frame or a matrix of data and a vector
#' containing the classification of each sample.
#'
#' @param data Dataset of class type data.frame or matrix
#' @param clClassification Vector of samples' clustering labels
#' @param ... Other arguments
#' @return A list with:\cr
#' * WCSSByClByVar - WCSS by cluster by variable
#' * WCSSByClB - WCSS by cluster
#' * WCSS - Sum of the total WCSS
#' * nClusters - number of clusters
#' * sizeCl - number of elements by cluster
#' @export
wcss <- function(data,clClassification,verbose=TRUE,... ) {
  if(is.matrix(data)){
    mat <-data
    df <- as.data.frame(mat)
  }else if(is.data.frame(data)){
    df <- data
    mat<-as.matrix(data)
  }else{
    if(verbose){
      cat("The data has to be a matrix or a dataframe\n",sep="")
    }
    stop()
  }
  if(nrow(mat)!=length(clClassification)){
    if(verbose){
      cat("The classification vector length (",length(clClassification),
          ") differs from the number of observation in the dataset (",nrow(mat),") \n",sep="")
    }
    stop()
  }else{
    sizeCl <- summary(as.factor(clClassification))
    WCSSByClByVar <- tapply(mat, list(rep(clClassification,ncol(mat)),col(mat)),
                          function(x) var(x, na.rm=TRUE)*(length(x)-1))
    WCSSByClByVar <- as.data.frame(WCSSByClByVar)
    WCSSByClByVar <- setNames(WCSSByClByVar, names(df))
    WCSSByCl <- rowSums(WCSSByClByVar)
    WCSS <- sum(WCSSByCl)
  }
  nClusters = nlevels(as.factor(clClassification))
  list(
    WCSSByClByVar=WCSSByClByVar,
    WCSSByCl = WCSSByCl,
    WCSS = WCSS,
    nClusters=nClusters,
    sizeCl=sizeCl
  )
}

#' Prints the information of an object returned by wcss
#'
#' This function prints all the information contained into [wcss]
#' It takes in input an object returned by [wcss]
#'
#' @param x Object returned by [wcss] function
#' @param ... Other arguments
#' @return A print with a brief explanation of:\cr
#' * WCSSByClByVar - WCSS by cluster by variable
#' * WCSSByClB - WCSS by cluster
#' * WCSS - Sum of the total WCSS
#' * nClusters - number of clusters
#' * sizeCl - number of elements by cluster
#' @export
print.wcss <- function(x,...){
  cat("\nWithin Cluster Sum of Squares by cluster by variable:\n")
  print(x$WCSSByClByVar, ...)
  cat("\nWithin Cluster Sum of Squares by cluster:\n")
  print(x$WCSSByCl, ...)
  cat("\nNumber of clusters:\n")
  print(x$nClusters, ...)
  cat("\nNumber of elements by cluster:\n")
  print(x$sizeCl, ...)
  cat("\nAvailable components:\n")
  print(names(x))
  # print the object itself by calling the assigned variable
  # instead of printing the last string
  invisible(x)
}
```

##########################################################
# Data ingestion
# The data source is https://www.kaggle.com/datasets/russellyates88/suicide-rates-overview-1985-to-2016/download?datasetVersionNumber=1
# the data has to be downloaded manually, as a login in is required, which had to be done manually
##########################################################
suicideraw <- read.csv("master.csv")

##########################################################
# plot of average number of suicides per year
##########################################################
suicideraw %>% group_by(year) %>% summarize(annual_suicide_no=sum(suicides_no)) %>% ggplot(aes(x=year, y=annual_suicide_no)) + geom_bar(stat='identity') + labs(x = "year", y = "count of suicides") + scale_y_continuous(labels=fancy_scientific)
suicideraw <- suicideraw[which(suicideraw$year != "2016"), ]

##########################################################
# plot of average number of suicides per year in today's Russion Federation
##########################################################
suicideraw %>% filter(country == "Russian Federation" & year < 1996) %>%  group_by(year) %>% summarize(annual_suicide_no=sum(suicides_no)) %>% ggplot(aes(x=year, y=annual_suicide_no)) + geom_bar(stat='identity') + labs(x = "year", y = "count of suicides") + scale_y_continuous(labels=fancy_scientific)
suicideraw <- suicideraw[which(suicideraw$year >= "1990"), ]
suicideraw <- suicideraw[which(suicideraw$country != "Macau"),]

##############################################################
# map of total suicide numbersin different countries according to kaggle
# for each country the total suicide numbers are summed up for the whole time period under consideration
##############################################################
gross_suicides_country <- suicideraw %>% group_by(country) %>% summarise(sum_suicides_no=sum(suicides_no))

##############################################################
# the map's data are read from a variable given in ggplot2
##############################################################
world <- map_data("world")

##############################################################
# because of different designations of countries' names in the "world" and the "suicideraw" data, names have to be reworked a little
##############################################################
world[which(world$region == "Russia"),]$region                                       <- "Russian Federation"
world[which(world$region == "Cape Verde"),]$region                                   <- "Cabo Verde"
world[which(world$region == "USA"),]$region                                          <- "United States"
world[which(world$region == "UK"),]$region                                           <- "United Kingdom"
world[which(world$region == "South Korea"),]$region                                  <- "Republic of Korea"
world[which(world$region == "Antigua" | world$region == "Barbuda"),]$region          <- "Antigua and Barbuda"
world[which(world$region == "Trinidad" | world$region == "Tobago"),]$region          <- "Trinidad and Tobago"
world[which(world$region == "Saint Kitts" | world$region == "Nevis"),]$region        <- "Saint Kitts and Nevis"
world[which(world$region == "Saint Vincent" | world$region == "Grenadines"),]$region <- "Saint Vincent and Grenadines"

##############################################################
# plot the world map data
##############################################################
world %>%
  merge(gross_suicides_country, by.x = "region", by.y = "country", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = sum_suicides_no)) +
  geom_polygon(color = "gray90", size = 0.1) +
  scale_fill_viridis(name="total\nsuicides", option="H", begin=0.6, end=0.9, na.value = "white", labels = fancy_scientific, limits = c(0, 12e5)) +
  theme(aspect.ratio=0.6, axis.text = element_blank(), axis.title = element_blank())
``

##############################################################
# Turning to data from a different source
##############################################################

##############################################################
# to avoid multiple access to OWID during the project development, the data are read in only once, 
# and then stored in an object that is written to disk in an .rds-file.
# If that file exists, no attempt is made at re-reading the data from OWID source.
##############################################################
if (file.exists("./suicid_owid_big_list.rds")) {
    suicid_owid_big_list <- readRDS(file = "./suicid_owid_big_list.rds") 
    list_of_suicid_owid_tables <- readRDS(file = "./list_of_suicid_owid_tables.rds") 
    other_owid_big_list <- readRDS(file = "./other_owid_big_list.rds") 
    list_of_other_owid_tables <- readRDS(file = "./list_of_other_owid_tables.rds") 
} else {
    suicide_owid <- as_tibble(owid_search("suicide"))
    for(i in 1:nrow(suicide_owid)) {
        assign(tolower(gsub("\\-", "_", suicide_owid[i,]$chart_id)), 
           owid(suicide_owid[i,]$chart_id)
        )
    }
    suicid_owid_big_list <- lapply( tolower(gsub("\\-", "_", suicide_owid$chart_id)), get)
    names(suicid_owid_big_list) <- tolower(gsub("\\-", "_", suicide_owid$chart_id))
    saveRDS(suicid_owid_big_list, "./suicid_owid_big_list.rds")
    list_of_suicid_owid_tables <- suicide_owid %>% mutate(proj_chart_id = tolower(gsub("\\-", "_", chart_id))) # save the chart_id with minus sign replaced by underscore
    saveRDS(list_of_suicid_owid_tables, "./list_of_suicid_owid_tables.rds")
    ##############################################################
    # remove objects, that are now in suicid_owid_big_list to avoid cluttering of the environment
    ##############################################################
    rm(list=list_of_suicid_owid_tables$proj_chart_id)

    ##############################################################
    # grab other possibly relevant data from OWID
    ##############################################################
    other_OWID <- do.call("rbind", list(
    data.frame("titles"="Human Development Index vs. Corruption Perception Index",                    "chart_id"="human-development-index-vs-corruption-perception-index")             ,
    data.frame("titles"="Human Development Index vs. GDP per capita",                                 "chart_id"="human-development-index-vs-gdp-per-capita")                          ,
    data.frame("titles"="Gender Inequality Index from the Human Development Report",                  "chart_id"="gender-inequality-index-from-the-human-development-report")          ,
    data.frame("titles"="Human Development Index",                                                    "chart_id"="human-development-index")                                            ,
    data.frame("titles"="Life expectancy vs. expected years lived with disability or disease",        "chart_id"="life-expectancy-vs-expected-years-lived-with-disability")            ,
    data.frame("titles"="Life expectancy vs. GDP per capita",                                         "chart_id"="life-expectancy-vs-gdp-per-capita")                                  ,
    data.frame("titles"="Inequality in life expectancy vs. health expenditure per capita",            "chart_id"="inequality-in-life-expectancy-vs-health-expenditure-per-capita")     ,
    data.frame("titles"="Life expectancy vs. health expenditure",                                     "chart_id"="life-expectancy-vs-health-expenditure")                              ,
    data.frame("titles"="Life expectancy vs. health expenditure per capita",                          "chart_id"="life-expectancy-vs-health-expenditure-per-capita")                   ,
    data.frame("titles"="Life expectancy at birth, including the UN projections",                     "chart_id"="life-expectancy-at-birth-including-the-un-projections")              ,
    data.frame("titles"="Share in poverty vs. Life expectancy",                                       "chart_id"="poverty-vs-life-expectancy")                                         ,
    data.frame("titles"="Life expectancy",                                                            "chart_id"="life-expectancy")                                                    ,
    data.frame("titles"="Population by broad age group, World",                                       "chart_id"="population-by-broad-age-group")                                      ,
    data.frame("titles"="Life satisfaction vs. life expectancy",                                      "chart_id"="life-satisfaction-vs-life-expectancy")                               ,
    data.frame("titles"="Healthy life expectancy and years lived with disability",                    "chart_id"="healthy-life-expectancy-and-years-lived-with-disability")            ,
    data.frame("titles"="Prevalence of alcohol use disorders by age",                                 "chart_id"="prevalence-of-alcohol-use-disorders-by-age")                         ,
    data.frame("titles"="Prevalence of alcohol use disorders in males vs. females",                   "chart_id"="prevalence-of-alcohol-disorders-males-vs-females")                   ,
    data.frame("titles"="Child mortality vs. Prevalence of child wasting",                            "chart_id"="child-mortality-vs-wasting")                                         ,
    data.frame("titles"="Child mortality vs. Prevalence of stunting",                                 "chart_id"="child-mortality-vs-prevalence-of-stunting")                          ,
    data.frame("titles"="Prevalence of ADHD in males vs females",                                     "chart_id"="prevalence-adhd-in-males-vs-females")                                ,
    data.frame("titles"="Prevalence of anxiety disorders by age",                                     "chart_id"="prevalence-of-anxiety-disorders-by-age")                             ,
    data.frame("titles"="Prevalence of anxiety disorders, males vs. females",                         "chart_id"="prevalence-of-anxiety-disorders-males-vs-females")                   ,
    data.frame("titles"="Prevalence of bipolar disorder by age",                                      "chart_id"="prevalence-of-bipolar-disorder-by-age")                              ,
    data.frame("titles"="Prevalence of bipolar disorder in males vs. females",                        "chart_id"="prevalence-of-bipolar-disorder-in-males-vs-females")                 ,
    data.frame("titles"="Prevalence of depression by age",                                            "chart_id"="prevalence-of-depression-by-age")                                    ,
    data.frame("titles"="Prevalence of depression, males vs. females",                                "chart_id"="prevalence-of-depression-males-vs-females")                          ,
    data.frame("titles"="Prevalence of eating disorders by age",                                      "chart_id"="prevalence-of-eating-disorders-by-age")                              ,
    data.frame("titles"="Prevalence of schizophrenia by age",                                         "chart_id"="prevalence-of-schizophrenia-by-age")                                 ,
    data.frame("titles"="Prevalence of schizophrenia in males vs. females",                           "chart_id"="prevalence-of-schizophrenia-in-males-vs-females")                    ,
    data.frame("titles"="Average years of schooling vs. GDP per capita",                              "chart_id"="average-years-of-schooling-vs-gdp-per-capita")                       ,
    data.frame("titles"="How important is God in your life? vs GDP per capita, 2014",                 "chart_id"="how-important-is-god-in-your-life-vs-gdp-per-capita")                ,
    data.frame("titles"="Self-reported life satisfaction, 2003 to 2020",                              "chart_id"="happiness-cantril-ladder")                                           ,
    data.frame("titles"="Happiness vs. life satisfaction, 2015",                                      "chart_id"="happiness-wvs-vs-gallup")                                            ,
    data.frame("titles"="Civil liberties, 2021",                                                      "chart_id"="civil-liberties")                                                    ,
    data.frame("titles"="Share of people agreeing with the statement \"most people can be trusted\"", "chart_id"="self-reported-trust-attitudes")
    ))
    for(i in 1:nrow(other_OWID)) {
        assign(tolower(gsub("\\-", "_", other_OWID[i,]$chart_id)), 
           owid(other_OWID[i,]$chart_id)
        )
    }
    other_owid_big_list <- lapply( tolower(gsub("\\-", "_", other_OWID$chart_id)), get)
    names(other_owid_big_list) <- tolower(gsub("\\-", "_", other_OWID$chart_id))
    saveRDS(other_owid_big_list, "./other_owid_big_list.rds")
    list_of_other_owid_tables <- other_OWID %>% mutate(proj_chart_id = tolower(gsub("\\-", "_", chart_id))) # save the chart_id with minus sign replaced by underscore
    saveRDS(list_of_other_owid_tables, "./list_of_other_owid_tables.rds")
    ##############################################################
    # remove objects, that are now in other_owid_big_list to avoid cluttering of the environment
    ##############################################################
    rm(list=list_of_other_owid_tables$proj_chart_id)
}

##############################################################
# collect data on Human Development Index (which on the UNDPP-site are more complete compared to OWID)
##############################################################
if (file.exists("./UNDP_HDI.rds")) {
    UNDP_HDI <- readRDS(file = "./UNDP_HDI.rds")
} else {
    fname <- "https://hdr.undp.org/sites/default/files/2021-22_HDR/HDR21-22_Composite_indices_complete_time_series.csv"
    if(!file.exists("./HDR21-22_Composite_indices_complete_time_series.csv")){
         download.file(fname,"HDR21-22_Composite_indices_complete_time_series.csv",
                       method="curl",
                       mode="wb") # "wb" means "write binary"
    }
    UNDP_HDI <- read.csv("./HDR21-22_Composite_indices_complete_time_series.csv")
    UNDP_HDI <- UNDP_HDI[, c("iso3", "country","hdi_1997", "hdi_1998", "hdi_1999", "hdi_2000", "hdi_2001", "hdi_2002", "hdi_2003", "hdi_2004", "hdi_2005", "hdi_2006", "hdi_2007", "hdi_2008", "hdi_2009", "hdi_2010", "hdi_2011", "hdi_2012", "hdi_2013", "hdi_2014", "hdi_2015", "hdi_2016", "hdi_2017")]
    names(UNDP_HDI)[3:23] <- (names(UNDP_HDI)[3:23] %>%  str_sub(-4))
    UNDP_HDI <- UNDP_HDI %>%  gather(year, hdi, `1997`:`2017`) %>% mutate(year = as.numeric(year))
    UNDP_HDI <- UNDP_HDI[order(UNDP_HDI$iso3), ] 
    names(UNDP_HDI)[1] <- "code"
    saveRDS(UNDP_HDI, "./UNDP_HDI.rds")
}

##############################################################
# collect data on Gender Inequality Index from the UNDPP-site, for GII see: https://hdr.undp.org/content/measuring-key-disparities-human-development
##############################################################
if (file.exists("./UNDP_GII.rds")) {
    UNDP_GII <- readRDS(file = "./UNDP_GII.rds")
} else {
    fname <- "https://hdr.undp.org/sites/default/files/2021-22_HDR/HDR21-22_Composite_indices_complete_time_series.csv"
    if(!file.exists("./HDR21-22_Composite_indices_complete_time_series.csv")){
         download.file(fname,"HDR21-22_Composite_indices_complete_time_series.csv",
                       method="curl",
                       mode="wb") # "wb" means "write binary"
    }
    UNDP_GII <- read.csv("./HDR21-22_Composite_indices_complete_time_series.csv")
    UNDP_GII <- UNDP_GII[, c("iso3", "country","gii_1997", "gii_1998", "gii_1999", "gii_2000", "gii_2001", "gii_2002", "gii_2003", "gii_2004", "gii_2005", "gii_2006", "gii_2007", "gii_2008", "gii_2009", "gii_2010", "gii_2011", "gii_2012", "gii_2013", "gii_2014", "gii_2015", "gii_2016", "gii_2017")]
    names(UNDP_GII)[3:23] <- (names(UNDP_GII)[3:23] %>%  str_sub(-4))
    UNDP_GII <- UNDP_GII %>%  gather(year, gii, `1997`:`2017`) %>% mutate(year = as.numeric(year))
    UNDP_GII <- UNDP_GII[order(UNDP_GII$iso3), ] 
    names(UNDP_GII)[1] <- "code"
    saveRDS(UNDP_GII, "./UNDP_GII.rds")
}

##############################################################
# GDP-data from the Maddison Project Database 2020 are being used (with Geary–Khamis dollar based on 1990: https://en.wikipedia.org/wiki/International_dollar https://stats.oecd.org/glossary/detail.asp?ID=5528)
##############################################################
fname <- "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx"
if(!file.exists("./mpd2020.xlsx")){
     download.file(fname,"mpd2020.xlsx",
                   method="curl",
                   mode="wb") # "wb" means "write binary"
}
maddison_gdp_data <- read.xlsx(xlsxFile = "./mpd2020.xlsx", sheet = "Full data", startRow = 1, colNames = TRUE)
colnames(maddison_gdp_data)[1] <- "code"
colnames(maddison_gdp_data)[2] <- "entity"
colnames(maddison_gdp_data)[5] <- "population"

##############################################################
# Calculation for WorldMap of OWID Total Suicide Numbers
##############################################################
# for each country the total suicide numbers are summed up for the whole time period that is represented in the OWID-data
##############################################################
OWID_gross_suicides_country <- suicid_owid_big_list["suicide_deaths_by_age"] %>% first()
names(OWID_gross_suicides_country)[names(OWID_gross_suicides_country) == "Deaths - Self-harm - Sex: Both - Age: 70+ years (Number)"] <- "age70"
names(OWID_gross_suicides_country)[names(OWID_gross_suicides_country) == "Deaths - Self-harm - Sex: Both - Age: 50-69 years (Number)"] <- "age50"
names(OWID_gross_suicides_country)[names(OWID_gross_suicides_country) == "Deaths - Self-harm - Sex: Both - Age: 15-49 years (Number)"] <- "age15"
names(OWID_gross_suicides_country)[names(OWID_gross_suicides_country) == "Deaths - Self-harm - Sex: Both - Age: 5-14 years (Number)"] <- "age5"
OWID_gross_suicides_country <- OWID_gross_suicides_country %>% mutate(yearly_sum = age70+age50+age15+age5)
OWID_gross_suicides_country <- OWID_gross_suicides_country %>% group_by(entity) %>% summarise(entity_suicides=sum(yearly_sum))
##############################################################
# the aggregated entities (like "world", "G20" etc. are being removed from the data
##############################################################
OWID_gross_suicides_country <- OWID_gross_suicides_country %>% filter(entity != "World") %>% filter(entity != "G20") %>% filter(entity != "OECD Countries") %>% filter(!grepl("World Bank ", entity)) %>% filter(!grepl("(WB)", entity)) %>% filter(!grepl("(WHO)", entity))
##############################################################
# the partial entities of the UK (like "Wales" etc. are being removed from the data
##############################################################
OWID_gross_suicides_country <- OWID_gross_suicides_country %>% filter(!(entity %in% c("Wales", "Scotland", "England", "Northern Ireland")))

##############################################################
# the map's data are read from a variable given in ggplot2
##############################################################
world <- map_data("world")

##############################################################
# because of different designations of countries' names in the "world" and the "OWID_gross_suicides_country" data, names have to be reworked a little
# code to identify non-matching country-names:
# wrong_country_names <- left_join(OWID_gross_suicides_country, world, by=c("entity" = "region")) %>% filter(is.na(long))%>%pull(entity)
##############################################################
world[which(world$region == "Saint Kitts" | world$region == "Nevis"),]$region        <- "Saint Kitts and Nevis"
world[which(world$region == "Saint Vincent" | world$region == "Grenadines"),]$region <- "Saint Vincent and the Grenadines"
world[which(world$region == "Trinidad" | world$region == "Tobago"),]$region          <- "Trinidad and Tobago"
world[which(world$region == "UK"),]$region                                           <- "United Kingdom"
world[which(world$region == "USA"),]$region                                          <- "United States"
world[which(world$region == "Antigua" | world$region == "Barbuda"),]$region          <- "Antigua and Barbuda"
world[which(world$region == "Democratic Republic of the Congo"),]$region             <- "Democratic Republic of Congo"
world[which(world$region == "Republic of Congo"),]$region                            <- "Congo"
world[which(world$region == "Ivory Coast"),]$region                                  <- "Cote d'Ivoire"
world[which(world$region == "Timor-Leste"),]$region                                  <- "Timor"
world[which(world$region == "Czech Republic"),]$region                               <- "Czechia"
world[which(world$region == "Virgin Islands"),]$region                               <- "United States Virgin Islands"
world[which(world$region == "Micronesia"),]$region                                   <- "Micronesia (country)"

countries_without_suicide_data <- world %>% merge(OWID_gross_suicides_country, by.x = "region", by.y = "entity", all.x = T) %>% filter(is.na(entity_suicides)) %>% pull(region) %>% as_tibble() %>% unique() %>% pull(value) %>% str_flatten(collapse = ", ")

##############################################################
# Plotting WorldMap of OWID Total Suicide Numbers
##############################################################
# plot the world map data
##############################################################
world %>%
  merge(OWID_gross_suicides_country, by.x = "region", by.y = "entity", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = entity_suicides)) +
  geom_polygon(color = "gray90", size = 0.1) +
  scale_fill_viridis(name="total no. of\nsuicides\nover entire\ntime-span\ncovered", option="H", begin=0.6, end=0.9, na.value = "white", labels = fancy_scientific, limits = c(0, 56e5)) +
  theme(aspect.ratio=0.6, axis.text = element_blank(), axis.title = element_blank())

##############################################################
# Getting info on the source of OWID tables
##############################################################
# to avoid multiple access to OWID during the project development, the data are read in only once, 
# and then stored in an object that is written to disk in an .rds-file.
# If that file exists, no attempt is made at re-reading the data from OWID source.
# The following code blocks require a Windows computer with Chrome-browser and Chromium Driver installed
# On other OSs the code would have to be modified to fit the particular conditions of the resp. OS
##############################################################
library(RSelenium)
if (file.exists("./owid_sources.rds")) {
    owid_sources <- readRDS(file = "./owid_sources.rds") 
    detach("package:RSelenium", unload = TRUE) ## to free up memory space
} else {
    ##############################################################
    # Google Chrome browser and Chromium Driver is needed (pay attention to the correct version): https://chromedriver.chromium.org/downloads
    ##############################################################
    driver <- RSelenium::rsDriver(browser = "chrome",
                                  port=4884L, 
                                  chromever =
                                  system2(command = "wmic",
                                          args = 'datafile where name="C:\\\\Program Files\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                          stdout = TRUE,
                                          stderr = TRUE) %>%
                                  stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                                  magrittr::extract(!is.na(.)) %>%
                                  stringr::str_replace_all(pattern = "\\.",
                                                           replacement = "\\\\.") %>%
                                  paste0("^",  .) %>%
                                  stringr::str_subset(string =
                                                         binman::list_versions(appname = "chromedriver") %>%
                                                         dplyr::last()) %>%
                                  as.numeric_version() %>%
                                  max() %>%
                                  as.character())
    remote_driver <- driver[["client"]]
    owid_sources <- data.frame() 
    for (i in 1:nrow(list_of_suicid_owid_tables)) {
         ##############################################################
         # determine URL ffrom list of OWID suicide-related tables
         ##############################################################
         url <- paste0("https://ourworldindata.org/grapher/", list_of_suicid_owid_tables$chart_id[i], "?tab=table")

         ##############################################################
         # connect to the URL
         ##############################################################
         remote_driver$navigate(url)
         ##############################################################
         # from the tables tab, determine where the "SOURCES"-tab is (sometimes 3rd, sometimes 4th etc)
         ##############################################################
         footer_tabs <- remote_driver$findElement("class name", "ControlsFooter")$getElementText()
         currentTab <- footer_tabs %>%  str_split(pattern="\\n") %>% rbind.data.frame()
         colnames(currentTab) <- ("x")
         currentTab <- currentTab[ which(is.na(as.numeric(currentTab$x))), ] %>% as_tibble() 
         colnames(currentTab) <- ("x")
         currentTab <- which(currentTab$x=="SOURCES") 
         ##############################################################
         # click the "SOURCES"-tab 
         ##############################################################
         remote_driver$findElement("xpath", paste0("/html/body/main/figure/div/div[2]/div/nav/ul/li[",currentTab,"]/a"))$clickElement()
         ##############################################################
         # read the contents
         ##############################################################
          owid_sources <- rbind(owid_sources, c(list_of_suicid_owid_tables$titles[i], list_of_suicid_owid_tables$chart_id[i], list_of_suicid_owid_tables$proj_chart_id[i], remote_driver$findElement("xpath", "/html/body/main/figure/div/div[3]/div/div")$getElementAttribute('innerHTML') ) )
    }
    colnames(owid_sources)<-c("title", "chart_id", "proj_chart_id", "sources_content")
    saveRDS(owid_sources, "./owid_sources.rds")
}
library(RSelenium)
if (file.exists("./other_owid_sources.rds")) {
    other_owid_sources <- readRDS(file = "./other_owid_sources.rds") 
    detach("package:RSelenium", unload = TRUE) ## to free up memory space
} else {
    ##############################################################
    # Google Chrome browser and Chromium Driver is needed (pay attention to the correct version): https://chromedriver.chromium.org/downloads
    ##############################################################
    driver <- RSelenium::rsDriver(browser = "chrome",
                                  port=4897L, 
                                  chromever =
                                  system2(command = "wmic",
                                          args = 'datafile where name="C:\\\\Program Files\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                          stdout = TRUE,
                                          stderr = TRUE) %>%
                                  stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                                  magrittr::extract(!is.na(.)) %>%
                                  stringr::str_replace_all(pattern = "\\.",
                                                           replacement = "\\\\.") %>%
                                  paste0("^",  .) %>%
                                  stringr::str_subset(string =
                                                         binman::list_versions(appname = "chromedriver") %>%
                                                         dplyr::last()) %>%
                                  as.numeric_version() %>%
                                  max() %>%
                                  as.character())
    remote_driver <- driver[["client"]]
    other_owid_sources <- data.frame() 
    for (i in 1:nrow(list_of_other_owid_tables)) {
         ##############################################################
         # determine URL ffrom list of other OWID  tables
         ##############################################################
         url <- paste0("https://ourworldindata.org/grapher/", list_of_other_owid_tables$chart_id[i], "?tab=table")

         ##############################################################
         # connect to the URL
         ##############################################################
         remote_driver$navigate(url)
         ##############################################################
         # from the tables tab, determine where the "SOURCES"-tab is (sometimes 3rd, sometimes 4th etc)
         ##############################################################
         footer_tabs <- remote_driver$findElement("class name", "ControlsFooter")$getElementText()
         currentTab <- footer_tabs %>%  str_split(pattern="\\n") %>% rbind.data.frame()
         colnames(currentTab) <- ("x")
         currentTab <- currentTab[ which(is.na(as.numeric(currentTab$x))), ] %>% as_tibble() 
         colnames(currentTab) <- ("x")
         currentTab <- which(currentTab$x=="SOURCES") 
         ##############################################################
         # click the "SOURCES"-tab 
         ##############################################################
         remote_driver$findElement("xpath", paste0("/html/body/main/figure/div/div[2]/div/nav/ul/li[",currentTab,"]/a"))$clickElement()
         ##############################################################
         # read the contents
         ##############################################################
          other_owid_sources <- rbind(other_owid_sources, c(list_of_other_owid_tables$titles[i], list_of_other_owid_tables$chart_id[i], list_of_other_owid_tables$proj_chart_id[i], remote_driver$findElement("xpath", "/html/body/main/figure/div/div[3]/div/div")$getElementAttribute('innerHTML') ) )
    }
    colnames(other_owid_sources)<-c("title", "chart_id", "proj_chart_id", "sources_content")
    saveRDS(other_owid_sources, "./other_owid_sources.rds")
}
```

##############################################################
# Building the analysis data set
##############################################################
# Data from the UN Population Division are being used
##############################################################
fname <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx"
if(!file.exists("./WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx")){
     download.file(fname,"WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
                   method="curl", #use "curl" for OS X / Linux, "wininet" for Windows
                   mode="wb") # "wb" means "write binary"
}
UN_pop_data <- read.xlsx(xlsxFile = "./WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx", sheet = "Estimates", startRow = 17, colNames = TRUE)
colnames(UN_pop_data)[6] <- "code"
colnames(UN_pop_data)[11] <- "year"
colnames(UN_pop_data)[13] <- "population"
colnames(UN_pop_data)[14] <- "m_population"
colnames(UN_pop_data)[15] <- "f_population"
colnames(UN_pop_data)[18] <- "median_age"

##############################################################
# for each country the total suicide numbers are summed up, this time only for the time period 1990 - 2017
##############################################################
OWID_gross_suicides_country <- suicid_owid_big_list["suicide_deaths_by_age"] %>% first()
OWID_gross_suicides_country <- OWID_gross_suicides_country[which(OWID_gross_suicides_country$year >= 1990 &OWID_gross_suicides_country$year <= 2017) ,]
OWID_gross_suicides_country[which(OWID_gross_suicides_country$entity %in% c("Wales", "Scotland", "England", "Northern Ireland")) ,]$code <- "GBR" # set country code for United Kingdom
OWID_gross_suicides_country[which(OWID_gross_suicides_country$entity %in% c("Wales", "Scotland", "England", "Northern Ireland")) ,]$entity <- "United Kingdom"
names(OWID_gross_suicides_country)[names(OWID_gross_suicides_country) == "Deaths - Self-harm - Sex: Both - Age: 70+ years (Number)"] <- "age70plus"
names(OWID_gross_suicides_country)[names(OWID_gross_suicides_country) == "Deaths - Self-harm - Sex: Both - Age: 50-69 years (Number)"] <- "age50_69"
names(OWID_gross_suicides_country)[names(OWID_gross_suicides_country) == "Deaths - Self-harm - Sex: Both - Age: 15-49 years (Number)"] <- "age15_49"
names(OWID_gross_suicides_country)[names(OWID_gross_suicides_country) == "Deaths - Self-harm - Sex: Both - Age: 5-14 years (Number)"] <- "age5_14"
OWID_gross_suicides_country <- OWID_gross_suicides_country %>% mutate(yearly_sum = age70plus+age50_69+age15_49+age5_14)
OWID_gross_suicides_country <- rbind(OWID_gross_suicides_country %>% filter(code != "GBR"), OWID_gross_suicides_country %>% filter(code == "GBR") %>% group_by(year) %>% filter(yearly_sum == max(yearly_sum)) ) # the separate data for "Wales", "Scotland", "England", and "Northern Ireland" have to be deleted, as this information is now integrated in "United Kingdom"
OWID_gross_suicides <- OWID_gross_suicides_country

##############################################################
# the aggregated entities (like "world", "G20" etc. are being removed from the data
##############################################################
OWID_gross_suicides_country <- OWID_gross_suicides_country %>% filter(entity != "World") %>% filter(entity != "G20") %>% filter(entity != "OECD Countries") %>% filter(!grepl("World Bank ", entity)) %>% filter(!grepl("(WB)", entity)) %>% filter(!grepl("(WHO)", entity))

##############################################################
# calculation of suicide rates based on UN population data
##############################################################
OWID_gross_suicides_rates <- merge(x = OWID_gross_suicides_country, y = UN_pop_data[, c("code", "year", "population")], by=c("code", "year"), all.x=TRUE) %>% mutate(population=round(as.numeric(population)*1000)) %>% group_by(entity) %>% summarise(mean_suicides=mean(yearly_sum), mean_population=mean(population)) %>% mutate(avg_suicide_rate=(mean_suicides/(mean_population/100000)))

##############################################################
# a more full table is calculated for the purpose of further analyses
##############################################################

##############################################################
# read in median_age, total, male & female population (1st of July each year) from UN population division data
##############################################################
OWID_gross_suicides <- merge(x = OWID_gross_suicides, y = UN_pop_data[, c("code", "year", "population", "m_population", "f_population", "median_age")], by=c("code", "year"), all.x=TRUE) %>% mutate(median_age=as.numeric(median_age), population=round(as.numeric(population)*1000), m_population=round(as.numeric(m_population)*1000), f_population=round(as.numeric(f_population)*1000)) %>% filter(!(entity %in% c("World", "G20", "OECD Countries"))) %>% filter(!grepl("World Bank ", entity)) %>% filter(!grepl("(WB)", entity)) %>% filter(!grepl("(WHO)", entity))

##############################################################
# death from self-harm (OWID, poulation age-standardized)
##############################################################
OWID_gross_suicides <- merge(x = OWID_gross_suicides, y = ( first(suicid_owid_big_list["suicide_death_rates_by_sex"]) %>% select(-"code") ), by=c("entity", "year"), all.x=TRUE)
names(OWID_gross_suicides)[names(OWID_gross_suicides) == "Male suicide rate"] <- "m_suic_rate"
names(OWID_gross_suicides)[names(OWID_gross_suicides) == "Female suicide rate"] <- "f_suic_rate"
names(OWID_gross_suicides)[names(OWID_gross_suicides) == "Deaths - Self-harm - Sex: Both - Age: Age-standardized (Rate)"] <- "mf_slfhrmdeath_age_stand"

##############################################################
# death from interpersonal violence (OWID, poulation age-standardized)
##############################################################
OWID_gross_suicides <- merge(x = OWID_gross_suicides, y = ( first(suicid_owid_big_list["suicide_vs_homicide_rate"]) %>% select(-"code")  %>% select(-"Population") %>% select(-"Countries Continents") %>% select(-"Deaths - Self-harm - Sex: Both - Age: Age-standardized (Rate)") ), by=c("entity", "year"), all.x=TRUE) %>% mutate(suic_rate=(yearly_sum/(population/100000)))
names(OWID_gross_suicides)[names(OWID_gross_suicides) == "Deaths - Interpersonal violence - Sex: Both - Age: Age-standardized (Rate)"] <- "mf_ipvioldeath_age_stand"

##############################################################
# population by broad age groups
##############################################################
tmp <- first(other_owid_big_list["population_by_broad_age_group"]) %>% select(-"code") 
names(tmp)[names(tmp) == "Population by broad age group - Sex: all - Age: 65+ - Variant: estimates"] <- "popage65plus"
names(tmp)[names(tmp) == "Population by broad age group - Sex: all - Age: 25-64 - Variant: estimates"] <- "popage25to64"
names(tmp)[names(tmp) == "Population by broad age group - Sex: all - Age: 15-24 - Variant: estimates"] <- "popage15to24"
names(tmp)[names(tmp) == "Population by broad age group - Sex: all - Age: 5-14 - Variant: estimates"] <- "popage5to14"
names(tmp)[names(tmp) == "Population by broad age group - Sex: all - Age: 0-4 - Variant: estimates"] <- "popage0to4"
OWID_gross_suicides <- merge(x = OWID_gross_suicides, y = tmp, by=c("entity", "year"), all.x=TRUE)
rm(tmp)

##############################################################
# prevalence of mental and substance abuse disorders (OWID, poulation age-standardized)
##############################################################
OWID_gross_suicides <- merge(x = OWID_gross_suicides, y = ( first(suicid_owid_big_list["suicide_rates_vs_prevalence_of_mental_and_substance_use_disorders"]) %>% select(-"code") %>% select(-"Deaths - Self-harm - Sex: Both - Age: Age-standardized (Rate)") %>% select(-"Population") %>% select(-"Countries Continents") ), by=c("entity", "year"), all.x=TRUE)
names(OWID_gross_suicides)[names(OWID_gross_suicides) == "Prevalence - Mental and substance use disorders - Sex: Both - Age: Age-standardized (Rate)"] <- "mf_prev_md_s_ab_age_stand"

##############################################################
# prevalence of ADHD (OWID, poulation age-standardized)
##############################################################
tmp <- first(other_owid_big_list["prevalence_adhd_in_males_vs_females"]) %>% select(-"code") %>% select(-"Population") %>% select(-"Countries Continents") 
names(tmp)[names(tmp) == "Prevalence - Attention-deficit/hyperactivity disorder - Sex: Male - Age: Age-standardized (Percent)"] <- "m_prev_adhd"
names(tmp)[names(tmp) == "Prevalence - Attention-deficit/hyperactivity disorder - Sex: Female - Age: Age-standardized (Percent)"] <- "f_prev_adhd"
OWID_gross_suicides <- merge(x = OWID_gross_suicides, y = tmp, by=c("entity", "year"), all.x=TRUE)
rm(tmp)

##############################################################
# prevalence of alcohol disorders (OWID, poulation age-standardized)
##############################################################
tmp <- first(other_owid_big_list["prevalence_of_alcohol_disorders_males_vs_females"]) %>% select(-"code") %>% select(-"Population") %>% select(-"Countries Continents") 
names(tmp)[names(tmp) == "Prevalence - Alcohol use disorders - Sex: Male - Age: Age-standardized (Percent)"] <- "m_prev_alco"
names(tmp)[names(tmp) == "Prevalence - Alcohol use disorders - Sex: Female - Age: Age-standardized (Percent)"] <- "f_prev_alco"
OWID_gross_suicides <- merge(x = OWID_gross_suicides, y = tmp, by=c("entity", "year"), all.x=TRUE)
rm(tmp)

##############################################################
# prevalence of depressive disorders (OWID, poulation age-standardized)
##############################################################
tmp <- first(other_owid_big_list["prevalence_of_depression_males_vs_females"]) %>% select(-"code") %>% select(-"Population") %>% select(-"Countries Continents") 
names(tmp)[names(tmp) == "Prevalence - Depressive disorders - Sex: Male - Age: Age-standardized (Percent)"] <- "m_prev_depr"
names(tmp)[names(tmp) == "Prevalence - Depressive disorders - Sex: Female - Age: Age-standardized (Percent)"] <- "f_prev_depr"
OWID_gross_suicides <- merge(x = OWID_gross_suicides, y = tmp, by=c("entity", "year"), all.x=TRUE)
rm(tmp)

##############################################################
# prevalence of anxiety disorders (OWID, poulation age-standardized)
##############################################################
tmp <- first(other_owid_big_list["prevalence_of_anxiety_disorders_males_vs_females"]) %>% select(-"code") %>% select(-"Population") %>% select(-"Countries Continents") 
names(tmp)[names(tmp) == "Prevalence - Anxiety disorders - Sex: Male - Age: Age-standardized (Percent)"] <- "m_prev_anxiety"
names(tmp)[names(tmp) == "Prevalence - Anxiety disorders - Sex: Female - Age: Age-standardized (Percent)"] <- "f_prev_anxiety"
OWID_gross_suicides <- merge(x = OWID_gross_suicides, y = tmp, by=c("entity", "year"), all.x=TRUE)
rm(tmp)

##############################################################
# prevalence of bipolar disorders (OWID, poulation age-standardized)
##############################################################
tmp <- first(other_owid_big_list["prevalence_of_bipolar_disorder_in_males_vs_females"]) %>% select(-"code") %>% select(-"Population") %>% select(-"Countries Continents") 
names(tmp)[names(tmp) == "Prevalence - Bipolar disorder - Sex: Male - Age: Age-standardized (Percent)"] <- "m_prev_bipolar"
names(tmp)[names(tmp) == "Prevalence - Bipolar disorder - Sex: Female - Age: Age-standardized (Percent)"] <- "f_prev_bipolar"
OWID_gross_suicides <- merge(x = OWID_gross_suicides, y = tmp, by=c("entity", "year"), all.x=TRUE)
rm(tmp)

##############################################################
# prevalence of schizophrenia (OWID, poulation age-standardized)
##############################################################
tmp <- first(other_owid_big_list["prevalence_of_schizophrenia_in_males_vs_females"]) %>% select(-"code") %>% select(-"Population") %>% select(-"Countries Continents") 
names(tmp)[names(tmp) == "Prevalence - Schizophrenia - Sex: Male - Age: Age-standardized (Percent)"] <- "m_prev_schizoph"
names(tmp)[names(tmp) == "Prevalence - Schizophrenia - Sex: Female - Age: Age-standardized (Percent)"] <- "f_prev_schizoph"
OWID_gross_suicides <- merge(x = OWID_gross_suicides, y = tmp, by=c("entity", "year"), all.x=TRUE)
rm(tmp)

##############################################################
# join left the HDI-data from UNDP_HDI
##############################################################
OWID_gross_suicides <- left_join(OWID_gross_suicides, (UNDP_HDI %>% select (-country)), by=c("code", "year"))

##############################################################
# join left the GII-data from UNDP_GII
##############################################################
OWID_gross_suicides <- left_join(OWID_gross_suicides, (UNDP_GII %>% select (-country)), by=c("code", "year"))

##############################################################
# join left the GDP-data from Madddison Project, see: https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2020
##############################################################
# population differs between Maddison and UN data
OWID_gross_suicides <- left_join(OWID_gross_suicides, (maddison_gdp_data %>% select (-population) %>% select (-entity)), by=c("code", "year"))
OWID_gross_suicides <- OWID_gross_suicides %>% mutate(dollars_per_day = gdppc/365)

##############################################################
# Add data on People Living in Poverty from OWID github
##############################################################
fname <- "https://nyc3.digitaloceanspaces.com/owid-public/data/poverty/pip_dataset.csv"
if(!file.exists("./pip_dataset.csv")){
     download.file(fname,"./pip_dataset.csv",
                   method="curl",
                   mode="wb")
}
pip_data <- read.csv(file = "./pip_dataset.csv", header = TRUE, sep = ",")
pip_data <- pip_data %>% filter(year >= 1990 & year <= 2017 & ppp_version == 2017 & reporting_level == "national") %>% filter(!is.na(gini)) %>% mutate(ID = row_number())
pip_data <- pip_data[which(!rownames(pip_data) %in% (pip_data[, 1:2][duplicated(pip_data[, 1:2]),] %>% rownames())), ]
pip_data <- pip_data[, c("country", "year", "headcount_ratio_100", "headcount_ratio_1000", "gini")]
colnames(pip_data)[1] <- "entity"
colnames(pip_data)[3] <- "less_1_ratio"
colnames(pip_data)[4] <- "less_10_ratio"
OWID_gross_suicides <- left_join(OWID_gross_suicides, pip_data, by=c("entity", "year"))

##############################################################
# join left the self reprted trust variable
##############################################################
tmp <- first(other_owid_big_list["self_reported_trust_attitudes"])
colnames(tmp)[4] <- "self_rep_trust"
tmp <- tmp %>%  group_by (code) %>% summarize(entity = first(entity),self_rep_trust= mean(self_rep_trust) )
OWID_gross_suicides <- left_join(OWID_gross_suicides, (tmp %>% select (-entity)), by="code")
rm(tmp)

##############################################################
# Add data on Religious Commitment from PEW Research
##############################################################
fname <- "https://www.pewresearch.org/religion/wp-content/uploads/sites/7/2018/06/ReligiousCommitment-FULL-WEB.pdf"
if(!file.exists("./ReligiousCommitment-FULL-WEB.pdf")){
     download.file(fname,"./ReligiousCommitment-FULL-WEB.pdf",
                   method="curl",
                   mode="wb") # "wb" means "write binary"
}
txt <- pdf_text("./ReligiousCommitment-FULL-WEB.pdf")
raw_data_religious_commitment <- txt[65:68]

tab <- str_split(raw_data_religious_commitment, "\n")
tab <- rbind(
    tab[[1]][13:49] %>% str_trim %>% str_split("\\s{2,}", simplify = TRUE) %>% data.frame(stringsAsFactors = FALSE),
    tab[[2]][ 9:43] %>% str_trim %>% str_split("\\s{2,}", simplify = TRUE) %>% data.frame(stringsAsFactors = FALSE),
    tab[[3]][10:44] %>% str_trim %>% str_split("\\s{2,}", simplify = TRUE) %>% data.frame(stringsAsFactors = FALSE),
    tab[[4]][10:20] %>% str_trim %>% str_split("\\s{2,}", simplify = TRUE) %>% data.frame(stringsAsFactors = FALSE)
)

##############################################################
# some manual reworking is necessary as missing value are represented by blanks in the pdf, 
# also in the first lines of the tables there are percent signs, 
# and some lines of the pdf are distributed over multiple lines in the scanned data
##############################################################
## World
tab[1, 2:13] <- tab[1, 2:13] %>% str_sub(1, 2)
## Latin America-Caribbean
tab[4,1] <- paste0(tab[4,1], tab[6,1])
tab[4,2:13] <- tab[5,1:12]
tab <- tab[-6, ]
tab <- tab[-5, ]
## Middle East-North Africa
tab[5,1] <- paste0(tab[5,1], tab[7,1])
tab[5,2:13] <- tab[6,1:12]
tab <- tab[-7, ]
tab <- tab[-6, ]
## Sub-Saharan Africa
tab[6,1] <- paste0(tab[6,1], " ", tab[8,1])
tab[6,2:13] <- tab[7,1:12]
tab <- tab[-8, ]
tab <- tab[-7, ]

## Burkina Faso
tab[23,13] <- tab[23, 7]
tab[23, 7]  <- ""
tab[23,10] <- tab[23, 6]
tab[23, 6]  <- ""
tab[23, 9] <- tab[23, 5]
tab[23, 5]  <- ""
tab[23, 6] <- tab[23, 4]
tab[23, 4]  <- ""
tab[23, 5] <- tab[23, 3]
tab[23, 3]  <- ""

## Czech Republic
tab[32, 2:13] <- tab[32, 2:13] %>% str_sub(1, 2)

## India
tab[52, 12:13] <- tab[52, 9:10]
tab[52, 8:10] <- tab[52, 6:8]
tab[52, 7] <- ""
tab[52, 4:6] <- tab[52, 3:5]
tab[52, 3] <- ""

## Japan
tab[59, 12:13] <- tab[59, 9:10]
tab[59, 8:10] <- tab[59, 6:8]
tab[59, 7] <- ""
tab[59, 4:6] <- tab[59, 3:5]
tab[59, 3] <- ""

## Liberia
tab[67, 8:13] <- tab[67, 7:12]
tab[67, 7:8] <- c("85%", "83%")
tab[67, 2:13] <- str_replace(tab[67, 2:13], "%", "")

## Tunisia
tab[102, 2:13] <- str_replace(tab[102, 2:13], "%", "")

## Vietnam
tab[111, 12:13] <- tab[111, 9:10]
tab[111, 8:10] <- tab[111, 6:8]
tab[111, 7] <- ""
tab[111, 4:6] <- tab[111, 3:5]
tab[111, 3] <- ""

colnames(tab) <- c("entity", "all_affiliate", "all_weekly", "all_pray_d", "all_veryimp", "u40_affiliate", "u40_weekly", "u40_pray_d", "u40_veryimp", "40p_affiliate", "40p_weekly", "40p_pray_d", "40p_veryimp")
rel_commitment <- tab
rel_commitment <- rel_commitment %>% mutate_at(-1, parse_number) %>% as_tibble()
rm (tab)
### renaming entities based on the results of: anti_join(rel_commitment, OWID_gross_suicides, by="entity")
rel_commitment[which(rel_commitment$entity =="Bosnia-Herz."), ]$entity <- "Bosnia and Herzegovina"
rel_commitment[which(rel_commitment$entity =="Czech Republic"), ]$entity <- "Czechia"
rel_commitment[which(rel_commitment$entity =="Congo DR"), ]$entity <- "Democratic Republic of Congo"
rel_commitment[which(rel_commitment$entity =="Dominican Rep."), ]$entity <- "Dominican Republic"
rel_commitment[which(rel_commitment$entity =="Palestinian Terr."), ]$entity <- "Palestine"

OWID_gross_suicides <- left_join(OWID_gross_suicides, (rel_commitment %>% select(c("entity", "all_veryimp"))), by="entity")

##############################################################
# Add data on "Happiness" based on World Happiness Report: https://worldhappiness.report/ed/2022/
# https://happiness-report.s3.amazonaws.com/2022/DataForTable2.1.xls => for some reason there is a difference between WHR22 - data and OWID-data: apparently OWID data use wrong years
##############################################################
fname <- "https://happiness-report.s3.amazonaws.com/2022/DataForTable2.1.xls"
if(!file.exists("./DataForTable2.1.xls")){
     download.file(fname,"./DataForTable2.1.xls",
                   method="curl",
                   mode="wb") # "wb" means "write binary"
}
whr22_data <- read_excel("./DataForTable2.1.xls")
colnames(whr22_data)[1] <- "entity"
colnames(whr22_data)[3] <- "life_ladder"
colnames(whr22_data)[10] <- "positive_affect"
colnames(whr22_data)[11] <- "negative_affect"
whr22_data <- whr22_data[, c(1,2,3,10,11)]
### renaming entities based on the results of: anti_join(whr22_data, OWID_gross_suicides, by="entity")
whr22_data[which(whr22_data$entity =="Congo (Brazzaville)"), ]$entity <- "Congo"
whr22_data[which(whr22_data$entity =="Congo (Kinshasa)"), ]$entity <- "Democratic Republic of Congo"
whr22_data[which(whr22_data$entity =="Ivory Coast"), ]$entity <- "Cote d'Ivoire"
whr22_data[which(whr22_data$entity =="Palestinian Territories"), ]$entity <- "Palestine"
whr22_data[which(whr22_data$entity =="Taiwan Province of China"), ]$entity <- "Taiwan"

##############################################################
# join left the WorldHappiness Report 2022-data from https://worldhappiness.report/ed/2022/
##############################################################
OWID_gross_suicides <- left_join(OWID_gross_suicides, whr22_data, by=c("entity", "year"))

OWID_gross_suicides_no_imputation <- OWID_gross_suicides

##############################################################
# in the following: IMPUTATION of MISSING VALUES by linear regression
# IMPUTATION is mostly likely somewhat error-prone as linearity of trends is not guaranteed
##############################################################


##############################################################
# identify entities where hdi is missing & put those in a temporary df, excluding multi-national entities
##############################################################
tmp <- OWID_gross_suicides[which(is.na(OWID_gross_suicides$hdi)),  ] %>% select(c(entity, hdi)) %>% unique()
tmp <- tmp %>% filter(entity != "World") %>% filter(entity != "G20") %>% filter(entity != "OECD Countries") %>% filter(!grepl("World Bank ", entity)) %>% filter(!grepl("(WB)", entity)) %>% filter(!grepl("(WHO)", entity))

##############################################################
# impute missing hdi-data by linear regression
##############################################################
for(i in 1:nrow(tmp)) {
    tmp_mean <- OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity), ] %>% select(c(year, hdi)) 
    if ( !is.na(tmp_mean$hdi %>% mean(na.rm=TRUE)) ) {
        m <- lm(hdi ~ year, data=tmp_mean)
        OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$hdi) ), ]$hdi <- m$coefficients[1] + (m$coefficients[2]*OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$hdi) ), ]$year)
        rm(m, tmp_mean)
    }
}
rm(tmp)

##############################################################
# identify entities where gii is missing & put those in a temporary df, excluding multi-national entities
##############################################################
tmp <- OWID_gross_suicides[which(is.na(OWID_gross_suicides$gii)),  ] %>% select(c(entity, gii)) %>% unique()
tmp <- tmp %>% filter(!(entity %in% c("World", "G20", "OECD Countries"))) %>% filter(!grepl("World Bank ", entity)) %>% filter(!grepl("(WB)", entity)) %>% filter(!grepl("(WHO)", entity))

##############################################################
# impute missing gii-data by linear regression
##############################################################
for(i in 1:nrow(tmp)) {
    tmp_mean <- OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity), ] %>% select(c(year, gii)) 
    if ( !is.na(tmp_mean$gii %>% mean(na.rm=TRUE)) ) {
        m <- lm(gii ~ year, data=tmp_mean)
        OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$gii) ), ]$gii <- m$coefficients[1] + (m$coefficients[2]*OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$gii) ), ]$year)
        rm(m, tmp_mean)
    }
}
rm(tmp)

##############################################################
# identify entities where life_ladder, positive affect and negative_affect is missing & put those in a temporary df, excluding multi-national entities
##############################################################
tmp <- OWID_gross_suicides[which(is.na(OWID_gross_suicides$life_ladder)),  ] %>% select(c(entity, life_ladder)) %>% unique()
tmp <- tmp %>% filter(!(entity %in% c("World", "World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income", "World Bank Upper Middle Income"))) %>% filter(!grepl("(OECD)", entity)) %>% filter(!grepl("(WHO)", entity)) %>% filter(!grepl("(WB)", entity))

##############################################################
# impute missing life_ladder-data by linear regression
##############################################################
for(i in 1:nrow(tmp)) {
    tmp_mean <- OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity), ] %>% select(c(year, life_ladder)) 
    if ( !is.na(tmp_mean$life_ladder %>% mean(na.rm=TRUE)) ) {
        m <- lm(life_ladder ~ year, data=tmp_mean)
        OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$life_ladder) ), ]$life_ladder <- m$coefficients[1] + (m$coefficients[2]*OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$life_ladder) ), ]$year)
        rm(m, tmp_mean)
    }
}
##############################################################
# impute missing positive affect-data by linear regression
##############################################################
for(i in 1:nrow(tmp)) {
    tmp_mean <- OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity), ] %>% select(c(year, positive_affect)) 
    if ( !is.na(tmp_mean$positive_affect %>% mean(na.rm=TRUE)) ) {
        m <- lm(positive_affect ~ year, data=tmp_mean)
        OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$positive_affect) ), ]$positive_affect <- m$coefficients[1] + (m$coefficients[2]*OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$positive_affect) ), ]$year)
        rm(m, tmp_mean)
    }
}
##############################################################
# impute missing negative_affect-data by linear regression
##############################################################
for(i in 1:nrow(tmp)) {
    tmp_mean <- OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity), ] %>% select(c(year, negative_affect)) 
    if ( !is.na(tmp_mean$negative_affect %>% mean(na.rm=TRUE)) ) {
        m <- lm(negative_affect ~ year, data=tmp_mean)
        OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$negative_affect) ), ]$negative_affect <- m$coefficients[1] + (m$coefficients[2]*OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$negative_affect) ), ]$year)
        rm(m, tmp_mean)
    }
}
rm(tmp)

##############################################################
# identify entities where less_1_ratio, less_10_ratio, and gini is missing & put those in a temporary df, excluding multi-national entities
##############################################################
tmp <- OWID_gross_suicides[which(is.na(OWID_gross_suicides$gini)),  ] %>% select(c(entity, gini)) %>% unique()
tmp <- tmp %>% filter(!(entity %in% c("World", "World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income", "World Bank Upper Middle Income"))) %>% filter(!grepl("(OECD)", entity)) %>% filter(!grepl("(WHO)", entity)) %>% filter(!grepl("(WB)", entity))

##############################################################
# impute missing less_1_ratio-data by linear regression
##############################################################
for(i in 1:nrow(tmp)) {
    tmp_mean <- OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity), ] %>% select(c(year, less_1_ratio)) 
    if ( !is.na(tmp_mean$less_1_ratio %>% mean(na.rm=TRUE)) ) {
        m <- lm(less_1_ratio ~ year, data=tmp_mean)
        OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$less_1_ratio) ), ]$less_1_ratio <- m$coefficients[1] + (m$coefficients[2]*OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$less_1_ratio) ), ]$year)
        rm(m, tmp_mean)
    }
}
##############################################################
# impute missing less_10_ratio-data by linear regression
##############################################################
for(i in 1:nrow(tmp)) {
    tmp_mean <- OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity), ] %>% select(c(year, less_10_ratio)) 
    if ( !is.na(tmp_mean$less_10_ratio %>% mean(na.rm=TRUE)) ) {
        m <- lm(less_10_ratio ~ year, data=tmp_mean)
        OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$less_10_ratio) ), ]$less_10_ratio <- m$coefficients[1] + (m$coefficients[2]*OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$less_10_ratio) ), ]$year)
        rm(m, tmp_mean)
    }
}
##############################################################
# impute missing gini-data by linear regression
##############################################################
for(i in 1:nrow(tmp)) {
    tmp_mean <- OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity), ] %>% select(c(year, gini)) 
    if ( !is.na(tmp_mean$gini %>% mean(na.rm=TRUE)) ) {
        m <- lm(gini ~ year, data=tmp_mean)
        OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$gini) ), ]$gini <- m$coefficients[1] + (m$coefficients[2]*OWID_gross_suicides[which(OWID_gross_suicides$entity==tmp[i,]$entity & is.na(OWID_gross_suicides$gini) ), ]$year)
        rm(m, tmp_mean)
    }
}
rm(tmp)

# saveRDS(OWID_gross_suicides, "./OWID_gross_suicides.rds")
# save.image("./.RData")

OWID_gross_suicides_country <- OWID_gross_suicides_no_imputation %>% group_by(entity) %>% summarize (
age70plus                  = mean(age70plus                , na.rm=TRUE ),
age50_69                   = mean(age50_69                 , na.rm=TRUE ),
age15_49                   = mean(age15_49                 , na.rm=TRUE ),
age5_14                    = mean(age5_14                  , na.rm=TRUE ),
yearly_sum                 = 0                                           ,
population                 = mean(population               , na.rm=TRUE ),
m_population               = mean(m_population             , na.rm=TRUE ),
f_population               = mean(f_population             , na.rm=TRUE ),
popage65plus               = mean(popage65plus             , na.rm=TRUE ),
popage25to64               = mean(popage25to64             , na.rm=TRUE ),
popage15to24               = mean(popage15to24             , na.rm=TRUE ),
popage5to14                = mean(popage5to14              , na.rm=TRUE ),
popage0to4                 = mean(popage0to4               , na.rm=TRUE ),
f_suic_rate                = mean(f_suic_rate              , na.rm=TRUE ),
median_age                 = mean(median_age               , na.rm=TRUE ),
m_suic_rate                = mean(m_suic_rate              , na.rm=TRUE ),
mf_slfhrmdeath_age_stand   = mean(mf_slfhrmdeath_age_stand , na.rm=TRUE ),
mf_ipvioldeath_age_stand   = mean(mf_ipvioldeath_age_stand , na.rm=TRUE ),
suic_rate                  = mean(suic_rate                , na.rm=TRUE ),
mf_prev_md_s_ab_age_stand  = mean(mf_prev_md_s_ab_age_stand, na.rm=TRUE ),
m_prev_adhd                = mean(m_prev_adhd              , na.rm=TRUE ),
f_prev_adhd                = mean(f_prev_adhd              , na.rm=TRUE ),
m_prev_alco                = mean(m_prev_alco              , na.rm=TRUE ),
f_prev_alco                = mean(f_prev_alco              , na.rm=TRUE ),
m_prev_depr                = mean(m_prev_depr              , na.rm=TRUE ),
f_prev_depr                = mean(f_prev_depr              , na.rm=TRUE ),
m_prev_anxiety             = mean(m_prev_anxiety           , na.rm=TRUE ),
f_prev_anxiety             = mean(f_prev_anxiety           , na.rm=TRUE ),
m_prev_bipolar             = mean(m_prev_bipolar           , na.rm=TRUE ),
f_prev_bipolar             = mean(f_prev_bipolar           , na.rm=TRUE ),
m_prev_schizoph            = mean(m_prev_schizoph          , na.rm=TRUE ),
f_prev_schizoph            = mean(f_prev_schizoph          , na.rm=TRUE ),
hdi                        = mean(hdi                      , na.rm=TRUE ),
gii                        = mean(gii                      , na.rm=TRUE ),
gdppc                      = mean(gdppc                    , na.rm=TRUE ),
dollars_per_day            = mean(dollars_per_day          , na.rm=TRUE ),
less_1_ratio               = mean(less_1_ratio             , na.rm=TRUE ),
less_10_ratio              = mean(less_10_ratio            , na.rm=TRUE ),
gini                       = mean(gini                     , na.rm=TRUE ),
self_rep_trust             = mean(self_rep_trust           , na.rm=TRUE ),
all_veryimp                = mean(all_veryimp              , na.rm=TRUE ),
life_ladder                = mean(life_ladder              , na.rm=TRUE ),
positive_affect            = mean(positive_affect          , na.rm=TRUE ),
negative_affect            = mean(negative_affect          , na.rm=TRUE )) %>% mutate(yearly_sum = age70plus+age50_69+age15_49+age5_14)

#####################################################################################
# unload packages that are no longer needed
#####################################################################################
detach("package:owidR", unload = TRUE)
detach("package:pdftools", unload = TRUE)
detach("package:openxlsx", unload = TRUE)
detach("package:data.table", unload = TRUE)


#####################################################################################
# World map of OWID Suicides per 100.000 people
#####################################################################################
world %>%
  merge(OWID_gross_suicides_rates, by.x = "region", by.y = "entity", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = avg_suicide_rate)) +
  geom_polygon(color = "gray90", size = 0.1) +
  scale_fill_viridis(name="suicides per\n100,000 popul.\nover entire\ntime-span\ncovered", option="H", begin=0.6, end=0.9, na.value = "white", labels = fancy_scientific, limits = c(0, 85)) +
  theme(aspect.ratio=0.6, axis.text = element_blank(), axis.title = element_blank())


#####################################################################################
# Time trends of suicide rates
#####################################################################################
# plot the trend lines for suicides in all countries except Greenland and multi-national entities, 
# and entities with a population less or equal to 100,000 (which might induce a bias)
##############################################################
OWID_gross_suicides_no_imputation %>% 
filter(!(entity %in% c("World", "G20", "OECD Countries"))) %>% filter(!grepl("World Bank ", entity)) %>% filter(!grepl("(WB)", entity)) %>% filter(!grepl("(WHO)", entity)) %>% filter(!is.na(suic_rate) & population >= 100000) %>% 
ggplot(aes(x=year, y=suic_rate)) + geom_line(aes(colour=code), show.legend = FALSE) + labs(x = "year", y = "suicide rate per 100,000 population")


##############################################################
# violin plot of suicide rates in all countries except entities with a population less or equal to 100,000
##############################################################
OWID_gross_suicides_country %>% filter(population >= 100000) %>% ggplot(aes(x=1, y=suic_rate)) + geom_violin(scale="area", fill="#FFCCCC")+ 
  stat_summary(fun = "mean", geom = "crossbar", width = 0.25, aes(color = "Mean")) +
  stat_summary(fun = "median", geom = "crossbar", width= 0.25, aes(color = "Median")) +
  scale_colour_manual(values = c("#0072B2", "#D55E00"), name = "") + 
  labs(y = "suicide rate per 100,000 population") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

##############################################################
# prepare a subset of data for clustering an dplotting a dissimilarity matrix
##############################################################
cd <- OWID_gross_suicides_no_imputation %>% filter(!(entity %in% c("World", "G20", "OECD Countries"))) %>%
  filter(!grepl("World Bank ", entity)) %>% filter(!grepl("(WB)", entity)) %>% filter(!grepl("(WHO)", entity)) %>%
  filter(!is.na(suic_rate) & population >= 100000) %>% select(c(code,year,suic_rate)) %>%  
  group_by(code) %>% pivot_wider(names_from = year, values_from = suic_rate)                          # transform data into matrix cd (cluster data)
cd.c <- cd[complete.cases(cd), ]                                                                      # take only complete cases
fviz_dist(dist(scale(cd.c[, -1], center=T, scale=T)), show_labels = FALSE) + labs(title = "suicide: temporal trends")

##############################################################
# determining clustering method
##############################################################
if(!require(RankAggreg)) install.packages("RankAggreg", repos = "http://cran.us.r-project.org")
library(RankAggreg)
clv <- clValid(scale(cd.c[, -1], center=T, scale=T), nClust=2:7, clMethods=c("hierarchical", "pam", "kmeans", "fanny"), validation="internal")
summary(clv) 
res <- getRanksWeights(clv)
CEWS <- RankAggreg(x=res$ranks, k=5, weights=res$weights, seed=123, verbose=FALSE)
CEWS
detach("package:RankAggreg", unload = TRUE)

##############################################################
# plot the min-max normalized trend lines for suicides in all countries except Greenland and multi-national entities, 
# and entities with a population less or equal to 100,000 (which might induce a bias)
##############################################################
nor <- function(x) { (x - min(x))/(max(x)-min(x)) }                                                   # function for min-max normalization
calc_wss <- function(df) sum(as.matrix(dist(df)^2)) / (2 * nrow(df))                                  # calculate within sum of squares for a matrix, see: https://stackoverflow.com/questions/54419329/calculate-total-sum-of-squares-between-clusters-in-r
cd.n <- cd.c[, -1]                                                                                    # strip of the column with the country code
cd.n <- t(apply(cd.n,1,nor))                                                                          # perform min-max normalization
cd.s <- t(apply(cd.c[, -1],1,scale))                                                                  # perform Z-normalization

wss <- calc_wss(cd.n)                                                                                 # calc within-sum-of-squares if no clustering is done, i.e.: number of clusters = 1
for (i in 2:10) wss[i] <- sum(kmeans(cd.n, centers=i)$withinss)                                       # an arbitrary max. number of cluster of 10 is set
##############################################################
# the "elbow"-plot based on Z-standradized scores is only marginally different from that based on min-max-normalization, and thus not included in the final report
##############################################################
# wss <- calc_wss(cd.s)
# for (i in 2:10) wss[i] <- sum(kmeans(cd.s, centers=i)$withinss)
##############################################################

cd.n.hclust <- as.data.frame(cd.n) 
rownames(cd.n.hclust) <- cd.c[complete.cases(cd.c), ]$code
h <- hclust(dist(abs(cor(na.omit(t(cd.n.hclust))))))
hclust.3.groups <- cutree(h, k = 3)
hclust.2.groups <- cutree(h, k = 2)
pamk.cd.n <- pamk(cd.n)$pamobject$clustering
pamk.cd.s <- pamk(cd.s)$pamobject$clustering
pam.cd.n <- pam(cd.n, 3)$clustering

##############################################################
# put data in wide format
##############################################################
out <- head(round(cd.n[, 1:14], digits=2), 2)
kbl(out, caption = "partial view of suicide rates-data transformed into a wide format") %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = "condensed", full_width = T, position = "left") 

##############################################################
# plot hierarchical clustering
##############################################################
plot(h, cex = 0.40, main = "", xlab = "", sub = "") 

##############################################################
# Determining the optimal number of clusters to choose
##############################################################
plot(1:10, wss, type="b", xlab="number of Clusters", ylab="within groups sum of squares")
gap.stat <- clusGap(cd.n, kmeans, 5)
gap.stat$Tab
qplot(1:nrow(gap.stat$Tab), gap.stat$Tab[, "gap"], xlab = "no. of clusters", ylab="gap", main="Estimating no. of clusters using gap-statistic", geom=c("line", "point")) + geom_errorbar(aes(1:nrow(gap.stat$Tab), ymin=gap.stat$Tab[, "gap"]-gap.stat$Tab[, "SE.sim"], ymax=gap.stat$Tab[, "gap"]+gap.stat$Tab[, "SE.sim"]), size=0.3, width=0.2)
rownames(cd.n) <- cd.c[complete.cases(cd.c), ]$code
fviz_cluster(pam(cd.n, 2, keep.diss = TRUE), data = data, palette = c("#FC4E07", "#00AFBB"), ellipse.type = "euclid", star.plot = F, repel = T, ggtheme = theme_minimal() )
rownames(cd.n) <- 1:nrow(cd.n)

##############################################################
# plot PAM clustering
##############################################################
fviz_cluster(pam(cd.n, 2), ellipse.type="norm")
plot(silhouette(pam(cd.n, 2, keep.diss = TRUE)))

##############################################################
# Using the NbClust-package
##############################################################
if(!require(NbClust)) { install.packages("NbClust", repos = "http://cran.us.r-project.org") }
library(NbClust)
set.seed(1)
nbclust.cd.n <- NbClust(data = cd.n, distance = "euclidean",  method = "kmeans", index = "alllong", alphaBeale = 0.1)
detach("package:NbClust", unload = TRUE)


##############################################################
# Doing time-series clustering using *TSclust*
##############################################################
theme_set(theme_pubr())
pam.clus.cor <- pam(dist(diss(cd.n,"COR")), 3)
pam.clus.cor$data <- diss(cd.n,"COR")
pam.cor <- fviz_cluster(pam.clus.cor, geom = "point", main = "") 

pam.clus.euc <- pam(dist(diss(cd.n,"EUC")), 3)
pam.clus.euc$data <- diss(cd.n,"EUC")
pam.euc <- fviz_cluster(pam.clus.euc, geom = "point", main = "") 

pam.clus.dtw <- pam(dist(diss(cd.n,"DTWARP")), 3)
pam.clus.dtw$data <- diss(cd.n,"DTWARP")
pam.dtw <- fviz_cluster(pam.clus.dtw, geom = "point", main = "") 

set.seed(1)
kmeans.clus.cor <- kmeans(scale(diss(cd.n,"COR")), 3, nstart = 25)
kmeans.cor <- fviz_cluster(kmeans(scale(diss(cd.n,"COR")), 3, nstart = 25), data = diss(cd.n,"COR"), palette = c("#FE9F6F", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex", main = "")

set.seed(1)
kmeans.clus.euc <- kmeans(scale(diss(cd.n,"EUC")), 3, nstart = 25)
kmeans.euc <- fviz_cluster(kmeans(scale(diss(cd.n,"EUC")), 3, nstart = 25), data = diss(cd.n,"EUC"), palette = c("#FE9F6F", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex", main = "")

set.seed(1)
kmeans.clus.dtw <- kmeans(scale(diss(cd.n,"DTWARP")), 3, nstart = 25)
kmeans.dtw <- fviz_cluster(kmeans(scale(diss(cd.n,"DTWARP")), 3, nstart = 25), data = diss(cd.n,"DTWARP"), palette = c("#FE9F6F", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex", main = "")

ggarrange(pam.cor, kmeans.cor, pam.euc, kmeans.euc, pam.dtw, kmeans.dtw,
          labels = c("pam (cor)", "kmeans (cor)", "pam (euc)", "kmeans (euc)", "pam (dtwarp)", "kmeans (dtwarp)"),
          ncol = 2, nrow = 3)

##############################################################
# presenting a table on time series clustering
##############################################################
kbl(
 cbind(
  c(
   "hierarchical 3 clusters",
   "pam 3 clusters",
   "time-series (using TSclust) pam (cor, 3 clu.)",
   "time-series (using TSclust) pam (euc, 3 clu.)",
   "time-series (using TSclust) pam (dtwarp, 3 clu.)",
   "time-series (using TSclust) k-means (cor, 3 clu.)",
   "time-series (using TSclust) k-means (euc, 3 clu.)",
   "time-series (using TSclust) k-means (dtwarp, 3 clu.)"
  ),
  c(
   round(wcss(cd.n, hclust.3.groups)$WCSS, digits=2),
   round(wcss(cd.n, pam.cd.n)$WCSS, digits=2),
   round(wcss(cd.n, pam.clus.cor$clustering)$WCSS, digits=2),
   round(wcss(cd.n, pam.clus.euc$clustering)$WCSS, digits=2),
   round(wcss(cd.n, pam.clus.dtw$clustering)$WCSS, digits=2),
   round(wcss(cd.n, kmeans.clus.cor$cluster)$WCSS, digits=2),
   round(wcss(cd.n, kmeans.clus.euc$cluster)$WCSS, digits=2),
   round(wcss(cd.n, kmeans.clus.dtw$cluster)$WCSS, digits=2)
  )
 )
, booktabs = T, caption = "within cluster sum of squares", col.names=(c("clustering method", "wcss"))) %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = c("striped", "condensed"), full_width = F, position = "left") %>% column_spec(1, width = "9.5cm") %>% column_spec(2, width = "4cm", bold = TRUE)

##############################################################
# plot the world map data with colors indicating to which cluster an entity belongs
##############################################################
world %>%
  merge(
  left_join( OWID_gross_suicides_rates, 
  (
  left_join(
    cbind((as_tibble(kmeans.clus.euc$cluster)%>%`colnames<-`("cluster")), 
           code=cd.c[complete.cases(cd.c), ]$code)
    ,  
    (OWID_gross_suicides %>% select(c(entity, code)) %>% unique()), 
  by = "code") %>% 
  select(c(entity, cluster))
  ), by = "entity")
  , 
  by.x = "region", by.y = "entity", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = factor(cluster))) +
  geom_polygon(color = "gray90", linewidth = 0.1) +
  scale_fill_manual(values=c("#FE9F6F", "#00AFBB", "#E7B800", "#E5E4E2"), name="Results of \nclustering", na.value = "#E5E4E2", labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster NA")) +
  theme(aspect.ratio=0.6, axis.line=element_blank(), axis.ticks=element_blank(), axis.text = element_blank(), axis.title = element_blank())
detach("package:viridis", unload = TRUE)


##############################################################
# Attempt at graphing factors that contribute to suicidality
##############################################################
set.seed(1)
if(!require(ggdag)) install.packages("ggdag", repos = "http://cran.us.r-project.org")
library(ggdag)
suic_dag <- dagify(suicide ~ choice,
				   choice ~ mental + livingconditions + values + sex + wellbeing,
				   values ~ relaffil,
				   # wellbeing ~ selfreptrust + lifeladder + posaffect + negaffect,
				   # mental ~ alc + depr + anx + bipol + schiz,
				   # socdev ~ hdi + gii + gini + socwealth, 
				   # socwealth ~ gdppc + less10ratio,
				   livingconditions ~ age + mental + socdev,
				   labels = c(mental = "clinical condition",
							  socdev = "societal development", 
							  wellbeing = "well-being", 
							  age = "age", 
							  relaffil = "religious affiliation", 
							  selfreptrust = "self reported trust", 
							  lifeladder = "happiness", 
							  posaffect = "enjoyment and laughter", 
							  negaffect = "worry, sadness, and anger",
							  hdi = "human development\nindex",
							  gii = "gender inequality\nindex", 
							  alc = "alcohol abuse",
							  depr = "depressive condition",
							  anx = "Aaxiety disorder",
							  bipol = "bipolar disorder",
							  schiz = "schizophrenia",
							  gini = "gini - index",
							  gdppc = "gross Domestic\nproduct per capita",
							  less10ratio = "ratio of people\nliving on < 10$ per day",
							  socwealth = "societal wealth", 
							  livingconditions = "living conditions",
							  values = "patient's values",
							  choice = "choice",
							  sex = "sex",
							  suicide = "SUICIDE"),
							  outcome = "suicide") %>% ggdag (text = F, use_labels = "label", edge_type= "link_arc", node_size = 6)
q <- ggplot_build(suic_dag)
q$data[[3]][8, ]$colour <- "red"
q$data[[4]][q$data[[4]]$label == "societal development", ]$y <- 3.2
q$data[[4]][q$data[[4]]$label == "living conditions", ]$x <- -0.2
q$data[[4]][q$data[[4]]$label == "patient's values", ]$y <- -1.5
q$data[[4]][q$data[[4]]$label == "clinical condition", ]$y <- 1
q$data[[4]][q$data[[4]]$label == "clinical condition", ]$x <- 0.75
q$data[[4]][q$data[[4]]$label == "choice", ]$x <- -1.05
q$data[[4]][q$data[[4]]$label == "choice", ]$y <- 0.65
q <- ggplot_gtable(q)
plot(q)	
detach("package:ggdag", unload = TRUE)

##############################################################
# plot of variables possibly contributing to suicide rate and demostrating aoutlier
##############################################################
qplot(OWID_gross_suicides_country$suic_rate, geom = "boxplot", notch=TRUE, ylab="", xlab="suicide rate")

OWID_gross_suicides_country <- OWID_gross_suicides_country %>% filter(entity!="Greenland")

mf1 <- OWID_gross_suicides_country %>% ggplot(aes(suic_rate, ((m_prev_adhd    /100*m_population)+(f_prev_adhd    /100*f_population))/population*100)) + geom_point(pch=4) + labs(x="both sexes suicide rate", y="prev. adhd"    ) + ylim(0, 6)  + xlim(0,70) + theme_classic()
mf2 <- OWID_gross_suicides_country %>% ggplot(aes(suic_rate, ((m_prev_alco    /100*m_population)+(f_prev_alco    /100*f_population))/population*100)) + geom_point(pch=4) + labs(x="both sexes suicide rate", y="prev. alco"    ) + ylim(0, 8)  + xlim(0,70) + theme_classic()
mf3 <- OWID_gross_suicides_country %>% ggplot(aes(suic_rate, ((m_prev_anxiety /100*m_population)+(f_prev_anxiety /100*f_population))/population*100)) + geom_point(pch=4) + labs(x="both sexes suicide rate", y="prev. anxiety" ) + ylim(0,11)  + xlim(0,70) + theme_classic()
mf4 <- OWID_gross_suicides_country %>% ggplot(aes(suic_rate, ((m_prev_depr    /100*m_population)+(f_prev_depr    /100*f_population))/population*100)) + geom_point(pch=4) + labs(x="both sexes suicide rate", y="prev. depr"    ) + ylim(0, 9)  + xlim(0,70) + theme_classic()
mf5 <- OWID_gross_suicides_country %>% ggplot(aes(suic_rate, ((m_prev_bipolar /100*m_population)+(f_prev_bipolar /100*f_population))/population*100)) + geom_point(pch=4) + labs(x="both sexes suicide rate", y="prev. bipolar" ) + ylim(0,1.8) + xlim(0,70) + theme_classic()
mf6 <- OWID_gross_suicides_country %>% ggplot(aes(suic_rate, ((m_prev_schizoph/100*m_population)+(f_prev_schizoph/100*f_population))/population*100)) + geom_point(pch=4) + labs(x="both sexes suicide rate", y="prev. schizoph") + ylim(.1,0.6)+ xlim(0,70) + theme_classic()

m1 <- OWID_gross_suicides_country %>% ggplot(aes(m_suic_rate, m_prev_adhd    )) + geom_point(pch=4) + labs(x="male suicide rate", y="") + ylim(0, 6)  + xlim(0,70) + theme_classic()
m2 <- OWID_gross_suicides_country %>% ggplot(aes(m_suic_rate, m_prev_alco    )) + geom_point(pch=4) + labs(x="male suicide rate", y="") + ylim(0, 8)  + xlim(0,70) + theme_classic()
m3 <- OWID_gross_suicides_country %>% ggplot(aes(m_suic_rate, m_prev_anxiety )) + geom_point(pch=4) + labs(x="male suicide rate", y="") + ylim(0,11)  + xlim(0,70) + theme_classic()
m4 <- OWID_gross_suicides_country %>% ggplot(aes(m_suic_rate, m_prev_depr    )) + geom_point(pch=4) + labs(x="male suicide rate", y="") + ylim(0, 9)  + xlim(0,70) + theme_classic()
m5 <- OWID_gross_suicides_country %>% ggplot(aes(m_suic_rate, m_prev_bipolar )) + geom_point(pch=4) + labs(x="male suicide rate", y="") + ylim(0,1.8) + xlim(0,70) + theme_classic()
m6 <- OWID_gross_suicides_country %>% ggplot(aes(m_suic_rate, m_prev_schizoph)) + geom_point(pch=4) + labs(x="male suicide rate", y="") + ylim(.1,0.6)+ xlim(0,70) + theme_classic()

f1 <- OWID_gross_suicides_country %>% ggplot(aes(f_suic_rate, f_prev_adhd    )) + geom_point(pch=4) + labs(x="female suicide rate", y="") + ylim(0, 6)  + xlim(0,70) + theme_classic() 
f2 <- OWID_gross_suicides_country %>% ggplot(aes(f_suic_rate, f_prev_alco    )) + geom_point(pch=4) + labs(x="female suicide rate", y="") + ylim(0, 8)  + xlim(0,70) + theme_classic() 
f3 <- OWID_gross_suicides_country %>% ggplot(aes(f_suic_rate, f_prev_anxiety )) + geom_point(pch=4) + labs(x="female suicide rate", y="") + ylim(0,11)  + xlim(0,70) + theme_classic() 
f4 <- OWID_gross_suicides_country %>% ggplot(aes(f_suic_rate, f_prev_depr    )) + geom_point(pch=4) + labs(x="female suicide rate", y="") + ylim(0, 9)  + xlim(0,70) + theme_classic() 
f5 <- OWID_gross_suicides_country %>% ggplot(aes(f_suic_rate, f_prev_bipolar )) + geom_point(pch=4) + labs(x="female suicide rate", y="") + ylim(0,1.8) + xlim(0,70) + theme_classic() 
f6 <- OWID_gross_suicides_country %>% ggplot(aes(f_suic_rate, f_prev_schizoph)) + geom_point(pch=4) + labs(x="female suicide rate", y="") + ylim(.1,0.6)+ xlim(0,70) + theme_classic() 

multiplot <- ggarrange(mf1, m1, f1, mf2, m2, f2, mf3, m3, f3, mf4, m4, f4, mf5, m5, f5, mf6, m6, f6, ncol = 3, nrow = 6)
print(multiplot)
rm(mf1, mf2, mf3, mf4, mf5, mf6, m1, m2, m3, m4, m5, m6, f1, f2, f3, f4, f5, f6) 

##############################################################
# plot sex ratio
##############################################################
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
library(ggrepel)
OWID_gross_suicides_country %>% mutate(maleproportion = m_population/population) %>% 
  ggplot(aes(suic_rate, maleproportion)) + geom_point() + 
    geom_label_repel(data= OWID_gross_suicides_country %>% mutate(maleproportion = m_population/population) %>% filter(maleproportion > 0.55),
   	  aes(label=entity)) + theme_bw()
detach("package:ggrepel", unload = TRUE)

##############################################################
# Suicidality and age
##############################################################
tmp <- OWID_gross_suicides_country %>% select(c(entity, suic_rate, age5_14, age15_49, age50_69, age70plus, popage0to4, popage5to14, popage15to24, popage25to64, popage65plus)) %>% 
    mutate(srate5_14 = age5_14 / popage5to14, srate15_49 = age15_49 / (popage15to24+(0.62*popage25to64)), srate50_69 = age50_69 / (0.38*popage25to64), srate70plus = age70plus / popage65plus) %>% 
    summarize(srate5_14 = mean(srate5_14, na.rm = T), srate15_49 = mean(srate15_49, na.rm=T), srate50_69 = mean(srate50_69,na.rm=T), srate70plus = mean(srate70plus, na.rm=T)) %>% t() %>% as.data.frame()
tmp[, 1] <- tmp[, 1]*1e5
rownames(tmp) <- c("5 to 14", "15 to 49", "50 to 69", "70+")
tmp <- cbind(tmp, OWID_gross_suicides_country %>% select(c(age5_14, age15_49, age50_69, age70plus, popage0to4, popage5to14, popage15to24, popage25to64, popage65plus)) %>% summarize(age5_14 = sum(age5_14, na.rm=T), age15_49 = sum(age15_49, na.rm=T), age50_69 = sum(age50_69, na.rm=T), age70plus = sum(age70plus, na.rm=T)) %>% t())
names(tmp) <- c("suicide rate per 100.000", "total no. of suicides") 
tmp <- t(tmp)
tmp[2, ] <- sprintf("%i", round(tmp[2,]))
tmp[1, ] <- round(as.numeric(tmp[1, ]), digits=2)
kbl(tmp, booktabs = T, caption = "suicide rates and suicides in different age groups") %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = "condensed", full_width = F, position = "left") 
rm(tmp)

##############################################################
# Suicidality and mental disorder
##############################################################
suic_mental_corr <- cbind(cbind(
  rbind(
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(((m_prev_adhd    /100*m_population)+(f_prev_adhd    /100*f_population))/population*100 , suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(((m_prev_adhd    /100*m_population)+(f_prev_adhd    /100*f_population))/population*100 , suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(((m_prev_alco    /100*m_population)+(f_prev_alco    /100*f_population))/population*100 , suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(((m_prev_alco    /100*m_population)+(f_prev_alco    /100*f_population))/population*100 , suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(((m_prev_anxiety /100*m_population)+(f_prev_anxiety /100*f_population))/population*100 , suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(((m_prev_anxiety /100*m_population)+(f_prev_anxiety /100*f_population))/population*100 , suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(((m_prev_depr    /100*m_population)+(f_prev_depr    /100*f_population))/population*100 , suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(((m_prev_depr    /100*m_population)+(f_prev_depr    /100*f_population))/population*100 , suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(((m_prev_bipolar /100*m_population)+(f_prev_bipolar /100*f_population))/population*100 , suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(((m_prev_bipolar /100*m_population)+(f_prev_bipolar /100*f_population))/population*100 , suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(((m_prev_schizoph/100*m_population)+(f_prev_schizoph/100*f_population))/population*100 , suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(((m_prev_schizoph/100*m_population)+(f_prev_schizoph/100*f_population))/population*100 , suic_rate)$p.value ))
  )
  ,
  rbind(
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(m_prev_adhd    /100*m_population, m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(m_prev_adhd    /100*m_population, m_suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(m_prev_alco    /100*m_population, m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(m_prev_alco    /100*m_population, m_suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(m_prev_anxiety /100*m_population, m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(m_prev_anxiety /100*m_population, m_suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(m_prev_depr    /100*m_population, m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(m_prev_depr    /100*m_population, m_suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(m_prev_bipolar /100*m_population, m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(m_prev_bipolar /100*m_population, m_suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(m_prev_schizoph/100*m_population, m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(m_prev_schizoph/100*m_population, m_suic_rate)$p.value ))
  )
 )
 , 
 rbind(
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(f_prev_adhd    /100*f_population, f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(f_prev_adhd    /100*f_population, f_suic_rate)$p.value )),
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(f_prev_alco    /100*f_population, f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(f_prev_alco    /100*f_population, f_suic_rate)$p.value )),
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(f_prev_anxiety /100*f_population, f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(f_prev_anxiety /100*f_population, f_suic_rate)$p.value )),
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(f_prev_depr    /100*f_population, f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(f_prev_depr    /100*f_population, f_suic_rate)$p.value )),
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(f_prev_bipolar /100*f_population, f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(f_prev_bipolar /100*f_population, f_suic_rate)$p.value )),
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(f_prev_schizoph/100*f_population, f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(f_prev_schizoph/100*f_population, f_suic_rate)$p.value ))
 )
) 
rownames(suic_mental_corr) <- c("prevalence  ADHD", "prevalence alcohol rel. dis.", "prevalence anxiety dis.", "prevalence depressive dis.", "prevalence bipolar dis.", "prevalence schizophrenia")
suic_mental_corr[,2] <- paste0("(", sprintf("%.2f", suic_mental_corr[,2]), ")")
suic_mental_corr[,4] <- paste0("(", sprintf("%.2f", suic_mental_corr[,4]), ")")
suic_mental_corr[,6] <- paste0("(", sprintf("%.2f", suic_mental_corr[,6]), ")")
suic_mental_corr[,1] <- round(suic_mental_corr[,1], digits=2)
suic_mental_corr[,3] <- round(suic_mental_corr[,3], digits=2)
suic_mental_corr[,5] <- round(suic_mental_corr[,5], digits=2)

kbl(suic_mental_corr, booktabs = T, caption = "correlations between suicide rates and rates of mental illnesses", align = "c") %>% add_header_above(c(" " = 1, "Both Sexes" = 2, "Males" = 2, "Females" = 2)) %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = "condensed", full_width = F, position = "left") %>% column_spec(3, italic = T) %>% column_spec(5, italic = T) %>% column_spec(7, italic = T) %>% column_spec(1, bold = T) %>% footnote(general = "r: Pearson correlation, p: two-sided p-value")


##############################################################
# Suicidality and religious commitment
##############################################################
suic_relig_corr <- cbind(
 cbind(
  rbind(data.frame("r" = with(OWID_gross_suicides_country, cor.test(all_veryimp, suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(all_veryimp, suic_rate)$p.value ))),
  rbind(data.frame("r" = with(OWID_gross_suicides_country, cor.test(all_veryimp, m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(all_veryimp, m_suic_rate)$p.value ))  )
 ),rbind(data.frame("r" = with(OWID_gross_suicides_country, cor.test(all_veryimp, f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(all_veryimp, f_suic_rate)$p.value )) )
) 
rownames(suic_relig_corr) <- c("importance of religious practice")
suic_relig_corr[,2] <- paste0("(", sprintf("%.2f", suic_relig_corr[,2]), ")")
suic_relig_corr[,4] <- paste0("(", sprintf("%.2f", suic_relig_corr[,4]), ")")
suic_relig_corr[,6] <- paste0("(", sprintf("%.2f", suic_relig_corr[,6]), ")")
suic_relig_corr[,1] <- round(suic_relig_corr[,1], digits=2)
suic_relig_corr[,3] <- round(suic_relig_corr[,3], digits=2)
suic_relig_corr[,5] <- round(suic_relig_corr[,5], digits=2)

kbl(suic_relig_corr, booktabs = T, caption = "correlations between suicide rates and importance of religious affiliation", align = "c") %>% add_header_above(c(" " = 1, "Both Sexes" = 2, "Males" = 2, "Females" = 2)) %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = "condensed", full_width = F, position = "left") %>% column_spec(3, italic = T) %>% column_spec(5, italic = T) %>% column_spec(7, italic = T) %>% column_spec(1, bold = T) %>% footnote(general = "r: Pearson correlation, p: two-sided p-value")

##############################################################
# Suicidality and well-being
##############################################################
suic_welbeing_corr <- cbind(cbind(
  rbind(
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(self_rep_trust,  suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(self_rep_trust,  suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(life_ladder,     suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(life_ladder,     suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(positive_affect, suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(positive_affect, suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(negative_affect, suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(negative_affect, suic_rate)$p.value ))
  )
  ,
  rbind(
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(self_rep_trust,  m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(self_rep_trust,  m_suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(life_ladder,     m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(life_ladder,     m_suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(positive_affect, m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(positive_affect, m_suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(negative_affect, m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(negative_affect, m_suic_rate)$p.value ))
  )
 )
 , 
 rbind(
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(self_rep_trust,  f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(self_rep_trust,  f_suic_rate)$p.value )),
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(life_ladder,     f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(life_ladder,     f_suic_rate)$p.value )),
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(positive_affect, f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(positive_affect, f_suic_rate)$p.value )),
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(negative_affect, f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(negative_affect, f_suic_rate)$p.value ))
 )
) 
rownames(suic_welbeing_corr) <- c("self-reported trust", "happines acc. to cantril ladder", "positive affect today or prev. day", "negative affect today or prev. day")
suic_welbeing_corr[,2] <- paste0("(", sprintf("%.2f", suic_welbeing_corr[,2]), ")")
suic_welbeing_corr[,4] <- paste0("(", sprintf("%.2f", suic_welbeing_corr[,4]), ")")
suic_welbeing_corr[,6] <- paste0("(", sprintf("%.2f", suic_welbeing_corr[,6]), ")")
suic_welbeing_corr[,1] <- round(suic_welbeing_corr[,1], digits=2)
suic_welbeing_corr[,3] <- round(suic_welbeing_corr[,3], digits=2)
suic_welbeing_corr[,5] <- round(suic_welbeing_corr[,5], digits=2)

kbl(suic_welbeing_corr, booktabs = T, caption = "correlations between suicide rates and psychological well-being", align = "c") %>% add_header_above(c(" " = 1, "Both Sexes" = 2, "Males" = 2, "Females" = 2)) %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = "condensed", full_width = F, position = "left") %>% column_spec(3, italic = T) %>% column_spec(5, italic = T) %>% column_spec(7, italic = T) %>% column_spec(1, bold = T) %>% footnote(general = "r: Pearson correlation, p: two-sided p-value")

ggarrange(
(OWID_gross_suicides_country %>% ggplot(aes(  suic_rate, life_ladder)) + geom_point(pch=4) + labs(x="both sexes suicide rate", y="") + ylim(3.2, 8)  + xlim(0,70) + theme_classic()),
(OWID_gross_suicides_country %>% ggplot(aes(m_suic_rate, life_ladder)) + geom_point(pch=4) + labs(x="male suicide rate", y="")       + ylim(3.2, 8)  + xlim(0,70) + theme_classic()),
(OWID_gross_suicides_country %>% ggplot(aes(f_suic_rate, life_ladder)) + geom_point(pch=4) + labs(x="female suicide rate", y="")     + ylim(3.2, 8)  + xlim(0,70) + theme_classic()),
ncol = 3, nrow = 1)

##############################################################
# Suicidality and societal conditions 
##############################################################
suic_societal_corr <- cbind(cbind(
  rbind(
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(hdi,           suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(hdi,           suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(gii,           suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(gii,           suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(gdppc,         suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(gdppc,         suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(less_10_ratio, suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(less_10_ratio, suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(gini,          suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(gini,          suic_rate)$p.value ))
  )
  ,
  rbind(
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(hdi,           m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(hdi,           m_suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(gii,           m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(gii,           m_suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(gdppc,         m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(gdppc,         m_suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(less_10_ratio, m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(less_10_ratio, m_suic_rate)$p.value )),
  data.frame("r" = with(OWID_gross_suicides_country, cor.test(gini,          m_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(gini,          m_suic_rate)$p.value ))
  )
 )
 , 
 rbind(
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(hdi,           f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(hdi,           f_suic_rate)$p.value )),
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(gii,           f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(gii,           f_suic_rate)$p.value )),
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(gdppc,         f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(gdppc,         f_suic_rate)$p.value )),
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(less_10_ratio, f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(less_10_ratio, f_suic_rate)$p.value )),
 data.frame("r" = with(OWID_gross_suicides_country, cor.test(gini,          f_suic_rate)$estimate ), "p" = with(OWID_gross_suicides_country, cor.test(gini,          f_suic_rate)$p.value ))
 )
) 
rownames(suic_societal_corr) <- c("Human Development Index", "Gender Inequality Index", "Gross Domestic Prod. per capita", "Percentage of people living on < $10 / day", "GINI-Index")
suic_societal_corr[,2] <- paste0("(", sprintf("%.2f", suic_societal_corr[,2]), ")")
suic_societal_corr[,4] <- paste0("(", sprintf("%.2f", suic_societal_corr[,4]), ")")
suic_societal_corr[,6] <- paste0("(", sprintf("%.2f", suic_societal_corr[,6]), ")")
suic_societal_corr[,1] <- round(suic_societal_corr[,1], digits=2)
suic_societal_corr[,3] <- round(suic_societal_corr[,3], digits=2)
suic_societal_corr[,5] <- round(suic_societal_corr[,5], digits=2)

kbl(suic_societal_corr, booktabs = T, caption = "correlations between suicide rates and societal factors", align = "c") %>% add_header_above(c(" " = 1, "Both Sexes" = 2, "Males" = 2, "Females" = 2)) %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = "condensed", full_width = F, position = "left") %>% column_spec(3, italic = T) %>% column_spec(5, italic = T) %>% column_spec(7, italic = T) %>% column_spec(1, bold = T) %>% footnote(general = "r: Pearson correlation, p: two-sided p-value")

##############################################################
# Multivariate modeling of suicidality
##############################################################
# prepearing the data and loading olsrr library
##############################################################
lmdataset <- OWID_gross_suicides_country %>% 
  mutate(maleproportion = m_population/population,
         prev_adhd     = ((m_prev_adhd    /100*m_population)+(f_prev_adhd    /100*f_population))/population*100,
         prev_alco     = ((m_prev_alco    /100*m_population)+(f_prev_alco    /100*f_population))/population*100, 
         prev_anxiety  = ((m_prev_anxiety /100*m_population)+(f_prev_anxiety /100*f_population))/population*100, 
         prev_depr     = ((m_prev_depr    /100*m_population)+(f_prev_depr    /100*f_population))/population*100, 
         prev_bipolar  = ((m_prev_bipolar /100*m_population)+(f_prev_bipolar /100*f_population))/population*100, 
         prev_schizoph = ((m_prev_schizoph/100*m_population)+(f_prev_schizoph/100*f_population))/population*100) %>% 
  dplyr::select(-c(entity, age70plus, age50_69, age15_49, age5_14, population, m_population, f_population, popage65plus, f_suic_rate, popage25to64, mf_prev_md_s_ab_age_stand, 
            popage15to24, popage5to14, popage0to4, yearly_sum, m_suic_rate, m_prev_adhd, f_prev_adhd, m_prev_alco, f_prev_alco, mf_slfhrmdeath_age_stand, 
            m_prev_depr, f_prev_depr, m_prev_anxiety, f_prev_anxiety, m_prev_bipolar, f_prev_bipolar, m_prev_schizoph, f_prev_schizoph, less_1_ratio)) %>% 
  filter(maleproportion <= 0.55)
if(!require(olsrr)) install.packages("olsrr", repos = "http://cran.us.r-project.org")
library(olsrr)
print(names(lmdataset))

##############################################################
# modeling
##############################################################
model <- lm(suic_rate ~ . , data = na.omit(lmdataset))

##############################################################
# following the prodecure that is shown in: https://www.youtube.com/watch?v=5K_7Pxmhzwg and https://www.youtube.com/watch?v=vFH--Xdt3Pk
##############################################################
# not run because maybe a little too time consuming (on my machin approx 30 min.)
##############################################################
# k <- ols_step_all_possible(model)
# kmaxadjr <- k[which.max(k$adjr), ]
# kminaic  <- k[which.min(k$aic), ]
# kmincp   <- k[which.min(k$cp), ]
##############################################################
m6 <- lm(suic_rate ~ median_age + gii + all_veryimp + maleproportion + prev_alco + prev_bipolar, data = na.omit(lmdataset))
m8 <- lm(suic_rate ~ median_age + gii + all_veryimp + life_ladder + maleproportion + prev_alco + prev_bipolar + prev_schizoph, data = na.omit(lmdataset))

##############################################################
# the results of this stepwise procedure are -- in a next step -- use for a lm()-function
##############################################################
m10 <- ols_step_both_p(model, penter = 0.05, prem = 0.1) 
m10 <- lm(formula = paste("suic_rate", "~", paste(m10$predictors, collapse = " + ")), data = na.omit(lmdataset))

##############################################################
# summarizing the models
##############################################################
ols_regress(m6)
ols_regress(m8)
ols_regress(m10)

##############################################################
# plotting results from the multivariate linear modeling
##############################################################
if(!require(ggplotify)) install.packages("ggplotify", repos = "http://cran.us.r-project.org")
library(ggplotify)
if(!require(R.devices)) install.packages("R.devices", repos = "http://cran.us.r-project.org")
library(R.devices)
nulldev()
m6_resfitspreadplot  <- as.ggplot(ols_plot_resid_fit_spread(m6))  + labs(title= "6 predictor model") 
m8_resfitspreadplot  <- as.ggplot(ols_plot_resid_fit_spread(m8))  + labs(title= "8 predictor model") 
m10_resfitspreadplot <- as.ggplot(ols_plot_resid_fit_spread(m10)) + labs(title="10 predictor model") 
m6_residlevplot  <- as.ggplot(ols_plot_resid_lev(m6))  + labs(title= "6 predictors") 
m8_residlevplot  <- as.ggplot(ols_plot_resid_lev(m8))  + labs(title= "8 predictors") 
m10_residlevplot <- as.ggplot(ols_plot_resid_lev(m10)) + labs(title="10 predictors") 
dev.off()

ggarrange(m6_resfitspreadplot, m8_resfitspreadplot, m10_resfitspreadplot, ncol = 1, nrow = 3)
ggarrange(m6_residlevplot, m8_residlevplot, m10_residlevplot, ncol = 2, nrow = 2)

##############################################################
# printing the sources of the OWID-data on suicide (only the first two are done here as an example, as the code repeats itself...)
##############################################################
headings <- owid_sources[which(owid_sources$title == "Suicide rate vs. death rate from violence"),]$sources_content 
headings <- headings %>% minimal_html() %>% html_elements("h2") %>% html_text()
tables <- owid_sources[which(owid_sources$title == "Suicide rate vs. death rate from violence"),]$sources_content 
tables <- tables %>% minimal_html() %>% html_table() 
for (i in 1:length(headings)) { names(tables[[i]]) <- c("Var", "Source") }

out<- tables[[1]]
kable(out) %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = "condensed", full_width = F, font_size = 7, position = "left") %>% column_spec(1, width = "3.5cm") %>% column_spec(2, width = "12cm")

out<- tables[[2]]
kable(out) %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = "condensed", full_width = F, font_size = 7, position = "left") %>% column_spec(1, width = "3.5cm") %>% column_spec(2, width = "12cm")

out<- tables[[3]]
kable(out) %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = "condensed", full_width = F, font_size = 7, position = "left") %>% column_spec(1, width = "3.5cm") %>% column_spec(2, width = "12cm")


headings <- owid_sources[which(owid_sources$title == "Suicide rate vs. homicide rate"),]$sources_content 
headings <- headings %>% minimal_html() %>% html_elements("h2") %>% html_text()
tables <- owid_sources[which(owid_sources$title == "Suicide rate vs. homicide rate"),]$sources_content 
tables <- tables %>% minimal_html() %>% html_table() 
for (i in 1:length(headings)) { names(tables[[i]]) <- c("Var", "Source") }

out<- tables[[1]]
kable(out) %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = "condensed", full_width = F, font_size = 7, position = "left") %>% column_spec(1, width = "3.5cm") %>% column_spec(2, width = "12cm")

out<- tables[[2]]
kable(out) %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = "condensed", full_width = F, font_size = 7, position = "left") %>% column_spec(1, width = "3.5cm") %>% column_spec(2, width = "12cm")

out<- tables[[3]]
kable(out) %>% kable_styling(latex_options = "HOLD_position",bootstrap_options = "condensed", full_width = F, font_size = 7, position = "left") %>% column_spec(1, width = "3.5cm") %>% column_spec(2, width = "12cm")

