## ----setup, echo=FALSE, include=FALSE-----------------------------------------
library(knitr) #to produce dynamic reports, allowing the embedding of R code, its results, and text into a single document
# knitr::opts_chunk$set(echo = TRUE)
library(haven) #to import and export data in SPSS, Stata, and SAS formats, enhancing interoperability with other statistical software
library(rmarkdown) #is used to convert files that contain text and R code into documents of various formats, such as HTML, PDF, and Word
library(writexl) #to export tabular data from R to Excel files
library(Hmisc) #provides a collection of functions for data manipulation, statistical analysis, and graph creation, facilitating various stages of the analytical process
library(naniar) #exploring and visualizing missing data in the dataset
library(here)
library(rstatix)
library(DescTools)
rm(list = ls())
here()
getwd()

knitr::opts_knit$set(root.dir = "C:\\Users\\Joana Cima\\Documents\\GitHub\\RIntro")

# Define working directory
setwd("C:\\Users\\Joana Cima\\Documents\\GitHub\\RIntro")


## ----Libraries & Data, echo=FALSE,results='hide',message=FALSE , include=FALSE----
library(tidyverse) #provides a set of tools for data manipulation, visualization, and modeling
library(readxl) # allows us to read Excel files (.xls or .xlsx) directly into R. 
library(visdat) #to explore our dataset through visualizations.
library(stargazer) #to produce beautiful LaTeX or HTML tables and descriptive statistics from R statistical output
library(GGally) #to extend ggplot2 functionality to create a scatterplot matrix



## -----------------------------------------------------------------------------
nlswork <- as.data.frame(read_excel("nlswork.xlsx"))
# nlswork <- read_dta("nlswork.dta") # in case you have a Stata data source

head(nlswork)

colnames(nlswork)

str(nlswork)



## -----------------------------------------------------------------------------
nlswork_s<- nlswork %>% 
  select(idcode, ln_wage) 



## -----------------------------------------------------------------------------
nlswork_r <- nlswork %>% 
  rename(cae = ind_code)



## -----------------------------------------------------------------------------
nlswork_f<- nlswork %>% 
  filter(age > 20) 



## -----------------------------------------------------------------------------
 nlswork_m <- nlswork %>% 
  mutate(ln_asd=log(age))



## -----------------------------------------------------------------------------
nlswork_new <- nlswork %>% 
  rename(cae = ind_code) %>%
  select(ln_wage, age, year, race, union, collgrad, cae, ttl_exp, hours ) %>% 
  filter(age>=20) %>%
  mutate(age2=age^2)



## -----------------------------------------------------------------------------

vis_miss(nlswork_new)



## -----------------------------------------------------------------------------
gg_miss_var(nlswork_new) + labs(y = "Total missing values for each variable")
gg_miss_upset(nlswork_new)



## -----------------------------------------------------------------------------

gg_miss_fct(x = nlswork_new,fct = year)



## -----------------------------------------------------------------------------
vis_dat(nlswork_new)



## -----------------------------------------------------------------------------

library(tidyverse)
# Filling Missing Data 

## (with the average - this is an example - it does not make sense in this case)
nlswork_filled <- nlswork %>%
  mutate(across(c("union"), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) 

## (with the mode)
    ### Create a function to compute mode

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

nlswork_filled2 <- nlswork

union_mode <- mode(nlswork$union[!is.na(nlswork$union)])
nlswork_filled2$union[is.na(nlswork$union)] <- union_mode




## -----------------------------------------------------------------------------

nlswork_no_na <- na.omit(nlswork_new)



## -----------------------------------------------------------------------------
boxplot(nlswork_no_na$age, main="Boxplot for Outlier Detection - Age")


## -----------------------------------------------------------------------------
boxplot(nlswork_no_na$ln_wage, main="Boxplot for Outlier Detection - ln_wage")


## -----------------------------------------------------------------------------
outliers <- identify_outliers(as.data.frame(nlswork_no_na$ln_wage))
extreme_outliers <- outliers[outliers$is.extreme, ]
extreme_outliers


## -----------------------------------------------------------------------------
extreme_values_to_remove <- extreme_outliers$`nlswork_no_na$ln_wage`
nlswork_no_outliers <- nlswork_no_na[!nlswork_no_na$ln_wage %in% extreme_values_to_remove, ]


## -----------------------------------------------------------------------------
nlswork_no_na$ln_wage_winsorized <- Winsorize(nlswork_no_na$ln_wage, 
									probs = c(0, 0.99))


## -----------------------------------------------------------------------------
summary(nlswork_no_na) 



## ----echo = FALSE, results='asis'---------------------------------------------
nlswork_no_na  %>%
  dplyr::select(age, collgrad, ttl_exp, union, hours, ln_wage_winsorized) %>% 
  stargazer(title="Shorter statistics",
            type= "text", out = "Statistics_output.html",
            digits = 2)



## ----echo = FALSE, results='asis'---------------------------------------------
nlswork_no_na %>%
  dplyr::select(age, collgrad, ttl_exp, union, hours, ln_wage_winsorized) %>% 
  stargazer(title="Shorter statistics",
            type= "text", out = "Statistics_output.txt",
            digits = 3)



## ----echo = FALSE, results='asis'---------------------------------------------
nlswork_no_na %>%
  dplyr::select(age, collgrad, ttl_exp, union, hours, ln_wage_winsorized) %>% 
  stargazer(title="Shorter statistics",
            type= "text", out = "Statistics_output2.html",
            digits = 3, flip=TRUE)



## ----echo = FALSE, results='asis', warning=FALSE------------------------------
nlswork_no_na %>%
  dplyr::select(age, collgrad, ttl_exp, union, hours) %>% 
  stargazer(title="Shorter statistics",
            type= "latex",
            digits = 3, flip=TRUE)



## ----echo=FALSE, warning=FALSE, results='asis'--------------------------------
nlswork_no_na %>% 
      ggplot(aes(ttl_exp,ln_wage_winsorized)) +
      labs(title = "Ln Wage vs. Experience") +
      ylab("Ln Wage") +
      xlab("Experience") +
      geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")




## -----------------------------------------------------------------------------
ggplot(data = nlswork_no_na) +
  geom_bar(mapping=aes(x=as.factor(collgrad))) +
  xlab("College graduate (1=Yes)")



## -----------------------------------------------------------------------------
ggplot(data = nlswork_no_na) + geom_histogram(mapping = aes(x = age), binwidth = 1) + 
  scale_x_continuous(breaks = seq(20, 50, by = 5))



## -----------------------------------------------------------------------------
nlswork_no_na %>% ggplot(aes(x=as.factor(collgrad), y=ln_wage_winsorized)) +
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("College graduate (1=Yes)")


## ----warning=FALSE------------------------------------------------------------
nlswork_no_na %>% ggplot(mapping = aes(x = ln_wage, y = ..density..)) +
    xlab("ln(wage)") +
    ylab("Density") +
    geom_freqpoly(mapping = aes(colour = factor(collgrad, labels=c("No", "Yes")))) +
  labs(color ="College degree")


## ----warning=FALSE------------------------------------------------------------
ggpairs(nlswork_no_na[, c("age","ttl_exp","hours")], title="Correlogram with ggpairs()")


## ----include=FALSE------------------------------------------------------------
#CONVERSÃO DO CÓDIGO RMARKDOWN PARA O CÓDIGO R

options(knitr.duplicate.label = "allow")

knitr::purl(input= "RIntro.Rmd", output = "RIntro.R")


