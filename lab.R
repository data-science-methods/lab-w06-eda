#' ---
#' title: "Data Science Methods, Lab for Week 9"
#' author: Dennis Perez
#' email: dperez81@ucmerced.edu
#' output:
#'   html_document:
#'     toc: true
#' ---

# How to ask a question in R: ??ames_raw


















#' Introduction
#' *In this lab, we'll be looking at a set of housing sales data assembled by Dean De Cock of Truman State University in Missouri.  The dataset has been widely used for learning about predictive modeling or machine learning.  But we'll focus on using the techniques from today's notes to get a handle on a dataset with dozens of variables.*  
#' 
#' *De Cock's paper documenting the dataset is here:  <http://jse.amstat.org/v19n3/decock.pdf>.  The abstract notes notes some major features of the dataset:*
#' 
#' > *This paper presents a data set describing the sale of individual residential property in Ames, Iowa from 2006 to 2010. The data set contains 2930 observations and a large number of explanatory variables (23 nominal, 23 ordinal, 14 discrete, and 20 continuous) involved in assessing home values.* 
#' 
#' *We'll be accessing the dataset using the `AmesHousing` package.  (Strictly speaking, we'll be using a CSV that I generated from one version in the `AmesHousing` package, because it gives us a chance to learn a few things about factors.)  It'll be useful to have the documentation for `AmesHousing::ames_raw` open, because it gives short descriptions of the many variables in the dataset:  <https://cran.r-project.org/web/packages/AmesHousing/AmesHousing.pdf>.  You can find a full codebook for the dataset at <http://jse.amstat.org/v19n3/decock/DataDocumentation.txt>.* 
#' 
AmesHousing::ames_raw
#' # Reflexivity #
#' *Before starting the lab, spend 3 minutes writing a response to each reflexivity question.  Use a timer.  Answer these questions off the top of your head: don't worry about consulting or citing outside sources or about getting the answers "right" or "wrong."* 
#' 1. *What do I already know about this subject?*
#' I do not know anything as of yet.
#' 2. *Why am I studying this?* 
#' I am assuming we are studying this to get a familiarity with factors- as stated in the introduction.
#' 3. *What do I expect or hope to find/learn, and why?* 
#' I believe this exercise will provide me familiarity with variable usage.
#' 4. *Who is affected by this topic, and how am I connected to them?*  
#' I am not entirely sure who is affected. Although, I would guess this topic affects home owners. I am connected to them because I would like to purchase a home myself someday.
#' 

## Setup
## **IMPORTANT**: Add all dependencies to `DESCRIPTION`
library(tidyverse)
library(skimr)
library(visdat)

library(AmesHousing)





#' # Problem 1 #
# problem 1 -------
#' *We'll start with Peng and Matsui's step 1, "Formulate your question."  The Ames dataset is often used to teach predictive modeling tasks, where the goal is to predict the final selling price.  So our question will be _which variables in the dataset are mostly highly correlated with sale price?_*
#' 
#' 1. *Look through the short descriptions in `?ames_raw` (or online, <https://cran.r-project.org/web/packages/AmesHousing/AmesHousing.pdf>).  Which variable reports the actual sale price?* 
#' I looked through the short descriptions and I am not sure which variable reports the actual sales price. 
#' The descriptions did not contain that information, but when I scrolled down, the pdf mentioned that spaces and special characters were changed to snake case. They mentioned Sale_Price here so I assume this is the actual sales price.
#' However, I was unable to spot this variable when after running AmesHousing::ames_raw.


#' 2. *As you were looking through the variable descriptions, you probably noticed a few variables that might be good predictors of sale price.  List two or three here.* 
#' Neighborhood
#' House Style
#' Overall Cond





#' # Problem 2: Loading the data #
# problem 2 -------
#' *`AmesHousing` includes a few different representations of the data.  We'll be working with `ames_raw`, which represents what you'd get from reading in the original CSV file.  However — like a lot of CSV files — the column names aren't R-friendly.*
#' 1. *Try running the following line.  Can you explain why this causes an error?*
# ames_raw$MS SubClass
#' The system says that there is an unexpected token


#' 2. *We can use `set_names()` to modify a variable's names (here, the column names) in a pipe-friendly way.  In particular, `set_names()` supports passing a function to modify the names.  
#' Write a pipe that starts with `ames_raw`, 
#' uses `make.names()` to deal with spaces and column names that start with numbers, 
#' and then uses `tolower()` to make all the names lowercase.  
#' Assign the result to `dataf`, which will be our primary working dataframe.*
#' 
dataf = ames_raw %>% 
    set_names(make.names) %>%
    set_names(tolower) 



#' # Problem 3 #
# problem 3 -------
#' *Next we have step 3, "Check the packaging," and 5, "Check your 'n's." Use `skimr::skim()` and `visdat` functions to answer the following questions.  To report the values that you find, replace the value assigned to the `problem` variable.*  
#' 
#' 1. *The paper abstract (see above) reports 2930 rows.  How many observations (rows) are in our version of the dataset?*  
#' Upon running AmesHousing::ames_raw, I noticed the report said there were 2,930 observations
#' Now I will use skimr and visdat as instructed to confirm whether this is true of the data set we made

skimr::skim(dataf)
vis_dat(dataf)
vis_miss(dataf)
# Skimr confirmed there are 2,930 observations

#problem3.1 = 1.7e15 # scientific notation: 1.7 x 10^15
problem3.1 = 2.93e3 # Scientific notation: 2.93 x 10^3



#' 2. *The abstract also reports 80 "explanatory variables (23 nominal, 23 ordinal, 14 discrete, and 20 continuous)."  How many factor, character, and numeric variables do we have in the dataframe?*
# problem3.2.factors = 7
# problem3.2.characters = 18000
# problem3.2.numerics = 12

# After running skimr, I noticed the following
problem3.2.factors = 0
problem3.2.characters = 45
problem3.2.numerics = 37
# For a total of 82 variables in our newly created dataset


#' 3. *Explain the relationship between the variables in the dataset and the variables in the dataframe as we've loaded it.* 
#' As of right now, I do not see an obvious relationship between the original dataset and dataframe we created.
#' However, given that our main task is to create a dataset with highly correlated variables with the sales price
#' I would assume that the variables we are using are better correlated with the sales price


#' 4. *How many variables have missing values?  Hint: Check the class of the output of `skim()`.* 
#' It looks like there are 27 variables/rows with missing values
#' I counted the variables that had missing values in the output of skim
#' I also ran rowSums(is.na(dataf)) to verify my count was correct.
problem3.4 = rowSums(is.na(dataf)) 
 
#' Number of missing values total (just an additional check I performed):
sum(is.na(dataf))
# There are 13,997 missing values
#' Upon running sum(is.na(dataf)), I got the output: [1] 13997
#' This was confirmed when I hand counted the numbers in the n_missing column of the skim output



#' # Problem 4 #
# problem 4 -------
#' *(This problem is a quick comprehension check for `dplyr` functions + pipes.)  `summarize()` is a tidyverse function that collapses multiple rows of data into a single row.  Like `mutate()` and `count()`, it respects groups constructed by `group_by()`.  Here's an example:* 

# dataf %>% 
#     group_by(ms.zoning) %>% 
#     summarize(sale.price = mean(sale.price)) %>% 
#     ungroup()

#' 1. *Examine the full codebook, at <http://jse.amstat.org/v19n3/decock/DataDocumentation.txt>.  What do the values of MS_Zoning represent?* 
#' The codebook states that the values of MS_Zoning identify the general zoning classification of the sale 
#' 
#' 


#' 2. *Run the following two expressions.  Why do they give different results?* 
#' 
#' 
#' First, the code threw an error because the variable "sale.price" was named incorrectly. I looked at the environment of the dataframe we created and it has the variable as "saleprice" 
#' Went through both pipe examples and changed the variable names accordingly. Re ran the code after.
dataf %>%
    group_by(ms.zoning) %>%
    filter(saleprice > 100000) %>%
    summarize(saleprice = mean(saleprice)) %>%
    ungroup()

dataf %>%
    group_by(ms.zoning) %>%
    summarize(saleprice = mean(saleprice)) %>%
    filter(saleprice > 100000) %>%
    ungroup()

# These are different because:
# The first pipe example filters for individual sales price above 100000, and then summarizes this filtered data with the given equation
# The second pipe example first summarizes the data with the applied equation, and then it filters for the sales prices means above 100000
# One pipe is calculating the mean for sales price case by case first and then looking to see if there are prices above 100000. The other pipe is calculating the mean for sales price as a group and then searching for means above 100000 after. 


#' # Problem 5: Duplicate rows #
# problem 5 -------
#' *Now we'll take a look at some of the items on the checklist from Huebner et al.* 
#' 
#' 1. *Read the docs for `dplyr::distinct()`.  Then use this function to create a dataframe `dataf_nodup` with the duplicate rows removed.*  
#' 
dataf_nodup <- dataf %>%
    distinct()

#' 2. *How many duplicate rows are in the dataset?*
#' It seems as though there aren't any duplicated rows.
n_duplicate = sum(duplicated(dataf))- sum(duplicated(dataf_nodup))
# Need help with this section------


#' # Problem 6: Coding ordinal variables #
# problem 6 --------
#' *Because the CSV format doesn't have a way to document the variable type for its columns, ordinal variables will be represented either as strings or numerically.  When we load the CSV into R, these get parsed as character or numeric variables, respectively.*
#' 
#' *(It's actually a little more complicated for strings.  The base R function `read.csv()` involves a call to `as.data.frame()`, which has an argument `stringsAsFactors` that, if true, coerces all strings/characters into factors.  Prior to R version 4, the default value for this was `TRUE`, because back in the 1990s it was relatively rare to have actual text variables in the data.  So, up until last year, `read.csv()` by default would parse string columns into factors.  Both `readr::read_csv()` (a tidyverse package) and the current version of `read.csv()` by default parse string columns into characters.)*
#' 
#' 1. *Let's take a look at two condition variables, for the overall and exterior.  These are `overall.cond` and `exter.cond`, respectively. How are these two ordinal variables represented?*
#' 
view(dataf$overall.cond) # showed that these are represented numerically
view(dataf$exter.cond) # showed that these are represented with characters


#' 2. *Since ultimately we're going to construct a Spearman rank correlation matrix (the quick-and-dirty approach to the problem), we need to get `exter.cond` into an integer representation. First, let's generate a table that shows the distribution across values of `exter.cond`, using `dplyr::count()`. Call this `ex_cond_count`. We'll use this to check the conversion process over the next few steps.*
ex_cond_count = dataf %>%
    dplyr::count(exter.cond)
view(ex_cond_count)


#' 3. *As a first attempt, write a function `char_to_int()` that takes a character vector as input,
 
#'  coerces to a factor using `as.factor()`,

#' and then coerces it to `as.integer()`. 
 
#' Using `mutate()` and `count()`,
 
#' apply this function to `exter.cond` and check the distribution against your answer for #2


# 
char_to_int <- function(characterVec) {
    as.factor(characterVec) %>%
        as.integer()

}

check_if_worked <- dataf %>%
    mutate(exter.cond = char_to_int(exter.cond)) %>%
    count(exter.cond)



#' 4. *Can you explain what went wrong?  Hint:  Check the docs for the `levels` argument of `factor()`.*
#' According to a google search, numeric and character variables can be made into factors, but the levels of the factor will remain character values
#' https://www.stat.berkeley.edu/~s133/factors.html (source)
#' I am assuming there is no way to correlate the numbers with an associated level


#' 5. *We can fix this by passing in a character vector of the levels in the desired order, namely, from Po (Poor) to Ex (Excellent). 
#' Modify `char_to_int()` to use such a vector in the `as.factor()` call.  
#' Why doesn't this work?*
#' The notes for the levels argument state:  The default is the unique set of values taken by as.character(x), sorted into increasing order of x
#' Which I believe means that the characters will be automatically sorted in alphabetical order
#' Getting an error message saying unused argument
#' 
# char_to_int <- function(characterVec) {
#     as.factor(characterVec) %>%
#         as.integer()
# 
# }
# 
# check_if_worked2 <- dataf %>%
#     mutate(exter.cond = char_to_int(exter.cond)) %>%
#     count(exter.cond)
#' 6. *The most efficient way to avoid this poor design is to use `forcats::fct_relevel()`.  This is loaded as part of the tidyverse, so you don't need to modify the packages loaded up above, or `DESCRIPTION`.  Rewrite `char_to_int()` again, using `fct_relevel()` in place of `as.factor()`, and check against your answer to #2 to ensure that this is all working as expected.*
#' 
# I received help from Ellis on this problem- mainly with implementing a second variable so that information could be passed into it.
# This handled an earlier error message of an unused variable


char_to_int <- function(characterVec, char_lev) {
    fct_relevel(characterVec, char_lev) %>%
        as.integer()

}

char_lev = c('Po', 'Fa', 'TA', 'Gd', 'Ex')

check_if_worked3 <- dataf %>%
    mutate(exter.cond = char_to_int(exter.cond, c('Po', 'Fa', 'TA', 'Gd', 'Ex'))) %>%
    count(exter.cond)

view(check_if_worked3)
#' 7. *Finally we want this factor to be in our analysis dataframe.  **Normally, to preserve immutability**, I would either do this in the pipe where we first loaded the CSV (as we did above, when we fixed the names), or start by creating something like `dataf_raw` and then write a pipe that did all the cleaning steps and assigning the result to `dataf`, including this.  
#' For the purposes of this lab, we'll just do it here.  
#' Using either `mutate_at()` or `mutate(across())`, 
#' apply `char_to_int()` to all of the condition variables represented using these same levels:  `exter.cond`, `bsmt.cond`, `heating.qc`, `garage.cond`.* 


# gdf %>% mutate(across(v1:v2, ~ .x + rnorm(1)))
# The change worked for each column, but I did not see a column for "garage.cond" in either dataf nor dataf_nodup
dataf = dataf %>%
    mutate(across(c('exter.cond', 'bsmt.cond', 'heating.qc', 'garage.cond'), char_to_int, char_lev))


# dataf = dataf %>%
#     mutate_at(c('exter.cond', 'bsmt.cond', 'heating.qc', 'garage.cond'), char_to_int, char_lev)


#' # Problem 7 #
# problem 7 -------
#' *Recall that we're interested in finding variables that are highly correlated with sale price.  We can use the function `cor()` to construct a correlation matrix, with correlations between all pairs of variables in the dataframe.  But this creates two challenges.  First, `cor()` only works with numerical inputs.  If we try it with our current dataframe, it throws an error*:  

cor(dataf)

#' *Second, the result will be a matrix — a 2D collection of numbers — rather than a dataframe.  We'll need to convert it back to a dataframe to use our familiar tidyverse tools, eg, using `arrange()` to put the correlations in descending order.* 
#' 


#' 1. *For the first problem (column types), we could use the tidyverse function `select()` to pull out a given set of columns from the dataframe.*

select(dataf, saleprice, overall.cond, gr.liv.area)


#' *But manually typing out all of the numerical covariates would be tedious and prone to error.  Fortunately `select()` is much more powerful than this.  You can read more in `?select` or here: <https://tidyselect.r-lib.org/reference/language.html>.  Then specifically read the docs for `where()`.*
#' 


#' *Write a pipe that `select()`s the numeric columns 
#' and passes the result to `cor()` for a Spearman regression 
#' and uses the `pairwise.complete.obs` method to handle missing values.  
#' Assign the result to `cor_matrix`.*   
#' Used this as a resource for cor function: https://rstudio-pubs-static.s3.amazonaws.com/240657_5157ff98e8204c358b2118fa69162e18.html
# used this example from the doc notes to understand how to select for numeric values: "iris %>% select(where(is.numeric))"
cor_matrix = dataf %>%
    select(where(is.numeric)) %>%
    cor(method = 'spearman', use = 'pairwise.complete.obs')



#' 2. *Now we convert the correlation matrix into a dataframe. Uncomment the following line, and explain what it's doing.* 
cor_df = as_tibble(cor_matrix, rownames = 'covar') #tibble of the correlation matrix compared with
#' view(cor_df)
#' After viewing cor_df, I can see that the correlation matrix was turned into a tibble, with the first column named covar. 
#' The corresponding rows contain the variable names of selected values with numeric information


#' 3. *What do the rows of `cor_df` represent?  The columns?  The values in each cell?* 
#' - rows: the variable with numeric info
#' - columns: the columns with numeric info
#' - values: I am assuming this displays the correlation of the row in comparison to whatever column is being considered


#' 4. *We've calculated the correlations for each pair of variables.  Now we want to construct a table with the top 10 most highly-correlated variables.  Write a pipe that does the following, in order:*  
#' - *Start with `cor_df`*
#' - *Select the columns with the name of each covariate and its correlation with sale price*
#' - *Constructs a new column with the absolute value of the correlation*
#' - *Arranges the rows in descending order by absolute correlation*
#' - *Keep the top 10 rows.  Hint: `?top_n`*
#' - *Assigns the result to the variable `top_10`*
#' Ask about this one as well
top_10 = cor_df %>%
    select(covar, saleprice) %>%
    mutate(new_var_absVal = abs(saleprice)) %>%
    arrange(desc(new_var_absVal)) %>%
    top_n(10)
view(top_10)

#' # Problem 8 #
# problem 8 -------
#' *In 1.2, you identified some variables that you thought might be good predictors of sale price.  How good were your expectations?* 
#' My expectations did not match what we found in this lab.
#' In fact, not one variable I predicted made it to the top 10.
#' 
