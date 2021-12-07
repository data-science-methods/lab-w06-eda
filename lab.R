#' ---
#' title: "Data Science Methods, Lab for Week 99"
#' author: "Your Name"
#' email: Your Email
#' output:
#'   html_document:
#'     toc: true
#' ---

#' Introduction
#' *In this lab, we'll be looking at a set of housing sales data assembled by Dean De Cock of Truman State University in Missouri.  The dataset has been widely used for learning about predictive modeling or machine learning.  But we'll focus on using the techniques from today's notes to get a handle on a dataset with dozens of variables.*  
#' 
#' *De Cock's paper documenting the dataset is here:  <http://jse.amstat.org/v19n3/decock.pdf>.  The abstract notes notes some major features of the dataset:*
#' 
#' > *This paper presents a data set describing the sale of individual residential property in Ames, Iowa from 2006 to 2010. The data set contains 2930 observations and a large number of explanatory variables (23 nominal, 23 ordinal, 14 discrete, and 20 continuous) involved in assessing home values.* 
#' 
#' *We'll be accessing the dataset using the `AmesHousing` package.  (Strictly speaking, we'll be using a CSV that I generated from one version in the `AmesHousing` package, because it gives us a chance to learn a few things about factors.)  It'll be useful to have the documentation for `AmesHousing::ames_raw` open, because it gives short descriptions of the many variables in the dataset:  <https://cran.r-project.org/web/packages/AmesHousing/AmesHousing.pdf>.  You can find a full codebook for the dataset at <http://jse.amstat.org/v19n3/decock/DataDocumentation.txt>.* 
#' 

#' # Reflexivity #
#' *Before starting the lab, spend 3 minutes writing a response to each reflexivity question.  Use a timer.  Answer these questions off the top of your head: don't worry about consulting or citing outside sources or about getting the answers "right" or "wrong."* 
#' 1. *What do I already know about this subject?*
#' I honestly do not know much other than the basic description above of the data.
#' 2. *Why am I studying this?*
#' to use as a way to test different eda methods in r
#' 3. *What do I expect or hope to find/learn, and why?*
#' I hope to be able to use the different types of data cleaning and analysis I will be learning below.
#' 4. *Who is affected by this topic, and how am I connected to them?* 
#' 

## Setup
## **IMPORTANT**: Add all dependencies to `DESCRIPTION`
library(tidyverse)
library(skimr)
library(visdat)

library(AmesHousing)

#' # Problem 1 #
# problem 1 ----
#' *We'll start with Peng and Matsui's step 1, "Formulate your question."  The Ames dataset is often used to teach predictive modeling tasks, where the goal is to predict the final selling price.  So our question will be _which variables in the dataset are mostly highly correlated with sale price?_*
#' #goal: predict final selling price
#' #question: which variables in the dataset are mostly highly correlated with sale price?
#' 1. *Look through the short descriptions in `?ames_raw` (or online, <https://cran.r-project.org/web/packages/AmesHousing/AmesHousing.pdf>).  Which variable reports the actual sale price?* 
summary(ames_raw)
ames_raw
#
#Sales Price is not in the raw file as far as I can see online and in the ames_raw when I look on here.
#' 
#' 

#' 2. *As you were looking through the variable descriptions, you probably noticed a few variables that might be good predictors of sale price.  List two or three here.* 
# Overall condition, building type, year built
#' 
#' 


#' # Problem 2: Loading the data #
# problem 2 ----
#' *`AmesHousing` includes a few different representations of the data.  We'll be working with `ames_raw`, which represents what you'd get from reading in the original CSV file.  However — like a lot of CSV files — the column names aren't R-friendly.*
#' 1. *Try running the following line.  Can you explain why this causes an error?*
ames_raw$MS SubClass
# We are trying to pull a column that we think is in the dataframe ames_raw, but it likely did not read over in the same way when uploaded from the csv into R. R is also case senstive, so this could be something setting it off, and the fact that there is a space in the name is an issue.

#' 2. *We can use `set_names()` to modify a variable's names (here, the column names) in a pipe-friendly way.  In particular, `set_names()` supports passing a function to modify the names. 
# Write a pipe that starts with `ames_raw`, uses `make.names()` to deal with spaces and column names that start with numbers, and then uses `tolower()` to make all the names lowercase.  Assign the result to `dataf`, which will be our primary working dataframe.*
dataf<- ames_raw %>%
  set_names(make.names)%>%
  set_names(tolower)
  
#tried a lot of different things, but ultimately realized I just was making it more complicated and trying to run make.names as its own function without set_names and wondering why it wouldn't turn dataf into a dataframe.
#this takes the datafram ames_raw, and sets all of the column names to have no spacing and instead words are separated by a '.', and then they are all now lower case since r is case sensitive.  

#' # Problem 3 #
# problem 3 ----
#' *Next we have step 3, "Check the packaging," and 5, "Check your 'n's." Use `skimr::skim()` and `visdat` functions to answer the following questions.  To report the values that you find, replace the value assigned to the `problem` variable.*  
#' 
#' 1. *The paper abstract (see above) reports 2930 rows.  How many observations (rows) are in our version of the dataset?*  
skimr::skim(dataf)
problem3.1 = 2.93e3 # scientific notation: 1.7 x 10^15

#' 2. *The abstract also reports 80 "explanatory variables (23 nominal, 23 ordinal, 14 discrete, and 20 continuous)."  How many factor, character, and numeric variables do we have in the dataframe?*
#' 
problem3.2.factors = 0 # I don't see factor listed on the side for this in my console
problem3.2.characters = 45 # data summary lists 45 charachters
problem3.2.numerics = 37 #data summary lists 37 numeric

#' 3. *Explain the relationship between the variables in the dataset and the variables in the dataframe as we've loaded it.* 
skimr::skim(ames_raw)
# It appears to me when I run the skim code on both, that they are the same, in terms of they are both 82 variables, and have the same number of entries, but just looking can see the difference in formatting in terms of how the variables are all named.
#' 

#' 4. *How many variables have missing values?  Hint: Check the class of the output of `skim()`.* 
#' 
#problem3.4 = ifelse(skim(dataf)$n_missing>0,1,0)
#summary(problem3.4)
#skim(dataf) %>% 
  #select(skim_variable, skim_type, n_missing, complete_rate)

#' # Problem 4 #
# problem 4 ----
#' *(This problem is a quick comprehension check for `dplyr` functions + pipes.)  `summarize()` is a tidyverse function that collapses multiple rows of data into a single row.  Like `mutate()` and `count()`, it respects groups constructed by `group_by()`.  Here's an example:* 

dataf %>% 
 group_by(ms.zoning) %>% 
summarize(saleprice = mean(saleprice)) %>% ##had to delete . from sales.price to get it to run
ungroup()

#' 1. *Examine the full codebook, at <http://jse.amstat.org/v19n3/decock/DataDocumentation.txt>.  What do the values of MS_Zoning represent?* 
#It is a description of all of the types of zoning classifications of the sale
#' 
#' 

#' 2. *Run the following two expressions.  Why do they give different results?* 
#' 
#' 
#' 
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
#simply they give different results because you're filtering and summarizing in different orders. In the first, since filtering happens first, you're limiting the sales data to less than 100k first, then summarizing it and providing the mean. In the second since you summarize and find the mean first, this will result in different groups of data since the sales in the mean can be below 100k, changing the mean, and then filtering of above 100k occurs to the means of the sales prices.

#' # Problem 5: Duplicate rows #
# problem 5 ----
#' *Now we'll take a look at some of the items on the checklist from Huebner et al.* 
#' 
#' 1. *Read the docs for `dplyr::distinct()`.  Then use this function to create a dataframe `dataf_nodup` with the duplicate rows removed.*  
#' 

#' 2. *How many duplicate rows are in the dataset?*
#' 
# n_duplicate = ???


#' # Problem 6: Coding ordinal variables #
# problem 6 -----
#' *Because the CSV format doesn't have a way to document the variable type for its columns, ordinal variables will be represented either as strings or numerically.  When we load the CSV into R, these get parsed as character or numeric variables, respectively.*
#' 
#' *(It's actually a little more complicated for strings.  The base R function `read.csv()` involves a call to `as.data.frame()`, which has an argument `stringsAsFactors` that, if true, coerces all strings/characters into factors.  Prior to R version 4, the default value for this was `TRUE`, because back in the 1990s it was relatively rare to have actual text variables in the data.  So, up until last year, `read.csv()` by default would parse string columns into factors.  Both `readr::read_csv()` (a tidyverse package) and the current version of `read.csv()` by default parse string columns into characters.)*
#' 
#' 1. *Let's take a look at two condition variables, for the overall and exterior.  These are `overall.cond` and `exter.cond`, respectively. How are these two ordinal variables represented?*
#' 
#' 
#' 

#' 2. *Since ultimately we're going to construct a Spearman rank correlation matrix (the quick-and-dirty approach to the problem), we need to get `exter.cond` into an integer representation. First, let's generate a table that shows the distribution across values of `exter.cond`, using `dplyr::count()`. Call this `ex_cond_count`. We'll use this to check the conversion process over the next few steps.*
# ex_cond_count = ???

#' 3. *As a first attempt, write a function `char_to_int()` that takes a character vector as input, coerces to a factor using `as.factor()`, and then coerces it to `as.integer()`.  Using `mutate()` and `count()`, apply this function to `exter.cond` and check the distribution against your answer for #2.* 

#' 4. *Can you explain what went wrong?  Hint:  Check the docs for the `levels` argument of `factor()`.*
#' 
#' 
#' 

#' 5. *We can fix this by passing in a character vector of the levels in the desired order, namely, from Po (Poor) to Ex (Excellent).  Modify `char_to_int()` to use such a vector in the `as.factor()` call.  Why doesn't this work?*
#' 
#' 
#' 

#' 6. *The most efficient way to avoid this poor design is to use `forcats::fct_relevel()`.  This is loaded as part of the tidyverse, so you don't need to modify the packages loaded up above, or `DESCRIPTION`.  Rewrite `char_to_int()` again, using `fct_relevel()` in place of `as.factor()`, and check against your answer to #2 to ensure that this is all working as expected.*
#' 

#' 7. *Finally we want this factor to be in our analysis dataframe.  **Normally, to preserve immutability**, I would either do this in the pipe where we first loaded the CSV (as we did above, when we fixed the names), or start by creating something like `dataf_raw` and then write a pipe that did all the cleaning steps and assigning the result to `dataf`, including this.  For the purposes of this lab, we'll just do it here.  Using either `mutate_at()` or `mutate(across())`, apply `char_to_int()` to all of the condition variables represented using these same levels:  `exter.cond`, `bsmt.cond`, `heating.qc`, `garage.cond`.* 

# dataf = ???



#' # Problem 7 #
# problem 7 ----
#' *Recall that we're interested in finding variables that are highly correlated with sale price.  We can use the function `cor()` to construct a correlation matrix, with correlations between all pairs of variables in the dataframe.  But this creates two challenges.  First, `cor()` only works with numerical inputs.  If we try it with our current dataframe, it throws an error*:  

# cor(dataf)

#' *Second, the result will be a matrix — a 2D collection of numbers — rather than a dataframe.  We'll need to convert it back to a dataframe to use our familiar tidyverse tools, eg, using `arrange()` to put the correlations in descending order.* 
#' 
#' 1. *For the first problem (column types), we could use the tidyverse function `select()` to pull out a given set of columns from the dataframe.*

# select(dataf, saleprice, overall.cond, gr.liv.area)

#' *But manually typing out all of the numerical covariates would be tedious and prone to error.  Fortunately `select()` is much more powerful than this.  You can read more in `?select` or here: <https://tidyselect.r-lib.org/reference/language.html>.  Then specifically read the docs for `where()`.*
#' 
#' *Write a pipe that `select()`s the numeric columns and passes the result to `cor()` for a Spearman regression and uses the `pairwise.complete.obs` method to handle missing values.  Assign the result to `cor_matrix`.*  
# cor_matrix = ???


#' 2. *Now we convert the correlation matrix into a dataframe. Uncomment the following line, and explain what it's doing.* 
# cor_df = as_tibble(cor_matrix, rownames = 'covar')
#' 
#' 
#' 


#' 3. *What do the rows of `cor_df` represent?  The columns?  The values in each cell?* 
#' - rows: 
#' - columns: 
#' - values: 

#' 4. *We've calculated the correlations for each pair of variables.  Now we want to construct a table with the top 10 most highly-correlated variables.  Write a pipe that does the following, in order:*  
#' - *Start with `cor_df`*
#' - *Select the columns with the name of each covariate and its correlation with sale price*
#' - *Constructs a new column with the absolute value of the correlation*
#' - *Arranges the rows in descending order by absolute correlation*
#' - *Keep the top 10 rows.  Hint: `?top_n`*
#' - *Assigns the result to the variable `top_10`*
#' 


#' # Problem 8 #
# problem 8 ----
#' *In 1.2, you identified some variables that you thought might be good predictors of sale price.  How good were your expectations?* 
#' 
#' 
#' 
