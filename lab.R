#' ---
#' title: "Data Science Methods, Lab for Week 99"
#' author: "Richard Ellks"
#' email: rellks@ucsd.edu
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
#' Other than that EDA stands for Exploratory Data Analysis, I don't know too much about this subject. Since I'm doing this lab a bit late and we dove into EDA in class already, I want to address what I knew about EDA before learning about it. EDA to me was simply a way to look at data, although I had never put a name to it other than simple analysis. It covers a broad range of analysis approaches.
#' 
#' 2. *Why am I studying this?*
#' There are right ways to explore data and analyze it, and there are definitely wrong ways. I'm studying this to learn about approaches to analyze data that are not necessarily correct since I am of the firm belief that there is no right answer or one and only way, but approaches that interpret that data truthfully and transparently.
#' 3. *What do I expect or hope to find/learn, and why?*
#' I hope to learn more about EDA and the approaches that correctly interpret data. Since my knowledge of the topic is still somewhat limited, I hope to discover more about different approaches that I have never thought of and ones that possibly make data analysis easier in terms of clarity.
#' 4. *Who is affected by this topic, and how am I connected to them?* 
#' Anyone and everyone is affected by EDA in terms of who is doing the EDA and what audience they are appealing to, including myself. My target audience might be those interested in EEG or UX research but someone else who analyzes COVID data for example, might have an affect on the entire world.
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
#' 
#' 1. *Look through the short descriptions in `?ames_raw` (or online, <https://cran.r-project.org/web/packages/AmesHousing/AmesHousing.pdf>).  Which variable reports the actual sale price?* 
#' SalePrice
#'
#' 
#' 2. *As you were looking through the variable descriptions, you probably noticed a few variables that might be good predictors of sale price.  List two or three here.* 
#' 
#' LotArea, OverallQual, YearBuilt
#' 

#' # Problem 2: Loading the data #
# problem 2 ----
#' *`AmesHousing` includes a few different representations of the data.  We'll be working with `ames_raw`, which represents what you'd get from reading in the original CSV file.  However — like a lot of CSV files — the column names aren't R-friendly.*
#' 1. *Try running the following line.  Can you explain why this causes an error?*
# ames_raw$MS SubClass
#' There is a space in the name of the column therefore it cannot be directly accessed with the usual ${col_name} syntax.

#' 2. *We can use `set_names()` to modify a variable's names (here, the column names) in a pipe-friendly way.  In particular, `set_names()` supports passing a function to modify the names.  Write a pipe that starts with `ames_raw`, uses `make.names()` to deal with spaces and column names that start with numbers, and then uses `tolower()` to make all the names lowercase.  Assign the result to `dataf`, which will be our primary working dataframe.*
 
dataf <- ames_raw %>% 
    names() %>% 
    make.names() %>% 
    tolower() %>% 
    set_names(ames_raw, .)

#' # Problem 3 #
# problem 3 ----
#' *Next we have step 3, "Check the packaging," and 5, "Check your 'n's." Use `skimr::skim()` and `visdat` functions to answer the following questions.  To report the values that you find, replace the value assigned to the `problem` variable.*  
#' 
#' 1. *The paper abstract (see above) reports 2930 rows.  How many observations (rows) are in our version of the dataset?*  
#' 
skim_df <- skim(dataf)
vis_dat(dataf)
problem3.1 = nrow(dataf) # scientific notation: 1.7 x 10^15

#' 2. *The abstract also reports 80 "explanatory variables (23 nominal, 23 ordinal, 14 discrete, and 20 continuous)."  How many factor, character, and numeric variables do we have in the dataframe?*
#' 
problem3.2.factors = nrow(subset(skim_df, skim_type == 'factor'))
problem3.2.characters = nrow(subset(skim_df, skim_type == 'character'))
problem3.2.numerics = nrow(subset(skim_df, skim_type == 'numeric'))

#' 3. *Explain the relationship between the variables in the dataset and the variables in the dataframe as we've loaded it.* 
#' When we load the values from the dataset into the dataframe, they become 'typed' as in strings become character columns and numbers become numeric columns.
#' 
#' 

#' 4. *How many variables have missing values?  Hint: Check the class of the output of `skim()`.* 
#' 
problem3.4 = nrow(subset(skim_df, n_missing != 0))


#' # Problem 4 #
# problem 4 ----
#' *(This problem is a quick comprehension check for `dplyr` functions + pipes.)  `summarize()` is a tidyverse function that collapses multiple rows of data into a single row.  Like `mutate()` and `count()`, it respects groups constructed by `group_by()`.  Here's an example:* 

# dataf %>% 
#     group_by(ms.zoning) %>% 
#     summarize(sale.price = mean(sale.price)) %>% 
#     ungroup()

#' 1. *Examine the full codebook, at <http://jse.amstat.org/v19n3/decock/DataDocumentation.txt>.  What do the values of MS_Zoning represent?* 
#' MS_Zoning represents the general zoning classification of the sale such as agriculture or commercial
#' 
#' 

#' 2. *Run the following two expressions.  Why do they give different results?* 
#' 
#' The first expression is filtering the individual sale prices and then averaging whereas the second expression is averaging the sale prices and then filtering those averages.
#' 

dataf %>%
    group_by(ms.zoning) %>%
    filter(saleprice > 100000) %>%
    summarize(saleprice = mean(saleprice)) %>%
    ungroup()
# 
dataf %>%
    group_by(ms.zoning) %>%
    summarize(saleprice = mean(saleprice)) %>%
    filter(saleprice > 100000) %>%
    ungroup()


#' # Problem 5: Duplicate rows #
# problem 5 ----
#' *Now we'll take a look at some of the items on the checklist from Huebner et al.* 
#' 
#' 1. *Read the docs for `dplyr::distinct()`.  Then use this function to create a dataframe `dataf_nodup` with the duplicate rows removed.*  
#' 
dataf_nodup <- dataf %>% 
    distinct()

#' 2. *How many duplicate rows are in the dataset?*
#' 
n_duplicate = nrow(dataf) - nrow(dataf_nodup)


#' # Problem 6: Coding ordinal variables #
# problem 6 -----
#' *Because the CSV format doesn't have a way to document the variable type for its columns, ordinal variables will be represented either as strings or numerically.  When we load the CSV into R, these get parsed as character or numeric variables, respectively.*
#' 
#' *(It's actually a little more complicated for strings.  The base R function `read.csv()` involves a call to `as.data.frame()`, which has an argument `stringsAsFactors` that, if true, coerces all strings/characters into factors.  Prior to R version 4, the default value for this was `TRUE`, because back in the 1990s it was relatively rare to have actual text variables in the data.  So, up until last year, `read.csv()` by default would parse string columns into factors.  Both `readr::read_csv()` (a tidyverse package) and the current version of `read.csv()` by default parse string columns into characters.)*
#' 
#' 1. *Let's take a look at two condition variables, for the overall and exterior.  These are `overall.cond` and `exter.cond`, respectively. How are these two ordinal variables represented?*
#' 
#' overall.cond is represented as a numeric scale from 1-9 whereas exter.cond is represented as 2 letter codes such as TA, Ex, and Fa.
#' 

#' 2. *Since ultimately we're going to construct a Spearman rank correlation matrix (the quick-and-dirty approach to the problem), we need to get `exter.cond` into an integer representation. First, let's generate a table that shows the distribution across values of `exter.cond`, using `dplyr::count()`. Call this `ex_cond_count`. We'll use this to check the conversion process over the next few steps.*
ex_cond_count = dataf %>%
    count(exter.cond)

#' 3. *As a first attempt, write a function `char_to_int()` that takes a character vector as input, coerces to a factor using `as.factor()`, and then coerces it to `as.integer()`.  Using `mutate()` and `count()`, apply this function to `exter.cond` and check the distribution against your answer for #2.* 
char_to_int <- function(char_vec){
    return (char_vec %>% 
                as.factor() %>% 
                as.integer())
}

ex_cond_count2 <- dataf %>% 
    mutate(exter.cond = char_to_int(exter.cond)) %>% 
    count(exter.cond)
    

#' 4. *Can you explain what went wrong?  Hint:  Check the docs for the `levels` argument of `factor()`.*
#' The numbers corresponding to each classification are out of order for example Excellent is 1, but then Fair is 2 while Good is 3. 
#' 
#' 

#' 5. *We can fix this by passing in a character vector of the levels in the desired order, namely, from Po (Poor) to Ex (Excellent).  Modify `char_to_int()` to use such a vector in the `as.factor()` call.  Why doesn't this work?*
#' 
#' This gives an error because using the hardcoded vector does not correspond to the length of the input vector.
#' 
char_to_int <- function(char_vec){
    return (as.factor(c("Po", "Fa", "TA", "Gd", "Ex")) %>% 
                as.integer())
}

# ex_cond_count3 <- dataf %>%
#     mutate(exter.cond = char_to_int(exter.cond)) %>% 
#     count(exter.cond)

#' 6. *The most efficient way to avoid this poor design is to use `forcats::fct_relevel()`.  This is loaded as part of the tidyverse, so you don't need to modify the packages loaded up above, or `DESCRIPTION`.  Rewrite `char_to_int()` again, using `fct_relevel()` in place of `as.factor()`, and check against your answer to #2 to ensure that this is all working as expected.*
#' 
char_to_int <- function(char_vec){
    return (char_vec %>% 
                fct_relevel(.,"Po", "Fa", "TA", "Gd", "Ex") %>% 
                as.integer())
}

ex_cond_count4 <- dataf %>% 
    mutate(exter.cond = char_to_int(exter.cond)) %>% 
    count(exter.cond)

#' 7. *Finally we want this factor to be in our analysis dataframe.  **Normally, to preserve immutability**, I would either do this in the pipe where we first loaded the CSV (as we did above, when we fixed the names), or start by creating something like `dataf_raw` and then write a pipe that did all the cleaning steps and assigning the result to `dataf`, including this.  For the purposes of this lab, we'll just do it here.  Using either `mutate_at()` or `mutate(across())`, apply `char_to_int()` to all of the condition variables represented using these same levels:  `exter.cond`, `bsmt.cond`, `heating.qc`, `garage.cond`.* 

dataf <- dataf %>% 
    mutate_at(c("exter.cond", "bsmt.cond", "heating.qc", "garage.cond"), char_to_int)



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
cor_matrix = dataf %>% 
    select(where(is.numeric)) %>% 
    cor(use = "pairwise.complete.obs", method = "spearman")


#' 2. *Now we convert the correlation matrix into a dataframe. Uncomment the following line, and explain what it's doing.* 
cor_df = as_tibble(cor_matrix, rownames = 'covar')
#' This line converts the matrix into a dataframe where the first column of each row is the name of a numeric column from dataf and the rest of the columns compare that variable to each of the other variables.
#' 
#' 


#' 3. *What do the rows of `cor_df` represent?  The columns?  The values in each cell?* 
#' - rows: each row compares the variable specified by the variable in the first column to each of the other numeric variables from dataf
#' - columns: the columns compare the variable specified in the column name to each of the other variables
#' - values: each value represents the correlation between the variable specified by the column name and the row name

#' 4. *We've calculated the correlations for each pair of variables.  Now we want to construct a table with the top 10 most highly-correlated variables.  Write a pipe that does the following, in order:*  
#' - *Start with `cor_df`*
#' - *Select the columns with the name of each covariate and its correlation with sale price*
#' - *Constructs a new column with the absolute value of the correlation*
#' - *Arranges the rows in descending order by absolute correlation*
#' - *Keep the top 10 rows.  Hint: `?top_n`*
#' - *Assigns the result to the variable `top_10`*
#' 
top_10 <- cor_df %>% 
    select(covar, saleprice) %>% 
    mutate(abs_cor = abs(saleprice)) %>% 
    arrange(desc(abs_cor)) %>% 
    top_n(10)

#' # Problem 8 #
# problem 8 ----
#' *In 1.2, you identified some variables that you thought might be good predictors of sale price.  How good were your expectations?* 
#' 
#' One of my predictions (overall quality) was the highest correlated variable to sale price and my other prediction (year built) was fourth. I also predicted lot area but that did not make the top 10.
#' 
