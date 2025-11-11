#' Give appropriate anova test
#'
#' Performs appropriate anova test or alternative test based on the assumption tests, particularly normality and homogeneity for the data set.
#'
#' @param x A (non-empty) vector of characters or numbers (ie. integers, doubles).
#' @param y A (non-empty) vector of characters or numbers (ie. integers, doubles). If x is a character vector, y should be a numeric one with the same length as x. If x is a numerical vector, y should be a character one with the same length as x.
#' @param basic_info logical value, whether to print the numeric and graphic summaries.
#' @returns
#' An object of a list. The output has the following properties:
#'
#' * The groups are balanced or not
#' * Results of normality test (satisfied or violated)
#' * Results of homogeneity test (satisfied or violated)
#' * The p-value from the appropriate test for the given data set
#' * The specific name of test used for the data set
#' * The final conclusion for the test
#' * Graphics if necessary
#' * Further messages for some specific cases.
#' @examples
#' data("iris")
#' x <- as.numeric(iris$Sepal.Length)
#' y <- as.character(iris$Species)
#' comp_anova(x,y)
#'
#' data("airquality")
#' data <- airquality[1:40,]
#' x <- as.character(data$Month)
#' y <- data$Wind
#' comp_anova(x,y)
#'
#' z <- data$Solar.R
#'comp_anova(x,z)





comp_anova <- function(x, y, basic_info = T){
  library(car)
  library(PMCMRplus)
  library(pastecs)
  # determine which is c and which is q
  if((is.numeric(x)&is.character(y)) | (is.character(x)&is.numeric(y))){
    if(is.numeric(x)){
      quant <- x
      cate <- y
    } else{
      quant <- y
      cate <- x
    }
  } else {
    stop("Please enter one numeric vector as quantative variable and one character vector as categorical variable.")
  }
  if(length(x) != length(y)){
    stop("Please check your input length. x and y have to be the same length.")
  }

  res <- list(balance=NULL, normality=NULL, homogeneity=NULL, p_value=NULL, method=NULL, conclusion=NULL)



  # check for balance(chi-square goodness fit)
  tab <- table(cate)
  # check each variable >= 5

  if(basic_info==T){
    basic_info(x,y)
  }

  c_p <- chisq.test(tab)[[3]]
  # if balanced
  if(c_p>0.05){
    p_value <- na.omit(anova(aov(quant ~ cate))[[5]])[[1]]
    res$balance <- paste("T")
    res$method <- paste("ANOVA")
    res$p_value <- p_value
    message("Since the sample is balanced, we do not need to check for the normality and homogeneity.")

  } else {
    res$balance <- paste("F")
    # check for normality (Shapiro-Wilk)
    p_value_n <- rep(NA, length(tab))
    for(i in 1:length(tab)){
      p_value_n[i] <- tapply(quant, cate,shapiro.test)[[i]][2]
    }
    # normality violated (Kruskal-Wallis test)
    if(sum(p_value_n < 0.05)!=0){
      p_value <- kruskal.test(quant ~ cate)[[3]]
      # homogeneity test not needed
      homo <- na.omit(suppressWarnings(leveneTest(quant ~ cate)[[3]]))
      res$normality <- paste("violated")
      res$p_value <- p_value
      res$homogeneity <- paste("Since normality is satisfied, no matter homogeneity if violated or not, we use same method to check for the p-value.")
      res$method <- paste("Kruskal-Wallis test")
    } else {
      res$normality <- paste("satisfied")
      # homogeneity test (Levene's test)
      p_value <- na.omit(suppressWarnings(leveneTest(quant ~ cate)[[3]]))
      # homogeneity violated ( ANOVA )
      if(p_value < 0.05){
        p_value <- na.omit(anova(aov(quant ~ cate))[[5]])[[1]]
        res$p_value <- p_value
        res$homogeneity <- paste("violated")
        res$method <- paste("ANOVA")

      } else {
        # whelch's adj. test
        p_value <- oneway.test(quant ~ cate)[[3]]
        res$p_value <- p_value
        res$homogeneity <- paste("satisfied")
        res$method <- paste("whelch's adj. test")
      }
    }
  }

  if(p_value > 0.05){
    conc <- paste("Since p_value", p_value, "is greater than alpha 0.05, fail to reject. We have insignificant evidence to suggest H1.")
    res$conclusion <- conc
    return(res)
  } else{
    conc <- paste("Since p_value", p_value, "is less than alpha 0.05, reject. We have significant evidence to suggest H1.")
    res$conclusion <- conc
    return(res)
  }
}



