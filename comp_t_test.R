#' Give appropriate t test
#'
#' Performs appropriate t test or alternative test based on the assumption tests, particularly normality and homogeneity for the data set.
#'
#' @param x A (non-empty) numeric vector (ie. integers, doubles).
#' @param y An optional (non-empty) numeric or character vector and defaults to NULL. If y is a character vector, it should has the same length as your x input and it can be dichotomized. If y is a numeric vector, it will be regarded as another sample.
#' @param mu 	A number indicating the true value of the mean
#' @param independence A logical value indicating whether two variables are independent or not (if you want to test for two samples). It's not necessary if you want to do a one-sample or paired test.
#' @param basic_info logical value indicating whether to print the numeric and graphic summaries.
#' @returns
#' An object of a list. The output has the following properties:
#'
#' * Results of normality test (satisfied or violated)
#' * Results of homogeneity test (satisfied or violated)
#' * The p-value from the appropriate test for the given data set
#' * The specific name of test used for the data set
#' * The final conclusion for the test
#' * Graphics if necessary
#' * Further messages for some specific cases.
#'
#' @examples
#' data("mtcars")
#' x <- mtcars$hp
#' comp_t_test(x, mu=20, independent = T, basic.info = T)
#'
#' y <- as.character(mtcars$vs)
#' comp_t_test(x, y,independent = T, basic.info = T)
#'
#' data("airquality")
#' data <- airquality[1:61,]
#' x <- data$Wind
#' y <- data$Month
#' comp_t_test(x, y, independent = T, basic.info = T)


comp_t_test <- function(x, y=NULL, mu=0, independent=T, basic.info=F){
  normality_test <- function(x){
    library(car)
    # check for normality
    par(mfrow=c(1,2))
    #Histogram + normal curve
    hist(x, freq=F)
    lines(density(x))
    curve(dnorm(x, mean=mean(x), sd=sd(x)), col="red", lwd=2, add=T)

    ## Normal QQ plots
    qqnorm(x)
    qqline(x)
    par(mfrow=c(1,1))

    library(pastecs)
    val <- stat.desc(x, norm=TRUE, basic=FALSE)
    skew.2se <- val[[9]]
    kurt.2se <- val[[11]]

    shapiro.test(x)
    sh_p <- shapiro.test(x)[[2]]
    return(c(skew.2se,kurt.2se,sh_p))
  }
  res <- list(normality=NULL, homogeneity=NULL, p_value=NULL, test_method=NULL, conclusion=NULL)
  exact <- NULL
  # conf.int=0.95
  # x should be quantitative vectors


  if(!is.numeric(x)){
    stop("X has to be a quantitative vector. Please check for it again.")
  }

  if(is.character(y)){
    if(length(x)!=length(y)){
      stop("The quantitative vector should have the same length as the categorical variable. Please check for your input again.")
    }
  }

  if(basic.info==T){
    basic_info(x,y)
  }
  # one sample t.test
  if(is.null(y)){
    # check for exact
    n_x <- length(x)
    if(n_x >= 12){
      exact <- F
    } else{
      exact <- T
    }

    if(normality_test(x)[3] > 0.05){
      normality <- T
      if((normality_test(x)[1] < -1 | normality_test(x)[1] >1) | (normality_test(x)[2] < -1 | normality_test(x)[2] >1)){
        message("skew.2SE and/or kurt.2SE are not in the range of (-1,1), indicating that the normality might need to be checked more carefully, through the p-value of Shapiro-test seems good. ")
      }
      res$normality <- paste("Normality is satisfied.")
    } else {
      normality <- F
      res$normality <- paste("Normality is violated.")
    }

    # normality violated: one sample wilcoxon test
    if(normality==F){
      p_value <- wilcox.test(x, mu=mu, exact = exact)[[3]]
      res$p_value <- p_value
      res$test_method <- paste("One sample wiconxon test")
    } else {
      # one sample t.test
      p_value <- t.test(x, mu)[[3]]
      res$p_value <- p_value
      res$test_method <- paste("One sample t test")
    }
    res$homogeneity <- paste("One sample does not need a homogeneity test.")

  } else {
  # two sample tests
    if(length(unique(y))==2){
      if(is.numeric(y)){
        message("Y is regarded as quantitative variable. If you want to use it as a categorical variable since it is dichotomized, please change Y to characters.")
      }
      if(is.character(y)){
        temp_matix <- rbind(x,y)
        x <- as.numeric(temp_matix[1,][which(temp_matix[2,]==unique(y)[1])])
        y <- as.numeric(temp_matix[1,][which(temp_matix[2,]==unique(y)[2])])
      }
    } else {
      stop("Check for your categorical input since it can not be dichotomized.")
    }


    if(independent==F){
      # check for the length
      if(length(x)!=length(y)){
        stop("The sample lengths should be identical for paired samples")
      }

      # normality test
      xd <- y-x
      if(normality_test(xd)[3] > 0.05){
        normality <- T
        res$normality <- paste("Normality is satisfied.")
        norm_res <- normality_test(xd)
        if((norm_res[1] < -1 | norm_res[1] >1) | (norm_res[2] < -1 |norm_res[2] >1)){
          message("skew.2SE and/or kurt.2SE are not in the range of (-1,1), indicating that the normality might need to be checked more carefully, through the p-value of Shapiro-test seems good. ")
        }
      } else {
        normality <- F
        res$normality <- paste("Normality is violated.")
      }

      # normality violated: wilcoxon signed rank test
      if(normality==F){
        p_value <- wilcox.test(x, y, paired = T, exact = exact)[[3]]
        res$p_value <- p_value
        res$test_method <- paste("Wilcoxon signed rank test")
      } else {

        # one sample t.test
        p_value <- t.test(x, y, paired = T)[[3]]
        res$p_value <- p_value
        res$test_method <- paste("paired samples t test")
      }
      res$homogeneity <- paste("paired samples do not need a homogeneity test.")
    }

    if(independent==T){
      norm_res_x <- normality_test(x)
      norm_res_y <- normality_test(y)
      if(norm_res_x[3] > 0.05 & norm_res_y[3] > 0.05){
        normality <- T
        res$normality <- paste("Normality is satisfied.")
        if((norm_res_x[1] < -1 | norm_res_x[1] >1) | (norm_res_x[2] < -1 | norm_res_x[2] >1)){
          message("skew.2SE and/or kurt.2SE for X are not in the range of (-1,1), indicating that the normality might need to be checked more carefully, through the p-value of Shapiro-test seems good. ")
        }
        if((norm_res_y[1] < -1 | norm_res_y[1] >1) | (norm_res_y[2] < -1 | norm_res_y[2] >1)){
          message("skew.2SE and/or kurt.2SE for Y are not in the range of (-1,1), indicating that the normality might need to be checked more carefully, through the p-value of Shapiro-test seems good. ")
        }
      } else {
        normality <- F
        res$normality <- paste("Normality is violated.")
      }

      # normality violated:
      cate <- rep("A", length(x))
      cate<- as.factor(c(cate,rep("B", length(y))))
      quant <- x
      quant <- c(quant,y)
      if(normality==F){
        # homogeneity
        library(car)
        leveneTest(quant ~ cate)
        if(na.omit(leveneTest(quant ~ cate)[[3]]) < 0.05){
          res$homogeneity <- paste("Homogeneity is violated.")
        } else {
          res$homogeneity <- paste("Homogeneity is satisfied.")
        }
        # wilcoxon rank sum test
        message("Since normality is violated, no matter homogeneity satisfied or not, we use wilcoxon rank sum test.")
        p_value <- wilcox.test(x, y, exact = exact, paired=F)[[3]]
        res$p_value <- p_value
        res$test_method <- paste("Wilcoxon rank sum test")

      } else {
        # normality satisfied
        library(PMCMRplus)
        leveneTest(quant ~ cate)


        # homogeneity violated
        if(na.omit(leveneTest(quant ~ cate)[[3]]) < 0.05){
          # t.test unequal var
          p_value <- t.test(x,y,var.equal = F)[[3]]
          res$p_value <- p_value
          res$homogeneity <- paste("Homogeneity is violated.")
          res$test_method <- paste("Two sample t test (unequal variance)")
        } else {
          # t.test equal var
          p_value <- t.test(x,y,var.equal = T)[[3]]
          res$p_value <- p_value
          res$homogeneity <- paste("Homogeneity is satisfied.")
          res$test_method <- paste("Two sample t test (equal variance)")
        }

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









