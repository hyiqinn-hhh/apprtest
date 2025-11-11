#' Give proper basic information
#'
#' Performs appropriate numeric and graphic summaries for the input data set.
#'
#' @param x A (non-empty) vector of characters or numbers (ie. integers, doubles).
#' @param y An optional (non-empty) vector of characters or numbers (ie. integers, doubles) and defaults to NULL.
#'
#' @returns
#' An object of a list. The output has the following properties:
#'
#' * A list with a table for case C
#' * A list with an absolute frequency table, relative frequency table, conditional relative frequency by rows and conditional relative frequency by columns for case CC.
#' * A list with a group wise summary for case CQ
#' * A list with a numerical summary for case Q
#' * A list with a correlation for case QQ
#' * Appropriate graphics for each case
#' @examples
#' x <- c(1,4,3,5,6,4,7,8)
#' y <- c("a","b","a","b","a","b","a","b")
#' z <- c(3,5,2,5,6,2,10,5)
#' basic_info(x)
#' basic_info(x,y)
#' basic_info(x,z)




basic_info <- function(x, y=NULL){
  # categorical variables should be characters
  # quantitative var should be numeric

  if(is.character(x)){
    if(is.null(y)){
      # barchart
      tab0 <- table(x)
      barplot(tab0)
      return(list(barchart=tab0))
    }
    if(is.character(y)){
      tab2 <- table(x,y) # absolute fre
      rel_fre <- prop.table(tab2)
      cond_r <- prop.table(tab2, margin=1)
      cond_c <- prop.table(tab2, margin=2)
      # stacked barchart
      barplot(cond_c)
      barplot(cond_r)
      return(list(absolute_fre=tab2, relative_fre=rel_fre, conditional_relative_fre_row=cond_r, conditional_relative_fre_col=cond_c))
    }
    if(is.numeric(y)){
      # groupwise summaries
      g_mean <- tapply(y,x, FUN = mean)
      g_var <- tapply(y,x, FUN = var)
      # s-b-s boxplot
      boxplot(y~x, las=2)
      return(list(groupwise_mean=g_mean))
    }
  }

  if(is.numeric(x)){
    if(is.null(y)){
      num_s <- summary(x)
      boxplot(x)
      hist(x)
      return(list(numeric_summaries=num_s))
    }
    if(is.character(y)){
      # groupwise summaries
      g_mean <- tapply(X=x, INDEX=y, FUN = mean)
      g_var <- tapply(X=x, INDEX=y, FUN = var)
      # s-b-s boxplot
      boxplot(x~y, las=2)
      return(list(group_mean=g_mean, group_variance=g_var))
    }
    if(is.numeric(y)){
      corr <- cor(x=x,y=y)
      plot(x,y)
      return(list(correlation=corr))
    }
  }
}



