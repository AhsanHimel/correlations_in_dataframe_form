mosthighlycorrelated <- function(mydataframe,numtoreport=choose(n = ncol(mydataframe), k =2))
{
  # find the correlations
  cormatrix <- cor(mydataframe, use = "pairwise.complete.obs")
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable", "Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
mosthighlycorrelated(mtcars, 10)

dfnum <- df[sapply(df,is.numeric)] # takes only numeric values

library(GGally)
ggpairs(mtcars, aes(colour = as.character(am), alpha = 0.4))
