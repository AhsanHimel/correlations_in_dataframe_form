mosthighlycorrelated <- function(mydataframe,numtoreport=choose(n = ncol(mydataframe), k =2))
{
  dfnum <- mydataframe[sapply(mydataframe,is.numeric)] # takes only numeric variables
  # find the correlations
  cormatrix <- cor(dfnum, use = "pairwise.complete.obs")
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
mosthighlycorrelated(mtcars, numtoreport = 10)
mosthighlycorrelated(iris, numtoreport = 10)

dfnum <- iris[sapply(iris,is.numeric)] # takes only numeric variables
mosthighlycorrelated(iris) # gives all possible combination's correlation coefficients

# install and load GGally package for nice graphics
if(!require(GGally, quietly = T)){
  install.packages("GGally", quiet = T)
  require(GGally, quietly = T)
}
ggpairs(iris, aes(colour = as.character(Species), alpha = 0.4))
