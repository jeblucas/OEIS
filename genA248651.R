# Script (R, using base only) to build values in the sequence defined in A248651

# a(n) is the smallest number greater than a(n-1) such that no digit appears in the listing
# of all terms more than one time more than any other digit in the listing, with a(1) = 1.

digits <- c(0:9) #vector of the digits
digitCounts <- rep(0L,10) #vector for digit count tracking
A <- c() #vector of entries


A[1] <- 1L #first entry
failed <- 1L #failed entry
digitCounts[2] = 1L #impact of first entry on digitCounts

#Build a function that turns a number into a vector of its component digits
digits <- function(x) {
  if(length(x) > 1 ) {
    lapply(x, digits)
  } else {
    n <- floor(log10(x))+1L
    rev( x %/% 10^seq(0, length.out=n) %% 10 )
  }
}

# Engine for testing digits, runs VERY SLOW, but checks every integer. 
# I welcome improvements, note that it respects incrementing more than one digit at a 
# time if that makes sense.

while (length(A) < 101) {
  i <- max(failed, max(A)) + 1
  newCounts <- digitCounts
  digitList <- digits(i)
  
  for (j in digitList) {
    newCounts[j + 1] <- newCounts[j + 1] + 1
  }
  
  if (max(newCounts) - min(newCounts) > 1) {
       failed <- i } else {
        digitCounts <- newCounts
        A <- c(A,as.integer(i))
      }
}
