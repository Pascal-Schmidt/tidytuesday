pattern_1 <- function(number) {
  
  spaces <- c((number - 1): 1, 0)
  stars <- vector(mode = "integer", length = number)
  stars[1] <- 1
  for(i in 2:length(stars)) {
    
    stars[i] <- stars[i - 1] + 2
    
  }
  
  stars <- c(stars, stars[(length(stars) - 1):1])
  spaces <- c(spaces, spaces[(length(spaces) - 1):1])
  
  for(i in seq_along(stars)) {
    
    cat(strrep(" ", spaces[i]))
    cat(strrep("*", stars[i]))
    cat("\n")
    
  }
} 

pattern_1(4)

pattern_2 <- function(number) {
  
  spaces <- c((number - 1): 1, 0)
  stars <- vector(mode = "integer", length = number)
  stars[1] <- 1
  for(i in 2:length(stars)) {
    
    stars[i] <- stars[i - 1] + 1
    
  }
  
  stars <- c(stars, stars[(length(stars) - 1):1])
  spaces <- c(spaces, spaces[(length(spaces) - 1):1])
  
  for(i in seq_along(stars)) {
    
    cat(strrep(" ", spaces[i]))
    cat(strrep("* ", stars[i]))
    cat("\n")
    
  }
} 

pattern_2(4)

# def diamond(number):
#   
#   spaces = [0] * number
# for i in range(number):
#   spaces[i] = i 
# 
# spaces_reversed = spaces
# spaces = spaces[::-1] + spaces_reversed
# del spaces[-number]
# 
# stars = [0] * number
# stars[0] = 1
# for i in range(1, number):
#   stars[i] = stars[i - 1] + 2
# 
# stars_reversed = stars[::-1]
# stars = stars + stars_reversed
# del stars[-number]
# 
# for i in range(len(stars)):
#   
#   print(" " * spaces[i], "*" * stars[i])

x <- list(c(259,770), c(448,54), c(926,667), c(184,139), c(840,118), c(577,469))
y <- unlist(lapply(x, function(x) { x[1] - x[2] }))
x <- x[order(y)]
sum(unlist(lapply(x[1:(length(x) / 2)], function(x) { x[1] }))) +
  sum(unlist(lapply(x[(length(x) / 2 + 1):length(x)], function(x) { x[2] })))


x <- c(1, 5, 8, 3, 3)
counter <- 0

while(length(unique(x)) != 1) {
    
    x <- x + 1
    x[which(max(x) == x)[1]] <- max(x) - 1
    counter = counter + 1
  
}
