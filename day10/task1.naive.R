look_and_say <- function(original) {
  output <- original
  pointer <- 1
  while (pointer <= nchar(original)) {
    current <- substring(original, pointer, pointer)
    count <- 0
    while (substring(original, pointer, pointer) == current &&
             pointer <= nchar(original)) {
      pointer <- pointer + 1
      count <- count + 1
    }
    output <- paste(output, count, current, sep = "")
  }
  output
}

perform_transformation <- function(initial, times) {
  current <- initial
  for (i in 1:times) {
    print(i)
    current <- look_and_say(current)
  }
  current
}

input <- "1113122113"
iterations <- 40
result <- perform_transformation(input, iterations)
nchar(result)
