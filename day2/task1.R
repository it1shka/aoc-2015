library(parallel)
library(checkmate)

ribbon <- function(l, w, h) {
  for (arg in c(l, w, h)) {
    assertNumeric(arg, lower = 0, finite = TRUE)
  }
  sides <- c(l * w, w * h, l * h)
  output <- 2 * sum(sides) + min(sides)
  return(output)
}

input <- read.csv("input.txt", header = FALSE, sep = "x")
names(input) <- c("length", "width", "height")

cluster <- makeCluster(detectCores())
clusterExport(cluster, c("ribbon", "assertNumeric"))
ribbons <- parApply(cluster, input, 1, function(row) {
  args <- as.list(row)
  names(args) <- NULL
  do.call(ribbon, args)
})
stopCluster(cluster)

sum(ribbons)
