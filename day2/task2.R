library(parallel)

ribbon <- function(l, w, h) {
  perimeters <- 2 * c(l + w, w + h, l + h)
  volume <- l * w * h
  min(perimeters) + volume
}

input <- read.csv("input.txt", header = FALSE, sep = "x")
names(input) <- NULL

cluster <- makeCluster(detectCores())
clusterExport(cluster, c("ribbon"))
feet <- parApply(cluster, input, 1, function(row) {
  args <- as.list(row)
  do.call(ribbon, args)
})
stopCluster(cluster)

sum(feet)
