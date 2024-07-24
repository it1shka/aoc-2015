my_sequence <- "1113122113"
for (i in 1:40) {
  print(i)
  encoding <- rle(strsplit(my_sequence, "", fixed = TRUE))
  parts <- paste0(encoding$lengths, encoding$values, sep = "")
  my_sequence <- do.call(paste0, as.list(parts))
}
nchar(my_sequence)
