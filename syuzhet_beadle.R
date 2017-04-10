library(syuzhet)
library(tools)
library(dtt)

## path to text files
files <- list.files(path="/Users/mshort/Documents/pca 2017/beadle", pattern="*.txt", full.names=T, recursive=FALSE)

## initialize a data frame
df <- data.frame(matrix(ncol = 102))


for(i in files){
  ## convert text to string
  text <- get_text_as_string(i)
  ## get sentence vectors by tokenizing text on sentence boundaries
  s_v <- get_sentences(text)
  ## assess sentiment for each sentence using syuzhet dictionary
  sentiment_vector <- get_sentiment(s_v, method="syuzhet")
  ## apply simple discrete cosine transformation (DCT),
  ## which will produce a smoothed version of the sentiment vector
  dct_sent <- get_dct_transform(
    sentiment_vector, 
    low_pass_size = 5, 
    x_reverse_len = 100,
    scale_vals = F,
    scale_range = T
  )
  ## calculate moving average for each vector
  pwdw <- round(length(sentiment_vector)*.1)
  sentiment_rolled <- zoo::rollmean(sentiment_vector, k=pwdw)
  ## vectors will be different lengths, so resize
  sentiment_list <- rescale_x_2(sentiment_rolled)
  
  ## write raw sentiment vectors to CSV
  row <- as.matrix(cbind(t(sentiment_vector), c(basename(file_path_sans_ext(i)))))
  write.table(row, file = "/Users/mshort/Documents/pca 2017/sentiment_vector.csv", sep = ",", 
              col.names = FALSE, append=TRUE)
  ## write DCT sentiment vectors to CSV
  row <- as.matrix(cbind(t(dct_sent), c(basename(file_path_sans_ext(i)))))
  write.table(row, file = "/Users/mshort/Documents/pca 2017/dct_sentiment.csv", sep = ",", 
              col.names = FALSE, append=TRUE)
  
  ## simple_plot will apply three smoothing methods to raw sentiment vectors
  tiff_path <- file.path("/Users/mshort/Documents/pca 2017/simple_plots", paste(basename(file_path_sans_ext(i)), ".tiff", sep = ""))
  tiff(file=tiff_path, width = 10, height = 10, units = 'in', res = 400)
  simple_plot(sentiment_vector, title = basename(i), legend_pos = "top")
  dev.off()
  ## plot rescaled vectors for the purpose of comparison
  tiff_path <- file.path("/Users/mshort/Documents/pca 2017/plots", paste(basename(file_path_sans_ext(i)), ".tiff", sep = ""))
  tiff(file=tiff_path, width = 10, height = 5, units = 'in', res = 400)
  plot(sentiment_list$x, sentiment_list$z, type="l", col="blue", xlab = "Narrative Time", ylab = "Emotional Valence", main = basename(i))
  dev.off()
}

