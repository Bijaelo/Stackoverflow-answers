

# Set region and connection variable
region <- ''
key <- ''
secret <- ''
bucket <- ''

calculate_s3_etag <- function(file_path, chunk_size = 100 * 1024**2){
  require(digest)
  # Open and read file
  f <- file(file_path, 'rb')
  on.exit(close(f))
  md5 <- list()
  while(!identical((r <- readBin(f, raw(), chunk_size)), raw())){
    md5 <- c(md5, list(r))
  }
  md5 <- unlist(md5)
  list('etag' = digest::digest(as.raw(md5), 'md5', serialize = FALSE),
       'size' = length(md5))
}
# Import tag from S3
if(key != ''){
  head <- head_object(object = obj,
                      bucket = pbucket,
                      key = key,
                      secret = secret,
                      region = region)
}else{
  # If no one has a key, use known value from own S3 bucket.
  head <- ''
  attr(head, 'etag') <- "d8c329c9d51543437ebe20e5a02b834e"
  attr(head, 'size') <- 221770
}
calc <- calculate_s3_etag('daily.parquet')
tag <- calc$etag
size <- calc$size
cat('Tag identical? - \n', identical(tag,
                                     # tag taken from S3 head.
                                     gsub('\\"', '', attr(head, 'etag'))), '\n')

cat('Size identical? - \n', identical(size,
                                     # tag taken from S3 head.
                                     as.integer(attr(head, 'content-length'))), '\n')

