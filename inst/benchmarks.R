remotes::install_github("paws-r/paws/paws.common")

library(ggplot2)
# Note: I believe python's encoder will have a slight disadvantage
# due to it needing to convert to R.
parse <- reticulate::import("urllib.parse")

################################################################################
# Static parameters
################################################################################
string <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-._~`!@#$%^&*()=+[{]}\\|;:'\",<>? "
iter <- 1:7

################################################################################
# Benchmark with default settings
################################################################################
bm <- list()
for (i in iter) {
  url <- paste(sample(strsplit(string, "")[[1]], (10 ^ i), replace = T), collapse = "")
  bm[[i]] <- bench::mark(
    url_encode_rcpp = paws.common:::paws_url_encoder(url),
    url_encode_cpp11 = EncodeCpp11::cpp11_url_encoder(url),
    url_encode_curl = curl::curl_escape(url),
    url_encode_python = parse$quote(url),
    iterations = 100
  )
}

result <- do.call(rbind, bm)
result$size <- sort(rep(10 ^ iter, 4))
result$expression <- as.character(result$expression)

res <- tidyr::unnest(result, c(time, gc))
p <- ggplot(res, aes(time, expression)) +
  geom_violin() +
  facet_wrap("size", scales = "free_x") +
  theme(
    text = element_text(size = 8),
    axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)
  )
ggsave("inst/benchmark_results_default.jpg", plot = p, scale = .5)


result[, c("expression", "min", "median",
           "itr/sec", "mem_alloc", "gc/sec",
           "n_itr", "n_gc", "total_time", "size")] |>
  write.csv("inst/benchmark_results_default.csv", quote = F, row.names = F)

################################################################################
# Benchmark with additional ASCII characters added to "safe"
################################################################################
bm <- list()
for (i in iter) {
  url <- paste(sample(strsplit(string, "")[[1]], (10 ^ i), replace = T), collapse = "")
  bm[[i]] <- bench::mark(
    url_encode_rcpp = paws.common:::paws_url_encoder(url, ";:>? "),
    url_encode_cpp11 = EncodeCpp11::cpp11_url_encoder(url, ";:>? "),
    url_encode_python = parse$quote(url, ";:>? "),
    iterations = 100
  )
}

result <- do.call(rbind, bm)
result$size <- sort(rep(10 ^ iter, 3))
result$expression <- as.character(result$expression)

res <- tidyr::unnest(result, c(time, gc))
p <- ggplot(res, aes(time, expression)) +
  geom_violin() +
  facet_wrap("size", scales = "free_x") +
  theme(
    text = element_text(size = 8),
    axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)
  )
ggsave("inst/benchmark_results_safe.jpg", plot = p, scale = .5)

result[, c("expression", "min", "median",
           "itr/sec", "mem_alloc", "gc/sec",
           "n_itr", "n_gc", "total_time", "size")] |>
  write.csv("inst/benchmark_results_safe.csv", quote = F, row.names = F)
