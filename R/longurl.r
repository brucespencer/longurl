s_HEAD <- purrr::safely(httr::HEAD)
s_STATUS <- purrr::safely(httr::warn_for_status)

#' Expand a vector of (short) URLs using
#'
#' Pass in a vector of URLs (ostensibly "short" URLs) and receive
#' a data frame of the original URLs and expanded URLs
#'
#' @md
#' @param urls_to_expand character vector of URLs
#' @param warn show any warnings (API or otherwise) as messages
#' @param check.status after expanding, check the status of  the resulting URL
#' @param agent user agent to use (some sites switchup content based on user agents).
#'        Defaults to "`longurl-r-package`".
#' @param seconds number of seconds to wait for a response until giving up. Cannot be <1ms.
#' @param .progress display a progress bar (generally only useful in
#'        interactive sesions)
#' @return a data.table with the orignial URLs in `orig_url`, expanded URLs in
#'        `expanded_url` and the HTTP `status_code` of the expanded URL. Completely
#'        invalid URLs result in a `NA` value for `expanded_url` & `status_code`.
#' @export
#' @examples
#' test_urls <- c("http://t.co/D4C7aWYIiA",
#'                "1.usa.gov/1J6GNoW",
#'                "ift.tt/1L2Llfr")
#' big_urls <- expand_urls(test_urls)
#' head(big_urls)

expand_urls <- function(urls_to_expand,
                        warn = TRUE,
                        check.status = TRUE,
                        agent = "longurl-r-package",
                        seconds = 5,
                        .progress = interactive()) {

  if (.progress) pb <- progress_estimated(length(urls_to_expand))

    urls_to_expand <- as.character(urls_to_expand)

    all.triples = lapply(urls_to_expand, function(x) {

        if (.progress) pb$tick()$print()

        res <- s_HEAD(x, httr::user_agent(agent), httr::timeout(seconds))

        if (is.null(res$result)) {
            warning(sprintf("Invalid URL: [%s]", x))
            NULL
        } else if (!check.status) {
            list(orig_url = x, expanded_url = res$result$url)
        } else {
            sres <- s_STATUS(res$result)
            if (is.null(sres$result)) {
                warning("httr::warn_for_status() on HEAD request result")
                list(orig_url = x, expanded_url = NA, status_code = NA)
            } else {
                list(orig_url = x, expanded_url = res$result$url, status_code = res$result$status_code)
            }
        }
    })

    rbindlist(all.triples)
}

