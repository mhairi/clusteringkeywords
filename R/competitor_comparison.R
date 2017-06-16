#' @export
weighted_mean <- function(x, w, ..., na.rm = FALSE){

  if(na.rm){

    df_omit <- na.omit(data.frame(x, w))
    return(weighted.mean(df_omit$x, df_omit$w, ...))

  }

  weighted.mean(x, w, ...)
}

#' @export
competitor_comparison_data <- function(df, my_domain, competitors, groups){

  domains <- c(my_domain, competitors)
  n_clusters <- max(groups$group)

  df %>%
    left_join(groups) %>%
    filter(base_domain %in% domains) %>%
    mutate(base_domain = factor(base_domain, levels = domains)) %>%
    group_by(base_domain, group) %>%
    summarise(
      mean_rank = weighted_mean(google_rank, search_volume, na.rm = TRUE),
      mean_volume = mean(search_volume, na.rm = TRUE),
      n_urls = n_distinct(href),
    ) %>%
    summarise(
      mean_rank = weighted_mean(mean_rank, mean_volume, na.rm = TRUE),
      mean_volume = mean(mean_volume, na.rm = TRUE),
      clusters_present = n_distinct(group),
      clusters_canablised = sum(n_urls > 1)
    ) %>%
    mutate(
      clusters_absent = n_clusters - clusters_present
    )
}
