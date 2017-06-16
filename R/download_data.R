#' @export
cluster_summary_data <- function(df, groups, my_domain){
  unique_urls <-
    df %>%
    left_join(groups) %>%
    filter(base_domain == my_domain) %>%
    group_by(group) %>%
    summarise(
      n_urls      = n(),
      unique_urls = n_distinct(href),
      urls        = paste(unique(href), collapse = ', ')
    )

  weighted_average_google_rank <-
    df %>%
    filter(base_domain == my_domain) %>%
    left_join(groups) %>%
    group_by(group) %>%
    summarise(
      weighted_average_ranking = weighted_mean(google_rank, search_volume, na.rm = TRUE)
    ) %>%
    filter(!is.nan(weighted_average_ranking))

  cluster_summary <-
    df %>%
    left_join(groups) %>%
    arrange(group, desc(search_volume)) %>%
    group_by(group) %>%
    summarise(
      n_keywords = n_distinct(keyword),
      highest_search_volume = first(search_volume),
      total_search_volume = sum(search_volume, na.rm = TRUE),
      keywords      = paste(unique(keyword), collapse = ', '),
      first_keyword = first(keyword)
    )  %>%
    left_join(unique_urls) %>%
    left_join(weighted_average_google_rank) %>%
    mutate_if(is.double, ~if_else(is.nan(.x), NA_real_, .x))

  return(cluster_summary)
}

#' @export
top_10_clusters <- function(cluster_summary){
  cluster_summary %>%
    arrange(desc(total_search_volume)) %>%
    select(
      `Cluster ID` = group,
      `Highest Volume Keyword` = first_keyword,
      `Highest Volume Keyword Search Volume` = highest_search_volume,
      `Total Search Volume` = total_search_volume,
      `Number of Keywords` = n_keywords,
      `Weighted Average Ranking for Your Domain` = weighted_average_ranking,
      `Your Domain URLs in Cluster` = n_urls,
      `Unique Your Domain URLs in Cluster` = unique_urls,
      `Keywords` = keywords,
      `URLs` = urls
    )
}

#' @export
cannibalised_clusters <- function(cluster_summary){
  cluster_summary %>%
    filter(unique_urls > 1) %>%
    select(
      `Cluster ID` = group,
      `Highest Volume Keyword` = first_keyword,
      `Highest Volume Keyword Search Volume` = highest_search_volume,
      `Total Search Volume` = total_search_volume,
      `Number of Keywords` = n_keywords,
      `Weighted Average Ranking for Your Domain` = weighted_average_ranking,
      `Your Domain URLs in Cluster` = n_urls,
      `Unique Your Domain URLs in Cluster` = unique_urls,
      `Keywords` = keywords,
      `URLs` = urls
    )
}

#' @export
consolidation_oppertunities <- function(cluster_summary){
  cluster_summary %>%
    filter(highest_search_volume < total_search_volume) %>%
    arrange(desc(highest_search_volume)) %>%
    select(
      `Cluster ID` = group,
      `Highest Volume Keyword` = first_keyword,
      `Highest Volume Keyword Search Volume` = highest_search_volume,
      `Total Search Volume` = total_search_volume,
      `Number of Keywords` = n_keywords,
      `Weighted Average Ranking for Your Domain` = weighted_average_ranking,
      `Your Domain URLs in Cluster` = n_urls,
      `Unique Your Domain URLs in Cluster` = unique_urls,
      `Keywords` = keywords,
      `URLs` = urls
    )
}
