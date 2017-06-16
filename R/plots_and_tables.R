
#' @export
make_table <- function(m_measures){
  m_measures %>%
    arrange(desc(m)) %>%
    filter(m != 0) %>%
    select(`Keyword 1` = keyword_1,
           `Keyword 2` = keyword_2,
           `Similarity Score` = m)
}


#' @export
make_heatmap <- function(m_measures){
  m_measures %>%
    ggplot +
    aes(x = keyword_1, y = keyword_2, fill = m) +
    geom_tile() +
    scale_fill_distiller(direction = 1) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(x = '', y = '', fill = 'Similarity')
}


#' @export
network_plot <- function(D, groups, df, domain){

  net <-
    D %>%
    as.numeric() %>%
    matrix(nrow = nrow(D)) %>%
    network()

  links <-
    net %>%
    as.edgelist() %>%
    as.data.frame() %>%
    rename(source = V1, target = V2) %>%
    mutate(source = source - 1,
           target = target - 1)

  # If we have no links, set one link going from the first node
  # to its self. This gives the correct plot.
  if (nrow(links) == 0){
    links <-
      data_frame(
        source = 0,
        target = 0
      )
  }

  node_attributes <-
    data_frame(
      keyword = colnames(D)
    ) %>%
    left_join(
      df %>%
        group_by(keyword) %>%
        summarise(has_domain = domain %in% base_domain)
    )

  nodes <-
    node_attributes %>%
    rename(name = keyword, group = has_domain) %>%
    mutate(
      group = as.character(group),
      group = recode(group,
                     `FALSE` = 'Does not include Domain',
                     `TRUE`  = 'Includes Domain')
    )

  forceNetwork(Links = links,
               Nodes = nodes, Source = "source",
               Target = "target", NodeID = "name",
               Group = "group", legend = TRUE, opacity = 1,
               opacityNoHover = 1)

}

#' @export
all_domains_table <- function(df){
  df %>%
    filter(base_domain != '') %>%
    filter(!is.na(base_domain)) %>%
    group_by(base_domain) %>%
    summarise(
      mean_rank = mean(google_rank),
      n = n()
    ) %>%
    arrange(desc(n), mean_rank) %>%
    slice(1:10) %>%
    rename(
      Domain = base_domain,
      `Times Domain is Ranked` = n,
      `Average Rank` = mean_rank
    )
}



#' @export
competitor_comparison_table <- function(competitor_comparison){

  competitor_comparison %>%
    dplyr::select(
      `Domain` = base_domain,
      `Weighted Average Rank` = mean_rank,
      `Clusters Present` = clusters_present,
      `Clusters Absent` = clusters_absent,
      `Clusters Cannabalised` = clusters_canablised
    )
}

#' @export
competitor_bubble_chart <- function(competitor_comparison){

  ggplot(competitor_comparison) +
    aes(x = clusters_canablised,
        y = mean_rank,
        label = base_domain) +
    geom_point(aes(size = mean_volume)) +
    geom_label_repel() +
    labs(
      x = '\nClusters Cannabalised',
      y = 'Weighted Average Google Rank\n',
      size = 'Mean Volume'
    ) +
    theme_minimal()

}

