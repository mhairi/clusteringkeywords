#' @export
m_measure <- function(set_1, set_2){
  # Finds the m-measure difference for two ranked lists, as described in this paper
  # https://pdfs.semanticscholar.org/d78b/d6bf2f7abb7b8ff52ccf7b58de1e8a3bea88.pdf

  k <- 12 # Only compare the top 12 results
  denom <- k + 1
  i <- 1:denom
  norm <- sum(2*(1/i - 1.0/(k+1)))

  Z = intersect(set_1, set_2)
  S = setdiff(set_1, set_2)
  Tee = setdiff(set_2, set_1)

  Z_sum = sum(abs(1/match(Z, set_1) - 1/match(Z, set_2)))
  S_sum = sum(1/match(S, set_1) - 1/denom)
  T_sum = sum(1/match(Tee, set_2) - 1/denom)

  M_prime = Z_sum + S_sum + T_sum
  M = 1 - (M_prime/norm)

  return(M)
}


#' @export
find_m_measures <- function(df){
  # Get m_measures for every combination of keywords in a dataframe
  k <- 12

  df <-
  df %>%
    filter(google_rank <= k)

  m_measures <-
  df$keyword %>%
    unique %>%
    combn(2) %>%
    t %>%
    as_data_frame %>%
    rename(keyword_1 = V1, keyword_2 = V2) %>%
    filter(keyword_1 != keyword_2) %>%
    right_join(select(df, urls_1 = href, keyword), by = c('keyword_1' = 'keyword')) %>%
    right_join(select(df, urls_2 = href, keyword), by = c('keyword_2' = 'keyword')) %>%
    filter(!is.na(keyword_1)) %>%
    filter(!is.na(keyword_2))

  m_measures <-
    m_measures %>%
    group_by(keyword_1, keyword_2) %>%
    summarise(
      m = m_measure(urls_1, urls_2)
    ) %>%
    ungroup

  return(m_measures)

}
