#' @export
find_distance_matrix <- function(keywords, m_measures, min_dist){

  # Make distance matrix
  D <-
    m_measures %>%
    rbind(
      data_frame(
        keyword_1 = keywords,
        keyword_2 = keywords,
        m = 1
      )
    ) %>%
    spread(keyword_2, m, fill = 0) %>%
    select(-keyword_1) %>%
    as.matrix()

  # Only have half a distance matrix, need to copy to upper half
  D <- D + t(D)
  diag(D) <- diag(D)/2
  rownames(D) <- colnames(D)

  # Distance is 1 if in same cluster, 0 otherwise
  D <- D > 1 - min_dist

  return(D)
}


#' @export
find_groups <- function(D){
  # Go from distance matrix to a cluster ID for each keyword

  # Find group number for each keyword
  groups <-
    data_frame(
      keyword = colnames(D),
      group   = rep(NA, nrow(D))
    )

  group <- 1

  for (row in rep(1:nrow(groups), 2)){

    if (is.na(groups$group[row])){

      # If we have no group for this keyword
      # Set it to the current group
      groups$group[row] <- group
      keyword_group <- group

      # Next time we have no group, we will use the next index
      group <- group + 1

    } else {

      # If we do have a group
      keyword_group <- groups$group[row]

    }

    # Set any keywords that are linked to this keyword to the same group
    for (keyword in (row + 1):nrow(groups)){

      if (row + 1 > nrow(groups)) break

      if (D[row, keyword]){
        groups$group[keyword] <- keyword_group
      }

    }

  }

 return(groups)
}
