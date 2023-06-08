#' Repeat specified indices in a list
#'
#' This function takes a list, indices to repeat, and the number of repetitions,
#' and returns a new list with the specified indices repeated at the end.
#' It also checks if the specified indices exist in the input list before performing the operation.
#'
#' @param input_list The input list to be modified.
#' @param indices_to_repeat A vector of indices to repeat.
#' @param num_repetitions The number of times to repeat the specified indices.
#'
#' @return A new list with the specified indices repeated at the end.
#' @examples
#' # Repeat indices 1 and 2, 5 times
#' new_list <- repeat_indices(criteria_list, c(1, 2), 5)
#' # Repeat the entire list once
#' new_list <- repeat_indices(criteria_list, seq_along(criteria_list), 1)
#'
#' @export
repeat_segments <- function(input_list, indices_to_repeat, num_repetitions) {
  # Check if the indices exist in the input list
  if (any(!indices_to_repeat %in% seq_along(input_list))) {
    stop("One or more of the specified indices do not exist in the input list")
  }
  
  # Repeat the indices and append to the input list
  repeated_items <- input_list[indices_to_repeat]
  for (i in 1:num_repetitions) {
    input_list <- c(input_list, repeated_items)
  }
  
  return(input_list)
}
