# Define a function to handle the logic
handleAddButton <- function(selected_var, input_number, numbers) {
  idx <- which(numbers$numbersList == selected_var)
  
  if (input_number == 0) {
    # If the entered number is 0, remove the variable from the list
    if (selected_var %in% numbers$numbersList) {
      numbers$numbersList <- numbers$numbersList[-idx]
      numbers$numericValues <- numbers$numericValues[-idx]
    }
  } else {
    # Calculate the sum including the new value
    new_sum <- numbers$sum - sum(numbers$numericValues[idx]) + input_number
    
    if (new_sum <= 1.0) {
      if (selected_var %in% numbers$numbersList) {
        # Update the numeric value associated with the variable
        numbers$numericValues[idx] <- input_number
      } else {
        # If it's a new variable, add it to the list
        numbers$numbersList <- c(numbers$numbersList, selected_var)
        numbers$numericValues <- c(numbers$numericValues, input_number)
      }
      
      # Update the sum
      numbers$sum <- new_sum
    }
  }
}