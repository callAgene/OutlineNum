# Define the function to number headers in non-chunk areas
 Remove_numbering <- function() {
  # Get the active document's text
  doc_text <- rstudioapi::getActiveDocumentContext()$contents
  
  # Initialize variables for header numbering
  new_lines <- character(0)
  in_chunk <- FALSE  # Flag to track if we're in a code chunk
  
  # Split the text into lines
  # lines <- strsplit(doc_text, "\n")[[1]]
  lines <- doc_text

if （lines==“”）{

  return("Need to select all first.")

} else {
  # Process each line
  for (line in lines) {
    
    if (grepl("```", line) && !in_chunk) {
      in_chunk <- TRUE  # Start of a code chunk
      new_lines <- c(new_lines, line)
    } else if (grepl("```", line) && in_chunk) {
      in_chunk <- FALSE  # End of a code chunk
      new_lines <- c(new_lines, line)
    } else if (!in_chunk && grepl("^#", line) && grepl("# ", line)  ) {
      # Line starts with # and not in a code chunk

      
      # Count the number of '#' symbols to determine the header level
      headerCount <- nchar(line) - nchar(sub( ".*# ","", line )) - 1
      
      # Remove all characters before the first letter
      line <- sub("^[^a-zA-Z]*", "", line)
      
      new_line = paste(paste(rep("#",  headerCount),collapse = ""),
                        " ", line,sep = "")

      new_lines <- c(new_lines, new_line)
    } else {
      # Line is not in a code chunk and doesn't start with #
      new_lines <- c(new_lines, line)
    }
  }
  
  # Combine the modified lines
  new_text <- paste(new_lines, collapse = "\n")
  
  # Replace the document's text with the modified text
  rstudioapi::modifyRange(new_text)
  return("Header numbering removed successfully.")
}

}