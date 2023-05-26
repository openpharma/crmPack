#!/bin/bash

# Check if the filename argument is provided
if [ -z "$1" ]; then
  echo "Please provide the filename as the first argument."
  exit 1
fi

# Specify input file
input_file="$1"

# Create a temporary file for storing the updated content
temp_file="temp_file.R"

# Read the input file line by line
while IFS= read -r line
do
  # Check if the line contains the class indicator
  if [[ $line =~ ^##\ class\ ----$ ]]
  then
    # Extract the class name from the previous line
    class_name=$(sed -n '/^#\ '\''\?[^[:space:]]/s/[^#]*#\ '\''\([^'\'']*\).*/\1/p' "$input_file")

    # Update the class name with simple title and lifecycle badge
    updated_line="#' \`${class_name}\`\n#' \n#' \`r lifecycle::badge(\"stable\")\`\n#' \n"

    # Append the updated line to the temporary file
    echo "$updated_line" >> "$temp_file"
  else
    # Append the line to the temporary file as it is
    echo "$line" >> "$temp_file"
  fi
done < "$input_file"

# Check if the temporary file was successfully created
if [ -f "$temp_file" ]; then
  # Move the temporary file to replace the original file
  mv "$temp_file" "$input_file"
  echo "File updated successfully!"
else
  echo "Error creating the temporary file. File not updated."
fi
