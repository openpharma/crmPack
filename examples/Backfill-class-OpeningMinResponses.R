# Create an OpeningMinResponses object that requires 2 responses
opening <- OpeningMinResponses(min_responses = 2, include_lower_doses = FALSE)

# Display the object
print(opening)

# Create a variant that includes lower doses
opening_inclusive <- OpeningMinResponses(
  min_responses = 2,
  include_lower_doses = TRUE
)
print(opening_inclusive)
