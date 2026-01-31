# Create a RecruitmentUnlimited object
recruitment <- RecruitmentUnlimited()

# Calculate maximum recruits for various active cohort sizes
max_recruits_10 <- maxRecruits(recruitment, active_cohort_size = 10)
print(max_recruits_10) # Returns 1e6

max_recruits_100 <- maxRecruits(recruitment, active_cohort_size = 100)
print(max_recruits_100) # Still returns 1e6 (unlimited)

# With RecruitmentUnlimited, the active_cohort_size is ignored
