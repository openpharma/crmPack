# Create a RecruitmentUnlimited object
recruitment <- RecruitmentUnlimited()
print(recruitment)

# Calculate maximum recruits for an active cohort of size 10
max_recruits <- maxRecruits(recruitment, active_cohort_size = 10)
print(max_recruits) # Practically unlimited (1e6)
