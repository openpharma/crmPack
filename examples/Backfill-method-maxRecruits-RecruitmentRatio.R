# Create a RecruitmentRatio object with ratio 0.5
recruitment <- RecruitmentRatio(ratio = 0.5)

# Calculate maximum recruits based on active cohort size
# For active cohort of 10: ceiling(0.5 * 10) = 5
max_recruits_10 <- maxRecruits(recruitment, active_cohort_size = 10)
print(max_recruits_10) # 5

# For active cohort of 7: ceiling(0.5 * 7) = ceiling(3.5) = 4
max_recruits_7 <- maxRecruits(recruitment, active_cohort_size = 7)
print(max_recruits_7) # 4

# For active cohort of 15: ceiling(0.5 * 15) = ceiling(7.5) = 8
max_recruits_15 <- maxRecruits(recruitment, active_cohort_size = 15)
print(max_recruits_15) # 8
