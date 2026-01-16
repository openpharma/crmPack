# Create a RecruitmentRatio object with ratio 0.5
# This means 1 backfill patient for every 2 patients in the active cohort
recruitment <- RecruitmentRatio(ratio = 0.5)
print(recruitment)

# Create a variant with ratio 1 (1:1)
recruitment_one_to_one <- RecruitmentRatio(ratio = 1)
print(recruitment_one_to_one)
