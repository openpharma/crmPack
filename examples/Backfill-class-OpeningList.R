# Create two simple opening criteria
opening1 <- OpeningMinDose(min_dose = 10)
opening2 <- OpeningMinCohorts(min_cohorts = 3)

# Create an OpeningList that combines them
opening_list <- OpeningList(opening1, opening2)
print(opening_list)

# You can also create with more than two criteria
opening3 <- OpeningNone()
opening_list_multi <- OpeningList(opening1, opening2, opening3)
print(opening_list_multi)
