
# create an object of class 'DataParts'
myData <- DataParts(x=c(0.1,0.5,1.5),
                    y=c(0,0,0),
                    doseGrid=c(0.1,0.5,1.5,3,6,
                               seq(from=10,to=80,by=2)),
                    part=c(1L,1L,1L),
                    nextPart=1L,
                    part1Ladder=c(0.1,0.5,1.5,3,6,10))

# update the data with a new cohort
# to be noted that since we reached the last level from part1Ladder then
# nextPart is switched from '1' to '2'
myData <- update(myData,
                 x=10,
                 y=c(0))


