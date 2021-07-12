testSIDInAnyWave <- tibble(SID=c(2,4,6,8,10,12,14),
                           HID1=c(10,10,10,20,20,20,NA),
                           HID2=c(10,10,10,NA, 20,20,20))
ccVecOutput <- list(c(1,2), c(1,2), c(1,2), c(1.0,1.5), c(1,2), c(1,2), c(1.5,2.0))
 names(ccVecOutput) = c("2", "4", "6", "8", "10", "12", "14")
