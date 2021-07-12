ex <- as.integer(c(3,3,3,4))
sx <- as.integer(c(3,3,3,3))

missingXHID <- tibble(HID=c(10, 10, 20,20),
                      WID=c(1,2,1,2),
                      Elig=ex,
                      Surveyed=sx,
                      Missing=c(0,0,0,0.25))
