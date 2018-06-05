# Make unit rasters for testing accuracy calculations, using the examples
# provided on Pontius and Santacruz 2014

library(raster)

# Function to create a dummy raster and assign values
# Raster is created with a projection, so it can be used
# to test projection behavior as well

make_ras <- function(vals){
        raster(xmn=630084,
             xmx=630086,
             ymn = 4833438,
             ymx = 4833441,
             resolution = 1,
             crs = "+proj=utm +zone=23 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
             vals = vals)
}

# Create unit raster pairs as P & S examples, substituting A, B and C by 1,2,3

q6_e0_s0 <- brick(make_ras(c(1,1,1,1,1,1)),
                  make_ras(c(2,2,2,2,2,2)))
#plot(q6_e0_s0)

q0_e6_s0 <- brick(make_ras(c(1,2,1,2,1,2)),
                  make_ras(c(2,1,2,1,2,1)))
#plot(q0_e6_s0)

q0_e0_s6 <- brick(make_ras(c(1,1,2,2,3,3)),
                  make_ras(c(3,3,2,2,1,1)))
#plot(q0_e0_s6)

q2_e4_s0 <- brick(make_ras(c(1,1,2,2,3,3)),
                  make_ras(c(2,2,1,1,1,1)))
#plot(q2_e4_s0)

q4_e0_s2 <- brick(make_ras(c(1,1,1,1,2,2)),
                  make_ras(c(2,2,2,2,3,3)))
#plot(q4_e0_s2)

q0_e2_s3 <- brick(make_ras(c(1,1,2,2,3,3)),
                  make_ras(c(2,3,1,1,2,3)))
#plot(q0_e2_s3)

q1_e2_s3 <- brick(make_ras(c(1,1,2,2,3,3)),
                  make_ras(c(2,3,1,1,2,2)))
#plot(q1_e2_s3)

# Create name vector, for testing class name assignment
ps_names <- c("A","B","C")
