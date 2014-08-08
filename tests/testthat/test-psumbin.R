context("Distribution function")

# Test using Example 1 (Table 2) of Butler and Stephens (1993).

U <- psumbin(0:25, size=c(5,5,5,5,5), prob=c(0.02, 0.04, 0.06, 0.08, 0.10))

test_that("within rounding error of Example 1 of Butler and Stephens (1993)", {
  expect_that(U[1+1], equals(0.551513, tolerance=0.000001))  
  expect_that(U[2+1], equals(0.813946, tolerance=0.000001))  
  expect_that(U[3+1], equals(0.941627, tolerance=0.000001)) 
  expect_that(U[4+1], equals(0.985710, tolerance=0.000001))  
  expect_that(U[5+1], equals(0.997203, tolerance=0.000001))  
  expect_that(U[6+1], equals(0.999554, tolerance=0.000001))  
  expect_that(U[7+1], equals(0.999941, tolerance=0.000001))  
})

# Test using Example 2 (Table 3) of Butler and Stephens (1993).
 
U <- psumbin(0:750, size=c(50,100,150,200,250), prob=c(0.1,0.2,0.3,0.4,0.5))

test_that("within rounding error of Example 2 of Butler and Stephens (1993)", {
  expect_that(U[275+1], equals(0.516777, tolerance=0.000001))  
  expect_that(U[283+1], equals(0.748050, tolerance=0.000001))  
  expect_that(U[291+1], equals(0.901928, tolerance=0.000001)) 
  expect_that(U[296+1], equals(0.953696, tolerance=0.000001))  
  expect_that(U[300+1], equals(0.976850, tolerance=0.000001))  
  expect_that(U[305+1], equals(0.991358, tolerance=0.000001))  
  expect_that(U[311+1], equals(0.997782, tolerance=0.000001))  
  expect_that(U[315+1], equals(0.999197, tolerance=0.000001))  
  expect_that(U[320+1], equals(0.999801, tolerance=0.000001))  
  expect_that(U[326+1], equals(0.999969, tolerance=0.000001))  
})


# Test using Example 3 (Table 4) of Butler and Stephens (1993).
 
U <- psumbin(0:500, size=c(100,100,100,100,100), prob=c(0.010, 0.015, 0.020, 0.025,0.030))

test_that("within rounding error of Example 3 of Butler and Stephens (1993)", {
  expect_that(U[10+1], equals(0.583047, tolerance=0.000001))  
  expect_that(U[12+1], equals(0.793728, tolerance=0.000001))  
  expect_that(U[14+1], equals(0.918908, tolerance=0.000001)) 
  expect_that(U[15+1], equals(0.953221, tolerance=0.000001))  
  expect_that(U[16+1], equals(0.974420, tolerance=0.000001))  
  expect_that(U[17+1], equals(0.986718, tolerance=0.000001))  
  expect_that(U[19+1], equals(0.996913, tolerance=0.000001))  
  expect_that(U[21+1], equals(0.999405, tolerance=0.000001))  
  expect_that(U[23+1], equals(0.999904, tolerance=0.000001))  
  expect_that(U[25+1], equals(0.999987, tolerance=0.000001))  
})

# Test using Example 4 (Table 5) of Butler and Stephens (1993).
# Note that the value for p_3 is rounded in the table to 0.0033,
# which gives incorrect results for matching.
 
U <- psumbin(0:1500, size=c(500,400,300,200,100), prob=c(0.0020, 0.0025, 1/300, 0.0050, 0.0100))

test_that("within rounding error of Example 4 of Butler and Stephens (1993)", {
  expect_that(U[ 5+1], equals(0.615961, tolerance=0.000001))  
  expect_that(U[ 6+1], equals(0.762519, tolerance=0.000001))  
  expect_that(U[ 7+1], equals(0.867107, tolerance=0.000001)) 
  expect_that(U[ 8+1], equals(0.932354, tolerance=0.000001))  
  expect_that(U[ 9+1], equals(0.968503, tolerance=0.000001))  
  expect_that(U[10+1], equals(0.986511, tolerance=0.000001))  
  expect_that(U[11+1], equals(0.994659, tolerance=0.000001))  
  expect_that(U[12+1], equals(0.998036, tolerance=0.000001)) 
  expect_that(U[13+1], equals(0.999326, tolerance=0.000001))  
  expect_that(U[14+1], equals(0.999783, tolerance=0.000001))  
  expect_that(U[15+1], equals(0.999935, tolerance=0.000001))  
  expect_that(U[16+1], equals(0.999981, tolerance=0.000001))  
})
