# Author(s): Phillipe Loher

# setup system
	# define speed of sound (will be better tuned later)
	v = .34; # rough speed of sound is .34 m/ms (everything in meters and/or miliseconds)

	# define absolute mic locations (each mics x,y,z location on seperate row)
	nummics = 5;
	AbsoluteMicLocations = matrix(c(2, 2, 0), ncol = 3); 					#mic0
	AbsoluteMicLocations = rbind (AbsoluteMicLocations, c(4, 2, 0));  #mic1
	AbsoluteMicLocations = rbind (AbsoluteMicLocations, c(2, 4, 0));  #mic2
	AbsoluteMicLocations = rbind (AbsoluteMicLocations, c(4, 4, 0));  #mic3
	AbsoluteMicLocations = rbind (AbsoluteMicLocations, c(3, 3, 2));  #mic3
# end system setup


# get relative mic locations (by setting origin to the first mic)
RelativeMicLocations = t(apply(AbsoluteMicLocations,1, '-', AbsoluteMicLocations[1,]));
RelativeMicLocations

# define a known sound source (for testing/reverse engineering) and generate Mic Timings for the known source
knownsource = matrix(c(2, 2, 20), ncol = 3);
testMicTimes = matrix (apply(RelativeMicLocations,1, function(x) sqrt ((x[1]-knownsource[1,1])^2 + (x[2]-knownsource[1,2])^2 + (x[3]-knownsource[1,3])^2 )/v ), ncol=1)
print ("Original Mic Times");
print (testMicTimes);
testMicTimes = matrix (apply(testMicTimes,1, function(x) x[1] - testMicTimes[1,1]), ncol=1); # set all mic times relative to t0
if (testMicTimes[2,1] == 0) { testMicTimes[2,1] = 0.00001; }
testMicTimes

# generate linear equations mentioned in Equation 8 from http://en.wikipedia.org/wiki/Multilateration#3-D_Solution
funcA <- function (m) { ((2 * RelativeMicLocations[m,1])/(v*testMicTimes[m,])) - ((2*RelativeMicLocations[2,1])/(v*testMicTimes[2,])) }
funcB <- function (m) { ((2 * RelativeMicLocations[m,2])/(v*testMicTimes[m,])) - ((2*RelativeMicLocations[2,2])/(v*testMicTimes[2,])) }
funcC <- function (m) { ((2 * RelativeMicLocations[m,3])/(v*testMicTimes[m,])) - ((2*RelativeMicLocations[2,3])/(v*testMicTimes[2,])) }
funcD <- function (m) { v*testMicTimes[m,] - v*testMicTimes[2,] - ((RelativeMicLocations[m,1]^2+RelativeMicLocations[m,2]^2+RelativeMicLocations[m,3]^2)/(v*testMicTimes[m,])) + ((RelativeMicLocations[2,1]^2+RelativeMicLocations[2,2]^2+RelativeMicLocations[2,3]^2)/(v*testMicTimes[2,])) }

linMatrixABC = NULL;
linMatrixD   = NULL;
for (m in seq (3, nummics, by=1))
{
	linMatrixABC = rbind (linMatrixABC, matrix (c(funcA(m), funcB(m), funcC(m)), ncol=3));
	linMatrixD   = rbind (linMatrixD, -matrix (c(funcD(m)), ncol=1));
}

# now solve the generated linear equation
linMatrixABC
linMatrixD
solve (linMatrixABC, linMatrixD);

# the solution in the above solve should equal what you put in the knownsource variable above (that was reverse engineered)
