# -*- encoding: utf-8 -*-

from math import *

"""
Convolution of 2 casual signals, x(t<0) = y(t<0) = 0, using discrete
summation.
    x*y(t) = \int_{u=0}^t x(u) y(t-u) du = y*x(t)
where the size of x[], y[], x*y[] are P, Q, N=P+Q-1 respectively.
"""
def conv(x, y, shape='full'):
	P, Q, N = len(x), len(y), len(x)+len(y)-1
	z = []
	for k in range(N):
		t, lower, upper = 0, max(0, k-(Q-1)), min(P-1, k)
		for i in range(lower, upper+1):
			t = t + x[i] * y[k-i]
		z.append(t)
	if shape=='same':
		z = z[Q/2:-Q/2+1]
	return z

			
"""
Returns a Gaussian window of length M with standard-deviation std.

@input M   length
@input std standard diviation
@input sym o_O?

@return SJOV
"""
def make_gaussian(M,std,sym=1):
    if M < 1:
        return [] #Numeric.array([])
    if M == 1:
        return [1] #Numeric.ones(1,'d')
    odd = M % 2
    if not sym and not odd:
        M = M + 1
    n = [i-(M-1.0)/2.0 for i in range(0,M)]
	
    sig2 = 2*std*std
    w = [exp(-i**2 / sig2) for i in n]
    s = sum(w)
    w = [i/s for i in w]
    if not sym and not odd:
        w = w[:-1]
    return w
