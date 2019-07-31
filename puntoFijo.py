import math

def g(x):
    return (math.pow(x,3)-1)/-5


x = 2.4
y = round(g(x), 3)
w = 0

while math.fabs(y-x) > 1*math.pow(10,-8):
    x = y
    y = round(g(x), 3)
    print math.fabs(y-x)
    w += 1
print w
