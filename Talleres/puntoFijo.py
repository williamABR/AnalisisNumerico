import math

def g(x):
    return (math.pow(x,3)-1)/-5


x = 8
y = round(g(x), 3)
w = 0

while math.fabs(y-x) > math.pow(10,-8):
    x = y
    y = round(g(x), 3)
    print math.fabs(y-x)
    w += 1
print w
