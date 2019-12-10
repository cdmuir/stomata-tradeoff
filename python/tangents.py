# stomata are a circle with radius R, centered at (x_c, y_c)
# R ^ 2 = (x - x_c) ^ 2 + (y - y_c) ^ 2
# pathogen is located at point (x_p, y_p)
# point-slope form of line from pathogen tangent to stomata
# y - y_p = m * (x - x_p)
# y = m * x + b, where b = y_p - m * x_p
#
# solve for intersection between line and stomata where:
# R ^ 2 = (x - x_c) ^ 2 + (y - y_c) ^ 2
# R ^ 2 = (x - x_c) ^ 2 + (m * x + b - y_c) ^ 2
# 0 = (1 + m ^ 2) * x ^ 2 + 
#       2 * (b * m - m * y_c - x_c) * x + 
#       x_c ^ 2 + (b - y_c) ^ 2 - R ^ 2
#
# This a quadratic formula where:
# A = 1 + m^2
# B = 2 * (b * m - m * y_c - x_c) * x
# C = x_c ^ 2 + (b - y_c) ^ 2 - R ^ 2
#
# Tangents occurs where 0 = B ^ 2 - 4 * A * C

from sympy import solve, sqrt, symbols
m, R, x_c, x_p, y_c, y_p = symbols('m, R, x_c, x_p, y_c, y_p')
b = y_p - m * x_p
A = 1 + m ** 2
B = 2 * (b * m - m * y_c - x_c)
C = x_c ** 2 + (b - y_c) ** 2 - R ** 2
slopes = solve(B ** 2 - 4 * A * C, m)

b = y_p - slopes[0] * x_p
A = 1 + slopes[0] ** 2
B = 2 * (b * slopes[0] - slopes[0] * y_c - x_c)
C = x_c ** 2 + (b - y_c) ** 2 - R ** 2
check1 = (B ** 2 - 4 * A * C).simplify()

b = y_p - slopes[1] * x_p
A = 1 + slopes[1] ** 2
B = 2 * (b * slopes[1] - slopes[1] * y_c - x_c)
C = x_c ** 2 + (b - y_c) ** 2 - R ** 2
check2 = (B ** 2 - 4 * A * C).simplify()

# There are two slopes, slopes[0] and slopes[1]
# Now plug back into solve points of tangency
# 0 = (1 + m ^ 2) * x ^ 2 + 
#       2 * (b * m - m * y_c - x_c) * x + 
#       x_c ^ 2 + (b - y_c) ^ 2 - R ^ 2
m = slopes[0]
b = y_p - slopes[0] * x_p
A = (1 + m ** 2)
B = 2 * (b * m - m * y_c - x_c)
x1 = (-B / (2 * A)).simplify()
y1 = (m * x1 + b).simplify()

m = slopes[1]
b = y_p - slopes[1] * x_p
A = (1 + m ** 2)
B = 2 * (b * m - m * y_c - x_c)
x2 = (-B / (2 * A)).simplify()
y2 = (m * x2 + b).simplify()
