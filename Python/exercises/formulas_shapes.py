from math import pi

h = 5.0  # height
b = 2.0  # base
r = 1.5  # radius

area_parallelogram = h*b
print ('The area of the parallelogram is %g' % area_parallelogram)

area_square = b**2
print ('The area of the square is %g' % area_square)

area_circle = pi*r**2
print ('The area of the circle is %.5f' % area_circle)

volume_cone = (1/3)*pi*(r**2)*h
print ('The volume of a cone is %.3f' % volume_cone)
