meters = 640

inches = meters*100/2.54
feet = inches / 12
yards = feet / 3
miles = yards / 1760

print('''\
meters = %d
inches = %.2f
feet = %.2f
yards = %.2f
miles = %.4f\
''' % (meters, inches, feet, yards, miles)
)
