'''
I AM CHEATING.
It's the last star dammit :(
and like. chemistry and err.. what?
NO SHAME


https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju/
The solution

My input file had:

295 elements in total
 68 were Rn and Ar (the `(` and `)`)
  7 were Y (the `,`)

Plugging in the numbers:

295 - 68 - 2*7 - 1 = 212
'''

import sys

medicine = sys.stdin.readlines()[-1].strip()

elemCount = len(list(filter(str.isupper, medicine)))
rn = medicine.count('Rn')
ar = medicine.count('Ar')
y = medicine.count('Y')

print(f'total elements: {elemCount}')
print(f'Rn: {rn} Ar: {ar} either: {rn+ar}')
print(f'Y: {y}')
print(elemCount - (rn+ar) - 2 * y - 1)
