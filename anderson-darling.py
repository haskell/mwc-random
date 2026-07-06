import pprint
import json
import numpy as np
from scipy.stats import anderson

with open("digits.txt") as f:
  data = json.load(f)

res = anderson(data, dist='norm', method='interpolate')
#res = anderson(data, dist='norm')

pprint.pprint(res)

print(f"sum = {sum(data)}")
print(f"sum(abs) = {sum([abs(d) for d in data])}")
print(f"min = {min(data)}")
print(f"max = {max(data)}")

# float_list = json.loads(float_string)
# print(float_list)  # [1.2, 3.4, 5.6]
# print(type(float_list[0]))  # <class 'float'>
