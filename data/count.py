import numpy as np
import os


p2013 = "./pan_2013/"
p2015 = "./pan_2015/"


count = []

for folder in os.listdir(p2013):
    path = p2013 + folder + "/"
    for text in os.listdir(path):
        path = p2013 + folder+ "/" + text
        f = open(path, "r").read()
        count.append(len(f.split()))
          
print "PAN 2013 Word Count Average:", np.mean(count)


for folder in os.listdir(p2015):
    path = p2015 + folder + "/"
    for text in os.listdir(path):
        path = p2015 + folder+ "/" + text
        f = open(path, "r").read()
        count.append(len(f.split()))

          
print "PAN 2015 Word Count Average:", np.mean(count)
