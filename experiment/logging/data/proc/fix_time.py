import numpy as np
import sys
import csv

p_amount = 4

def parse_list(bytes):
    s = bytes.decode("utf-8")
    if len(s)<1: return ""
    if len(s[1:len(s)-1].split(","))<=1:
        return []
    return s[1:len(s)-1].split(",")

# Conversion functions for parsing:
f2f = lambda x: float(x)
b2b = lambda x: str(x) == "b'true'"
s2s = lambda x: x.decode("utf-8")
l2l = parse_list

tbl = np.genfromtxt(sys.argv[1], delimiter=';', skip_header=1, max_rows=99999999, names=['time', 'name', 'x', 'z', 'xlook', 'ylook', 'zlook', 'vis'], converters = {'time': f2f, 'name':s2s, 'x':f2f, 'z':f2f, 'xlook':f2f, 'ylook':f2f, 'zlook':f2f, 'vis':l2l}, dtype='object')


t = 0
errors = []
for i in range(0, len(tbl), p_amount):
    errors.append(abs(t-tbl[i]['time']))
    t+=0.05

print("Ftime: " + str(tbl[i]['time']), end="\t")
print("MxErr: " + str(max(errors)), end="\t")
print("AmErr: " + str(len([e for e in errors if e>0.01])) + " out of " + str(len(tbl)/p_amount), end="\t")
print("File:" + sys.argv[1])
