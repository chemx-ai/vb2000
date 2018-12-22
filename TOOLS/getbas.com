%kjob l302
# cc-pVDZ gfinput

Get basis for atoms lists in Z-matrix. It is a good idea
to have the x distances integers 1 - N, as this is the number
in the gfinput output that has to be manually changed to the
atom symbol.

0 1
H 1.0 0.0  0.0
He 2.0 0.0  0.0
B 3.0 0.0  0.0
C 4.0 0.0  0.0
N 5.0 0.0  0.0
O 6.0 0.0  0.0
F 7.0 0.0  0.0
Ne 8.0 0.0  0.0
Al 9.0 0.0  0.0
Si 10.0 0.0  0.0
P 11.0 0.0  0.0
S 12.0 0.0  0.0
Cl 13.0 0.0  0.0
Ar 14.0 0.0  0.0
Ga 15.0 0.0  0.0
Ge 16.0 0.0  0.0
As 17.0 0.0  0.0
Se 18.0 0.0  0.0
Br 19.0 0.0  0.0
Kr 20.0 0.0  0.0

