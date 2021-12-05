SETS
i "origins" /s1,s2,s3,s4/, j "destinations" /d1,d2,d3,d4/, l "commodities" /Shoes,Socks,Pants,Shirts/;

TABLE A(l,i) "supply amount of commodity l at origin i"
        s1     s2    s3    s4
Shoes   900    800   1000  1100
Socks   680    890   550   800
Pants   950    860   780   980
Shirts  1000   1010  800   950 ;

TABLE B(l,j) "current-stage demand of commodity l in destination j"
        d1     d2    d3    d4
Shoes   420    380   280   300
Socks   620    680   550   430
Pants   800    760   550   610
Shirts  600    590   320   480 ;

TABLE Bdash(l,j) "next-stage demand of commodity l in destination j"
        d1     d2    d3    d4
Shoes   530    440   480   505
Socks   706    808   645   558
Pants   797    1023  907   886
Shirts  830    680   654   589 ;

TABLE P(l,j) "unit inventory cost of commodity l in destination j"
        d1     d2    d3    d4
Shoes   6      4     7     5
Socks   2      1     3     4
Pants   17     16    15    14
Shirts  9      8     6     7 ;

TABLE U(l,i) "unit inventory cost of commodity l at origin i"
        s1     s2    s3    s4
Shoes   5      2     5     5
Socks   3      7     2     4
Pants   10     13    15    26
Shirts  13     11    10    11 ;

TABLE C(l,i,j) "current stage shipping cost of commodity l from origin i to destination j"
            d1     d2    d3    d4
Shoes.s1    3      6     3     4
Shoes.s2    4      3     5     5
Shoes.s3    6      3     7     3
Shoes.s4    7      5     3     2 
Socks.s1    2      3     5     1
Socks.s2    3      7     4     5
Socks.s3    4      2     3     5
Socks.s4    6      8     3     5
Pants.s1    11     15    16    27
Pants.s2    22     30    19    20
Pants.s3    19     22    24    28
Pants.s4    30     27    23    18
Shirts.s1   10     8     9     11
Shirts.s2   9      8     10    13
Shirts.s3   12     14    15    12
Shirts.s4   10     9     8     7  ;

PARAMETER
Q(i)  / s1 = 900, s2 = 840, s3 = 1080, s4 = 1500 /
R(j)  / d1 = 1400, d2 = 900, d3 = 1500, d4 = 2500 /
M(l)  / Shoes = 1, Socks = 2, Pants = 3, Shirts = 2 /;

VARIABLES X(l,i,j),V(l,i,j),X5(l,i),OBJ,D(l,i,j),C5(l,i),Cdash(l,i);

INTEGER VARIABLES X(l,i,j),V(l,i,j),X5(l,i);

EQUATIONS
Cost,cstr1(l),cstr2(l,i),cstr3(l,j),cstr4(l,j),cstr5(i),cstr6(j),cstr8,eqn1(l,i,j),eqn2(l,i),eqn3(l,i);

Cost.. OBJ =e= sum(l, sum(i, sum(j, C(l,i,j)*X(l,i,j)))) + sum(l, sum(i, sum(j, D(l,i,j)*V(l,i,j)))) + sum(l, sum(i, C5(l,i)*X5(l,i)));
eqn1(l,i,j).. D(l,i,j) =e= C(l,i,j) + P(l,j);
cstr1(l).. sum(i, A(l,i)) =g= sum(j, B(l,j));
cstr2(l,i).. sum(j, X(l,i,j)) + sum(j, V(l,i,j)) + X5(l,i) =e= A(l,i);
cstr3(l,j).. sum(i, X(l,i,j)) =e= B(l,j);
cstr4(l,j).. sum(i, V(l,i,j)) =l= Bdash(l,j);
cstr5(i).. sum(l, X5(l,i)*M(l)) =l= Q(i);
cstr6(j).. sum(l, sum(i, V(l,i,j))*M(l)) =l= R(j);
cstr8.. sum(l, (sum(i, A(l,i)) - sum(j, B(l,j)))*M(l)) =l= sum(i, Q(i)) + sum(j, R(j));
eqn2(l,i).. C5(l,i) =e= U(l,i) + Cdash(l,i);
eqn3(l,i).. Cdash(l,i) =e= (sum(j, Bdash(l,j)))/(sum(j, Bdash(l,j)/C(l,i,j)));

model transportation /all/;

solve transportation using minlp minimizing OBJ;

display X.l,V.l,Cdash.l;