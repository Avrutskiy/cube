(* ::Package:: *)

(* ::Input::Initialization:: *)
$HistoryLength=0;
\[Sigma]=0.03;
h=2/40;
cf2=With[{x=\[Sigma]},Compile[{{a,_Real}},
Chop[1/(x Sqrt[2\[Pi]]) Exp[-(1/2) a^2/x^2]]
,
CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True,RuntimeOptions->"Speed"]];

Clear[fx,fy,tx,ty,tz,cx,cy]

cf=Compile[{{a,_Real},{b,_Real,1}},Table[a-Compile`GetElement[b,j],{j,1,Length[b]}],
CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True,RuntimeOptions->"Speed"];

dir[X_]:=Module[{ans},
ans=0;
ans+=(X[[2,1]]-X[[1,1]])(X[[2,2]]+X[[1,2]]);
ans+=(X[[3,1]]-X[[2,1]])(X[[3,2]]+X[[2,2]]);
ans+=(X[[4,1]]-X[[3,1]])(X[[4,2]]+X[[3,2]]);
ans+=(X[[1,1]]-X[[4,1]])(X[[1,2]]+X[[4,2]]);
Sign[ans]
]
ptProj[{x_,y_,z_}]:={(fx tx+cx tz+fx x+cx z)/(tz+z),(fy ty+cy tz+fy y+cy z)/(tz+z)}
tarea[T_]:=0.5Abs[T[[1,1]](T[[2,2]]-T[[3,2]])+T[[2,1]]*(T[[3,2]]-T[[1,2]])+T[[3,1]]*(T[[1,2]]-T[[2,2]])];

xw={{0.33333333333333,0.33333333333333,0.22500000000000},
{0.47014206410511,0.47014206410511,0.13239415278851},
{0.47014206410511,0.05971587178977,0.13239415278851},
{0.05971587178977,0.47014206410511,0.13239415278851},
{0.10128650732346,0.10128650732346,0.12593918054483},
{0.10128650732346,0.79742698535309,0.12593918054483},
{0.79742698535309,0.10128650732346,0.12593918054483}};

CPTS[T_]:=Module[{arr,x1,x2,x3,x4,x5,x6,x7,y1,y2,y3,y4,y5,y6,y7,n,area},

n=1;
x1=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y1=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
n=2;
x2=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y2=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
n=3;
x3=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y3=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
n=4;
x4=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y4=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
n=5;
x5=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y5=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
n=6;
x6=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y6=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
n=7;
x7=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y7=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
area=tarea[T];

{{x1,y1,xw[[1,3]]*area},
{x2,y2,xw[[2,3]]*area},
{x3,y3,xw[[3,3]]*area},
{x4,y4,xw[[4,3]]*area},
{x5,y5,xw[[5,3]]*area},
{x6,y6,xw[[6,3]]*area},
{x7,y7,xw[[7,3]]*area}}
]

CPTSc=Compile[{{T,_Real,2}},Module[{xw,arr,x1,x2,x3,x4,x5,x6,x7,y1,y2,y3,y4,y5,y6,y7,n,area},

xw={{0.33333333333333,0.33333333333333,0.22500000000000},
{0.47014206410511,0.47014206410511,0.13239415278851},
{0.47014206410511,0.05971587178977,0.13239415278851},
{0.05971587178977,0.47014206410511,0.13239415278851},
{0.10128650732346,0.10128650732346,0.12593918054483},
{0.10128650732346,0.79742698535309,0.12593918054483},
{0.79742698535309,0.10128650732346,0.12593918054483}};

n=1;
x1=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y1=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
n=2;
x2=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y2=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
n=3;
x3=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y3=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
n=4;
x4=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y4=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
n=5;
x5=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y5=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
n=6;
x6=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y6=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
n=7;
x7=T[[1,1]](1-xw[[n,1]]-xw[[n,2]])+T[[2,1]]xw[[n,1]]+T[[3,1]]xw[[n,2]];
y7=T[[1,2]](1-xw[[n,1]]-xw[[n,2]])+T[[2,2]]xw[[n,1]]+T[[3,2]]xw[[n,2]];
area=0.5Abs[T[[1,1]](T[[2,2]]-T[[3,2]])+T[[2,1]]*(T[[3,2]]-T[[1,2]])+T[[3,1]]*(T[[1,2]]-T[[2,2]])];

{{x1,y1,xw[[1,3]]*area},
{x2,y2,xw[[2,3]]*area},
{x3,y3,xw[[3,3]]*area},
{x4,y4,xw[[4,3]]*area},
{x5,y5,xw[[5,3]]*area},
{x6,y6,xw[[6,3]]*area},
{x7,y7,xw[[7,3]]*area}}
],CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True,RuntimeOptions->"Speed"];

fx=5;
fy=5;
tx=0;
ty=0;
tz=-5;
cx=0;
cy=0;
pek1=Reverse[1/5 {{1,-1,1},{1,1,1},{1,1,-1},{1,-1,-1}}];
pek2=1/5 {{-1,-1,1},{-1,1,1},{-1,1,-1},{-1,-1,-1}};
pek3=1/5 {{-1,-1,-1},{-1,1,-1},{1,1,-1},{1,-1,-1}};
pek4=1/5 {{1,-1,1},{1,1,1},{-1,1,1},{-1,-1,1}};
pek5=1/5 {{1,-1,1},{-1,-1,1},{-1,-1,-1},{1,-1,-1}};
pek6=1/5 {{-1,1,1},{1,1,1},{1,1,-1},{-1,1,-1}};
PEK={pek1,pek2,pek3,pek4,pek5,pek6} . RotationMatrix[\[Pi]/2,{0,0,1}];
PEK=PEK . RotationMatrix[\[Pi]/4,{1,0,0}];
PEK=PEK . RotationMatrix[0.6154797,{0,1,0}];


GEO3D[rot_,\[Phi]_]:=Module[{PEKT,PRJ,fs,fcs},
PEKT=PEK . RotationMatrix[\[Phi],rot]
]

GEO3DX[rot_,\[Phi]_,vec_]:=Module[{PEKT,PRJ,fs,fcs},
PEK . RotationMatrix[\[Phi],rot]+Table[vec,{i,1,6},{j,1,4}]
]

GEO3DS[GEO3D_,vec_]:=GEO3D+Table[vec,{i,1,6},{j,1,4}];


GEOProj[GEO3DS_]:=Module[{PEKT,PRJ,fs,fcs},
PRJ=Table[ptProj/@GEO3DS[[i]],{i,1,Length[GEO3DS]}];

(*fs=Table[If[dir[PRJ[[i]]]<0,i,0],{i,1,6}];*)
fs=Table[If[dir[PRJ[[i]]]<0,i,0],{i,1,6}];
fcs=DeleteCases[fs,0];

Simplify[Table[{RGBColor[{1,1,1}i/6],PRJ[[i]]},{i,fcs}]]
]

GEOM[vec_,rot_,\[Phi]_]:=Module[{PEKT,PRJ,fs,fcs},
PEKT=PEK . RotationMatrix[\[Phi],rot]+Table[vec,{i,1,6},{j,1,4}];
PRJ=Table[ptProj/@PEKT[[i]],{i,1,Length[PEKT]}];

(*fs=Table[If[dir[PRJ[[i]]]<0,i,0],{i,1,6}];*)
fs=Table[If[dir[PRJ[[i]]]<0,i,0],{i,1,6}];
fcs=DeleteCases[fs,0];

Simplify[Table[{RGBColor[{1,1,1}i/6],PRJ[[i]]},{i,fcs}]]
]

qarea[Q_]:=0.5Abs[(Q[[1,1]]Q[[2,2]]+Q[[2,1]]Q[[3,2]]+Q[[3,1]]Q[[4,2]]+Q[[4,1]]Q[[1,2]])-(Q[[2,1]]Q[[1,2]]+Q[[3,1]]Q[[2,2]]+Q[[4,1]]Q[[3,2]]+Q[[1,1]]Q[[4,2]])];

SIDESa[PEKT_]:=Module[{PRJ,fs,fcs},
PRJ=Table[ptProj/@PEKT[[i]],{i,1,Length[PEKT]}];
fs=Table[If[dir[PRJ[[i]]]<0,i,0],{i,1,6}];
DeleteCases[fs,0]
]

SIDES[vec_,rot_,\[Phi]_]:=Module[{PEKT,PRJ,fs,fcs},
PEKT=PEK . RotationMatrix[\[Phi],rot]+Table[vec,{i,1,6},{j,1,4}];
PRJ=Table[ptProj/@PEKT[[i]],{i,1,Length[PEKT]}];
fs=Table[If[dir[PRJ[[i]]]<0,i,0],{i,1,6}];
DeleteCases[fs,0]
]

VEC[GEO_]:=Module[{PRJ},
PRJ=GEO[[All,2]];
Show[Graphics[{RGBColor[{0,0,0}],Polygon[{{-1,-1},{-1,1},{1,1},{1,-1}}]}],Graphics[Table[{GEO[[i,1]],Polygon[PRJ[[i]]]},{i,1,Length[GEO]}]],ImageSize->400]
];

d3GRAD[rot_,\[Phi]_,vec_]:=Module[{r1,r2,r3,PEKT0,PEKT1,PEKT2,PEKT3,PEKT4,PEKT5,PEKT6},

r1={{0,0,0},{0,0,-1},{0,1,0}};r2={{0,0,1},{0,0,0},{-1,0,0}};r3={{0,-1,0},{1,0,0},{0,0,0}};

PEKT4=PEK . RotationMatrix[\[Phi],rot] . r1;
PEKT5=PEK . RotationMatrix[\[Phi],rot] . r2;
PEKT6=PEK . RotationMatrix[\[Phi],rot] . r3;

PEKT1=Table[{1,0,0},{i,1,6},{j,1,4}];
PEKT2=Table[{0,1,0},{i,1,6},{j,1,4}];
PEKT3=Table[{0,0,1},{i,1,6},{j,1,4}];

{PEKT1,PEKT2,PEKT3,PEKT4,PEKT5,PEKT6}

]

d3GRADsh[GEO_]:=Module[{r1,r2,r3,PEKT0,PEKT1,PEKT2,PEKT3,PEKT4,PEKT5,PEKT6},

r1={{0,0,0},{0,0,-1},{0,1,0}};r2={{0,0,1},{0,0,0},{-1,0,0}};r3={{0,-1,0},{1,0,0},{0,0,0}};

PEKT4=GEO . r1;
PEKT5=GEO . r2;
PEKT6=GEO . r3;

PEKT1=Table[{1,0,0},{i,1,6},{j,1,4}];
PEKT2=Table[{0,1,0},{i,1,6},{j,1,4}];
PEKT3=Table[{0,0,1},{i,1,6},{j,1,4}];

{PEKT1,PEKT2,PEKT3,PEKT4,PEKT5,PEKT6}

]

dGRAD=Compile[{{g3d,_Real,3},{vx,_Real},{vy,_Real},{vz,_Real}},
{{{{5/(-5+vz+g3d[[1,1,3]]),0},{5/(-5+vz+g3d[[1,2,3]]),0},{5/(-5+vz+g3d[[1,3,3]]),0},{5/(-5+vz+g3d[[1,4,3]]),0}},{{5/(-5+vz+g3d[[2,1,3]]),0},{5/(-5+vz+g3d[[2,2,3]]),0},{5/(-5+vz+g3d[[2,3,3]]),0},{5/(-5+vz+g3d[[2,4,3]]),0}},{{5/(-5+vz+g3d[[3,1,3]]),0},{5/(-5+vz+g3d[[3,2,3]]),0},{5/(-5+vz+g3d[[3,3,3]]),0},{5/(-5+vz+g3d[[3,4,3]]),0}},{{5/(-5+vz+g3d[[4,1,3]]),0},{5/(-5+vz+g3d[[4,2,3]]),0},{5/(-5+vz+g3d[[4,3,3]]),0},{5/(-5+vz+g3d[[4,4,3]]),0}},{{5/(-5+vz+g3d[[5,1,3]]),0},{5/(-5+vz+g3d[[5,2,3]]),0},{5/(-5+vz+g3d[[5,3,3]]),0},{5/(-5+vz+g3d[[5,4,3]]),0}},{{5/(-5+vz+g3d[[6,1,3]]),0},{5/(-5+vz+g3d[[6,2,3]]),0},{5/(-5+vz+g3d[[6,3,3]]),0},{5/(-5+vz+g3d[[6,4,3]]),0}}},{{{0,5/(-5+vz+g3d[[1,1,3]])},{0,5/(-5+vz+g3d[[1,2,3]])},{0,5/(-5+vz+g3d[[1,3,3]])},{0,5/(-5+vz+g3d[[1,4,3]])}},{{0,5/(-5+vz+g3d[[2,1,3]])},{0,5/(-5+vz+g3d[[2,2,3]])},{0,5/(-5+vz+g3d[[2,3,3]])},{0,5/(-5+vz+g3d[[2,4,3]])}},{{0,5/(-5+vz+g3d[[3,1,3]])},{0,5/(-5+vz+g3d[[3,2,3]])},{0,5/(-5+vz+g3d[[3,3,3]])},{0,5/(-5+vz+g3d[[3,4,3]])}},{{0,5/(-5+vz+g3d[[4,1,3]])},{0,5/(-5+vz+g3d[[4,2,3]])},{0,5/(-5+vz+g3d[[4,3,3]])},{0,5/(-5+vz+g3d[[4,4,3]])}},{{0,5/(-5+vz+g3d[[5,1,3]])},{0,5/(-5+vz+g3d[[5,2,3]])},{0,5/(-5+vz+g3d[[5,3,3]])},{0,5/(-5+vz+g3d[[5,4,3]])}},{{0,5/(-5+vz+g3d[[6,1,3]])},{0,5/(-5+vz+g3d[[6,2,3]])},{0,5/(-5+vz+g3d[[6,3,3]])},{0,5/(-5+vz+g3d[[6,4,3]])}}},{{{-((5 (vx+g3d[[1,1,1]]))/(-5+vz+g3d[[1,1,3]])^2),-((5 (vy+g3d[[1,1,2]]))/(-5+vz+g3d[[1,1,3]])^2)},{-((5 (vx+g3d[[1,2,1]]))/(-5+vz+g3d[[1,2,3]])^2),-((5 (vy+g3d[[1,2,2]]))/(-5+vz+g3d[[1,2,3]])^2)},{-((5 (vx+g3d[[1,3,1]]))/(-5+vz+g3d[[1,3,3]])^2),-((5 (vy+g3d[[1,3,2]]))/(-5+vz+g3d[[1,3,3]])^2)},{-((5 (vx+g3d[[1,4,1]]))/(-5+vz+g3d[[1,4,3]])^2),-((5 (vy+g3d[[1,4,2]]))/(-5+vz+g3d[[1,4,3]])^2)}},{{-((5 (vx+g3d[[2,1,1]]))/(-5+vz+g3d[[2,1,3]])^2),-((5 (vy+g3d[[2,1,2]]))/(-5+vz+g3d[[2,1,3]])^2)},{-((5 (vx+g3d[[2,2,1]]))/(-5+vz+g3d[[2,2,3]])^2),-((5 (vy+g3d[[2,2,2]]))/(-5+vz+g3d[[2,2,3]])^2)},{-((5 (vx+g3d[[2,3,1]]))/(-5+vz+g3d[[2,3,3]])^2),-((5 (vy+g3d[[2,3,2]]))/(-5+vz+g3d[[2,3,3]])^2)},{-((5 (vx+g3d[[2,4,1]]))/(-5+vz+g3d[[2,4,3]])^2),-((5 (vy+g3d[[2,4,2]]))/(-5+vz+g3d[[2,4,3]])^2)}},{{-((5 (vx+g3d[[3,1,1]]))/(-5+vz+g3d[[3,1,3]])^2),-((5 (vy+g3d[[3,1,2]]))/(-5+vz+g3d[[3,1,3]])^2)},{-((5 (vx+g3d[[3,2,1]]))/(-5+vz+g3d[[3,2,3]])^2),-((5 (vy+g3d[[3,2,2]]))/(-5+vz+g3d[[3,2,3]])^2)},{-((5 (vx+g3d[[3,3,1]]))/(-5+vz+g3d[[3,3,3]])^2),-((5 (vy+g3d[[3,3,2]]))/(-5+vz+g3d[[3,3,3]])^2)},{-((5 (vx+g3d[[3,4,1]]))/(-5+vz+g3d[[3,4,3]])^2),-((5 (vy+g3d[[3,4,2]]))/(-5+vz+g3d[[3,4,3]])^2)}},{{-((5 (vx+g3d[[4,1,1]]))/(-5+vz+g3d[[4,1,3]])^2),-((5 (vy+g3d[[4,1,2]]))/(-5+vz+g3d[[4,1,3]])^2)},{-((5 (vx+g3d[[4,2,1]]))/(-5+vz+g3d[[4,2,3]])^2),-((5 (vy+g3d[[4,2,2]]))/(-5+vz+g3d[[4,2,3]])^2)},{-((5 (vx+g3d[[4,3,1]]))/(-5+vz+g3d[[4,3,3]])^2),-((5 (vy+g3d[[4,3,2]]))/(-5+vz+g3d[[4,3,3]])^2)},{-((5 (vx+g3d[[4,4,1]]))/(-5+vz+g3d[[4,4,3]])^2),-((5 (vy+g3d[[4,4,2]]))/(-5+vz+g3d[[4,4,3]])^2)}},{{-((5 (vx+g3d[[5,1,1]]))/(-5+vz+g3d[[5,1,3]])^2),-((5 (vy+g3d[[5,1,2]]))/(-5+vz+g3d[[5,1,3]])^2)},{-((5 (vx+g3d[[5,2,1]]))/(-5+vz+g3d[[5,2,3]])^2),-((5 (vy+g3d[[5,2,2]]))/(-5+vz+g3d[[5,2,3]])^2)},{-((5 (vx+g3d[[5,3,1]]))/(-5+vz+g3d[[5,3,3]])^2),-((5 (vy+g3d[[5,3,2]]))/(-5+vz+g3d[[5,3,3]])^2)},{-((5 (vx+g3d[[5,4,1]]))/(-5+vz+g3d[[5,4,3]])^2),-((5 (vy+g3d[[5,4,2]]))/(-5+vz+g3d[[5,4,3]])^2)}},{{-((5 (vx+g3d[[6,1,1]]))/(-5+vz+g3d[[6,1,3]])^2),-((5 (vy+g3d[[6,1,2]]))/(-5+vz+g3d[[6,1,3]])^2)},{-((5 (vx+g3d[[6,2,1]]))/(-5+vz+g3d[[6,2,3]])^2),-((5 (vy+g3d[[6,2,2]]))/(-5+vz+g3d[[6,2,3]])^2)},{-((5 (vx+g3d[[6,3,1]]))/(-5+vz+g3d[[6,3,3]])^2),-((5 (vy+g3d[[6,3,2]]))/(-5+vz+g3d[[6,3,3]])^2)},{-((5 (vx+g3d[[6,4,1]]))/(-5+vz+g3d[[6,4,3]])^2),-((5 (vy+g3d[[6,4,2]]))/(-5+vz+g3d[[6,4,3]])^2)}}},{{{(5 (vx+g3d[[1,1,1]]) g3d[[1,1,2]])/(-5+vz+g3d[[1,1,3]])^2,(5 g3d[[1,1,2]] (vy+g3d[[1,1,2]]))/(-5+vz+g3d[[1,1,3]])^2+(5 g3d[[1,1,3]])/(-5+vz+g3d[[1,1,3]])},{(5 (vx+g3d[[1,2,1]]) g3d[[1,2,2]])/(-5+vz+g3d[[1,2,3]])^2,(5 g3d[[1,2,2]] (vy+g3d[[1,2,2]]))/(-5+vz+g3d[[1,2,3]])^2+(5 g3d[[1,2,3]])/(-5+vz+g3d[[1,2,3]])},{(5 (vx+g3d[[1,3,1]]) g3d[[1,3,2]])/(-5+vz+g3d[[1,3,3]])^2,(5 g3d[[1,3,2]] (vy+g3d[[1,3,2]]))/(-5+vz+g3d[[1,3,3]])^2+(5 g3d[[1,3,3]])/(-5+vz+g3d[[1,3,3]])},{(5 (vx+g3d[[1,4,1]]) g3d[[1,4,2]])/(-5+vz+g3d[[1,4,3]])^2,(5 g3d[[1,4,2]] (vy+g3d[[1,4,2]]))/(-5+vz+g3d[[1,4,3]])^2+(5 g3d[[1,4,3]])/(-5+vz+g3d[[1,4,3]])}},{{(5 (vx+g3d[[2,1,1]]) g3d[[2,1,2]])/(-5+vz+g3d[[2,1,3]])^2,(5 g3d[[2,1,2]] (vy+g3d[[2,1,2]]))/(-5+vz+g3d[[2,1,3]])^2+(5 g3d[[2,1,3]])/(-5+vz+g3d[[2,1,3]])},{(5 (vx+g3d[[2,2,1]]) g3d[[2,2,2]])/(-5+vz+g3d[[2,2,3]])^2,(5 g3d[[2,2,2]] (vy+g3d[[2,2,2]]))/(-5+vz+g3d[[2,2,3]])^2+(5 g3d[[2,2,3]])/(-5+vz+g3d[[2,2,3]])},{(5 (vx+g3d[[2,3,1]]) g3d[[2,3,2]])/(-5+vz+g3d[[2,3,3]])^2,(5 g3d[[2,3,2]] (vy+g3d[[2,3,2]]))/(-5+vz+g3d[[2,3,3]])^2+(5 g3d[[2,3,3]])/(-5+vz+g3d[[2,3,3]])},{(5 (vx+g3d[[2,4,1]]) g3d[[2,4,2]])/(-5+vz+g3d[[2,4,3]])^2,(5 g3d[[2,4,2]] (vy+g3d[[2,4,2]]))/(-5+vz+g3d[[2,4,3]])^2+(5 g3d[[2,4,3]])/(-5+vz+g3d[[2,4,3]])}},{{(5 (vx+g3d[[3,1,1]]) g3d[[3,1,2]])/(-5+vz+g3d[[3,1,3]])^2,(5 g3d[[3,1,2]] (vy+g3d[[3,1,2]]))/(-5+vz+g3d[[3,1,3]])^2+(5 g3d[[3,1,3]])/(-5+vz+g3d[[3,1,3]])},{(5 (vx+g3d[[3,2,1]]) g3d[[3,2,2]])/(-5+vz+g3d[[3,2,3]])^2,(5 g3d[[3,2,2]] (vy+g3d[[3,2,2]]))/(-5+vz+g3d[[3,2,3]])^2+(5 g3d[[3,2,3]])/(-5+vz+g3d[[3,2,3]])},{(5 (vx+g3d[[3,3,1]]) g3d[[3,3,2]])/(-5+vz+g3d[[3,3,3]])^2,(5 g3d[[3,3,2]] (vy+g3d[[3,3,2]]))/(-5+vz+g3d[[3,3,3]])^2+(5 g3d[[3,3,3]])/(-5+vz+g3d[[3,3,3]])},{(5 (vx+g3d[[3,4,1]]) g3d[[3,4,2]])/(-5+vz+g3d[[3,4,3]])^2,(5 g3d[[3,4,2]] (vy+g3d[[3,4,2]]))/(-5+vz+g3d[[3,4,3]])^2+(5 g3d[[3,4,3]])/(-5+vz+g3d[[3,4,3]])}},{{(5 (vx+g3d[[4,1,1]]) g3d[[4,1,2]])/(-5+vz+g3d[[4,1,3]])^2,(5 g3d[[4,1,2]] (vy+g3d[[4,1,2]]))/(-5+vz+g3d[[4,1,3]])^2+(5 g3d[[4,1,3]])/(-5+vz+g3d[[4,1,3]])},{(5 (vx+g3d[[4,2,1]]) g3d[[4,2,2]])/(-5+vz+g3d[[4,2,3]])^2,(5 g3d[[4,2,2]] (vy+g3d[[4,2,2]]))/(-5+vz+g3d[[4,2,3]])^2+(5 g3d[[4,2,3]])/(-5+vz+g3d[[4,2,3]])},{(5 (vx+g3d[[4,3,1]]) g3d[[4,3,2]])/(-5+vz+g3d[[4,3,3]])^2,(5 g3d[[4,3,2]] (vy+g3d[[4,3,2]]))/(-5+vz+g3d[[4,3,3]])^2+(5 g3d[[4,3,3]])/(-5+vz+g3d[[4,3,3]])},{(5 (vx+g3d[[4,4,1]]) g3d[[4,4,2]])/(-5+vz+g3d[[4,4,3]])^2,(5 g3d[[4,4,2]] (vy+g3d[[4,4,2]]))/(-5+vz+g3d[[4,4,3]])^2+(5 g3d[[4,4,3]])/(-5+vz+g3d[[4,4,3]])}},{{(5 (vx+g3d[[5,1,1]]) g3d[[5,1,2]])/(-5+vz+g3d[[5,1,3]])^2,(5 g3d[[5,1,2]] (vy+g3d[[5,1,2]]))/(-5+vz+g3d[[5,1,3]])^2+(5 g3d[[5,1,3]])/(-5+vz+g3d[[5,1,3]])},{(5 (vx+g3d[[5,2,1]]) g3d[[5,2,2]])/(-5+vz+g3d[[5,2,3]])^2,(5 g3d[[5,2,2]] (vy+g3d[[5,2,2]]))/(-5+vz+g3d[[5,2,3]])^2+(5 g3d[[5,2,3]])/(-5+vz+g3d[[5,2,3]])},{(5 (vx+g3d[[5,3,1]]) g3d[[5,3,2]])/(-5+vz+g3d[[5,3,3]])^2,(5 g3d[[5,3,2]] (vy+g3d[[5,3,2]]))/(-5+vz+g3d[[5,3,3]])^2+(5 g3d[[5,3,3]])/(-5+vz+g3d[[5,3,3]])},{(5 (vx+g3d[[5,4,1]]) g3d[[5,4,2]])/(-5+vz+g3d[[5,4,3]])^2,(5 g3d[[5,4,2]] (vy+g3d[[5,4,2]]))/(-5+vz+g3d[[5,4,3]])^2+(5 g3d[[5,4,3]])/(-5+vz+g3d[[5,4,3]])}},{{(5 (vx+g3d[[6,1,1]]) g3d[[6,1,2]])/(-5+vz+g3d[[6,1,3]])^2,(5 g3d[[6,1,2]] (vy+g3d[[6,1,2]]))/(-5+vz+g3d[[6,1,3]])^2+(5 g3d[[6,1,3]])/(-5+vz+g3d[[6,1,3]])},{(5 (vx+g3d[[6,2,1]]) g3d[[6,2,2]])/(-5+vz+g3d[[6,2,3]])^2,(5 g3d[[6,2,2]] (vy+g3d[[6,2,2]]))/(-5+vz+g3d[[6,2,3]])^2+(5 g3d[[6,2,3]])/(-5+vz+g3d[[6,2,3]])},{(5 (vx+g3d[[6,3,1]]) g3d[[6,3,2]])/(-5+vz+g3d[[6,3,3]])^2,(5 g3d[[6,3,2]] (vy+g3d[[6,3,2]]))/(-5+vz+g3d[[6,3,3]])^2+(5 g3d[[6,3,3]])/(-5+vz+g3d[[6,3,3]])},{(5 (vx+g3d[[6,4,1]]) g3d[[6,4,2]])/(-5+vz+g3d[[6,4,3]])^2,(5 g3d[[6,4,2]] (vy+g3d[[6,4,2]]))/(-5+vz+g3d[[6,4,3]])^2+(5 g3d[[6,4,3]])/(-5+vz+g3d[[6,4,3]])}}},{{{-((5 g3d[[1,1,1]] (vx+g3d[[1,1,1]]))/(-5+vz+g3d[[1,1,3]])^2)-(5 g3d[[1,1,3]])/(-5+vz+g3d[[1,1,3]]),-((5 g3d[[1,1,1]] (vy+g3d[[1,1,2]]))/(-5+vz+g3d[[1,1,3]])^2)},{-((5 g3d[[1,2,1]] (vx+g3d[[1,2,1]]))/(-5+vz+g3d[[1,2,3]])^2)-(5 g3d[[1,2,3]])/(-5+vz+g3d[[1,2,3]]),-((5 g3d[[1,2,1]] (vy+g3d[[1,2,2]]))/(-5+vz+g3d[[1,2,3]])^2)},{-((5 g3d[[1,3,1]] (vx+g3d[[1,3,1]]))/(-5+vz+g3d[[1,3,3]])^2)-(5 g3d[[1,3,3]])/(-5+vz+g3d[[1,3,3]]),-((5 g3d[[1,3,1]] (vy+g3d[[1,3,2]]))/(-5+vz+g3d[[1,3,3]])^2)},{-((5 g3d[[1,4,1]] (vx+g3d[[1,4,1]]))/(-5+vz+g3d[[1,4,3]])^2)-(5 g3d[[1,4,3]])/(-5+vz+g3d[[1,4,3]]),-((5 g3d[[1,4,1]] (vy+g3d[[1,4,2]]))/(-5+vz+g3d[[1,4,3]])^2)}},{{-((5 g3d[[2,1,1]] (vx+g3d[[2,1,1]]))/(-5+vz+g3d[[2,1,3]])^2)-(5 g3d[[2,1,3]])/(-5+vz+g3d[[2,1,3]]),-((5 g3d[[2,1,1]] (vy+g3d[[2,1,2]]))/(-5+vz+g3d[[2,1,3]])^2)},{-((5 g3d[[2,2,1]] (vx+g3d[[2,2,1]]))/(-5+vz+g3d[[2,2,3]])^2)-(5 g3d[[2,2,3]])/(-5+vz+g3d[[2,2,3]]),-((5 g3d[[2,2,1]] (vy+g3d[[2,2,2]]))/(-5+vz+g3d[[2,2,3]])^2)},{-((5 g3d[[2,3,1]] (vx+g3d[[2,3,1]]))/(-5+vz+g3d[[2,3,3]])^2)-(5 g3d[[2,3,3]])/(-5+vz+g3d[[2,3,3]]),-((5 g3d[[2,3,1]] (vy+g3d[[2,3,2]]))/(-5+vz+g3d[[2,3,3]])^2)},{-((5 g3d[[2,4,1]] (vx+g3d[[2,4,1]]))/(-5+vz+g3d[[2,4,3]])^2)-(5 g3d[[2,4,3]])/(-5+vz+g3d[[2,4,3]]),-((5 g3d[[2,4,1]] (vy+g3d[[2,4,2]]))/(-5+vz+g3d[[2,4,3]])^2)}},{{-((5 g3d[[3,1,1]] (vx+g3d[[3,1,1]]))/(-5+vz+g3d[[3,1,3]])^2)-(5 g3d[[3,1,3]])/(-5+vz+g3d[[3,1,3]]),-((5 g3d[[3,1,1]] (vy+g3d[[3,1,2]]))/(-5+vz+g3d[[3,1,3]])^2)},{-((5 g3d[[3,2,1]] (vx+g3d[[3,2,1]]))/(-5+vz+g3d[[3,2,3]])^2)-(5 g3d[[3,2,3]])/(-5+vz+g3d[[3,2,3]]),-((5 g3d[[3,2,1]] (vy+g3d[[3,2,2]]))/(-5+vz+g3d[[3,2,3]])^2)},{-((5 g3d[[3,3,1]] (vx+g3d[[3,3,1]]))/(-5+vz+g3d[[3,3,3]])^2)-(5 g3d[[3,3,3]])/(-5+vz+g3d[[3,3,3]]),-((5 g3d[[3,3,1]] (vy+g3d[[3,3,2]]))/(-5+vz+g3d[[3,3,3]])^2)},{-((5 g3d[[3,4,1]] (vx+g3d[[3,4,1]]))/(-5+vz+g3d[[3,4,3]])^2)-(5 g3d[[3,4,3]])/(-5+vz+g3d[[3,4,3]]),-((5 g3d[[3,4,1]] (vy+g3d[[3,4,2]]))/(-5+vz+g3d[[3,4,3]])^2)}},{{-((5 g3d[[4,1,1]] (vx+g3d[[4,1,1]]))/(-5+vz+g3d[[4,1,3]])^2)-(5 g3d[[4,1,3]])/(-5+vz+g3d[[4,1,3]]),-((5 g3d[[4,1,1]] (vy+g3d[[4,1,2]]))/(-5+vz+g3d[[4,1,3]])^2)},{-((5 g3d[[4,2,1]] (vx+g3d[[4,2,1]]))/(-5+vz+g3d[[4,2,3]])^2)-(5 g3d[[4,2,3]])/(-5+vz+g3d[[4,2,3]]),-((5 g3d[[4,2,1]] (vy+g3d[[4,2,2]]))/(-5+vz+g3d[[4,2,3]])^2)},{-((5 g3d[[4,3,1]] (vx+g3d[[4,3,1]]))/(-5+vz+g3d[[4,3,3]])^2)-(5 g3d[[4,3,3]])/(-5+vz+g3d[[4,3,3]]),-((5 g3d[[4,3,1]] (vy+g3d[[4,3,2]]))/(-5+vz+g3d[[4,3,3]])^2)},{-((5 g3d[[4,4,1]] (vx+g3d[[4,4,1]]))/(-5+vz+g3d[[4,4,3]])^2)-(5 g3d[[4,4,3]])/(-5+vz+g3d[[4,4,3]]),-((5 g3d[[4,4,1]] (vy+g3d[[4,4,2]]))/(-5+vz+g3d[[4,4,3]])^2)}},{{-((5 g3d[[5,1,1]] (vx+g3d[[5,1,1]]))/(-5+vz+g3d[[5,1,3]])^2)-(5 g3d[[5,1,3]])/(-5+vz+g3d[[5,1,3]]),-((5 g3d[[5,1,1]] (vy+g3d[[5,1,2]]))/(-5+vz+g3d[[5,1,3]])^2)},{-((5 g3d[[5,2,1]] (vx+g3d[[5,2,1]]))/(-5+vz+g3d[[5,2,3]])^2)-(5 g3d[[5,2,3]])/(-5+vz+g3d[[5,2,3]]),-((5 g3d[[5,2,1]] (vy+g3d[[5,2,2]]))/(-5+vz+g3d[[5,2,3]])^2)},{-((5 g3d[[5,3,1]] (vx+g3d[[5,3,1]]))/(-5+vz+g3d[[5,3,3]])^2)-(5 g3d[[5,3,3]])/(-5+vz+g3d[[5,3,3]]),-((5 g3d[[5,3,1]] (vy+g3d[[5,3,2]]))/(-5+vz+g3d[[5,3,3]])^2)},{-((5 g3d[[5,4,1]] (vx+g3d[[5,4,1]]))/(-5+vz+g3d[[5,4,3]])^2)-(5 g3d[[5,4,3]])/(-5+vz+g3d[[5,4,3]]),-((5 g3d[[5,4,1]] (vy+g3d[[5,4,2]]))/(-5+vz+g3d[[5,4,3]])^2)}},{{-((5 g3d[[6,1,1]] (vx+g3d[[6,1,1]]))/(-5+vz+g3d[[6,1,3]])^2)-(5 g3d[[6,1,3]])/(-5+vz+g3d[[6,1,3]]),-((5 g3d[[6,1,1]] (vy+g3d[[6,1,2]]))/(-5+vz+g3d[[6,1,3]])^2)},{-((5 g3d[[6,2,1]] (vx+g3d[[6,2,1]]))/(-5+vz+g3d[[6,2,3]])^2)-(5 g3d[[6,2,3]])/(-5+vz+g3d[[6,2,3]]),-((5 g3d[[6,2,1]] (vy+g3d[[6,2,2]]))/(-5+vz+g3d[[6,2,3]])^2)},{-((5 g3d[[6,3,1]] (vx+g3d[[6,3,1]]))/(-5+vz+g3d[[6,3,3]])^2)-(5 g3d[[6,3,3]])/(-5+vz+g3d[[6,3,3]]),-((5 g3d[[6,3,1]] (vy+g3d[[6,3,2]]))/(-5+vz+g3d[[6,3,3]])^2)},{-((5 g3d[[6,4,1]] (vx+g3d[[6,4,1]]))/(-5+vz+g3d[[6,4,3]])^2)-(5 g3d[[6,4,3]])/(-5+vz+g3d[[6,4,3]]),-((5 g3d[[6,4,1]] (vy+g3d[[6,4,2]]))/(-5+vz+g3d[[6,4,3]])^2)}}},{{{(5 g3d[[1,1,2]])/(-5+vz+g3d[[1,1,3]]),-((5 g3d[[1,1,1]])/(-5+vz+g3d[[1,1,3]]))},{(5 g3d[[1,2,2]])/(-5+vz+g3d[[1,2,3]]),-((5 g3d[[1,2,1]])/(-5+vz+g3d[[1,2,3]]))},{(5 g3d[[1,3,2]])/(-5+vz+g3d[[1,3,3]]),-((5 g3d[[1,3,1]])/(-5+vz+g3d[[1,3,3]]))},{(5 g3d[[1,4,2]])/(-5+vz+g3d[[1,4,3]]),-((5 g3d[[1,4,1]])/(-5+vz+g3d[[1,4,3]]))}},{{(5 g3d[[2,1,2]])/(-5+vz+g3d[[2,1,3]]),-((5 g3d[[2,1,1]])/(-5+vz+g3d[[2,1,3]]))},{(5 g3d[[2,2,2]])/(-5+vz+g3d[[2,2,3]]),-((5 g3d[[2,2,1]])/(-5+vz+g3d[[2,2,3]]))},{(5 g3d[[2,3,2]])/(-5+vz+g3d[[2,3,3]]),-((5 g3d[[2,3,1]])/(-5+vz+g3d[[2,3,3]]))},{(5 g3d[[2,4,2]])/(-5+vz+g3d[[2,4,3]]),-((5 g3d[[2,4,1]])/(-5+vz+g3d[[2,4,3]]))}},{{(5 g3d[[3,1,2]])/(-5+vz+g3d[[3,1,3]]),-((5 g3d[[3,1,1]])/(-5+vz+g3d[[3,1,3]]))},{(5 g3d[[3,2,2]])/(-5+vz+g3d[[3,2,3]]),-((5 g3d[[3,2,1]])/(-5+vz+g3d[[3,2,3]]))},{(5 g3d[[3,3,2]])/(-5+vz+g3d[[3,3,3]]),-((5 g3d[[3,3,1]])/(-5+vz+g3d[[3,3,3]]))},{(5 g3d[[3,4,2]])/(-5+vz+g3d[[3,4,3]]),-((5 g3d[[3,4,1]])/(-5+vz+g3d[[3,4,3]]))}},{{(5 g3d[[4,1,2]])/(-5+vz+g3d[[4,1,3]]),-((5 g3d[[4,1,1]])/(-5+vz+g3d[[4,1,3]]))},{(5 g3d[[4,2,2]])/(-5+vz+g3d[[4,2,3]]),-((5 g3d[[4,2,1]])/(-5+vz+g3d[[4,2,3]]))},{(5 g3d[[4,3,2]])/(-5+vz+g3d[[4,3,3]]),-((5 g3d[[4,3,1]])/(-5+vz+g3d[[4,3,3]]))},{(5 g3d[[4,4,2]])/(-5+vz+g3d[[4,4,3]]),-((5 g3d[[4,4,1]])/(-5+vz+g3d[[4,4,3]]))}},{{(5 g3d[[5,1,2]])/(-5+vz+g3d[[5,1,3]]),-((5 g3d[[5,1,1]])/(-5+vz+g3d[[5,1,3]]))},{(5 g3d[[5,2,2]])/(-5+vz+g3d[[5,2,3]]),-((5 g3d[[5,2,1]])/(-5+vz+g3d[[5,2,3]]))},{(5 g3d[[5,3,2]])/(-5+vz+g3d[[5,3,3]]),-((5 g3d[[5,3,1]])/(-5+vz+g3d[[5,3,3]]))},{(5 g3d[[5,4,2]])/(-5+vz+g3d[[5,4,3]]),-((5 g3d[[5,4,1]])/(-5+vz+g3d[[5,4,3]]))}},{{(5 g3d[[6,1,2]])/(-5+vz+g3d[[6,1,3]]),-((5 g3d[[6,1,1]])/(-5+vz+g3d[[6,1,3]]))},{(5 g3d[[6,2,2]])/(-5+vz+g3d[[6,2,3]]),-((5 g3d[[6,2,1]])/(-5+vz+g3d[[6,2,3]]))},{(5 g3d[[6,3,2]])/(-5+vz+g3d[[6,3,3]]),-((5 g3d[[6,3,1]])/(-5+vz+g3d[[6,3,3]]))},{(5 g3d[[6,4,2]])/(-5+vz+g3d[[6,4,3]]),-((5 g3d[[6,4,1]])/(-5+vz+g3d[[6,4,3]]))}}}}
,CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True,RuntimeOptions->"Speed"];

CC=With[{\[Sigma]=\[Sigma]},Compile[{{c1,_Real},{c2,_Real},{c3,_Real},{c4,_Real},{c5,_Real},{c6,_Real}},
Chop[1/(2\[Pi] \[Sigma]) 1/2 E^(-((c4 c5-c3 c6)^2/(2 (c3^2+c5^2) \[Sigma]^2))) ((c2 Sqrt[2 \[Pi]] (-(((c3 c4+c5 c6) Erf[Abs[c3 c4+c5 c6]/(Sqrt[2] Sqrt[c3^2+c5^2] \[Sigma])])/Abs[c3 c4+c5 c6])+((c3^2+c3 c4+c5 (c5+c6)) Erf[Abs[c3^2+c3 c4+c5 (c5+c6)]/(Sqrt[2] Sqrt[c3^2+c5^2] \[Sigma])])/Abs[c3^2+c3 c4+c5 (c5+c6)]))/Sqrt[c3^2+c5^2]+c1 ((2 (E^(-((c3 c4+c5 c6)^2/(2 (c3^2+c5^2) \[Sigma]^2)))-E^(-((c3^2+c3 c4+c5 (c5+c6))^2/(2 (c3^2+c5^2) \[Sigma]^2)))) \[Sigma])/(c3^2+c5^2)+((c3 c4+c5 c6)^2 Sqrt[2 \[Pi]] Erf[Abs[c3 c4+c5 c6]/(Sqrt[2] Sqrt[c3^2+c5^2] \[Sigma])])/((c3^2+c5^2)^(3/2) Abs[c3 c4+c5 c6])-((c3^3 c4+c5^2 c6 (c5+c6)+c3 c4 c5 (c5+2 c6)+c3^2 (c4^2+c5 c6)) Sqrt[2 \[Pi]] Erf[Abs[c3^2+c3 c4+c5 (c5+c6)]/(Sqrt[2] Sqrt[c3^2+c5^2] \[Sigma])])/((c3^2+c5^2)^(3/2) Abs[c3^2+c3 c4+c5 (c5+c6)])))]
,
CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True,RuntimeOptions->"Speed"]];

dIMG[GEO_,VEL_,X_,Y_]:=Module[{ans,i,col,cyc,dcyc,nx,ny,a1,a2,dAx,dAy,dBx,dBy,Ax,Ay,Bx,By,coefs,nd},
ans=0;
For[i=1,i<=Length[GEO],i++,

col=GEO[[i,1,1]][[1]];
cyc=GEO[[i,2,{1,2,3,4,1}]];
dcyc=Reverse/@Differences[cyc];
dcyc*={{-1,1},{-1,1},{-1,1},{-1,1}};

a1=Normalize/@dcyc;
nx=a1[[All,1]];
ny=a1[[All,2]];

a2=VEL[[i,{1,2,3,4,1}]];
dAx=a2[[{1,2,3,4},1]];
dAy=a2[[{1,2,3,4},2]];
dBx=a2[[{2,3,4,5},1]];
dBy=a2[[{2,3,4,5},2]];

Ax=cyc[[{1,2,3,4},1]];
Ay=cyc[[{1,2,3,4},2]];
Bx=cyc[[{2,3,4,5},1]];
By=cyc[[{2,3,4,5},2]];

nd=Norm/@Differences[cyc];

coefs={-dAx nx+dBx nx-dAy ny+dBy ny,dAx nx+dAy ny,-Ax+Bx,Ax-X,-Ay+By,Ay-Y};
ans+=-col(CC[coefs[[1]],coefs[[2]],coefs[[3]],coefs[[4]],coefs[[5]],coefs[[6]]] . nd)
];
ans
]

dIMGc=Compile[{{GEO,_Real,3},{COL,_Real,1},{VEL,_Real,3},{X,_Real},{Y,_Real}},Module[{col,ans,i,cyc,dcyc,nx,ny,a1,a2,dAx,dAy,dBx,dBy,Ax,Ay,Bx,By,coefs,nd},
ans=0.;
For[i=1,i<=Length[GEO],i++,

(*cyc={{-1.,1.},{-1.,1.},{-1.,1.},{-1.,1.}};
dcyc={{-1.,1.},{-1.,1.},{-1.,1.},{-1.,1.}};*)

col=COL[[i]];
cyc=GEO[[i,{1,2,3,4,1}]];
dcyc=Reverse/@Differences[cyc];
dcyc*={{-1.,1.},{-1.,1.},{-1.,1.},{-1.,1.}};

a1=Normalize/@dcyc;
nx=a1[[All,1]];
ny=a1[[All,2]];

a2=VEL[[i,{1,2,3,4,1}]];
dAx=a2[[{1,2,3,4},1]];
dAy=a2[[{1,2,3,4},2]];
dBx=a2[[{2,3,4,5},1]];
dBy=a2[[{2,3,4,5},2]];

Ax=cyc[[{1,2,3,4},1]];
Ay=cyc[[{1,2,3,4},2]];
Bx=cyc[[{2,3,4,5},1]];
By=cyc[[{2,3,4,5},2]];

nd=Norm/@Differences[cyc];

coefs={-dAx nx+dBx nx-dAy ny+dBy ny,dAx nx+dAy ny,-Ax+Bx,Ax-X,-Ay+By,Ay-Y};
ans+=-col(CC[coefs[[1]],coefs[[2]],coefs[[3]],coefs[[4]],coefs[[5]],coefs[[6]]] . nd)
];
ans
]
];

D2c[ra_,\[Phi]_,vc_,\[Delta]_]:=Module[{g3D,AV,AG,velALL,geoALL,GP,agIMG},
g3D=GEO3D[ra,\[Phi]];
AV=ALLoVEL2[g3D,vc,\[Delta]];
AG=ALLoGEO2[g3D,vc,\[Delta]];
velALL=Flatten[AV,2];
geoALL=Flatten[AG,1];
GP=Table[GEOProj[geoALL[[i]]],{i,1,Length[geoALL]}];
agIMG=ParallelTable[dIMGc[GP[[i]][[All,2]],1.0GP[[i]][[All,1,1]][[All,1]],velALL[[i]],x,y],{i,1,42},{y ,1,-1,-h},{x,-1,1,h},ProgressReporting->False];
Table[(agIMG[[i]]-agIMG[[i+1]])/(2\[Delta]),{i,1,Length[velALL]-1,2}][[{16,17,18,1,7,12,19,20,2,8,13,21,3,9,14,4,5,6,10,11,15}]]
]


IMG[GEO_,prec_]:=Module[{PRJ,ANS,FG,k,color,kek,mcoo,mcls,triangles,trsQ,f1,f2,X,XS,YS,fa},

PRJ=GEO[[All,2]];
ANS=Table[0,{x,-1,1,h},{y,-1,1,h}];

For[k=1,k<=Length[GEO],k++,
color=GEO[[k,1,1,1]];
kek=TriangulateMesh[Polygon[PRJ[[k]]],MeshQualityGoal->1,MaxCellMeasure->prec+0RegionMeasure[Polygon[PRJ[[k]]]]];
mcoo=MeshCoordinates[kek];
mcls=MeshCells[kek,2];

triangles=Table[mcoo[[mcls[[i,1]]]],{i,1,Length[mcls]}];
trsQ=CPTSc/@triangles;

f1=Flatten[trsQ[[All,All,1]]];
f2=Flatten[trsQ[[All,All,2]]];
X=Table[X,{X,-1,1,h}];  (*X aka Y*)

XS=cf2[cf[f1,X]];
YS=cf2[cf[f2,X]];
fa=Flatten[trsQ[[All,All,3]]];

ANS+=ParallelTable[color (XS[[All,i]] . (YS[[All,j]]*fa)),{j,Length[YS[[1]]],1,-1},{i,1,Length[XS[[1]]]}];
];
ANS
];

R[\[Alpha]_,\[Beta]_,\[Gamma]_]:={{Cos[\[Beta]] Cos[\[Gamma]],-Cos[\[Beta]] Sin[\[Gamma]],Sin[\[Beta]]},{Cos[\[Gamma]] Sin[\[Alpha]] Sin[\[Beta]]+Cos[\[Alpha]] Sin[\[Gamma]],Cos[\[Alpha]] Cos[\[Gamma]]-Sin[\[Alpha]] Sin[\[Beta]] Sin[\[Gamma]],-Cos[\[Beta]] Sin[\[Alpha]]},{-Cos[\[Alpha]] Cos[\[Gamma]] Sin[\[Beta]]+Sin[\[Alpha]] Sin[\[Gamma]],Cos[\[Gamma]] Sin[\[Alpha]]+Cos[\[Alpha]] Sin[\[Beta]] Sin[\[Gamma]],Cos[\[Alpha]] Cos[\[Beta]]}};

ALLoVEL2[g3D_,vc_,\[Delta]_]:=Module[{ANS,g3Ds,g3Da,g3Db},
g3Ds=GEO3DS[g3D,vc];
ANS={};

g3Da=g3D . R[\[Delta],0,0];
g3Db=g3D . R[-\[Delta],0,0];
AppendTo[ANS,Transpose[{dGRAD[g3Da,vc[[1]],vc[[2]],vc[[3]]][[All,SIDESa[g3Ds]]][[{1,2,3,4,5,6}]],dGRAD[g3Db,vc[[1]],vc[[2]],vc[[3]]][[All,SIDESa[g3Ds]]][[{1,2,3,4,5,6}]]}]];

g3Da=g3D . R[0,\[Delta],0];
g3Db=g3D . R[0,-\[Delta],0];
AppendTo[ANS,Transpose[{dGRAD[g3Da,vc[[1]],vc[[2]],vc[[3]]][[All,SIDESa[g3Ds]]][[{1,2,3,5,6}]],dGRAD[g3Db,vc[[1]],vc[[2]],vc[[3]]][[All,SIDESa[g3Ds]]][[{1,2,3,5,6}]]}]];

g3Da=g3D . R[0,0,\[Delta]];
g3Db=g3D . R[0,0,-\[Delta]];
AppendTo[ANS,Transpose[{dGRAD[g3Da,vc[[1]],vc[[2]],vc[[3]]][[All,SIDESa[g3Ds]]][[{1,2,3,6}]],dGRAD[g3Db,vc[[1]],vc[[2]],vc[[3]]][[All,SIDESa[g3Ds]]][[{1,2,3,6}]]}]];

g3Da=GEO3DS[g3D,{\[Delta],0,0}];
g3Db=GEO3DS[g3D,{-\[Delta],0,0}];
AppendTo[ANS,Transpose[{dGRAD[g3Da,vc[[1]],vc[[2]],vc[[3]]][[All,SIDESa[g3Ds]]][[{1,2,3}]],dGRAD[g3Db,vc[[1]],vc[[2]],vc[[3]]][[All,SIDESa[g3Ds]]][[{1,2,3}]]}]];

g3Da=GEO3DS[g3D,{0,\[Delta],0}];
g3Db=GEO3DS[g3D,{0,-\[Delta],0}];
AppendTo[ANS,Transpose[{dGRAD[g3Da,vc[[1]],vc[[2]],vc[[3]]][[All,SIDESa[g3Ds]]][[{2,3}]],dGRAD[g3Db,vc[[1]],vc[[2]],vc[[3]]][[All,SIDESa[g3Ds]]][[{2,3}]]}]];

g3Da=GEO3DS[g3D,{0,0,\[Delta]}];
g3Db=GEO3DS[g3D,{0,0,-\[Delta]}];
AppendTo[ANS,Transpose[{dGRAD[g3Da,vc[[1]],vc[[2]],vc[[3]]][[All,SIDESa[g3Ds]]][[{3}]],dGRAD[g3Db,vc[[1]],vc[[2]],vc[[3]]][[All,SIDESa[g3Ds]]][[{3}]]}]];
ANS
]


ALLoGEO2[g3D_,vc_,\[Delta]_]:=Module[{ANS,g3Ds,g3Da,g3Db},
ANS={};
g3Da=GEO3DS[g3D . R[\[Delta],0,0],vc];
g3Db=GEO3DS[g3D . R[-\[Delta],0,0],vc];
AppendTo[ANS,{g3Da,g3Db}];AppendTo[ANS,{g3Da,g3Db}];AppendTo[ANS,{g3Da,g3Db}];
AppendTo[ANS,{g3Da,g3Db}];AppendTo[ANS,{g3Da,g3Db}];AppendTo[ANS,{g3Da,g3Db}];
g3Da=GEO3DS[g3D . R[0,\[Delta],0],vc];
g3Db=GEO3DS[g3D . R[0,-\[Delta],0],vc];
AppendTo[ANS,{g3Da,g3Db}];AppendTo[ANS,{g3Da,g3Db}];AppendTo[ANS,{g3Da,g3Db}];
AppendTo[ANS,{g3Da,g3Db}];AppendTo[ANS,{g3Da,g3Db}];
g3Da=GEO3DS[g3D . R[0,0,\[Delta]],vc];
g3Db=GEO3DS[g3D . R[0,0,-\[Delta]],vc];
AppendTo[ANS,{g3Da,g3Db}];AppendTo[ANS,{g3Da,g3Db}];
AppendTo[ANS,{g3Da,g3Db}];AppendTo[ANS,{g3Da,g3Db}];

g3Ds=GEO3DS[g3D,vc];
g3Da=GEO3DS[g3Ds,{\[Delta],0,0}];
g3Db=GEO3DS[g3Ds,{-\[Delta],0,0}];
AppendTo[ANS,{g3Da,g3Db}];AppendTo[ANS,{g3Da,g3Db}];
AppendTo[ANS,{g3Da,g3Db}];
g3Da=GEO3DS[g3Ds,{0,\[Delta],0}];
g3Db=GEO3DS[g3Ds,{0,-\[Delta],0}];
AppendTo[ANS,{g3Da,g3Db}];
AppendTo[ANS,{g3Da,g3Db}];
g3Da=GEO3DS[g3Ds,{0,0,\[Delta]}];
g3Db=GEO3DS[g3Ds,{0,0,-\[Delta]}];
AppendTo[ANS,{g3Da,g3Db}];
ANS
]

D2[ra_,\[Phi]_,vc_,\[Delta]_]:=Module[{g3D,AV,AG,velALL,geoALL,GP,agIMG},
g3D=GEO3D[ra,\[Phi]];
AV=ALLoVEL2[g3D,vc,\[Delta]];
AG=ALLoGEO2[g3D,vc,\[Delta]];
velALL=Flatten[AV,2];
geoALL=Flatten[AG,1];
GP=Table[GEOProj[geoALL[[i]]],{i,1,Length[geoALL]}];
agIMG=ParallelTable[dIMG[GP[[i]],velALL[[i]],x,y],{i,1,42},{y ,1,-1,-h},{x,-1,1,h},ProgressReporting->False];
Table[(agIMG[[i]]-agIMG[[i+1]])/(2\[Delta]),{i,1,Length[velALL]-1,2}][[{16,17,18,1,7,12,19,20,2,8,13,21,3,9,14,4,5,6,10,11,15}]]
]

ALLd3GRAD[g3D_,\[Delta]_]:=Module[{vc,ANS,g3Ds,g3Da,g3Db},
vc={0.,0.,0.};
ANS={};
g3Da=d3GRADsh[GEO3DS[g3D . R[\[Delta],0,0],vc]];
g3Db=d3GRADsh[GEO3DS[g3D . R[-\[Delta],0,0],vc]];
AppendTo[ANS,Transpose[{g3Da,g3Db}]];
g3Da=d3GRADsh[GEO3DS[g3D . R[0,\[Delta],0],vc]];
g3Db=d3GRADsh[GEO3DS[g3D . R[0,-\[Delta],0],vc]];
AppendTo[ANS,Transpose[{g3Da[[{1,2,3,5,6}]],g3Db[[{1,2,3,5,6}]]}]];
g3Da=d3GRADsh[GEO3DS[g3D . R[0,0,\[Delta]],vc]];
g3Db=d3GRADsh[GEO3DS[g3D . R[0,0,-\[Delta]],vc]];
AppendTo[ANS,Transpose[{g3Da[[{1,2,3,6}]],g3Db[[{1,2,3,6}]]}]];
g3Ds=GEO3DS[g3D,vc];
g3Da=d3GRADsh[GEO3DS[g3Ds,{\[Delta],0,0}]];
g3Db=d3GRADsh[GEO3DS[g3Ds,{-\[Delta],0,0}]];
AppendTo[ANS,Transpose[{g3Da[[{1,2,3}]],g3Db[[{1,2,3}]]}]];
g3Da=d3GRADsh[GEO3DS[g3Ds,{0,\[Delta],0}]];
g3Db=d3GRADsh[GEO3DS[g3Ds,{0,-\[Delta],0}]];
AppendTo[ANS,Transpose[{g3Da[[{2,3}]],g3Db[[{2,3}]]}]];
g3Da=d3GRADsh[GEO3DS[g3Ds,{0,0,\[Delta]}]];
g3Db=d3GRADsh[GEO3DS[g3Ds,{0,0,-\[Delta]}]];
AppendTo[ANS,Transpose[{g3Da[[{3}]],g3Db[[{3}]]}]];
ANS
]
D2out[ra_,\[Phi]_,vc_,\[Delta]_]:=Module[{g3D,AV,AG,velALL,geoALL,GP,agIMG},
g3D=GEO3D[ra,\[Phi]];
AG=Flatten[ALLd3GRAD[g3D,\[Delta]],2];
Table[(AG[[i]]-AG[[i+1]])/(2\[Delta]),{i,1,Length[AG]-1,2}][[{16,17,18,1,7,12,19,20,2,8,13,21,3,9,14,4,5,6,10,11,15}]]
]

SOF[IMG_,dIMG_,d2IMG_,x_,y_,z_,\[Alpha]_,\[Beta]_,\[Gamma]_]:=IMG+x dIMG[[1]]+y dIMG[[2]]+z dIMG[[3]]+\[Alpha] dIMG[[4]]+ \[Beta] dIMG[[5]]+ \[Gamma] dIMG[[6]]+
d2IMG[[1]]x^2/2+d2IMG[[2]]x y+d2IMG[[3]]x z+d2IMG[[4]]x \[Alpha]+d2IMG[[5]]x \[Beta]+d2IMG[[6]]x \[Gamma]+
d2IMG[[7]]y^2/2+d2IMG[[8]]y z+d2IMG[[9]]y \[Alpha]+d2IMG[[10]]y \[Beta]+d2IMG[[11]]y \[Gamma]+d2IMG[[12]]z^2/2+d2IMG[[13]]z \[Alpha]+d2IMG[[14]]z \[Beta]+d2IMG[[15]]z \[Gamma]+d2IMG[[16]]\[Alpha]^2/2+d2IMG[[17]]\[Alpha] \[Beta]+d2IMG[[18]]\[Alpha] \[Gamma]+d2IMG[[19]]\[Beta]^2/2+d2IMG[[20]]\[Beta] \[Gamma]+d2IMG[[21]]\[Gamma]^2/2;





(* ::Input::Initialization:: *)
BATCH=1260;
TOT=117180;
INP=Import["6d300k.mx"];
\[Tau]=TimeUsed[];

j=1;
i=1;
KA=1;

(* SetDirectory["/Users/vsevolod/Documents/Wolfram Mathematica/CUBE/II/data/GEN"]; *)

IMGS=Table[0,{j,1,BATCH}];
dIMGS=Table[0,{j,1,BATCH}];
d2IMGS=Table[0,{j,1,BATCH}];
D3C=Table[0,{j,1,BATCH}];
dD3C=Table[0,{j,1,BATCH}];
d2D3C=Table[0,{j,1,BATCH}];
PRS=Table[0,{j,1,BATCH}];

While[i<=TOT,
\[Iota]=i;
vc=0.52INP[[3,\[Iota]]];
ra=INP[[1,\[Iota]]];
\[Phi]=1/8(INP[[2,\[Iota]]]-\[Pi]);


geo=GEOM[vc,ra,\[Phi]];
geo3D=GEO3DX[ra,\[Phi],vc];
ars=Sort[qarea/@geo[[All,2]]];
(*If[Length[ars]>1&&ars[[1]]>0.03,*)
If[1==1,
vector=VEC[geo];
pc=0.0003;

GEO=geo;
prec=pc;
PRJ=GEO[[All,2]];

ANS=Table[0.,{x,-1,1,h},{y,-1,1,h}];

(*FG=Graphics[Table[{GEO[[i,1]],Polygon[PRJ[[i]]]},{i,1,Length[GEO]}]];
For[k=1,k<=Length[GEO],k++,
color=GEO[[k,1,1,1]];
kek=TriangulateMesh[Polygon[PRJ[[k]]],MeshQualityGoal->1,MaxCellMeasure->prec+0RegionMeasure[Polygon[PRJ[[k]]]]];
mcoo=MeshCoordinates[kek];
mcls=MeshCells[kek,2];

triangles=Table[mcoo[[mcls[[i,1]]]],{i,1,Length[mcls]}];
trsQ=CPTSc/@triangles;

f1=Flatten[trsQ[[All,All,1]]];
f2=Flatten[trsQ[[All,All,2]]];
X=Table[X,{X,-1,1,h}];  (*X aka Y*)

XS=cf2[cf[f1,X]];
YS=cf2[cf[f2,X]];
fa=Flatten[trsQ[[All,All,3]]];

ANS+=ParallelTable[color (XS[[All,i]].(YS[[All,j]]*fa)),{j,Length[YS[[1]]],1,-1},{i,1,Length[XS[[1]]]}];
];*)

g3D=GEO3D[ra,\[Phi]];
g3DS=GEO3DS[g3D,vc];
geo=GEOProj[g3DS];
vel=dGRAD[g3D,vc[[1]],vc[[2]],vc[[3]]][[All,SIDES[vc,ra,\[Phi]]]];

(*dIMGS[[j]]=ParallelTable[dIMG[geo,vel[[i]],x,y],{i,1,6},{y ,1,-1,-h},{x,-1,1,h}];*)
dIMGS[[j]]=ParallelTable[dIMGc[geo[[All,2]],1.0geo[[All,1,1]][[All,1]],vel[[i]],x,y],{i,1,6},{y ,1,-1,-h},{x,-1,1,h}];

\[Delta]1=0.000005;
g3D=GEO3D[ra,\[Phi]];
AV=ALLoVEL2[g3D,vc,\[Delta]1];
AG=ALLoGEO2[g3D,vc,\[Delta]1];
velALL=Flatten[AV,2];
geoALL=Flatten[AG,1];
GP=Table[GEOProj[geoALL[[i]]],{i,1,Length[geoALL]}];
agIMG=ParallelTable[dIMGc[GP[[i]][[All,2]],1.0GP[[i]][[All,1,1]][[All,1]],velALL[[i]],x,y],{i,1,42},{y ,1,-1,-h},{x,-1,1,h},ProgressReporting->False];
d2IMGS[[j]]=Table[(agIMG[[i]]-agIMG[[i+1]])/(2\[Delta]1),{i,1,Length[velALL]-1,2}][[{16,17,18,1,7,12,19,20,2,8,13,21,3,9,14,4,5,6,10,11,15}]];
(*d2IMGS[[j]]=D2c[ra,\[Phi],vc,0.000005];*)

IMGS[[j]]=ANS;
D3C[[j]]=GEO3DX[ra,\[Phi],vc];
dD3C[[j]]=d3GRAD[ra,\[Phi],vc];
d2D3C[[j]]=D2out[ra,\[Phi],vc,0.000005];
PRS[[j]]={vc,ra,\[Phi]};



If[Mod[j,BATCH]==0,
Export["img-"<>ToString[KA]<>".mx",IMGS];
Export["d_img-"<>ToString[KA]<>".mx",dIMGS];
Export["d2_img-"<>ToString[KA]<>".mx",d2IMGS];
Export["3d-"<>ToString[KA]<>".mx",D3C];
Export["d_3d-"<>ToString[KA]<>".mx",dD3C];
Export["d2_3d-"<>ToString[KA]<>".mx",d2D3C];
Export["pars-"<>ToString[KA]<>".mx",PRS];
Print["saved ",KA," batch"];
j=0;
KA++;
];

j++;

];

If[Mod[i,10]==0,Print[i]];
i++;

];
Print[TimeUsed[]-\[Tau]];
