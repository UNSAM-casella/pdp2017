 %TP 2 Paradigma Logico.

%Punto 1: Materias.

materia(nombre, cantHorasTot).
esPromocionable(materia).
esInicial(materia).
%esto se leeria como que la materia uno es necesaria para poder cursar materia dos EJ:esNecesariaParaCustar(mateI, mateII).



esCorrelativaDe(Padre, Correlativa) :-
	esCorrelativa(Padre, Hijo).
	
esCorrelativaDe(Materia, Correlativa).

listaNecesariaParaCursar(Materia) :- esNecesariaParaCursar(_, Materia).

esPesada(Materia) :-
	materia(Materia,CantHoras), 
	CantHoras>100.
esPesada(Materia) :- not(esPromocionable(Materia)).



mayorCien(CantHs) :- (CantHs>100).
%largoNombre(Nombre) :-(size(Nombre,R)>15).

%Punto 2: Alumno.
 
 
 %Tests.
 materia(pdp,10).
 materia(laboratorioDeComputacionI,10).
 materia(laboratorioDeComputacionII,10).
 materia(matematicasI,10).
 materia(matematicasII,10).
 materia(sistemaDeProcesamientoDeDatos,10).
 materia(sistemasOperativos,10).
 materia(paradigmasDeProgramacion,10).
 materia(matematicasII,10).
 materia(matematicasII,10).
 
 esPromocionable(laboratorioDeComputacionI).
 esPromocionable(laboratorioDeComputacionII).
 esPromocionable(matematicasI).
 esPromocionable(matematicasII ).
 esPromocionable(sistemaDeProcesamientoDeDatos).
 esPromocionable(sistemasOperativos).
 esPromocionable(paradigmasDeProgramacion ).
 esPromocionable(matematicasII).
 esPromocionable(matematicasII).
 
 %Punto 6.
 esPesada(algoritmosI).
 esPesada(baseDeDatos).
 
 %Expected false.
 esPesada(metodosNumericos). 
 
 
 esCorrelativa(matematicasII, matematicasI).
 esCorrelativa(matematicasIII, matematicasII).
 
