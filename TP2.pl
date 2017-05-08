/*TP 2 Paradigma Logico*/.

/*Punto 1: Materias*/

materia(nombre, cantHorasTot).
esPromocionable(materia).
esInicial(materia).
esNecesariaParaCustar(materiaUno, materiaDos). /*esto se leeria como que la materia uno es necesaria para poder cursar materia dos EJ:esNecesariaParaCustar(mateI, mateII) */

esPesada(Materia) :- mayorCien(Materia(_,CantHs)).
esPesada(Materia) :- not(esPromocionable(Materia)),
	largoNombre(Materia(Nombre,_)).
	
mayorCien(CantHs) :- (CantHs>100).
largoNombre(Nombre) :-(Nombre>15).

/*Punto 2: Alumno*/.
 
 
 /*Tests*/
 
 materia(laboratorioDeComputaciónI, 10 ).
 materia(laboratorioDeComputaciónII, 10 ).
 materia(matematicasI, 10 ).
 materia(matematicasII, 10 ).
 materia(sistemaDeProcesamientoDeDatos, 10 ).
 materia(sistemasOperativos, 10 ).
 materia(paradigmasDeProgramacion, 10 ).
 materia(matematicasII, 10 ).
 materia(matematicasII, 10 ).
 
 esPromocionable(laboratorioDeComputaciónI, 10 ).
 esPromocionable(laboratorioDeComputaciónII, 10 ).
 esPromocionable(matematicasI, 10 ).
 esPromocionable(matematicasII, 10 ).
 esPromocionable(sistemaDeProcesamientoDeDatos, 10 ).
 esPromocionable(sistemasOperativos, 10 ).
 esPromocionable(paradigmasDeProgramacion, 10 ).
 esPromocionable(matematicasII, 10 ).
 esPromocionable(matematicasII, 10 ).
 
 /*Punto 6*/
 esPesada(algoritmosI).
 esPesada(baseDeDatos).
 esPesada(metodosNumericos). /*Expected false*/
 



 


