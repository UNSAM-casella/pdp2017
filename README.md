# Paradigmas de programación 2017 #
## Tratar de conquistar el mundo ##
En un laboratorio secreto de PDP, se están experimentando con distintos ratones para crear los nuevos medicamentos del mañana. Para lograrlo, nos pidieron que desarrollemos un sistema para ayudarlos. 

### Condiciones del TP ###
1.   Se evaluará el correcto uso de los conceptos del paradigma funcional vistos en clase.
2.   Todas las dudas que surjan podrán consultarlas por mail privado a su docente favorito. NO mandar cosas del TP a la lista del curso.
3.   Ambas partes incluyen una serie de pruebas para verificar el funcionamiento del programa. Para ello pueden:
4.   Hacer pruebas manuales por consola. En dicho caso deberán entregar, junto con el código, screenshots de las pruebas que hicieron.
5.   Hacer tests automatizados. Para eso contamos con dos guías que pueden encontrar en la parte de Apuntes del sitio de la materia. Recomendamos usar Hspec.
6.   Lo podrán resolver de a dos y la resolución se entregará por mail, copiando a su compañero, a: nahuel.palumbo@gmail.com

A continuación podrán encontrar los requerimientos obtenidos del relevamiento con el equipo de científicos:
## Parte 1 ##
### Obteniendo información ###
#### Estudios ####
Nos contaron que cada ratón posee edad, peso y altura.
Los científicos realizan estudios sobre los ratones para obtener indicadores del estado del ratón. Modelar los siguientes estudios:
1.   **Estudio de masa corporal**, que se calcula como peso / altura^2.
2.   **Estudio de antigüedad**, calculado como (edad + 5) / 85.

#### Análisis ####
Luego, se usan esos estudios para obtener un diagnóstico, y así saber si un ratón tiene buena salud o no. Un diagnóstico determina si un ratón está en peligro. 
Para transformar un estudio en diagnóstico hay que analizarlo. Existen 3 tipos de análisis:
1.    **De exceso**, que dan positivo cuando el índice producido por el análisis se encuentra por arriba de cierto valor crítico.
2.    **De rango medio**, que dan positivo cuando el índice producido por el análisis no se encuentra entre dos valores.
3.    **Berretas**, que nunca dan positivo.

### Curando ratones ###
#### Hierbas ####
Existen distintos tipos de hierbas que afectan de diferentes maneras al ratón:
	Hierba Buena: rejuvenece al ratón a la mitad de su edad.
	Por ejemplo, un ratón con 8 años queda con 4 años.
1.    Hierba Mala: envejece al ratón al doble de su edad. Por ejemplo, un ratón con 8 años queda con 16 años.
2.    Alcachofa: hace que el ratón pierda cierto porcentaje de su peso. Por ejemplo, una alcachofa de 10 hace que un ratón de 10 kilos quede con 9.	
3.    Hierba Zort: hace que el ratón se transforme en Pinky, el cual tiene 0 de edad, 0 de peso y 0 de altura.

#### Medicinas ####
Los científicos van experimentan para crear medicinas que curen a los ratones. Nos contaron que actualmente cuentan con las siguientes medicinas:
-    Cualquier hierba puede ser usada como medicina.
-    También pueden mezclar dos hierbas para obtener una nueva con el efecto de ambas.
-    **Medicamentos**: que se crean a partir de varias hierbas, y producen sobre un ratón el mismo efecto que todas ellas juntas.
-    **Tratamientos**: tienen en cuenta un diagnóstico y una lista de medicinas que se van a ir administrando sucesivamente hasta que el diagnóstico de negativo, o sea que el ratón se encuentra fuera de peligro.

## Casos de prueba ##
### Modelar los siguientes ratones: ###
-    Mickey Mouse (edad = 88, peso = 20 y altura = 0.8)
-    Jerry (edad = 76, peso = 2, altura = 0.3)

### Modelar los siguientes diagnósticos y verificar los resultados: ###
-    De antigüedad: analiza si el índice de antigüedad es mayor a 1. Debería dar positivo para Mickey y negativo para Jerry.
-    De masa corporal: analiza si el índice de masa corporal no se encuentra entre 18.5 y 25. Debería dar positivo para Mickey y negativo para Jerry.

### Modelar las siguientes medicinas y verificar las operaciones: ###
-    Verificar que mezclar una Hierba Buena con una Hierba Mala crea una hierba que no produce efecto.
-    Hacer el Ratisalil, un medicamento basado en una Hierba Zort y una Hierba Mala. Verificar que produce los mismos resultados que una Hierba Zort.
-    Hacer la Ponds Anti Age, que es un medicamento que está hecho con una Alcachofa de  10 y 3 hierbas buenas. Verificar que al administrárselo a Jerry queda con edad = 9.5, peso = 1.8 y altura = 0.3
-    Crear un tratamiento contra la antigüedad que conste de una Hierba Buena y una Ponds Anti Age. Verificar:
	-    Que al aplicársela a Mickey tiene el mismo efecto que una hierba buena.
	-    Que al aplicársela a Jerry tiene el mismo efecto que una alcachofa de 0.

