# LabParadigmas Funcional
Laboratorio de Paradigmas de Programación 2do semestre 2022
* Autor: Lucas Mesias Soza
* Sección: 13310-0-A-1
* Lenguaje: Scheme

## Antes de ejecutar
* Se requiere DrRacket versión 6.11 (https://download.racket-lang.org)
* Descargar el proyecto o clonar el repositorio con el comando 
```
git clone https://github.com/Skyrdow/LabParadigmas.git
```
## Ejecutando el laboratorio
Se debe abrir el archivo "script.rkt" con DrRacket y hacer click en el boton "run" o presionar CTRL+R, para una ejecución correcta, los archivos "image.rkt", "otras_funciones.rkt" y "pixel.rkt" deben estar en la misma carpeta que el archivo que estamos ejecutando.
## Uso de las funciones
Se asume que el usuario hace buen uso de las funciones implementadas, por lo que los valores de entrada deben tomar en cuenta las restricciones indicadas en la documentación de cada función. Adicionalmente estas restricciones también aplican para edición/modificación de valores, como puede ser el uso de adjustChannel para obtener pixeles con valores RGB fuera del rango permitido [0, 255].
El usuario se debe hacer cargo de usar las funciones específicas al tipo de imagen o pixel correspondiente, es decir, usar una función implementada exclusivamente para un pixrgb en un pixbit generará un error y se detendrá la ejecución del programa.
