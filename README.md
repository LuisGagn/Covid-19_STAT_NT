# Covid-19_STAT_NT
Analisis de datos sobre Covid-19 en R 
Materia: Nuevas Tecnologias para el Analisis Estadistico de Datos
Profesores: Natalia da Silva | Federico Molina
Realizado por: Nicolas Ferreira | Luis Gagñevin

Shiny: https://luisgagn.shinyapps.io/Proyecto_COVID-19/



Necesario para su reproduccion:
R 4.0.5
RStudio
LaTeX - TinyTex
Ghostscript 9.54.0 : https://ghostscript.com/download/gsdnld.html



## Instalacion:
1) Descargar las carpetas: RMD / Data / Proyecto Grupal, y sus archivos
2) Dejarlas en una ubicacion a gusto.

## Reproduccion:

RMarkDown
1) Abrir: COVID-19.Rmd en la carpeta RMD
2) Verificar la existencia de los packages en el sistema, de no existir alguno, instalarlo
3) Seleccionar Knit y esperar el tiempo
   En caso de fallos ir al final.
  
Shinyapp
1) Abrir app.R en ProyectoGrupal
2) Verificar la existencia de los packages en el sistema, de no existir alguno, instalarlo
3) Seleccionar RunApp y esperar el tiempo adecuado.
   En caso de fallos ir al final.






# FALLOS CONOCIDOS:

**GhostScript no se encuentra instalado o no esta en el PATH:**

WINDOWS:
Necesitas agregarlo al PATH para ello sigue los pasos:
- Panel de control > Sistema y Seguridad > Sistema > Opciones Avanzadas > Avanzado > Enviromental Variables
- Debajo de Variables del Sistema selecciona Path > Editar > Agrega el directorio de gs, por lo general es: C:\Program Files\gs\gs9.54\bin
- 
MAC: 
- Reinicia R
- Descargar y reinstalar GhostScript desde: http://pages.uoregon.edu/koch/
- Reiniciar R

**Archivos que no existen en el directorio**
- Fijarse que todas las carpetas esten descargadas, tanto las Fotos como la carpeta Data.
- Reinstalar readr [install.packages("readr")]
- En caso de descargar los archivos nuevamente y no funcionar mover la Data hacia la carpeta necesaria y cambiar su ruta

**Problema con la reproduccion del mapamundi**
- Reinstalar rgeos [install.packages(rgeos)]

**Fotos no funcionando en la Shiny**
- Fijarse de tener descargada la carpeta www de ProyectoGrupal y que este dentro de esta.

**Disconnected from shiny**
- Simplemente tocar reload y esperar, es un fallo del uso de Shinyapps al ser gratuita la suscripcion.
