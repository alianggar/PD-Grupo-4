
## 1. ¿Qué es un módulo en Haskell?
Un módulo es una unidad de código que agrupa tipos de datos, clases y funciones relacionadas bajo un mismo espacio de nombres. Sirve para estructurar proyectos grandes de forma limpia, separando responsabilidades y evitando que el código se convierta en un archivo monolítico e inmanejable.

## 2. ¿Qué es una lista de exportación?
Es la lista que se coloca entre paréntesis justo al lado de la declaración del módulo. Su función es actuar como una interfaz pública: determina exactamente qué tipos y funciones podrán ser utilizados por otros ficheros que importen este módulo. Todo lo que no esté en esa lista es privado (interno del módulo) y queda totalmente inaccesible desde fuera.

## 3. ¿Por qué ocultar el constructor de Jugador protege las invariantes?
En nuestro archivo `Jugador.hs`, exportamos el tipo `Jugador` pero no sus constructores (es decir, omitimos el `(..)` en la lista de exportación). Al hacer esto, convertimos `Jugador` en un **tipo de dato abstracto**.

Esto significa que si otro módulo (como `Mundo`) quiere un jugador nuevo o quiere modificarle la vida, el compilador no le dejará escribir `Jugador pos vel (-5) 0` directamente. Estará obligado a pasar por las funciones que sí hemos exportado (`crearJugador`, `perderVida`). 
Como nosotros controlamos esas funciones, podemos garantizar mediante su lógica interna que **el jugador siempre nacerá con 3 vidas** y que **las vidas nunca bajarán de 0**. Proteger el constructor evita que el resto del programa pueda crear accidentalmente (o a propósito) estados ilegales que rompan las matemáticas del juego.