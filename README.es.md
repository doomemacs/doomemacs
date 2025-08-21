<div align="center">

# Doom Emacs

[Instalar](#instalar) • [Documentación] • [FAQ] • [Capturas de pantalla] • [Contribuir](#contribuir)

![Made with Doom Emacs](https://img.shields.io/github/tag/doomemacs/doomemacs.svg?style=flat-square&label=release&color=58839b)
![Supports Emacs 27.1–30.1](https://img.shields.io/badge/Supports-Emacs_27.1–30.1-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
![Latest commit](https://img.shields.io/github/last-commit/doomemacs/doomemacs?style=flat-square)
<!-- ![Build status: master](https://img.shields.io/github/workflow/status/doomemacs/doomemacs/CI/master?style=flat-square) -->
[![Discord Server](https://img.shields.io/discord/406534637242810369?color=738adb&label=Discord&logo=discord&logoColor=white&style=flat-square)][Discord]
[![Discourse server](https://img.shields.io/discourse/users?server=https%3A%2F%2Fdiscourse.doomemacs.org&logo=discourse&label=Discourse&style=flat-square&color=9cf)][Discourse]

![Doom Emacs Screenshot](https://raw.githubusercontent.com/doomemacs/doomemacs/screenshots/main.png)

</div>

---

### Tabla de Contenidos
- [Introducción](#introducción)
- [Características](#características)
- [Prerrequisitos](#prerrequisitos)
- [Instalar](#instalar)
- [Hoja de ruta](#hoja-de-ruta)
- [Obtener ayuda](#obtener-ayuda)
- [Contribuir](#contribuir)

# Introducción
<a href="http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573">
  <img src="https://raw.githubusercontent.com/doomemacs/doomemacs/screenshots/cacochan.png" align="right" />
</a>

> Es una historia tan antigua como el tiempo. Un vimmer testarudo, habitante de
> la shell y melodramático—envidioso de las características de los editores de
> texto modernos—cae en la desesperación antes de sucumbir al [lado oscuro][evil-mode].
> Esta es su configuración.

Doom es un framework de configuración para [GNU Emacs] diseñado para veteranos
de la bancarrota de Emacs que desean menos framework en sus frameworks, un mínimo
de estabilidad (y reproducibilidad) de su gestor de paquetes, y el rendimiento
de una configuración hecha a mano (o mejor). Puede ser una base para tu propia
configuración o un recurso para que los entusiastas de Emacs aprendan más sobre
nuestro sistema operativo favorito.

Su diseño se guía por estos mantras:

+ **Hay que ir rápido.** El rendimiento de inicio y en tiempo de ejecución son
prioridades. Doom va más allá modificando los paquetes para que sean más rápidos
y se carguen de forma más perezosa.
+ **Cerca del metal.** Hay menos entre tú y Emacs vanilla por diseño. Eso es
menos que comprender y menos con lo que lidiar cuando trasteas. Los internos
deben escribirse como si leerlos fuera parte de la UX de Doom, ¡y lo es!
+ **Con opinión, pero no testarudo.** Doom se trata de valores predeterminados
razonables y opiniones seleccionadas, pero usa tanto o tan poco como quieras.
+ **Tu sistema, tus reglas.** Tú sabes más. Al menos, ¡Doom espera que sí! No
instalará *automáticamente* las dependencias del sistema (y obligará a los
plugins a que tampoco lo hagan). Confía en `doom doctor` para que te diga lo
que falta.
+ **¡Nix/Guix es una gran idea!** El ecosistema de Emacs es temperamental. Las
cosas se rompen y se rompen a menudo. ¡La recuperación ante desastres debe ser
una prioridad! La gestión de paquetes de Doom debe ser declarativa y tu
configuración privada reproducible, y viene con un medio para revertir
versiones y actualizaciones (aún en desarrollo).

Consulta las [FAQ][FAQ] para obtener respuestas a preguntas comunes sobre el proyecto.


# Características
- Aspecto minimalista inspirado en los editores modernos.
- Valores predeterminados seleccionados y sensatos para muchos paquetes, (principales)
sistemas operativos y el propio Emacs.
- Una estructura organizativa modular para separar las preocupaciones en tu
configuración.
- Una biblioteca estándar diseñada para simplificar tu "bike shedding" en elisp.
- Un [sistema de gestión de paquetes][package-management] declarativo (impulsado
por [straight.el]) con una interfaz de línea de comandos. Instala paquetes
desde cualquier lugar, no solo (M)ELPA, y fíjalos a cualquier commit.
- Emulación de vim opcional impulsada por [evil-mode], ¡incluyendo ports de
plugins populares de vim como [vim-sneak], [vim-easymotion], [vim-unimpaired]
y [más][ported-vim-plugins]!
- Integración LSP opcional para muchos lenguajes, usando [lsp-mode] o [eglot]
- Soporte para *muchos* lenguajes de programación. Incluye resaltado de sintaxis,
integración de linters/checkers, evaluación de código en línea, autocompletado
de código (donde sea posible), REPLs, búsqueda de documentación, snippets, ¡y
más!
- Soporte para *muchas* herramientas, como docker, pass, ansible, terraform y
más.
- Un [esquema de atajos de teclado][bindings] al estilo de Spacemacs, centrado
en las teclas de prefijo leader y localleader (<kbd>SPC</kbd> y
<kbd>SPC</kbd><kbd>m</kbd> para usuarios de evil, <kbd>C-c</kbd> y
<kbd>C-c l</kbd> para usuarios de vanilla).
- Un [gestor de popups][popup-system] basado en reglas para controlar cómo se
muestran (y se eliminan) los buffers temporales.
- Detección del estilo de indentación por archivo e integración con [editorconfig].
Deja que otros discutan sobre tabs vs **_espacios_**.
- Herramientas de gestión de proyectos y minor modes específicos del framework
con sus propias bibliotecas de snippets.
- Utilidades de búsqueda (y reemplazo) de proyectos, impulsadas por [ripgrep] e
[ivy] o [helm].
- Espacios de trabajo aislados y persistentes (también sustitutos de las pestañas
de vim).
- Soporte para sistemas de entrada chinos y japoneses.
- Guarda una instantánea de tu entorno de shell en un archivo para que Emacs lo
cargue al inicio. No más luchas para que Emacs herede tu `PATH`, entre otras
cosas.


# Prerrequisitos
- Git 2.23+
- Emacs 27.1–30.1 (**Recomendado: 30.1 +
[native-comp](https://www.emacswiki.org/emacs/GccEmacs)**)
- [ripgrep] 11.0+
- GNU `find`
- *OPCIONAL:* [fd] 7.3.0+ (mejora el rendimiento de la indexación de archivos
para algunos comandos)

> [!WARNING]
> Las compilaciones inestables y de pre-lanzamiento de Emacs -- que terminan en
> `.50`, `.60` o `.9X` (p. ej. `28.1.91`) -- **no son oficialmente compatibles**.
> Sin embargo, *hay* algún esfuerzo para soportar Emacs HEAD. [Sigue esta
> publicación en Discourse](https://discourse.doomemacs.org/t/3241) para más
> detalles.

> [!IMPORTANT]
> Doom se compone de [~150 módulos opcionales][Modules], algunos de los cuales
> pueden tener dependencias adicionales. [Visita su documentación][Modules] o
> ejecuta `bin/doom doctor` para comprobar si te has perdido alguno.



# Instalar
``` sh
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
```

Luego [lee nuestra guía de inicio][getting-started] para que te guíen a través
de la instalación, configuración y mantenimiento de Doom Emacs.

¡Es una buena idea añadir `~/.config/emacs/bin` a tu `PATH`! Otros comandos de
`bin/doom` que deberías conocer:

+ `doom sync` para sincronizar tu configuración privada con Doom instalando los
paquetes que faltan, eliminando los paquetes huérfanos y regenerando las
cachés. Ejecuta esto cada vez que modifiques tu `init.el` o `packages.el`
privado, o instales/elimines un paquete de Emacs a través del gestor de
paquetes de tu sistema operativo (p. ej. mu4e o agda).
+ `doom upgrade` para actualizar Doom a la última versión y todos los paquetes
instalados.
+ `doom doctor` para diagnosticar problemas comunes con tu sistema y
configuración.
+ `doom env` para volcar una instantánea de tu entorno de shell en un archivo
que Doom cargará al inicio. Esto permite que Emacs herede tu `PATH`, entre
otras cosas.


# Hoja de ruta
Doom es un proyecto activo y en curso. Para que ese desarrollo sea más
transparente, su hoja de ruta (y otras preocupaciones) se publican en tres
paneles de proyecto de github y un boletín:

+ [Hoja de ruta de desarrollo](https://discourse.doomemacs.org/t/development-roadmap/42):
describe a grandes rasgos nuestros objetivos entre los hitos de lanzamiento y
su progreso.
+ [Plugins en revisión](https://github.com/orgs/doomemacs/projects/5): enumera
los plugins que estamos observando y considerando para su inclusión, y cuál es
su estado para la inclusión. Consulta esta lista antes de solicitar nuevos
paquetes/características.
+ [Errores upstream](https://github.com/orgs/doomemacs/projects/7): enumera los
problemas que se originan en otros lugares, y si tenemos o no soluciones
locales o correcciones temporales para ellos.
+ ~~Boletín de Doom~~ (no terminado) contendrá registros de cambios entre
lanzamientos.
  

# Obtener ayuda
Emacs no es un viaje de solo mil millas. *Te* encontrarás con problemas y errores
misteriosos. Cuando lo hagas, aquí tienes algunos lugares donde puedes buscar
ayuda:

+ [Nuestra documentación][documentation] cubre muchos casos de uso.
+ [La sección de Configuración][configuration] cubre cómo configurar Doom y sus
  paquetes.
+ [La sección de Gestión de paquetes][package-management] cubre cómo instalar y
  desactivar paquetes.
+ [Esta sección][bin/doom] explica los comandos más importantes del script
  `bin/doom`.
+ [Esta sección][common-mistakes] enumera algunos errores de configuración
  comunes que cometen los nuevos usuarios, al migrar una configuración desde
  otra distribución o la suya propia.
+ [Esta respuesta][change-theme] te muestra cómo añadir tus propios temas a tu
  configuración privada.
+ [Esta respuesta][change-font] te muestra cómo cambiar la fuente por defecto.
+ Tu problema puede estar documentado en las [FAQ].
+ Con el sistema de ayuda integrado de Emacs, la documentación está a una tecla
de distancia:
+ Para funciones: <kbd>SPC h f</kbd> o <kbd>C-h f</kbd>
+ Para variables: <kbd>SPC h v</kbd> o <kbd>C-h v</kbd>
+ Para un atajo de teclado: <kbd>SPC h k</kbd> o <kbd>C-h k</kbd>
+ Para buscar atajos de teclado disponibles: <kbd>SPC h b b</kbd> o
  <kbd>C-h b b</kbd>
+ Ejecuta `bin/doom doctor` para detectar problemas comunes con tu entorno de
desarrollo y configuración privada.
+ Consulta las [FAQ] o las [FAQ de Discourse][discourse-faq], en caso de que tu
pregunta ya haya sido respondida.
+ Busca en el [rastreador de problemas de Doom](https://github.com/doomemacs/doomemacs/issues) en caso de que tu problema ya haya sido
reportado.
+ ¡Únete a [nuestro servidor de Discord][discord]; es activo y amigable! Mantén
un ojo en el canal #announcements, donde anuncio actualizaciones y lanzamientos
importantes.


# Contribuir
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](http://makeapullrequest.com) 
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple?style=flat-square)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on liberapay](https://img.shields.io/badge/liberapay-donate-1.svg?style=flat-square&logo=liberapay&color=blue)][liberapay]
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?style=flat-square&logo=paypal&color=blue)][paypal]

Doom es una labor de amor y locura incurable, pero solo soy un tipo. Doom no
estaría donde está hoy sin tu ayuda. ¡Agradezco las contribuciones de cualquier
tipo!

+ ¡Me :heart: las pull requests y los informes de errores (consulta las
[Directrices de contribución][contribute])!
+ No dudes en [decirme que mi Elisp-fu es una
mierda](https://github.com/doomemacs/doomemacs/issues/new/choose), pero por
favor dime por qué.
+ ¡Únete a [nuestro servidor de Discord][Discord] y saluda! Ayuda a otros, pasa
el rato o habla conmigo sobre Emacs, gamedev, programación, física, pixel art,
anime, juegos -- cualquier cosa que te guste. Nutre esta alma solitaria.
+ Si quieres apoyar mi trabajo económicamente, invítame a una copa a través de
[liberapay] o [paypal]. Mi trabajo compite con estudios, aventuras en el
gamedev indie y trabajo freelance. Las donaciones me ayudan a dedicar más
tiempo a mis travesuras de Emacs y OSS.


[contribute]: docs/contributing.org
[discord]: https://doomemacs.org/discord
[discourse]: https://discourse.doomemacs.org
[discourse-faq]: https://discourse.doomemacs.org/tag/faq
[Documentación]: docs/index.org
[faq]: https://github.com/hlissner/doom-emacs/blob/master/docs/faq.org
[getting-started]: docs/getting_started.org
[Install]: docs/getting_started.org#install
[backtrace]: docs/getting_started.org#how-to-extract-a-backtrace-from-an-error
[configuration]: docs/getting_started.org#configuring-doom
[package-management]: docs/getting_started.org#package-management
[bin/doom]: docs/getting_started.org#the-bindoom-utility
[common-mistakes]: docs/getting_started.org#common-mistakes-when-configuring-doom-emacs
[change-theme]: docs/faq.org#how-do-i-change-the-theme
[change-font]: docs/faq.org#how-do-i-change-the-fonts
[modules]: docs/modules.org
[popup-system]: modules/ui/popup/README.org
[Capturas de pantalla]: https://github.com/doomemacs/doomemacs/tree/screenshots#emacsd-screenshots

[bindings]: modules/config/default/+evil-bindings.el
[editorconfig]: http://editorconfig.org/
[evil-mode]: https://github.com/emacs-evil/evil
[fd]: https://github.com/sharkdp/fd
[gnu emacs]: https://www.gnu.org/software/emacs/
[helm]: https://github.com/emacs-helm/helm
[ivy]: https://github.com/abo-abo/swiper
[lsp-mode]: https://github.com/emacs-lsp/lsp-mode
[eglot]: https://github.com/joaotavora/eglot
[nix]: https://nixos.org
[ported-vim-plugins]: modules/editor/evil/README.org#ported-vim-plugins
[ripgrep]: https://github.com/BurntSushi/ripgrep
[straight.el]: https://github.com/radian-software/straight.el
[vim-easymotion]: https://github.com/easymotion/vim-easymotion
[vim-lion]: https://github.com/tommcdo/vim-lion
[vim-sneak]: https://github.com/justinmk/vim-sneak
[vim-unimpaired]: https://github.com/tpope/vim-unimpaired

[liberapay]: https://liberapay.com/hlissner/donate
[paypal]: https://paypal.me/henriklissner/10
