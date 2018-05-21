# kontrolled Markov Melody Generator
## English
This project explores the possibilities of **automated, user-assisted music composition** by training **n order Markov** chains. While conceived to be connected to a joystick-like device, this software also includes the possibility of simulating user input by interacting with a live chart of the melody generation.

A GUI has been developed to guide the user throughout the whole experiment, from **crawling** the web for MIDI files to model training and final MIDI export. Two live interactive charts have also been implemented to improve the user experience during the melody generation.

This project was started as a Master's Thesis under the direction of Belén Pérez Lancho and María Navarro Cáceres (University of Salamanca). Research has since continued, and a [first paper based on this software](http://www.pdf-archive.com/2016/07/15/kontrolledmarkovmelodygenerator/kontrolledmarkovmelodygenerator.pdf) will be released soon.

## Español
Este proyecto experimental explora las posibilidades de la composición inteligente de melodías controlable mediante dispositivos de tipo *joystick*, aunque también se ha implementado la posibilidad de controlarla directamente desde un gráfico de la aplicación.

La generación de la melodía está basada en un **modelo de _Markov_** entrenado con las melodías extraídas de un conjunto de ficheros **MIDI**. La aplicación incluye todo lo necesario para la obtención y extracción de los datos necesarios para el entrenamiento del modelo, y la melodía se compone en tiempo real pudiendo exportar el resultado como un fichero MIDI.

La investigación ha sido realizada para el TFM del Máster en Sistemas Inteligentes de la Universidad de Salamanca, bajo la dirección de Belén Pérez Lancho y María Navarro Cáceres. Se puede consultar directamente la [memoria de este trabajo](http://www.pdf-archive.com/2016/07/15/kontrolledmarkovmelodygenerator/kontrolledmarkovmelodygenerator.pdf) en la que se detalla toda la metolodología implementada en esta aplicación.
