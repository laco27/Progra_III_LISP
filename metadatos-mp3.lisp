; ¬_¬ ¬_¬ ¬_¬ ¬_¬ ~.~ ~.~ ~.~ ~.~ ~.~ ¬_¬ ¬_¬ ¬_¬ ¬_¬ 
;;Instituto Tecnologico de Costa Rica		  ~.~
;;Lenguajes de programacion			  ~.~
;;						  ~.~
;;Tarea programada 3				  ~.~
;;						  ~.~
;;Estudiantes:					  ~.~
;;Kevin Picado					  ~.~
;;Allan Rodriguez				  ~.~
;;Edgar Solorzano				  ~.~
; ¬_¬ ¬_¬ ¬_¬ ¬_¬ ~.~ ~.~ ~.~ ~.~ ~.~ ¬_¬ ¬_¬ ¬_¬ ¬_¬ 

(load "ltk") ;Se carga la librería ltk para hacer la interfaz

(setf Directorio nil) ;Variable que contiene la direccion de la carpeta, para la busqueda de mp3

;Variable para desplegar texto
(defvar *texto-desplegado*)

(defparameter *lista-metadatos* nil 
"Creamos una lista global en la que se almacenará toda la información de los MP3"
)

; ¬_¬ ¬_¬ ¬_¬ ¬_¬ ~.~ ~.~ ~.~ ~.~ ~.~ ¬_¬ ¬_¬ ¬_¬ ¬_¬ ~.~ ~.~ ~.~ ~.~ ~.~ ¬_¬ ¬_¬ ¬_¬ ¬_¬
; Las siguientes dos funciones se usan para crear un respaldo consistente de archivos ya cargados
; de tal manera si el usuario lo desea no deberá cargar de nuevo un directorio

(defun crear-BD ()
	(with-open-file (stream "BD.txt" :direction :output :if-exists :append)
		(dotimes (i (length *lista-metadatos*)) 
			(format stream "Artista: ")		
			(format stream (elt (elt *lista-metadatos* i) 0))
			(format stream "~%")
			(format stream "Titulo: ")		
			(format stream(elt (elt *lista-metadatos* i) 1))
			(format stream "~%")
			(format stream "Genero: ")		
			(format stream (elt (elt *lista-metadatos* i) 2))
			(format stream "~%")
			(format stream "Album: ")		
			(format stream (elt (elt *lista-metadatos* i) 3))
			(format stream "~%")		
			(format stream "siguiente~%")
		)
	)
)

(defun leer-BD()
	(ltk::clear-text *campo-texto*)
	(defvar lista-info)
	(defvar artista) ; variable que contiene el artista del archivo mp3
	(defvar titulo) ; variable que contiene el titulo del archivo mp3
	(defvar genero) ; variable que contiene el genero del archivo mp3
	(defvar album) ; variable que contiene el album del archivo mp3
	(WITH-OPEN-FILE (stream "BD.txt" :direction :input)
		(do ((line (read-line stream nil) (read-line stream nil)))
		    ((null line))		
			(dotimes (i 1)		
				(if (String-Equal (subseq line 0) "siguiente")
					(progn
						(setf lista-info (list artista titulo genero album)) 
						(setf *lista-metadatos* (agrega lista-info *lista-metadatos*))
					)
				)				
				(if (String-Equal (subseq line 0 7) "Artista")
					(setf artista (subseq line 9))
				)
				(if (String-Equal (subseq line 0 6) "Titulo")				
					(setf titulo (subseq line 8))
				)
				(if (String-Equal (subseq line 0 6) "Genero")					
					(setf genero (subseq line 8))
				)
				(if (String-Equal (subseq line 0 5) "Album")				
					(setf album (subseq line 7))
				)
			)
		)
	)
	(setf *texto-desplegado* "Información de los MP3 en la base de datos cargados con éxito")
	(ltk::append-text *campo-texto* *texto-desplegado*)
)

; ¬_¬ ¬_¬ ¬_¬ ¬_¬ ~.~ ~.~ ~.~ ~.~ ~.~ ¬_¬ ¬_¬ ¬_¬ ¬_¬ ~.~ ~.~ ~.~ ~.~ ~.~ ¬_¬ ¬_¬ ¬_¬ ¬_¬
; Funciones que se encargan de cargar y extraer los metadatos de los archivos mp3 encontradas
; Adémas se guarda en una lista listas con la informacion requerida (Artista Titulo Genero Album)
(defun formar-directorio (direccion)
	"completa la direcion que ingresa el usuario"
	(setf Directorio 
		(directory (concatenate 'string "~/" direccion "/*.mp3"))
	)
)

(defun agrega (lista1 lista2)
  "Concatena dos listas"
	(cons lista1 lista2)		
)

(defun extraer-metadatos-aux (direccion-mp3 nombre-mp3)	 
	"Funcion auxiliar para extraer los metadatos, luego los guarda en una lista"
	(defvar archivo)
	(defvar artista) ; variable que contiene el artista del archivo mp3
	(defvar titulo) ; variable que contiene el titulo del archivo mp3
	(defvar genero) ; variable que contiene el genero del archivo mp3
	(defvar album) ; variable que contiene el album del archivo mp3
	(setf archivo (concatenate 'string nombre-mp3 ".txt")) 
	(run-shell-command (concatenate 'string "extract \"" direccion-mp3 "\"" ) 
	:output archivo) ; comando que extrae la informacion del archivo mp3 y crea temporalmente un txt con la informacion
	(with-open-file (stream archivo)
		(do ((line (read-line stream nil) (read-line stream nil)))
		    ((null line))
			;Codigo de deteccion de tags
			(when (String-Equal (subseq line 0 6) "artist")
				(setf artista (subseq line 9))
			)
			(when (String-Equal (subseq line 0 5) "title")
				(setf titulo (subseq line 8))
			)
			(when (String-Equal (subseq line 0 5) "genre")
				(setf genero (subseq line 8))
			)
			(when (String-Equal (subseq line 0 5) "album")
				(setf album (subseq line 8))
			)
			
	)
	)
	(defvar lista-info)
	(setf lista-info (list artista titulo genero album)) ; creamos una lista con la informacion de un archivo
	; agregamos los datos del mp3 a la variable global *lista-metadatos*
	(setf *lista-metadatos* (agrega lista-info *lista-metadatos*)) ; agregamos la lista recien creada a la *lista-metadatos*
	;Ejecuta el comando que eliminara el archivo donde se encuentra la información de cada mp3
	(run-shell-command (concatenate 'string "rm \"" archivo "\"") )	; comando para eliminar el archivo temporal que se crea
)

(defun extraer-metadatos ()
	"Funcion para la extracion de metadatos"
	(setf *lista-metadatos* nil)
	(if (not (NULL Directorio)) 
		(dotimes (i (list-length Directorio)) 
			(extraer-metadatos-aux (namestring (ELT Directorio i))
			(file-namestring (ELT Directorio i))) ; llamada a la funcion auxiliar
			(setf *texto-desplegado* "Información de los MP3 encontrados cargados con éxito")
		)
		(setf *texto-desplegado* "No se encontraron archivos en el directorio especificado")
	)
)

;Funcion del boton cargar
(defun cargar-informacion ()
	"Funcion que se encarga de cargar los archivos mp3 y extraer para cada uno la informacion mediante la funcion extraer-metadatos"
	(crear-BD)	
	(ltk::clear-text *campo-texto*)
	(formar-directorio (ltk::text *entry-cargar*))
	(extraer-metadatos)
	(ltk::append-text *campo-texto* *texto-desplegado*)
	(if (not (NULL Directorio))
		(ltk::configure *entry-cargar* :Text " ")
	)
	(crear-BD)
)

; ¬_¬ ¬_¬ ¬_¬ ¬_¬ ~.~ ~.~ ~.~ ~.~ ~.~ ¬_¬ ¬_¬ ¬_¬ ¬_¬ ~.~ ~.~ ~.~ ~.~ ~.~ ¬_¬ ¬_¬ ¬_¬ ¬_¬
; Funciones encargadas de las consultas y mostrar al usuario la informacion

;Definicion de variables para crear listas con filtros
(defvar *artista-lista* nil "lista que obtendra la informacion de un artista")
(defvar *titulo-lista* nil "lista que obtendra la informacion de un titulo")
(defvar *genero-lista* nil "lista que obtendra la informacion de un genero")
(defvar *album-lista* nil "lista que obtendra la informacion de un album")


(defun filtrar-artista (artista)
	"Mostrar los datos para un autor específico"
	; eliminamos informacion existente en la lista
	(setf *artista-lista* nil)
	(dotimes (i (length *lista-metadatos*))
		(when (String-Equal artista (elt (elt *lista-metadatos* i) 0))
			(setf *artista-lista* (agrega (elt *lista-metadatos* i) *artista-lista*))
		)
	)
	(mostrar-informacion *artista-lista*)
)


(defun filtrar-titulo (titulo)	
	"Mostrar los datos para un titulo específico"
	; eliminamos informacion existente en la lista
	(setf *titulo-lista* nil)
	(dotimes (i (length *lista-metadatos*))
		(when (String-Equal titulo (elt (elt *lista-metadatos* i) 1)) 
			(setf *titulo-lista* (agrega (elt *lista-metadatos* i) *titulo-lista*))
		)
	)
	(mostrar-informacion *titulo-lista*)
)


(defun filtrar-genero (genero)
	"Mostrar los datos para un genero específico"
	; eliminamos informacion existente en la lista
	(setf *genero-lista* nil)
	(dotimes (i (length *lista-metadatos*))
		(when (String-Equal genero (elt (elt *lista-metadatos* i) 2))
			(setf *genero-lista* (agrega (elt *lista-metadatos* i) *genero-lista*))
		)
	)
	(mostrar-informacion *genero-lista*)
)

(defun filtrar-album (album)
	"Mostrar los datos para un album específico"
	; eliminamos informacion existente en la lista
	(setf *album-lista* nil)
	(dotimes (i (length *lista-metadatos*))
		(when (String-Equal album (elt (elt *lista-metadatos* i) 3))
			(setf *album-lista* (agrega (elt *lista-metadatos* i) *album-lista*))
		)
	)
	(mostrar-informacion *album-lista*)
)

(defun mostrar-informacion(*lista-mostrar*)
	"Funcion que se encarga de mostrarle la informacion (con filtros si es el caso) al usuario"
	(ltk::clear-text *campo-texto*)
	(when (= 0 (length *lista-mostrar*)) (ltk::append-text *campo-texto* 
		"No se encontraron archivos que cumplan con la busqueda!")
	) 
	(dotimes (i (length *lista-mostrar*)) 
		(ltk::append-text *campo-texto* "Artista: ")		
		(ltk::append-text *campo-texto* (elt (elt *lista-mostrar* i) 0))
		(ltk::append-text *campo-texto* " ||Titulo: ")		
		(ltk::append-text *campo-texto* (elt (elt *lista-mostrar* i) 1))
		(ltk::append-text *campo-texto* " ||Genero: ")		
		(ltk::append-text *campo-texto* (elt (elt *lista-mostrar* i) 2))
		(ltk::append-text *campo-texto* " ||Album: ")		
		(ltk::append-text *campo-texto* (elt (elt *lista-mostrar* i) 3))
		(ltk::append-text *campo-texto* (format nil " 
~A" "")
		)	
	)		
)

; ¬_¬ ¬_¬ ¬_¬ ¬_¬ ~.~ ~.~ ~.~ ~.~ ~.~ ¬_¬ ¬_¬ ¬_¬ ¬_¬ ~.~ ~.~ ~.~ ~.~ ~.~ ¬_¬ ¬_¬ ¬_¬ ¬_¬
; Interfaz Grafica

;Se crea la ventana principal del programa
(defun ventana-principal()
	(ltk::with-ltk ()
	(ltk::wm-title ltk::*tk*"Metadatos de Archivos MP3")
	
	;Frame, texto y barras de desplazamiento para el campo donde se despliegan los resultados
	(defvar *cuadro2* (make-instance 'ltk::frame))
	(defvar *campo-texto* (make-instance 'ltk::text :master *cuadro2* 
				:width 60 :height 30
				:wrap "none" ))
	(defvar *scrolly* (make-instance 'ltk::scrollbar :master *cuadro2*))
	(ltk::configure *campo-texto* 
		:yscrollcommand 
		(concatenate 'string (ltk::widget-path *scrolly*) " set"))
	(ltk::configure *scrolly* 
		:command 
		(concatenate 'string (ltk::widget-path *campo-texto*) " yview"))
	(defvar *scrollx* (make-instance 'ltk::scrollbar :master *cuadro2*
				 :orientation "horizontal"))
	(ltk::configure *campo-texto* 
		:xscrollcommand 
		(concatenate 'string (ltk::widget-path *scrollx*) " set"))
	(ltk::configure *scrollx* 
		:command 
		(concatenate 'string (ltk::widget-path *campo-texto*) " xview"))
	(ltk::grid *campo-texto* 0 0 :sticky "snew")
	(ltk::grid *scrolly* 0 1 :sticky "ns")
	(ltk::grid *scrollx* 1 0 :sticky "ew")
	
        ;Frame, botones y entradas para las diferentes busquedas
        
	(defvar *cuadro1* (make-instance 'ltk::frame))
	(ltk::grid *cuadro1* 0 0)
	(ltk::grid *cuadro2* 0 1)
	(defvar *boton-cargar* (make-instance 'ltk::button 
		:Text "Cargar Archivos"
		:master *cuadro1*
		:command (lambda () (cargar-informacion))))
	(defvar *label-ejemplo* (make-instance 'ltk::label 
		:Text "  Ingrese el directorio:
  EJ: Musica/Variada"
		:master *cuadro1*))
	(defvar *entry-cargar* (make-instance 'ltk::entry 
		:master *cuadro1*
		:width 25))
	(ltk::grid *label-ejemplo* 0 0 :Sticky "n")
	(ltk::grid *entry-cargar* 1 0 :Sticky "n")
	(ltk::grid *boton-cargar* 2 0 :Sticky "n")
	
	(defvar *vacio4* (make-instance 'ltk::label 
		:Text " "
		:master *cuadro1*))
	(ltk::grid *vacio4* 3 0 :columnspan 2 :Sticky "w")

	(defvar *boton-mostrar-todo* (make-instance 'ltk::button 
		:Text "Mostrar todos los metadatos"
		:master *cuadro1*
		:command (lambda () (mostrar-informacion *lista-metadatos*))))
	(ltk::grid *boton-mostrar-todo* 4 0  :Sticky "ns" )
	
	(defvar *vacio0* (make-instance 'ltk::label 
		:Text " "
		:master *cuadro1*))
	(ltk::grid *vacio0* 5 0 :columnspan 2 :Sticky "w")
	
	;Opciones para realizar la busqueda
	(defvar *label-ingrese* (make-instance 'ltk::label 
		:Text "Ingrese los parametros de la busqueda:"
		:master *cuadro1*))
	(ltk::grid *label-ingrese* 6 0 :columnspan 2 :Sticky "w")
	
; A continuacion se encuentran los botones para realizar las busquedas
; Se utiliza solo un Entry para ingresar los datos, entry-busqueda.

    (defvar *entry-busqueda* (make-instance 'ltk::entry 
		:master *cuadro1*
		:width 20))    
    (ltk::grid *entry-busqueda* 7 0 :Sticky "n")
    
    (defvar *vacio1* (make-instance 'ltk::label 
		:Text " "
		:master *cuadro1*))
	(ltk::grid *vacio1* 8 0 :columnspan 2 :Sticky "w")
	
	(defvar *label-busqueda* (make-instance 'ltk::label 
		:Text "Realizar busqueda por:"
		:master *cuadro1*))
	(ltk::grid *label-busqueda* 9 0 :columnspan 2 :Sticky "w")
	
	(defvar *vacio2* (make-instance 'ltk::label 
		:Text " "
		:master *cuadro1*))
	(ltk::grid *vacio2* 10 0 :columnspan 2 :Sticky "w")

;Botones de opciones

	(defvar *boton-titulo* (make-instance 'ltk::button 
		:Text "Titulo"
		:master *cuadro1*
		:command (lambda () 
		(filtrar-titulo (ltk::text *entry-busqueda*))))) ;Se llama a la funcion buscar titulo
	(ltk::grid *boton-titulo* 11 0 :Sticky "n")
	

	(defvar *boton-artista* (make-instance 'ltk::button 
		:Text "Artista"
		:master *cuadro1*
		:command (lambda () 
		(filtrar-artista (ltk::text *entry-busqueda*)))))  ;Se llama a la funcion buscar artista
	(ltk::grid *boton-artista* 12 0 :Sticky "n")
	

	(defvar *boton-genero* (make-instance 'ltk::button 
		:Text "Genero"
		:master *cuadro1*
		:command (lambda () 
		(filtrar-genero (ltk::text *entry-busqueda*))))) ;Se llama a la funcion buscar genero
	(ltk::grid *boton-genero* 13 0 :Sticky "n")
	

	(defvar *boton-album* (make-instance 'ltk::button 
		:Text "Album"
		:master *cuadro1*
		:command (lambda () 
		(filtrar-album (ltk::text *entry-busqueda*)))))  ;Se llama a la funcion buscar album
		(ltk::grid *boton-album* 14 0 :Sticky "n")

	
	(defvar *boton-cargar-BD* (make-instance 'ltk::button 
		:Text "Cargar BD"
		:master *cuadro1*
		:command (lambda () 
		(leer-BD))))  ;Se llama a la funcion que lee el archivo de la BD...
	(ltk::grid *boton-cargar-BD* 16 0 :Sticky "n")

	(defvar *vacio3* (make-instance 'ltk::label 
		:Text " "
		:master *cuadro1*))
	(ltk::grid *vacio3* 15 0 :columnspan 2 :Sticky "w")
	)	
	
)
(ventana-principal) ; llamada a la ventana de inicio :)




