(import
   (otus lisp)
   (file xml))
(import (file json))
(import (owl parse))

(import (scheme misc))
(import (file ini))

; public interface:

; загрузить уровень
;  filename: имя xml файла в формате TILED
(define (level:load filename)
   (await (mail 'level ['load filename])))

; получить характеристику уровня
; список:
;  'tileheight: высота тайла в пикселях
;  'tilewidth: ширина тайла в пикселях
(define (level:get property)
   (await (mail 'level ['get property])))
(define (level:set property value)
   (mail 'level ['set property value]))

; получить слой уровня по имени
(define (level:get-layer name)
   (getf (await (mail 'level ['get 'layers])) name))

; получить первый номер тайла, ассоциированный с тайлсетом name
(define (level:get-gid name)
   (getf (await (mail 'level ['get 'gids])) name))

; возвращает количество тайлов в одной строке тайлсета name
(define (level:get-columns name)
   (getf (await (mail 'level ['get 'columns])) name))

; нарисовать уровень
;  вызывать только изнутри цикла рендеринга
;  mouse - тайл под курсором
;  creatures - кого рисуем
(define (level:draw creatures)
   (await (mail 'level ['draw creatures])))
(define (level:draw-ex drawer)
   (await (mail 'level ['draw-ex drawer])))

; -----------------------------------------------
; что касается "занятости" мира расчетами
(setq *calculating* (box #f)) ; внутренняя переменная
(define (set-world-busy busy)
   (set-car! *calculating* busy))
(define (world-busy?)
   (car *calculating*))
; -----------------------------------------------

(actor 'levels (lambda ()
   (let this ((itself #empty))
      (let*((envelope (wait-mail))
            (sender msg envelope))
         (case msg
            ; low level interaction interface
            (['set key value]
               (this (put itself key value)))
            (['get key]
               (mail sender (getf itself key))
               (this itself))
            (['debug]
               (mail sender itself)
               (this itself)))))))

; -----------------------------------------------
; helper functions
(setq split-by-comma (string->regex "c/,/"))
(setq split-by-newline (string->regex "c/\n/"))

; гравная сопрограмму управления игровым уровнем
(actor 'level (lambda ()
   (let this ((itself #empty))
      (let*((envelope (wait-mail))
            (sender msg envelope))
         (case msg
            ; low level interaction interface
            (['set key value]
               (this (put itself key value)))
            (['get key]
               (mail sender (getf itself key))
               (this itself))
            (['debug]
               (mail sender itself)
               (this itself))

            ; загрузить новую карту
            (['load filename]
               (define fn (string->symbol filename))

               (define loadedlevel (await (mail 'levels ['get fn])))
               (define level
                  (if loadedlevel loadedlevel
                     ; если уровень еще не был прочитан - прочитаем:
                  else (begin
                     (for-each display (list "Loading new level '" filename "'... "))

                     (define json (try-parse json-parser (file->bytestream filename) #f))
                     (define level (car json))
                     (print "ok.")

                     ; имя уровня - название файла без расширения
                     (define name (s/([^.]+).*/\\1/ filename))
                     (for-each display (list "processing level named " name "... "))

                     ; compiling tilesets:
                     (define tilewidth (level 'tilewidth)) ; ширина тайла
                     (define tileheight (level 'tileheight)) ; высота тайла
                     (define width (level 'width))  ; количество тайлов по горизонтали
                     (define height (level 'height)); количество тайлов по вертикали

                     (define WIDTH (level 'width))  ; количество тайлов по горизонтали
                     (define HEIGHT (level 'height)); количество тайлов по вертикали

                     ; список тайлсетов
                     (define tilesets (vector->list (level 'tilesets)))

                     ; список начальных номеров тайлов
                     (define gids (alist->ff (map (lambda (tileset)
                           (cons
                              (string->symbol (tileset 'name))
                              (tileset 'firstgid)))
                        tilesets)))

                     ; количество колонок в тайлсете (для персонажей. отдельная строка - отдельная ориентация перса)
                     (define columns (alist->ff (map (lambda (tileset)
                           (cons
                              (string->symbol (tileset 'name))
                              (/ (tileset 'imagewidth)
                                 (tileset 'tilewidth))))
                        tilesets)))

                     ; все тайлы: [gid ширина имя]
                     (define tilenames
                        (map
                           (lambda (tileset)
                              (define name (tileset 'image "checker.png"))
                              (define first-gid (tileset 'firstgid))
                              (define columns (/ (tileset 'imagewidth) (tileset 'tilewidth)))

                              [first-gid columns name])
                           tilesets))
                     (print "ok.")

                     ; make ff (id > tileset element)
                     ; из всех тайлсетов соберем один индексированный список
                     (for-each display (list "preparing tileset ... "))
                     (define tileset
                     (fold (lambda (a b)
                                 (ff-union #f a b))
                        #empty
                        (map
                           (lambda (tileset)
                              (let ((tileoffset (tileset 'tileoffset {})))
                                 (define name (tileset 'image "checker.png"))
                                 (define id ; OpenGL texture atlas id
                                    (SOIL_load_OGL_texture (c-string name) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0))
                                 (define image-width (tileset 'imagewidth))
                                 (define image-height (tileset 'imageheight))

                                 (define first-gid (tileset 'firstgid))
                                 (define tile-width (tileset 'tilewidth))
                                 (define tile-height (tileset 'tileheight))
                                 (define tile-count (tileset 'tilecount))
                                 (define columns (/ image-width tile-width))

                                 (define tile-offsets (cons (tileoffset 'x 0)
                                                            (tileoffset 'y 0)))

                                 (define tiles (fold append #null
                                    (map (lambda (row)
                                          (map (lambda (col)
                                                (let ((ul_x (* col tile-width))
                                                      (ul_y (* row tile-height)))
                                                   [
                                                      id ; texture id
                                                      tile-width
                                                      tile-height
                                                      tile-offsets
                                                      ; texcoords:
                                                      [
                                                         (/ ul_x image-width)
                                                         (/ ul_y image-height)
                                                         (/ (+ ul_x tile-width) image-width)
                                                         (/ (+ ul_y tile-height) image-height)]]))
                                             (iota columns)))
                                       (iota (/ tile-count columns)))))
                                 (pairs->ff (map cons
                                       (iota (length tiles) first-gid)
                                       tiles))))
                           tilesets)))
                     (print "ok.")

                     ; фоновая картинка
                     (define backs (map (lambda (back)
                           (let ((file (file->bytevector (call/cc (lambda (ret)
                                                            (for-each (lambda (layer)
                                                                  (if (and (string-eq? (layer 'type "") "imagelayer")
                                                                           (string-eq? (layer 'name "") back))
                                                                     (ret (layer 'image ""))))
                                                               (vector->list (level 'layers)))
                                                            "checkers")))))
                              (SOIL_load_OGL_texture_from_memory file (size file) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)))
                        '("back-1" "back-2" "back-3" "back-4")))

                     ; слои
                     (display "loading layers...")
                     (define layers (alist->ff (map
                        (lambda (layer)
                              (define name (layer 'name))
                              (define data (layer 'data))
                              (define width (layer 'width))
                              (cons (string->symbol name)
                                    (let loop ((l #null) (data (vector->list data)))
                                       (if (null? data)
                                          (reverse l)
                                          (let* ((head tail (split data width)))
                                             (loop (cons head l) tail))))))
                        (filter (lambda (layer) (string-eq? (layer 'type "") "tilelayer"))
                           (vector->list (level 'layers))))))
                     (print "ok.")

                     ; для ускорения жизни collision-data сделаем вектором
                     (define collision-data (list->vector
                        (map list->vector (layers 'collision))))


                     (for-each display (list "loading npcs ... "))
                     ; npc
                     (define npcs
                        (ff-fold (lambda (& key value)
                              (print "key: " key)
                              (define coroutine (string->symbol (fold string-append
                                 name (list "/" ((if (symbol? key) symbol->string number->string) key)))))
                              (define npc (make-creature coroutine value))

                              ((npc 'set-location) (cons
                                 (value 'x)
                                 (value 'y)))
                              ; тут надо найти какому тайлсету принадлежит этот моб
                              (define gid (value 'gid))
                              (call/cc (lambda (done)
                                 (let loop ((old tilenames) (tiles tilenames))
                                    (if (or
                                          (null? tiles)
                                          (< gid (ref (car tiles) 1)))
                                       (begin
                                          (define r (string->regex "s/^.+\\/(.+)\\..+/\\1/"))
                                          (define name (string->symbol (r (ref old 3))))

                                          ((npc 'set-animation-profile)
                                             name
                                             (fold string-append "" (list "animations/" (symbol->string name) ".json"))
                                             (gids name)
                                             (columns name))
                                          
                                          (define orientation (div (- gid (ref old 1)) (ref old 2)))
                                          ((npc 'set-orientation) orientation)

                                          (done #t))
                                       (loop (car tiles) (cdr tiles))))))
                              (print "set-current-animation...")
                              ((npc 'set-current-animation) 'default)
                              (put & key npc))
                           {}
                           (fold (lambda (ff objectgroup)
                                    (fold (lambda (ff object)
                                             (if (string-eq? (object 'class) "npc") (begin
                                                ; npc id is a name as symbol or id as integer
                                                (define name (object 'name #false))
                                                (define id (if (and (string? name) (not (string-eq? name "")))
                                                               (string->symbol name) ; named npc?
                                                               (object 'id)))

                                                (put ff id {
                                                   'id  id
                                                   'name name
                                                   'gid (object 'gid)
                                                   'x (/ (object 'x) tilewidth)
                                                   'y (/ (object 'y) tileheight) }))
                                             else ff))
                                       ff
                                       (vector->list (objectgroup 'objects []))))
                              {}
                              (filter
                                 (lambda (layer)
                                    (string-eq? (layer 'type) "objectgroup")) ; and (layer 'name) == "objects"
                                 (vector->list (level 'layers))))))
                     (print "ok.")

                     (for-each display (list "loading mobs ... "))
                     (define mobs
                        (ff-fold (lambda (& key value)
                              (define coroutine (string->symbol (fold string-append
                                 name (list "/" ((if (symbol? key) symbol->string number->string) key)))))
                              (define npc (make-creature coroutine value))

                              ((npc 'set-location) (cons
                                 (value 'x)
                                 (value 'y)))
                              ; тут надо найти какому тайлсету принадлежит этот моб
                              (define gid (value 'gid))
                              (call/cc (lambda (done)
                                 (let loop ((old tilenames) (tiles tilenames))
                                    (if (or
                                          (null? tiles)
                                          (< gid (ref (car tiles) 1)))
                                       (begin
                                          (define name (string->symbol (s/^.+\/(.+)\..+/\1/ (ref old 3))))
                                          (print old " > " gid)

                                          ((npc 'set-animation-profile)
                                             name (list {
                                                'default {
                                                   'position (modulo (- gid (ref old 1)) (ref old 2))
                                                   'frames 3
                                                   'duration 100
                                                   'type "back_forth"
                                                }
                                             })
                                             (gids name)
                                             (columns name))
                                          
                                          (define orientation (div (- gid (ref old 1)) (ref old 2)))
                                          ((npc 'set-orientation) orientation)

                                          (done #t))
                                       (loop (car tiles) (cdr tiles))))))
                              (print "set-current-animation...")
                              ((npc 'set-current-animation) 'default)
                              (put & key npc))
                           {}
                           (fold (lambda (ff objectgroup)
                                    (fold (lambda (ff object)
                                             (if (string-eq? (object 'class) "mob") (begin
                                                ; npc id is a name as symbol or id as integer
                                                (define name (object 'name #false))
                                                (define id (if (and (string? name) (not (string-eq? name "")))
                                                               (string->symbol name) ; named npc?
                                                               (object 'id)))

                                                (put ff id {
                                                   'id  id
                                                   'name name
                                                   'gid (object 'gid)
                                                   'x (/ (object 'x) tilewidth)
                                                   'y (/ (object 'y) tileheight) }))
                                             else ff))
                                       ff
                                       (vector->list (objectgroup 'objects []))))
                              {}
                              (filter
                                 (lambda (layer)
                                    (string-eq? (layer 'type) "objectgroup")) ; and (layer 'name) == "objects"
                                 (vector->list (level 'layers))))))
                     (print "ok.")

                     ; порталы
                     (define portals
                        (fold (lambda (ff objectgroup)
                                 (fold (lambda (ff object)
                                          (if (string-eq? (object 'class) "portal") (begin
                                             (define name (object 'name ""))
                                             (define target ((string->regex "c/\\//") name))
                                             (define id (object 'id))

                                             (put ff id {
                                                'id id
                                                'target (cons
                                                   (string->symbol (lref target 0))
                                                   (string->symbol (lref target 1)))
                                                'x (/ (object 'x) tilewidth)
                                                'y (/ (object 'y) tileheight)
                                                'width  (/ (object 'width) tilewidth)
                                                'height (/ (object 'height) tileheight) }))
                                          else ff))
                                    ff
                                    (vector->list (objectgroup 'objects []))))
                           {}
                           (filter
                              (lambda (layer)
                                 (string-eq? (layer 'type "") "objectgroup"))
                              (vector->list (level 'layers)))))

                     ; точки куда ведут порталы
                     (define spawns
                        (fold (lambda (ff objectgroup)
                                 (fold (lambda (ff object)
                                          (if (string-eq? (object 'class "") "spawn") (begin
                                             (define name (object 'name ""))

                                             (define id (if (and (string? name) (not (string-eq? name "")))
                                                            (string->symbol name) ; named npc?
                                                            (object 'id)))
                                             (print "spawn id: " id)
                                             
                                             (put ff id {
                                                'id id
                                                'x (/ (object 'x) tilewidth)
                                                'y (/ (object 'y) tileheight) }))
                                          else ff))
                                    ff
                                    (vector->list (objectgroup 'objects []))))
                           {}
                           (filter
                              (lambda (layer)
                                 (string-eq? (layer 'type "") "objectgroup"))
                              (vector->list (level 'layers)))))

                     (print "spawns: " spawns)

                     ; парсинг и предвычисления закончены, создадим уровень
                     (define newlevel
                        (ff-union #f itself {
                           'width width  'height height
                           'tilewidth  tilewidth
                           'tileheight tileheight
                           'gids gids
                           'columns columns
                           'tileset tileset

                           'backs backs

                           'npcs npcs
                           'mobs mobs
                           'portals portals
                           'spawns spawns

                           'tilenames tilenames
                           'collision-data collision-data
                           'layers layers}
                           ))

                     ;; (define newlevel
                     ;;    (fold (lambda (ff kv) (put ff (car kv) (cdr kv))) itself `(
                     ;;       (width . ,width) (height . ,height)
                     ;;       (tilewidth . ,tilewidth)
                     ;;       (tileheight . ,tileheight)
                     ;;       (gids . ,gids)
                     ;;       (columns . ,columns)
                     ;;       (tileset . ,tileset)

                     ;;       (back-1 . ,back-1)

                     ;;       (npcs . ,npcs)
                     ;;       (portals . ,portals)
                     ;;       (spawns . ,spawns)

                     ;;       (tilenames . ,tilenames)
                     ;;       (collision-data . ,collision-data)
                     ;;       (layers . ,layers))))

                     (mail 'levels ['set fn newlevel])
                     newlevel)))
               (mail sender 'ok)
               (this level))
            ; draw the level on the screen
            (['draw creatures]; interact
               (let ((w (getf itself 'tilewidth))
                     (h (getf itself 'tileheight))
                     (width (getf itself 'width))
                     (height (getf itself 'height))
                     (tileset (getf itself 'tileset)))

                  (define (X x y tw th)
                     x)
                     ;; (- (* x (/ w 2))
                     ;;    (* y (/ w 2))
                     ;;    (/ w 2)))

                  (define (Y x y tw th)
                     y)
                     ;; (+ (* x (/ h 2))
                     ;;    (* y (/ h 2))
                     ;;    (- h th)))

                  ; эта функция рисует тайл на экране
                  ; примитивно, не оптимизировано - но ранняя оптимизация нам и не нужна
                  (define (draw-tile id i j) ; i means x, j means y
                     (let ((tile (getf tileset id)))
                        (if tile
                           (let*((x (X i j (ref tile 2) (ref tile 3)))
                                 (y (Y i j (ref tile 2) (ref tile 3)))
                                 (x (+ x (car (ref tile 4))))
                                 (y (+ y (cdr (ref tile 4))))
                                 (st (ref tile 5)))
                              (glBindTexture GL_TEXTURE_2D (ref tile 1))
                              (glBegin GL_QUADS)
                                 (glTexCoord2f (ref st 1) (ref st 2))
                                 (glVertex2f x y)

                                 (glTexCoord2f (ref st 3) (ref st 2))
                                 (glVertex2f (+ x 1#|(ref tile 2)|#) y)

                                 (glTexCoord2f (ref st 3) (ref st 4))
                                 (glVertex2f (+ x 1#|(ref tile 2)|#) (+ y 1#|(ref tile 3)|#))

                                 (glTexCoord2f (ref st 1) (ref st 4))
                                 (glVertex2f x (+ y 1#|(ref tile 3)|#))
                              (glEnd)))))


                  (glColor3f 1 1 1)
                  (glEnable GL_TEXTURE_2D)
                  (define (draw-layer data entities)
                     (map (lambda (line j)
                              ; отрисуем движимое
                              (for-each (lambda (entity)
                                          (let ((x (car (ref entity 1)))
                                                (y (cdr (ref entity 1))))
                                          (if (<= j y (+ j 1))
                                             (let ((tile (ref entity 2)))
                                                (if (pair? tile)
                                                   (draw-tile
                                                      (car tile)
                                                      x (- y 1)))))))
                                 entities)
                              ; и недвижимое имущество
                              (map (lambda (tid i)
                                    ; если есть что рисовать - рисуем
                                    (unless (eq? tid 0)
                                       (draw-tile tid i j)))
                                 line
                                 (iota (length line))))
                        data
                        (iota (length data))))

                  (define (draw-layers data1 data2 entities)
                     (map (lambda (line1 line2 j)
                              ; отрисуем движимое..
                              (for-each (lambda (entity)
                                          (let ((x (car (ref entity 1)))
                                                (y (cdr (ref entity 1))))
                                          (if (<= j y (+ j 1))
                                             (let ((tile (ref entity 2)))
                                                (if (pair? tile)
                                                   (draw-tile
                                                      (car tile)
                                                      x (- y 1)))))))
                                 entities)
                              ; .. и недвижимое имущество
                              (map (lambda (tid i)
                                    ; если есть что рисовать - рисуем
                                    (unless (eq? tid 0)
                                       (draw-tile tid i j)))
                                 line1
                                 (iota (length line1)))
                              (map (lambda (tid i)
                                    ; если есть что рисовать - рисуем
                                    (unless (eq? tid 0)
                                       (draw-tile tid i j)))
                                 line2
                                 (iota (length line2))))
                        data1
                        data2
                        (iota (length data1))))

                  ; 1. фон (пол, стены, мусор на полу)
                  (draw-layer ((itself 'layers) 'background)
                     #null)

                  ; x. персонажи
                  (for-each (lambda (entity)
                              (let ((x (car (ref entity 1)))
                                    (y (cdr (ref entity 1))))
                              (let ((tile (ref entity 2)))
                                 (if (pair? tile)
                                    (draw-tile
                                       (car tile)
                                       x (- y 1))))))
                     creatures)


                  ; 2. теперь очередь движимых и недвижимых объектов
                  ;   так как движимые объекты должны уметь прятаться за недвижимые, то
                  ;   рисовать мы их будем все вместе - слой "object" и наших creatures
                  ;; (define object-data ((itself 'layers) 'interior))

                  ;; (draw-layers
                  ;;    ((itself 'layers) 'interior)
                  ;;    ((itself 'layers) 'plates)
                  ;;    creatures)

                  (mail sender 'ok)
                  (this itself)))

            (['draw-ex drawer]; interact
               (let ((w (getf itself 'tilewidth))
                     (h (getf itself 'tileheight))
                     (width (getf itself 'width))
                     (height (getf itself 'height))
                     (tileset (getf itself 'tileset)))

                  ; эта функция рисует тайл на экране
                  ; примитивно, не оптимизировано - но ранняя оптимизация нам и не нужна
                  (define (draw-tile id i j) ; i means x, j means y
                     (let ((tile (getf tileset id)))
                        (if tile
                           (let*((x i) ;(+ i (car (ref tile 4)))) ; смещение тайла
                                 (y j) ;(+ j (cdr (ref tile 4)))) ; смещение тайла
                                 (w (/ (ref tile 2) 32))
                                 (h (/ (ref tile 3) 32))
                                 (st (ref tile 5))) ; texture coordinates
                              (glBindTexture GL_TEXTURE_2D (ref tile 1))
                              (glBegin GL_QUADS)
                                 (glTexCoord2f (ref st 1) (ref st 2))
                                 (glVertex2f x (- y h -1))

                                 (glTexCoord2f (ref st 3) (ref st 2))
                                 (glVertex2f (+ x w) (- y h -1))

                                 (glTexCoord2f (ref st 3) (ref st 4))
                                 (glVertex2f (+ x w) (+ y 1))

                                 (glTexCoord2f (ref st 1) (ref st 4))
                                 (glVertex2f x (+ y 1))
                              (glEnd)))))


                  (glColor3f 1 1 1)
                  (glEnable GL_TEXTURE_2D)
                  (define (draw-layer data entities)
                     (map (lambda (line j)
                              ; отрисуем движимое
                              (for-each (lambda (entity)
                                          (let ((x (car (ref entity 1)))
                                                (y (cdr (ref entity 1))))
                                          (if (<= j y (+ j 1))
                                             (let ((tile (ref entity 2)))
                                                (if (pair? tile)
                                                   (draw-tile
                                                      (car tile)
                                                      x (- y 1)))))))
                                 entities)
                              ; и недвижимое имущество
                              (map (lambda (tid i)
                                    ; если есть что рисовать - рисуем
                                    (unless (eq? tid 0)
                                       (draw-tile tid i j)))
                                 line
                                 (iota (length line))))
                        data
                        (iota (length data))))

                  (define (draw-layers data1 data2 entities)
                     (map (lambda (line1 line2 j)
                              ; отрисуем движимое..
                              (for-each (lambda (entity)
                                          (let ((x (car (ref entity 1)))
                                                (y (cdr (ref entity 1))))
                                          (if (<= j y (+ j 1))
                                             (let ((tile (ref entity 2)))
                                                (if (pair? tile)
                                                   (draw-tile
                                                      (car tile)
                                                      x (- y 1)))))))
                                 entities)
                              ; .. и недвижимое имущество
                              (map (lambda (tid i)
                                    ; если есть что рисовать - рисуем
                                    (unless (eq? tid 0)
                                       (draw-tile tid i j)))
                                 line1
                                 (iota (length line1)))
                              (map (lambda (tid i)
                                    ; если есть что рисовать - рисуем
                                    (unless (eq? tid 0)
                                       (draw-tile tid i j)))
                                 line2
                                 (iota (length line2))))
                        data1
                        data2
                        (iota (length data1))))

                  (drawer draw-layer draw-tile (itself 'layers))

                  (mail sender 'ok)
                  (this itself)))

            ;
            (else
               (print-to stderr "Unknown world command: " msg)
               (this itself)))))))

