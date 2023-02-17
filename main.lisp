#!/usr/bin/ol

; ----------------------------------
; зададим размеры графического окна
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config {
      ; напомню, что мы используем фиксированный шрифт размера 9*16
      'width  (* 2  9 80)      ; 80 знакомест в ширину
      'height (* 2 16 25)      ; 25 знакомест в высоту
      'scale  32               ; шкала увеличения
   })))
(import (lib gl config))

; -=( main )=------------------------------------
; подключаем графические библиотеки, создаем окно
(import (lib gl-2))
(gl:set-window-title "A swamp is a forested wetland.")
(import (otus ffi))
(import (lib soil))

; -=( сразу нарисуем сплеш )=---------------------------
(glOrtho 0 1 1 0 0 1)
(glEnable GL_TEXTURE_2D)

(define id
   (let ((file (file->bytevector "splash.png")))
      (SOIL_load_OGL_texture_from_memory file (size file) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)))
(glBindTexture GL_TEXTURE_2D id)
(glBegin GL_QUADS)
   ; рисуем на весь экран квадратик с текстурой
   (for-each (lambda (xy)
         (glTexCoord2f (car xy) (cdr xy))
         (glVertex2f (car xy) (cdr xy)))
      '((0 . 0) (1 . 0) (1 . 1) (0 . 1)))
(glEnd)
(glDisable GL_TEXTURE_2D)
(gl:redisplay)
(glDeleteTextures 1 (list id)) ; и спокойно удалим сплеш текстуру

; ----------------------------------------------------------
(define-library (enable vsync)
(export)
(import (scheme core)
   (owl async))
(cond-expand
   (Windows
      (import (OpenGL WGL EXT swap_control))
      (begin
         (if WGL_EXT_swap_control
            (wglSwapIntervalEXT 1))))
   (else
      (import (OpenGL GLX EXT swap_control))
      (begin
         (define context (await (mail 'opengl ['get 'context])))

         ; https://gist.github.com/Cloudef/9103499
         (if (and context glXSwapIntervalEXT)
            (glXSwapIntervalEXT (ref context 1) (ref context 3) 1))))))

; ----------------
; музычка...
;,load "music.lisp" ; временно отключена

; остальные библиотеки (в том числе игровые)
(import (lib keyboard))
(import (otus random!))

; -=( level )=-----------------
;     заведует игровой картой
,load "nani/creature.lisp"
,load "nani/level.lisp"

; ============================================================================
; 1. Загрузим первый игровой уровень
(level:load "main.json")

(define CELL-WIDTH (await (mail 'level ['get 'tilewidth])))
(define CELL-HEIGHT (await (mail 'level ['get 'tileheight])))

(define window [0 0 854 480])

(define (move-window dx dy) ; сдвинуть окно
   (set-ref! window 1 (+ (ref window 1) (floor dx)))
   (set-ref! window 2 (- (ref window 2) (floor dy)))
   (set-ref! window 3 (+ (ref window 3) (floor dx)))
   (set-ref! window 4 (- (ref window 4) (floor dy))))


; init
(glShadeModel GL_SMOOTH)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

(define time (box (cdr (syscall 96))))

; -----------------------------------------------------------------------------------------------
; draw
(define (playing-level-screen mouse)
   (define delta ; in usec (microseconds)
      (let*((ss us (uncons (syscall 96) #f)))
         (define delta (mod (+ (- us (unbox time)) 1000000) 1000000)) ; мы не ожидаем задержку больше чем 1 секунда
         (set-car! time us)
         delta))

   ; теперь можем и порисовать: очистим окно и подготовим оконную математику
   (glClearColor 0.0 0.0 0.0 1)
   (glClear GL_COLOR_BUFFER_BIT)
   (glLoadIdentity)

   (glEnable GL_TEXTURE_2D)

   ; рисуем задний фон
   (for-each (lambda (texture scale)
         (define x (/ (ref window 1) (- (ref window 3) (ref window 1)) scale))
         (glBindTexture GL_TEXTURE_2D texture)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
         (glBegin GL_QUADS)
            (glTexCoord2f x 0)
            (glVertex2f -1 1)

            (glTexCoord2f (+ x 1) 0)
            (glVertex2f 1 1)

            (glTexCoord2f (+ x 1) 1)
            (glVertex2f 1 -1)

            (glTexCoord2f x 1)
            (glVertex2f -1 -1)
         (glEnd))
      (await (mail 'level ['get 'backs]))
      (reverse (iota 4 1 1)))

   ; ------------------------------------------
   ; отмасштабируемся, чтобы нарисовать уровень
   (glOrtho (ref window 1) (ref window 3) (ref window 4) (ref window 2) -1 1)

   ; зададим пропорциональное увеличение
   (glScalef (config 'scale 40) (config 'scale 40) 1)

   (glEnable GL_BLEND)

   ; теперь попросим уровень отрисовать себя
   ; (герой входит в общий список npc)
   (define creatures
      (append
      (map (lambda (creature)
            (define npc (cdr creature))
            [ ((npc 'get-location))
              ((npc 'get-animation-frame))])
         ; отсортируем npc снизу вверх
         (sort (lambda (a b)
                  (< (cdr (((cdr a) 'get-location))) ; todo: speedup
                     (cdr (((cdr b) 'get-location)))))
               (ff->alist (await (mail 'level ['get 'npcs])))))

      (map (lambda (creature)
            (define npc (cdr creature))
            [ ((npc 'get-location))
              ((npc 'get-animation-frame))])
         ; отсортируем npc снизу вверх
         (sort (lambda (a b)
                  (< (cdr (((cdr a) 'get-location))) ; todo: speedup
                     (cdr (((cdr b) 'get-location)))))
               (ff->alist (await (mail 'level ['get 'mobs])))))
   ))

   (level:draw-ex (lambda (draw-layer draw-tile layers)

      ; 1. фон (пол, стены, мусор на полу)
      (draw-layer (layers 'background)
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
         creatures)))

   ; герой всегда имеет имя 'hero
   (define hero ((await (mail 'level ['get 'npcs])) 'hero #f))

   ; -------------
   ; обработчик состояния клавиатуры
   ;  внимание, это "состояние", а не "события"!
   ;  посему можно обрабатывать сразу несколько нажатий клавиатуры одновременно
   (if (key-pressed? KEY_ESC) (halt 1))

   ; -------------------------------------
   ;; функции работы с "тут можно ходить"
   ; временная функция работы с level-collision
   (define collision-data (level:get 'collision-data))
   (define W (level:get 'width)) ; ширина уровня
   (define H (level:get 'height)) ; высота уровня

   ; временная функция: возвращает collision data
   ;  по координатам x,y на карте
   (define (at x y)
      (let ((x (+ (floor x) 1))
            (y (+ (floor y) 1)))
         (if (and (< 0 x W) (< 0 y H))
            (ref (ref collision-data y) x))))
   (define wall-id ((level:get 'gids) 'collision))
   (define stair-id (+ wall-id 1))
   (define (wall? x y)
      (eq? (at x y) wall-id))
   (define (stair? x y)
      (eq? (at x y) stair-id))

   ; движение героя:
   ;; (define WALK_SPEED 0.0047) ; максимальная скорость героя
   (define WALK_SPEED 0.0030) ; максимальная скорость героя

   ;; пересчитаем скорость и положение (во время цикла ничто кроме препятствий
   ;; в процесс расчета не вмешивается, так что считаем нашу любимую параболу)
   (let*((speed loc (let repeat ((speed (or ((hero 'get) 'speed) (cons 0 0)))
                                 (loc ((hero 'get-location)))
                                 (delta (/ delta 1000))) ; in ms
      (let*((dt (min 9 delta)) ; шаг расчета движения (не больше 9 ms)
            (stay #false) ; изначально "не стоим"
            ;; (WALK_ACCEL 0.00002) ; per ms
            ;; (SLOW_ACCEL 0.00003) ; ...
            ;; (FALL_ACCEL 0.00004) ; эти цифры позволяют прыгать по ступенькам вверх
            ;; (JUMP_SPEED -0.0131) ; через одну по углам. удачно вышло, короче
            (WALK_ACCEL 0.00001) ; per ms
            (SLOW_ACCEL 0.00002) ; ...
            (FALL_ACCEL 0.00003)
            (JUMP_SPEED -0.0117)

            (vx vy (uncons speed #false))

            ;; движение вверх/вниз
            (ny vy ; посчитаем новую скорость и поправим положение
               (if (or ; мы на лестнице ?
                     (stair? (+ (car loc) 0.1) (- (cdr loc) 0.5)) ; 0.9 - высота персонажа
                     (stair? (+ (car loc) 0.7) (- (cdr loc)   0))
                     (stair? (+ (car loc) 0.1) (- (cdr loc)   0))
                     (stair? (+ (car loc) 0.7) (- (cdr loc) 0.5)))
                  (let*((vy (if (key-pressed? KEY_UP) (- (/ WALK_SPEED 1.5)) 0))
                        (vy (if (key-pressed? KEY_DOWN) (+ (/ WALK_SPEED 1.5)) vy))
                        (ny (+ (cdr loc) (* vy dt))))
                     (values ny vy))
               else ; ускорим падение, это поможет нам стоять на месте, если мы на блоке
                  (let* ((vy (+ vy (* FALL_ACCEL dt))) ; v = v + gt
                         (ny (+ (cdr loc) (* vy dt)))) ; s = s + vt
                  (values ny vy))))

            (ny vy stay ; посчитаем новую скорость и поправим положение
               (cond
                  ((and (< vy 0) ; летим вверх
                        (or ; сверху упираемся в потолок
                           (wall? (+ (car loc) 0.1) (- ny 0.9))
                           (wall? (+ (car loc) 0.7) (- ny 0.9))))
                     (values (cdr loc) 0 stay))
                  ((and (> vy 0) ; падаем
                        (or ; снизу упираемся в пол
                           (wall? (+ (car loc) 0.1) ny)
                           (wall? (+ (car loc) 0.7) ny)))
                     (values (floor ny) 0 #true))
                  (else
                     ;; (print "vy: " vy)
                     (values ny vy #false))))
            ; проверим прыжок, прыгаем только если стоим на поверхности
            (vy (if (and stay (key-pressed? KEY_UP))
               JUMP_SPEED else vy))
            (vy (min vy (* 3 WALK_SPEED)))

            ;; движение влево/вправо
            ; обработаем клавиши
            (vx (+ vx (if (key-pressed? KEY_RIGHT) (* WALK_ACCEL dt) 0)))
            (vx (- vx (if (key-pressed? KEY_LEFT)  (* WALK_ACCEL dt) 0)))

            ; уменьшим горизонтальную скорость (применим трение)
            (vx (if (not (or (key-pressed? KEY_RIGHT) (key-pressed? KEY_LEFT)))
                  (cond
                     ((> vx 0)
                        (max (- vx (* SLOW_ACCEL dt)) 0))
                     ((< vx 0)
                        (min (+ vx (* SLOW_ACCEL dt)) 0))
                     (else vx))
                  else vx))

            ; ограничение на максимальную скорость
            (vx (min vx (+ WALK_SPEED)))
            (vx (max vx (- WALK_SPEED)))

            (nx (+ (car loc) (* vx dt)))
            (nx vx
               (cond
                  ((and (< vx 0)
                        (or
                           (wall? (+ nx 0.1) (- ny 0.1))
                           (wall? (+ nx 0.1) (- ny 0.9)))) ; 0.9 - высота персонажа
                     (values (car loc) 0))
                  ((and (> vx 0)
                        (or
                           (wall? (+ nx 0.7) (- ny 0.1))
                           (wall? (+ nx 0.7) (- ny 0.9)))) ; 0.9 - высота персонажа
                     (values (car loc) 0))
                  (else
                     (values nx vx))))

            (speed (cons vx vy))
            (loc (cons nx ny))
            (delta (- delta dt))) ; уменьшенная дельта расчетов

         (if (= delta 0)
            (values speed loc)
            (repeat speed loc delta))))))

      ((hero 'set-location) loc)
      ((hero 'set) 'speed speed)

      ; и наконец повернем нашего персонажа в нужную сторону
      ((hero 'set-orientation) (cond
         ((> (car speed) 0) 1)
         ((< (car speed) 0) 3)
         ((key-pressed? KEY_UP) 0)
         (else 2)))

      ;; подвинем экран, чтобы все было видно
      ; левая граница
      (let loop ()
         (when (< (* (car loc) (config 'scale))
                  (+ (ref window 1) (/ (- (ref window 3) (ref window 1)) 5)))
            (move-window -1 0)
            (loop)))
      ; правая граница
      (let loop ()
         (when (> (* (car loc) (config 'scale))
                  (- (ref window 3) (/ (- (ref window 3) (ref window 1)) 5)))
            (move-window +1 0)
            (loop)))
      ; верхняя граница
      (let loop ()
         (when (< (* (cdr loc) (config 'scale))
                  (+ (ref window 2) (/ (- (ref window 4) (ref window 2)) 5)))
            (move-window 0 +1)
            (loop)))
      ; нижняя граница
      (let loop ()
         (when (> (* (cdr loc) (config 'scale))
                  (- (ref window 4) (/ (- (ref window 4) (ref window 2)) 5)))
            (move-window 0 -1)
            (loop)))
   )
   #true
)

(define renderer (box playing-level-screen))
(gl:set-renderer (lambda (mouse)
   ((unbox renderer) mouse)))
