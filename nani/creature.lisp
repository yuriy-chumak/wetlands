(import (file ini))
(import (scheme misc))
(import (owl parse))
(import (file json))

(define orientations {
   0 0 ; top
   1 1 ; right
   2 2 ; bottom
   3 3 ; left
}) ;left-top



(define-syntax make-setter
   (syntax-rules (name)
      ((make-setter (function this sender . args) . body)
         (list (quote function)
            (lambda (this sender . args)
               ;(print "\x1B;[22;32m" (quote function) ":\x1B;[0m")
               . body)
            (lambda all
               (mail name (cons (quote function) all)))
            ))))
(define-syntax make-getter
   (syntax-rules (name)
      ((make-getter (function this sender . args) . body)
         (list (quote function)
            (lambda (this sender . args)
               ;(print "\x1B;[22;34m" (quote function) ":\x1B;[0m")
               . body)
            (lambda all
               (await (mail name (cons (quote function) all))))
            ))))


; ----------------------------------
; todo: make automatic id generation
; создать новое "создание"
(define (make-creature name initial)
(let*((I (fold (lambda (ff function)
                  (cons
                     (put (car ff) (car function) (cadr function))
                     (put (cdr ff) (car function) (caddr function))))
            (cons initial #empty)
            (list
               ; debug staff
               (make-setter (set this sender key value)
                  (put this key value))
               (make-getter (get this sender key)
                  (mail sender (get this key #f))
                  this)
               (make-getter (debug this sender)
                  (mail sender this)
                  this)


               ; задать новое положение npc
               (make-setter (set-location this sender xy)
                  (put this 'location xy))

               (make-setter (set-orientation this sender orientation)
                  (put this 'orientation orientation))

               (make-getter (get-location this sender)
                  (mail sender (get this 'location '(0 . 0)))
                  this)


               ; конфигурирование анимации
               ;  задать имя тайловой карты и конфигурационный файл анимаций персонажа
               (make-setter (set-animation-profile this sender name ini fg columns)
                  (define json (if (list? ini)
                     ini
                     (try-parse json-parser (file->bytestream ini) #f)))
                  (print "json: " json)
                  (define animations (ff-map (lambda (key value)
                        ; make type a symbol (not a string)
                        (put value 'type (string->symbol (value 'type "default"))))
                     (car json)))

                  (let*((this (put this 'fg fg)) ;(level:get-gid name)))
                        (this (put this 'animations animations))
                        (this (put this 'columns columns))) ;(level:get-columns name))))
                     this))

               (make-getter (set-current-animation this sender animation)
                  ; set current animation (that will be changed, or not to default)
                  ; return animation cycle time (for feature use by caller)
                  (let*((this (put this 'animation animation))
                        (this (put this 'ssms (time-ms))))
                     ; сообщим вызывающему сколько будет длиться полный цикл анимации
                     ; (так как у нас пошаговая игра, то вызывающему надо подождать пока проиграется цикл анимации)
                     (let*((animation-info (getf (get this 'animations #empty) (getf this 'animation)))
                           (duration (getf animation-info 'duration))
                           (frames (get animation-info 'frames 4))
                           (animation-type (get animation-info 'type ""))
                           (back_forth (eq? animation-type 'back_forth))
                           (duration (if back_forth
                                       (floor (* (+ frames frames -1)))
                                       duration)))
                           ; decoded duration in ms
                        (mail sender #|duration|#
                           (if back_forth
                                 (* 100 (+ frames frames -1))
                                 (* 100 frames))
                        ))
                     this))
               ; ...
               (make-getter (get-animation-frame this sender)
                  ; todo: change frames count according to animation type (and fix according math)
                  (let*((animation (get this 'animation 'stance)) ; соответствующая состоянию анимация
                        (ssms (- (time-ms) (get this 'ssms 0))) ; количество ms с момента перехода в анимацию
                        (columns (get this 'columns 32))
                        (delta (getf this 'next-location))
                        (animations (get this 'animations #empty))
                        (animation (get animations animation #empty))
                        (animation-type (get animation 'type #false))
                        (duration (get animation 'duration 250))
                        (frames (get animation 'frames 4))
                        (back_forth (eq? animation-type 'back_forth))
                        (duration (if back_forth
                                    (floor (* (+ frames frames -1)))
                                    duration))
                        (duration
                           (if back_forth
                                 (* 100 (+ frames frames -1))
                                 (* 100 frames)))

                        (position (get animation 'position 0))
                        (orientation (get this 'orientation 0))
                        (frame (floor (/ (* ssms frames) duration)))

                        (delta (if delta (let ((n (if back_forth
                                                      (+ frames frames -1)
                                                      frames)))
                           (cons (* (min frame n) (/ (car delta) n))
                                 (* (min frame n) (/ (cdr delta) n))))))

                        (frame (cond
                           ((eq? animation-type 'play_once)
                              (min (- frames 1) frame))
                           ((eq? animation-type 'looped)
                              (mod frame frames))
                           ((eq? animation-type 'back_forth)
                              (list-ref (append (iota frames) (reverse (iota (- frames 2) 1))) (mod frame (+ frames frames -2)))))))

                     (mail sender (cons (+ (get this 'fg 0) position
                        frame
                        (* columns (get orientations orientation 0)))
                        delta)))
                  this)
            ))))

   (print "starting thread: " name "(" (type name) ")")

   (actor name (lambda ()
   (let this ((itself (car I)))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (let ((handler (getf itself (car msg))))
         ;; (print "handler: " handler ", itself: " itself)
         (unless handler ; just error message
            (print "Unhandled message " msg " from " sender))
         (this (if handler
                  (apply handler (cons itself (cons sender (cdr msg))))
                  else itself)))))))

   (cdr I)))

