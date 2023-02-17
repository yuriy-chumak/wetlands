#!/usr/bin/ol

; алгоритм поиска пути
; level: vector of vectors, collision сетка игрового уровня. все, что не 0 - проходимо
(define (A* level from to)
   (define (hash xy)
      (+ (<< (car xy) 16) (cdr xy)))

   ; пуста ли клетка карты, работает для любых координат, даже отрицательных
   (define (floor? x y)
      (let ((line (vector-ref level y)))
         (if line
            (not (eq? (vector-ref line x) 0)))))

   (unless (equal? from to) ; если не пришли
      (let step1 ((n 999) ; максимальное количество шагов поиска
                  (c-list-set #empty)
                  (o-list-set (put #empty (hash from)  [from #f  0 0 0])))
         (unless (empty? o-list-set) ; есть еще куда идти?
            ; найдем клетку с минимальной стоимостью:
            (let*((f (ff-fold (lambda (s key value)
                                 (if (< (ref value 5) (car s))
                                    (cons (ref value 5) value)
                                    s))
                        (cons 9999 #f) o-list-set))
                  (xy (ref (cdr f) 1)) ; положение клетки с минимальным весом '(x.y)
                  ; перенесем ее из открытого в закрытый список
                  (o-list-set (del o-list-set (hash xy)))
                  (c-list-set (put c-list-set (hash xy) (cdr f))))

               ;
               (if (or (eq? n 0)
                       (equal? xy to))
                  (let rev ((xy xy))
                     ; обратный проход по найденному пути, вернуть только первый шаг
                     ;  (в сторону предполагаемого маршрута
                     (let*((parent (ref (get c-list-set (hash xy) #f) 2)) ; todo: переделать
                           (parent-of-parent (ref (get c-list-set (hash parent) #f) 2)))
                        (if parent-of-parent (rev parent)
                           (cons ;[
                              (- (car xy) (car parent))
                              (- (cdr xy) (cdr parent))
                              ;c-list-set
                              ;o-list-set
                              ))))

                  ; 5: Проверяем все соседние клетки.
                  ;  Игнорируем те, которые находятся в закрытом списке или непроходимы
                  ;  (поверхность со стенами, водой), остальные добавляем в открытый список,
                  ;  если они там еще не находятся. Делаем выбранную клетку "родительской"
                  ;  для всех этих клеток.
                  (let*((x (car xy))
                        (y (cdr xy))
                        (o-list-set (fold (lambda (n v)
                                       (if (and
                                             (floor? (car v) (cdr v)) ; если туда можно передвинуться...
                                             (eq? #f (get c-list-set (hash v) #f)))
                                          (let ((G (+ (ref (get c-list-set (hash xy) #f) 3) 1)); G родителя + 1
                                                ; H calculated by "Manhattan method"
                                                ; http://www2.in.tu-clausthal.de/~zach/teaching/info_literatur/A_Star/A_star_tutorial/heuristics.htm.html
                                                (H (* (+ (abs (- (car v) (car to)))
                                                         (abs (- (cdr v) (cdr to))))
                                                      2))
                                                ; 6: Если соседняя клетка уже находится в открытом списке
                                                (got (get o-list-set (hash v) #f)))

                                             ; если эта клетка уже в списке
                                             (if got
                                                (if (< G (ref got 3)) ; но наш путь короче
                                                   (put n (hash v)  [v xy  G H (+ G H)])
                                                   ;else ничего не делаем
                                                   n)
                                                ; else
                                                (put n (hash v)  [v xy  G H (+ G H)])))
                                          n))
                                       o-list-set (list
                                                      (cons x (- y 1))
                                                      (cons x (+ y 1))
                                                      (cons (- x 1) y)
                                                      (cons (+ x 1) y)))))
                     (step1 (- n 1) c-list-set o-list-set))))))))

