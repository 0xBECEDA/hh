(eval-when (:compile-toplevel :load-toplevel :execute)
  #-clx
  (ql:quickload 'clx)
  #-zpng
  (ql:quickload 'zpng)
  #-cffi
  (ql:quickload 'cffi)
  (ql:quickload "png-read")
  (ql:quickload :bt-semaphore)
  (ql:quickload :thread-pool))

(defpackage #:cl-autogui
  (:use #:common-lisp #:xlib)
  (:export #:x-position
           #:x-size
           #:x-position
           #:x-move
           #:x-mouse-down
           #:x-mouse-up
           #:x-click
           #:x-dbclick
           #:x-vscroll
           #:x-hscroll
           #:x-scroll
           #:x-key-down
           #:x-key-up
           #:x-press
           #:x-snapshot
           #:x-snapsearch
           #:x-get-color
           #:x-find-color))

(in-package  #:cl-autogui)

(defparameter *langs* "rus+eng")
(defparameter *default-width* 1295)
(defparameter *default-height* 668)
(defparameter *teaser-width* 690)
(defparameter *snap-width* 755)
(defparameter *snap-height* 668)
(defparameter *snap-x* 440)
(defparameter *default-x* 60)
(defparameter *default-y* 37)
(defparameter *mouse-left* 1)
(defparameter *mouse-middle* 2)
(defparameter *hh-teaser-url*
  "https://hh.ru/search/vacancy?L_is_autosearch~false&area=2&clusters=true&enable_snippets=true&items_on_page=100&only_with_salary=true&salary=165000&specialization=1.221&page~~A"
  "https://spb.hh.ru/search/vacancy?L_is_autosearch~false&area=1&clusters=true&enable_snippets=true&items_on_page=100&only_with_salary=true&salary=165000&specialization=1.221&page~~A")

(defparameter *browser-path*  "/usr/bin/firefox")

(in-package  #:cl-autogui)

(defun open-browser (browser-path url)
  (let ((proc (sb-ext:run-program
               `,browser-path
               `(,url)
               :input :stream :output :stream)))
    (if proc
        (with-open-stream (input (sb-ext:process-input proc))
          (with-open-stream (output (sb-ext:process-output proc))
            (do ((a-line (read-line output nil 'eof)
                         (read-line output nil 'eof)))
                ((eql a-line 'eof))
              (format t "~A" a-line)
              (force-output output))))
    (format t "~% open-browser: didn't run firefox"))))

;; (block open-browser-test
;;  (open-browser "/usr/bin/firefox" *hh-teaser-url*))







(in-package  #:cl-autogui)

(defun x-size ()
  (with-default-screen (s)
    (values
     (screen-width s)
     (screen-height s))))

(defun x-move (x y)
  (if (and (integerp x) (integerp y))
      (with-default-display-force (d)
        (xlib/xtest:fake-motion-event d x y))
      (error "Integer only for position, (x: ~S, y: ~S)" x y)))

(defun mklist (obj)
  (if (and
       (listp obj)
       (not (null obj)))
      obj (list obj)))

(defmacro defun-with-actions (name params actions &body body)
  ;; "This macro defun a function which witch do mouse or keyboard actions,
  ;; body is called on each action."
  `(defun ,name ,params
     (mapcar
      #'(lambda (action)
          ,@body)
      (mklist ,actions))))

(macrolet ((def (name actions)
             `(defun-with-actions ,name
                  (&key (button 1) x y)
                  ,actions
                (funcall #'perform-mouse-action
                         action button :x x :y y))))
  (def x-mouse-down t)
  (def x-mouse-up nil)
  (def x-click '(t nil))
  (def x-dbclick '(t nil t nil)))

(defmacro with-scroll (pos neg clicks x y)
  `(let ((button (cond
                   ((= 0 ,clicks) nil)
                   ((> 0 ,clicks) ,pos)    ; scroll up/right
                   ((< 0 ,clicks) ,neg)))) ; scroll down/left
     (dotimes (_ (abs ,clicks))
       (x-click :button button :x ,x :y ,y))))

(defun x-vscroll (clicks &key x y)
  (with-scroll 4 5 clicks x y))

(defun x-scroll (clicks &key x y)
  (x-vscroll clicks :x x :y y))

(defun x-hscroll (clicks &key x y)
  (with-scroll 7 6 clicks x y))

(macrolet ((def (name actions)
             `(defun-with-actions ,name (keycode)
                  ,actions
                (funcall #'perform-key-action
                         action keycode))))
  (def x-key-down t)
  (def x-key-up nil)
  (def x-press '(t nil)))

  (in-package  #:cl-autogui)

  ;; (defun perform-mouse-action (press? button &key x y)
  ;;   (and x y (x-move x y))
  ;;   (with-default-display-force (d)
  ;;     (xlib/xtest:fake-button-event d button press?)))

  ;; (defun perform-key-action (press? keycode) ; use xev to get keycode
  ;;   (with-default-display-force (d)
  ;;     (xlib/xtest:fake-key-event d keycode press?)))

(defun perform-mouse-action (press? button &key x y)
  (and x y (x-move x y))
  (with-default-display-force (d)
    (xlib/xtest:fake-button-event d button press?)))

(defun perform-key-action (press? keycode) ; use xev to get keycode
  (with-default-display-force (d)
    (xlib/xtest:fake-key-event d keycode press?)))

  ;; (block perform-key-action-test
  ;;   (perform-key-action t 116)
  ;;   (sleep .1)
  ;;   (perform-key-action nil 116))

  ;; (block perform-mouse-action-test
  ;;   (perform-mouse-action t *mouse-left* :x 100 :y 100)
  ;;   (sleep .1)
  ;;   (perform-mouse-action nil *mouse-left* :x 100 :y 100))

(in-package  #:cl-autogui)

(defun make-bit-image (image-data)
  (destructuring-bind (height width &optional colors)
      (array-dimensions image-data)
    ;; функция может работать только с бинарными изобажениями
    (assert (null colors))
    (let* ((new-width (+ (logior width 7) 1))
           (bit-array (make-array (list height new-width)
                                  :element-type 'bit)))
      (do ((qy 0 (incf qy)))
          ((= qy height))
        (do ((qx 0 (incf qx)))
            ((= qx width))
          ;; если цвет пикселя не белый, считаем,
          ;; что это не фон и заносим в битовый массив 1
          (unless (equal (aref image-data qy qx) 255)
            (setf (bit bit-array qy qx) 1))))
      bit-array)))

;; (block make-bit-image
;;     (time
;;      (let* ((bit-arr1
;;              (make-bit-image (load-png "~/Pictures/test-bin.png"))))
;;        (format t "~% ~A" bit-arr1))))

(in-package  #:cl-autogui)

(defun append-xor (image-up image-down y-point)
  (destructuring-bind (height-up width-up &optional colors-up)
      (array-dimensions image-up)
    (destructuring-bind (height-down width-down &optional colors-down)
        (array-dimensions image-down)
      (assert (equal width-up width-down))
      (assert (equal colors-up colors-down))
      (let* ((new-height (+ height-down y-point))
             (new-dims (if (null colors-down)
                           (list new-height width-down)
                           (list new-height width-down colors-down)))
             (image-new (make-array new-dims :element-type '(unsigned-byte 8))))
        ;; макрос для прохода по блоку точек
        (macrolet ((cycle ((py px height width &optional &body newline)
                           &body body)
                     `(do ((qy ,py (incf qy)))
                          ((= qy ,height))
                        (do ((qx ,px (incf qx)))
                            ((= qx ,width))
                          ,@body)
                        ,@newline)))
          ;; копируем первую картинку в новый массив
          ;; от ее начала до ее конца (NB: тут отличие от append-image)
          (if (null colors-up)
              (cycle (0 0 height-up width-up)
                     (setf (aref image-new qy qx)
                           (aref image-up qy qx)))
              ;; else
              (cycle (0 0 height-up width-up)
                     (do ((qz 0 (incf qz)))
                         ((= qz colors-up))
                       (setf (aref image-new qy qx qz)
                             (aref image-up qy qx qz)))))
          ;; xor-им вторую картинку в новый массив
          ;; от ее начала до конца
          (if (null colors-down)
              (let ((new-y y-point))
                (cycle (0 0 height-down width-down (incf new-y))
                       (setf (aref image-new new-y qx)
                             (logxor (aref image-new new-y qx)
                                     (aref image-down qy qx)))))
              ;; else
              (let ((new-y y-point))
                (cycle (0 0 height-down width-down (incf new-y))
                       ;; ксорим 3 цвета
                       (do ((rz 0 (incf rz)))
                           ((= rz colors-down))
                         (setf (aref image-new new-y qx rz)
                               (logxor (aref image-new new-y qx rz)
                                       (aref image-down qy qx rz))))
                       ;; копируем альфа-канал
                       (setf (aref image-new new-y qx 3)
                             (aref image-down qy qx 3))
                       ))))
        image-new))))

;; (time
;;  (block test-append-xor-fullcolor
;;    (let* ((arr1 (x-snapshot :x 0 :y 0 :width 500 :height 300))
;;           (arr2 (x-snapshot :x 0 :y 100 :width 500 :height 300))
;;           (result (append-xor arr1 arr2 200)))
;;      (destructuring-bind (height width  &rest rest)
;;          (array-dimensions result)
;;        (save-png width height "~/Pictures/result.png" result)))))

;; (block test-append-xor-grayscale
;;   (let* ((arr1 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300)))
;;          (arr2 (binarization (x-snapshot :x 0 :y 100 :width 755 :height 300)))
;;          (array (append-xor arr1 arr2 200)))
;;     (destructuring-bind (height width  &rest rest)
;;         (array-dimensions array)
;;       (save-png width height "~/Pictures/result.png" array :grayscale))))

(in-package  #:cl-autogui)

(defun xor-area (image-up image-down y-point)
  (destructuring-bind (height-up width-up &optional colors-up)
      (array-dimensions image-up)
    (destructuring-bind (height-down width-down &optional colors-down)
        (array-dimensions image-down)
      ;; (format t "~% height-up ~A width-up ~A height-down ~A width-down ~A y ~A"
      ;;         height-up width-up height-down width-down y-point)
      (assert (equal width-up width-down))
      (assert (equal colors-up colors-down))
      (if (>= y-point height-up)
          nil
          (let* ((new-height (if (> height-up height-down )
                                 (+ height-up y-point)
                                 (+ height-down y-point)))
                 (intersect-area (if (> (- height-up y-point) height-down)
                                     height-down
                                     (- height-up y-point)))
                 (new-dims (if (null colors-down)
                               (list new-height width-down)
                               (list new-height width-down colors-down)))
                 (image-new (make-array new-dims :element-type '(unsigned-byte 8))))
            ;; макрос для прохода по блоку точек
            (macrolet ((cycle ((py px height width &optional &body newline)
                               &body body)
                         `(do ((qy ,py (incf qy)))
                              ((= qy ,height))
                            (do ((qx ,px (incf qx)))
                                ((= qx ,width))
                              ,@body)
                            ,@newline)))
              ;; для бинарных изображений
              (if (null colors-down)
                  (let ((new-y y-point))
                    ;; (- height-up y-point) = высота области наложения
                    (cycle (0 0 intersect-area width-down (incf new-y))
                           (setf (aref image-new qy qx)
                                 (logxor (aref image-up new-y qx)
                                         (aref image-down qy qx)))))
                  ;; для full-color изображений
                  (let ((new-y y-point))
                    (cycle (0 0 intersect-area width-down (incf new-y))
                           ;; ксорим 3 цвета
                           (do ((rz 0 (incf rz)))
                               ((= rz (- colors-down 1)))
                             (setf (aref image-new qy qx rz)
                                   (logxor (aref image-up new-y qx rz)
                                           (aref image-down qy qx rz))))
                           ;; копируем альфа-канал
                           (setf (aref image-new qy qx 3)
                                 (aref image-down qy qx 3))))))
            image-new)))))

;; (block xor-area-test
;;   (time
;;   (let* ((arr1 (binarization (load-png "~/Pictures/test-bin.png") 200))
;;          (arr2 (binarization (load-png "~/Pictures/test-bin.png") 200))
;;          (array (xor-area arr1 arr2 200)))
;;              (destructuring-bind (height width  &rest rest)
;;                 (array-dimensions array)
;;                (save-png width height "~/Pictures/area.png" array :grayscale)))))

;; (time
;;  (block xor-area-test-with-analysis
;;    (let* ((arr1  (binarization (x-snapshot :width 300 :height 600) 200))
;;           (arr2  (binarization (x-snapshot :y 200 :width 300 :height 200) 200))
;;           (arr1-bin (make-bit-image arr1))
;;           (arr2-bin (make-bit-image arr2))
;;           (amount)
;;           (res))
;;      (do ((i 0 (incf i)))
;;          ((= i (array-dimension arr1 0)))
;;        (setf amount (analysis (xor-area arr1-bin arr2-bin i) i))
;;        (if (car amount)
;;            (setf res (cons (cons amount i) res))))
;;      (setf res (find-best res))
;;      (let ((app-arr (append-image arr1 arr2 (cdr res))))
;;        (destructuring-bind (height width  &rest rest)
;;            (array-dimensions app-arr)
;;          (save-png width height "~/Pictures/area.png" app-arr :grayscale))))))


(in-package  #:cl-autogui)

(defstruct task
  (y-points '())
  (image-up nil)
  (image-down nil)
  (image-up-path nil)
  (image-down-path nil)
  fn)

(defun create-task (image-up image-down &optional image-up-path image-down-path)
  ;;   (format t "~% make-task tasks length ~A " (length tasks))
  (destructuring-bind (height-down width-down &optional colors-down)
      (array-dimensions image-down)
    (let* ((new-task (make-task :y-points (loop :for pnt :from height-down :downto 0 :collect pnt)
                                :image-up image-up
                                :image-down image-down
                                :image-up-path image-up-path
                                :image-down-path image-down-path
                                :fn
                                (lambda (image-up image-down y-points)
                                  (let* ((cur-results)
                                         (bit-image-up (make-bit-image image-up))
                                         (bit-image-down (make-bit-image
                                                          image-down)))
                                    (do ((i (length y-points) (- i 1)))
                                        ((= i 0))
                                      (let ((y-point (car y-points)))
                                        ;; убираем его из списка y-point-ов
                                        (setf y-points (cdr y-points))
                                        ;; если это первая итерация цикла и нет данных
                                        ;; и никаких результатов еще нет
                                        (if (null cur-results)
                                            ;; анализируем изображение с текущим y-point
                                            ;; и допустимым кол-вом белых точек по умолчанию
                                            (let ((amount (analysis
                                                           (xor-area bit-image-up
                                                                     bit-image-down
                                                                     y-point)
                                                           y-point)))
                                              ;; если какой-то результат получен,
                                              (if amount
                                                  (progn
                                                    (setf cur-results (cons
                                                                       (cons
                                                                        amount
                                                                        y-point)
                                                                       cur-results)
                                                          ))))
                                            ;; если результаты были, получаем новый
                                            ;; порог белых точек
                                            (let* ((last-result (car cur-results))
                                                   (white (cdr (car last-result)))
                                                   ;; вызываем анализ с этим порогом
                                                   (amount (analysis
                                                            (xor-area bit-image-up
                                                                      bit-image-down
                                                                      y-point)
                                                            y-point white)))
                                              ;; если какой-то результат получен,
                                              (if amount
                                                  ;; записываем в в текущий пулл результатов
                                                  (progn
                                                    (setf cur-results (cons
                                                                       (cons amount
                                                                             y-point)
                                                                       cur-results))
                                                    ))))))
                                    cur-results
                                    ))
                                )))
      ;;(format t "~% new-task ~A" new-task)
      (setf tasks (append tasks (list new-task)))
      )))
   (in-package  #:cl-autogui)
   
   (in-package  #:cl-autogui)
   
   (defstruct result
     black
     white
     y-point
     image-up image-down)
   
   (defstruct append-results
     append-image)
   
   
   (defun get-data (image-up-path image-down-path)
     ;; если тасков нет, а занчит, нет и пары изображений
     (format t "~% get-data: image-up ~A image-down ~A" image-up-path image-down-path)
     ;;(if (eql (fill-pointer tasks) 0)
     ;; сделать скриншот
     (let ((image-up
            (binarization
             (x-snapshot :x 440 :y 100
                         :width *snap-width* :height *snap-height*))))
       ;; провертим экран вниз
       (perform-key-action t 117)
       (sleep .1)
       (perform-key-action nil 117)
       (sleep .5)
       ;; сделать второй скриншот
       (let ((image-down
              (binarization (x-snapshot :x 440 :y 100 :width *snap-width*
                                        :height *snap-height*))))
         ;; сделать таск для них
         (create-task image-up image-down image-up-path image-down-path)
         ;; сохранить их
         (destructuring-bind (height-down width-down)
             (array-dimensions image-down)
           (save-png width-down height-down image-down-path image-down :grayscale))
         (destructuring-bind (height-up width-up)
             (array-dimensions image-up)
           (save-png width-up height-up image-up-path image-up :grayscale))))
     ;; else
     ;;(progn
     ;; провертим экран вниз
     (perform-key-action t 117)
     (sleep .1)
     (perform-key-action nil 117)
     (sleep .5))
   
;; 
;; 


;; 

(in-package #:cl-autogui)

(defun create-threads (num-of-cores)
  (let* ((lock (bt:make-lock))
         (thread-names))
    ;; генерим потоки
    (do ((i 0 (incf i)))
        ((= i (- num-of-cores 1)))
      (let ((t-name (intern (format nil "thread~A" i))))
        (format t "~%thread '~A' created" t-name)
        (let ((bt:*default-special-bindings*
               `((tasks . ,tasks)
                 (last? . ,last?)
                 (results . ,results)
                 (append-results . ,append-results))))
          (setf t-name
                (bt:make-thread
                 (lambda ()
                   (with-open-file (out (format nil "thread~A" i) :direction :output
                                        :if-exists :supersede)
                     ;; (format out "~% ~A" tasks)
                     (tagbody
                      top
                        (format out "~% thread ~A" t-name)
                        (format out "~% tasks ~A"  (length tasks))
                        ;; если таск есть, забираем немедленно
                        (let ((cur-task (unless (null tasks)
                                          (bt:with-lock-held (lock)
                                            (pop tasks)))))
                          ;; (format out "~% length tasks ~A" (length tasks))
                          ;; ОЖИДАЕМ ТАСКИ
                          (if *last?*
                              ;; Если установлен флаг LAST - Выходим из потока
                              nil
                              ;; Иначе
                              (if (null cur-task)
                                  ;; THEN: Таск не взят
                                  ;; Таски кончились, надо немного поспать,
                                  ;; а потом проверить снова
                                  (progn
                                    (format out "~% wait for task")
                                    (sleep 6)
                                    ;; если тасков так и нет
                                    (if (null tasks)
                                        ;; выход
                                        nil
                                        ;; иначе идем забирать таск
                                        (go top)))
                                  ;; ELSE: ТАСК ЕСТЬ
                                  ;; Далее я не понимаю зачем это делается и нигде не объяснено..
                                  ;; Ответ: берешь данные из таска, чтоб передерать их в лямбду
                                  ;; которая в таске см блок lambda
                                  (let* ((image-up    (task-image-up cur-task))
                                         (image-down  (task-image-down cur-task))
                                         (y-points    (task-y-points cur-task))
                                         (fn          (task-fn cur-task))
                                         ;; получаем все результаты анализа
                                         (cur-results (funcall fn image-up image-down y-points)))
                                    (format out " ~% task image-up ~A image-down ~A"
                                            (task-image-up-path cur-task)
                                            (task-image-down-path cur-task))
                                    (force-output out)
                                    ;; сортируем результаты
                                    ;; по количеству черных точек
                                    ;; от самого выского результата до самого низкого
                                    (format out " ~% cur-results ~A" cur-results)
                                    (let* ((best-res (find-best cur-results))
                                           (new-result (make-result
                                                        :white (cdr (car best-res))
                                                        :black (car (car best-res))
                                                        :y-point (cdr best-res)
                                                        :image-up (task-image-up cur-task)
                                                        :image-down (task-image-down cur-task))))
                                      ;;(format out "~% sorted-result ~A" best-res)
                                      ;; выводим результат
                                      (format out "~% ----
                                                   ~% y-point ~A ~% image-up ~A
                                                   ~% image-down ~A
                                                   ~% amount ~A
                                                   ~% ----"
                                              (result-y-point new-result)
                                              (task-image-up-path cur-task)
                                              (task-image-down-path cur-task)
                                              (result-black new-result))
                                      (force-output out)
                                      (bt:with-lock-held (lock)
                                        (setf *results-queue* (append *results-queue*
                                                              (list new-result))))
                                      ;;(format out "~% results ~A" results)
                                      ;; идем снова брать таск
                                      (go top)))))))))))
          (push t-name thread-names)))
      (values lock
              thread-names))))
(in-package #:cl-autogui)


(defun alive-threads-counter (thread-names)
  (let ((alive-t-cnt 0))
    (do ((i 0 (incf i)))
        ((= i (length thread-names)))
      ;;(format t "~% nth ~A thread-name ~A" i (nth i thread-names))
      ;; если поток жив
      (if (bt:thread-alive-p (nth i thread-names))
          ;; (format t "~% alive ~A "(nth i thread-names))
          ;; инкрементируем счетчик
          (incf alive-t-cnt)))
    alive-t-cnt))
(defun make-roll-test ()
  (format t "~% make-roll: amount of results ~A"
          (length *results-queue*))
  (let ((counter 0))
    (tagbody
     append-images
       (format t "~% append-images")
       ;; получаем результаты анализа
       (let* (;; (cur-task (vector-pop results))
              (cur-task (pop *results-queue*))
              (image-up (result-image-up cur-task))
              (image-down (result-image-down cur-task))
              (y-point (result-y-point cur-task)))
         (destructuring-bind (height-down width-down)
             (array-dimensions image-down)
           (destructuring-bind (height-up width-up)
               (array-dimensions image-up)
             (format t
                     "~% --------------------
                                       ~% height-down ~A width-down ~A
                                       ~% height-up ~A width-up ~A
                                       ~% --------------------"
                     height-down width-down height-up width-up)))
         (let ((appended-image
                (append-image image-up image-down y-point)))
           (destructuring-bind (height width)
               (array-dimensions appended-image)
             (save-png width height
                       (format nil "~~/Pictures/resut~A.png" counter)
                       appended-image :grayscale))
           (incf counter)
           ;; пушим результаты склейки
           (setf *append-results-queue* (append *append-results-queue*
                                                 (list appended-image))))
         ;; результаты аналза кончились?
         ;; (if (not (eql (fill-pointer results) 0))
         (if (not (eql 0 (length *results-queue*)))
             ;; нет!
             (go append-images)
             t)))))

(defun screen-scan ()
  (let ((screen-cnt 0))
    (tagbody
     get-data
       ;;скриним и составляем таски
       (get-data (format nil "~~/Pictures/screen~A.png" screen-cnt)
                 (format nil "~~/Pictures/screen~A.png" (incf screen-cnt)))
       (incf screen-cnt)
       (sleep 8)
       (format t "~% length results ~A" (length results))
       ;;(format t "~% length tasks ~A" (fill-pointer tasks))
       ;;(format t "~% length results ~A" (car results))
       (if (null results)
           ;; пока не дойдем до последней пары картинок
           (go get-data)
           ;; else [COMMENT:gmm] - Что здесь происходит и зачем?
           (progn
              ;; это отладочный цикл, который позволяет увидеть запушенные результаты:
              ;; y-point и кол-во черных точек
             (do ((i 0 (incf i)))
                 ((= i (length results)))
               (let ((struct (nth i results)))
                 (format t "~% y ~A black ~A"
                         (result-y-point struct)
                         (result-black struct))))
             (if *last?*
                 (progn
                   ;; (format t "~% length results ~A" (car results))
                   (tagbody
                    check-threads
                      ;; (format t "~% results ~A tasks~A" (length results)
                      ;;         (length tasks))
                      ;; счетчик живых потоков
                      (let ((alive-threads (alive-threads-counter thread-names)))
                        ;;(format t "~% alive threads ~A " alive-threads)
                        ;; если живых потоков нет
                        (if (eql 0 alive-threads)
                        ;; вызывать склейку
                            (progn
                              ;;(make-roll num-of-cores)
                              (format t "~% results ~A" (length *results-queue*))
                              (make-roll-test)
                              (return-from screen-scan t))
                            ;; иначе проверяем снова
                            (progn
                              (sleep .5)
                              ;;(format t "~% wait")
                              (go  check-threads))))))
                 (go get-data)))))))

(defun get-area-merge-results (num-of-cores)
  (let* ((tasks '())
         (last? nil)
         (results '())
         (append-results '()))
    (declare (special tasks last? results append-results))
    (multiple-value-bind (lock thread-names)
        (create-threads num-of-cores)
      (screen-scan))))

(in-package  #:cl-autogui)

;; (time
;; (block get-area-merge-reselt-test
;;    (open-browser "/usr/bin/firefox" "https://spb.hh.ru/")
;;    (sleep 8)
;;    (let ((result (get-area-merge-results 4)))
;;    )))

;; (time
;;  (block ofline-demo-test
;;    (demo-get-area-merge-results)))

;; (time
;;  (block online-demo-test
;;    (open-browser "/usr/bin/firefox" "https://spb.hh.ru/")
;;    (sleep 8)
;;    (demo-get-area-merge-results)))
