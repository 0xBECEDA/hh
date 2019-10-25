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
(defparameter *snap-x* 100)
(defparameter *snap-y* 100)
(defparameter *default-x* 60)
(defparameter *default-y* 37)
(defparameter *mouse-left* 1)
(defparameter *mouse-middle* 2)
(defparameter *hh-teaser-url*
  "https://hh.ru/search/vacancy?L_is_autosearch~false&area=2&clusters=true&enable_snippets=true&items_on_page=100&only_with_salary=true&salary=165000&specialization=1.221&page~~A"
  "https://spb.hh.ru/search/vacancy?L_is_autosearch~false&area=1&clusters=true&enable_snippets=true&items_on_page=100&only_with_salary=true&salary=165000&specialization=1.221&page~~A")

(defparameter *browser-path*  "/usr/bin/firefox")
(in-package #:cl-autogui)
(in-package  #:cl-autogui)

(defstruct append-results
  append-image)
  (in-package  #:cl-autogui)

  (defstruct result
    black
    white
    y-point
    image-up image-down)
(defstruct task
  (y-points '())
  (image-up nil)
  (image-down nil)
  (image-up-path nil)
  (image-down-path nil)
  fn)
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

(defun append-image (image-up image-down y-point)
  (destructuring-bind (height-down width-down &optional colors-down)
      (array-dimensions image-down)
    ;; (destructuring-bind (height-up width-up &optional colors-up)
    ;;     (array-dimensions image-up)
    (let* ((new-height (+ height-down y-point))
           (new-dims (if (null colors-down)
                         (list new-height width-down)
                         (list new-height width-down colors-down)))
           (image-new (make-array new-dims :element-type '(unsigned-byte 8))))
      (destructuring-bind (height-new width-new &optional colors-new)
          (array-dimensions image-new)
        (format t "~%  append-image: height-new ~A width-new ~A y-point ~A"
                height-new width-new y-point))
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
        ;; от ее начала до точки склейки, или до ее конца,
        ;; смотря что случится раньше
        (if (null colors-down)  ;; TODO: тут надо проверять цвета первой картинки
            ;;(cycle (0 0 (min height-down y-point) width-down)
            (cycle (0 0 y-point width-down)
                   (setf (aref image-new qy qx)
                         (aref image-up qy qx)))
            ;; else
            (cycle (0 0 y-point width-down)
                   (do ((qz 0 (incf qz)))
                       ((= qz colors-down))
                     (setf (aref image-new qy qx qz)
                           (aref image-up qy qx qz)))))
        ;; копируем вторую картинку в новый массив
        ;; от ее начала до конца
        (if (null colors-down)
            (let ((new-y y-point))
              (cycle (0 0 height-down width-down (incf new-y))
                     (setf (aref image-new new-y qx)
                           (aref image-down qy qx))))
            ;; else
            (let ((new-y y-point))
              (cycle (0 0 height-down width-down (incf new-y))
                     (do ((rz 0 (incf rz)))
                         ((= rz colors-down))
                       (setf (aref image-new new-y qx rz)
                             (aref image-down qy qx rz)))))))
      image-new)))

;; (block test-append-image-fullcolor
;;   (let* ((arr1 (x-snapshot :x 0 :y 0 :width 755 :height 300))
;;          (arr2 (x-snapshot :x 100 :y 100 :width 755 :height 300))
;;          (array (append-image arr1 arr2 200)))
;;     (destructuring-bind (height width  &rest rest)
;;         (array-dimensions array)
;;       (save-png width height "~/Pictures/result.png" array))))


;; (block test-append-image-grayscale
;;   (let* ((arr1 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 600)))
;;          (arr2 (binarization (x-snapshot :x 0 :y 555 :width 755 :height 130)))
;;          (array (append-image arr1 arr2 600)))
;;     (destructuring-bind (height width  &rest rest)
;;         (array-dimensions array)
;;       (save-png width height "~/Pictures/result.png" array :grayscale))))

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

(in-package #:cl-autogui)

(defparameter *task-queue* nil)
(defparameter *results-queue* nil)



(in-package #:cl-autogui)

(in-package #:cl-autogui)

(in-package #:cl-autogui)

(in-package  #:cl-autogui)

(defun save-png (width height pathname-str image
                 &optional (color-type :truecolor-alpha))
  (let* ((png (make-instance 'zpng:png :width width :height height
                             :color-type color-type))
         (vector (make-array ;; displaced vector - need copy for save
                  (* height width (zpng:samples-per-pixel png))
                  :displaced-to image :element-type '(unsigned-byte 8))))
    ;; Тут применен потенциально опасный трюк, когда мы создаем
    ;; объект PNG без данных, а потом добавляем в него данные,
    ;; используя неэкспортируемый writer.
    ;; Это нужно чтобы получить третью размерность массива,
    ;; который мы хотим передать как данные и при этом
    ;; избежать создания для этого временного объекта
    (setf (zpng::%image-data png) (copy-seq vector))
    (zpng:write-png png pathname-str)))
(in-package  #:cl-autogui)

(defun load-png (pathname-str)
  "Возвращает массив size-X столбцов по size-Y точек,
   где столбцы идут слева-направо, а точки в них - сверху-вниз
   ----
   В zpng есть указание на возможные варианты COLOR:
   ----
         (defmethod samples-per-pixel (png)
           (ecase (color-type png)
             (:grayscale 1)
             (:truecolor 3)
             (:indexed-color 1)
             (:grayscale-alpha 2)
             (:truecolor-alpha 4)))
  "
  (let* ((png (png-read:read-png-file pathname-str))
         (image-data (png-read:image-data png))
         (color (png-read:colour-type png))
         (dims (cond ((or (equal color :truecolor-alpha)
                          (equal color :truecolor))
                      (list (array-dimension image-data 1)
                            (array-dimension image-data 0)
                            (array-dimension image-data 2)))
                     ((or (equal color :grayscale)
                          (equal color :greyscale))
                      (list (array-dimension image-data 1)
                            (array-dimension image-data 0)))
                     (t (error 'unk-png-color-type :color color))))
         (result ;; меняем размерности X и Y местами
          (make-array dims :element-type '(unsigned-byte 8))))
    ;; (format t "~% new-arr ~A "(array-dimensions result))
    ;; ширина, высота, цвет => высота, ширина, цвет
    (macrolet ((cycle (&body body)
                 `(do ((y 0 (incf y)))
                      ((= y (array-dimension result 0)))
                    (do ((x 0 (incf x)))
                        ((= x (array-dimension result 1)))
                      ,@body))))
      (cond ((or (equal color :truecolor-alpha)
                 (equal color :truecolor))
             (cycle (do ((z 0 (incf z)))
                        ((= z (array-dimension result 2)))
                      (setf (aref result y x z)
                            (aref image-data x y z)))))
            ((or (equal color :grayscale)
                 (equal color :greyscale))
             (cycle (setf (aref result y x)
                          (aref image-data x y))))
            (t (error 'unk-png-color-type :color color)))
      result)))

(let ((screen-cnt 0))
  (defun save-screenshot (img)
    (let ((path (format nil "img-~A" (incf screen-cnt))))
      (cons path
            (destructuring-bind (height width &optional colors)
                (array-dimensions img)
              (if colors
                  (progn
                    (save-png width height path img)
                    img)
                  (progn
                    (save-png width height path img :grayscale)
                    img)))))))

(in-package #:cl-autogui)

(defmacro with-display (host (display screen root-window) &body body)
  `(let* ((,display (xlib:open-display ,host))
          (,screen (first (xlib:display-roots ,display)))
          (,root-window (xlib:screen-root ,screen)))
     (unwind-protect (progn ,@body)
       (xlib:close-display ,display))))

(defmacro with-default-display ((display &key (force nil)) &body body)
  `(let ((,display (open-default-display)))
     (unwind-protect
          (unwind-protect
               ,@body
            (when ,force
              (display-force-output ,display)))
       (close-display ,display))))

(defmacro with-default-display-force ((display) &body body)
  `(with-default-display (,display :force t) ,@body))

(defmacro with-default-screen ((screen) &body body)
  (let ((display (gensym)))
    `(with-default-display (,display)
       (let ((,screen (display-default-screen ,display)))
         ,@body))))

(defmacro with-default-window ((window) &body body)
  (let ((screen (gensym)))
    `(with-default-screen (,screen)
       (let ((,window (screen-root ,screen)))
         ,@body))))
(in-package  #:cl-autogui)

(defun x-size ()
  (with-default-screen (s)
    (values
     (screen-width s)
     (screen-height s))))

(defun x-move (x y)
  (if (and (integerp x) (integerp y))
      (with-default-display-force (d)
        (xtest:fake-motion-event d x y))
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
  ;;     (xtest:fake-button-event d button press?)))

  ;; (defun perform-key-action (press? keycode) ; use xev to get keycode
  ;;   (with-default-display-force (d)
  ;;     (xtest:fake-key-event d keycode press?)))

(defun perform-mouse-action (press? button &key x y)
  (and x y (x-move x y))
  (with-default-display-force (d)
    (xtest:fake-button-event d button press?)))

(defun perform-key-action (press? keycode) ; use xev to get keycode
  (with-default-display-force (d)
    (xtest:fake-key-event d keycode press?)))

  ;; (block perform-key-action-test
  ;;   (perform-key-action t 116)
  ;;   (sleep .1)
  ;;   (perform-key-action nil 116))

  ;; (block perform-mouse-action-test
  ;;   (perform-mouse-action t *mouse-left* :x 100 :y 100)
  ;;   (sleep .1)
  ;;   (perform-mouse-action nil *mouse-left* :x 100 :y 100))

(defun pgdn ()
  (perform-key-action t 117)
  (sleep .1)
  (perform-key-action nil 117)
  (sleep .5))

(in-package  #:cl-autogui)

(in-package  #:cl-autogui)

(defun raw-image->png (data width height)
  (let* ((png (make-instance 'zpng:png :width width :height height
                             :color-type :truecolor-alpha
                             :image-data data))
         (data (zpng:data-array png)))
    (dotimes (y height)
      (dotimes (x width)
        ;; BGR -> RGB, ref code: https://goo.gl/slubfW
        ;; diffs between RGB and BGR: https://goo.gl/si1Ft5
        (rotatef (aref data y x 0) (aref data y x 2))
        (setf (aref data y x 3) 255)))
    png))

(defun x-snapshot (&key (x *default-x*) (y *default-y*)
                     (width *default-width*) (height *default-height*)
                     path)
  ;; "Return RGB data array (The dimensions correspond to the height, width,
  ;; and pixel components, see comments in x-snapsearch for more details),
  ;; or write to file (PNG only), depend on if you provide the path keyword"
  (with-default-window (w)
    (let ((image
           (raw-image->png
            (xlib:get-raw-image w :x x :y y
                                :width width :height height
                                :format :z-pixmap)
            width height)))
      (if path
          (let* ((ext (pathname-type path))
                 (path
                  (if ext
                      path
                      (concatenate 'string path ".png")))
                 (png? (or (null ext) (equal ext "png"))))
            (cond
              (png? (zpng:write-png image path))
              (t (error "Only PNG file is supported"))))
          (zpng:data-array image)))))

;; (block save-load-binarixation-test
;;   (x-snapshot :x *snap-height*
;;               :width  *snap-width*
;;               :path "~/Pictures/snapshot-test.png"))
(in-package  #:cl-autogui)

(in-package  #:cl-autogui)

;; Ошибка, возникающая когда мы пытаемся прочитать png
;; в котором неизвестно сколько байт на точку
(define-condition unk-png-color-type (error)
  ((color :initarg :color :reader color))
  (:report
   (lambda (condition stream)
     (format stream "Error in LOAD-PNG: unknown color type: ~A"
             (color condition)))))

(defun binarization (image &optional threshold)
  (let* ((dims (array-dimensions image))
         (new-dims (cond ((equal 3 (length dims))  (butlast dims))
                         ((equal 2 (length dims))  dims)
                         (t (error 'binarization-error))))
         (result (make-array new-dims :element-type '(unsigned-byte 8))))
    (macrolet ((cycle (&body body)
                 `(do ((y 0 (incf y)))
                      ((= y (array-dimension image 0)))
                    (do ((x 0 (incf x)))
                        ((= x (array-dimension image 1)))
                      ,@body))))
      (cond ((equal 3 (length dims))
             (cycle (do ((z 0 (incf z)))
                        ((= z (array-dimension image 2)))
                      (let ((avg (floor (+ (aref image y x 0)
                                           (aref image y x 1)
                                           (aref image y x 2))
                                        3)))
                        (when threshold
                          (if (< threshold avg)
                              (setf avg 255)
                              (setf avg 0)))
                        (setf (aref result y x) avg)))))
            ((equal 2 (length dims))
             (cycle (let ((avg (aref image y x)))
                      (when threshold
                        (if (< threshold avg)
                            (setf avg 255)
                            (setf avg 0)))
                      (setf (aref result y x) avg))))
            (t (error 'binarization-error))))
    result))

;; (in-package  #:cl-autogui)
;;
;; (block save-load-binarixation-test
;;   (x-snapshot :x 440 :width  *snap-width*
;;               :path "~/Pictures/test.png")
;;   (let* ((image (load-png "~/Pictures/test.png"))
;;          (image (binarization image 200)))
;;     (destructuring-bind (dh dw)
;;         (array-dimensions image)
;;       (save-png dw dh "~/Pictures/test-bin.png"
;;                image  :grayscale))))
;;
;; (block save-load-full-color-test
;;   (x-snapshot :x 440 :width *snap-width*
;;               :path "~/Pictures/test.png")
;;   (sleep .1)
;;   (let* ((image (load-png "~/Pictures/test.png")))
;;   (destructuring-bind (dh dw colors)
;;       (array-dimensions image)
;;     (save-png dw dh "~/Pictures/test-full-color.png" image))))

(defun take-screenshot ()
  ;;(binarization
  (x-snapshot :x *snap-x* :y *snap-y*
              :width *snap-width* :height *snap-height*)
  ;; )
  )

(defstruct task
  (y-points '())
  (image-up nil)
  (image-down nil)
  (image-up-path nil)
  (image-down-path nil)
  fn)

(let ((prev-img))
  (defun producer (cv task-queue-lock)
    (loop
       ;; Если предыдущего изображения нет - сделаем его
       (unless prev-img
         (setf prev-img (save-screenshot (take-screenshot))))
       ;; Прокрутим экран вниз
       (pgdn)
       ;; Сделаем следующее изображение
       (let ((next-img (save-screenshot (take-screenshot))))
         ;; Сформируем новый таск
         (destructuring-bind (height-down width-down &optional colors)
             (array-dimensions (cdr next-img))
           (declare (ignore width-down))
           (let ((new-task (make-task :y-points (loop
                                                   :for pnt
                                                   :from height-down
                                                   :downto 0
                                                   :collect pnt)
                                      :image-up (cdr prev-img)
                                      :image-down (cdr next-img)
                                      :image-up-path (car prev-img)
                                      :image-down-path (car next-img)
                                      :fn #'analize-img-pair)))
             ;; Запишем его в очередь
             (bt:with-lock-held (task-queue-lock)
               (setf *task-queue*
                     (append *task-queue*
                             (list new-task))))
             ;; Сделаем последнее изображение новым предыдущим
             (setf prev-img next-img)
             ;; Уведомим потребителей об обновлении очереди задач
             (bt:condition-notify cv))))
       ;; Теперь можно поспать, чтобы не быть слишком быстрым
       (sleep 5))))
(in-package #:cl-autogui)

(defparameter *task-cnt* 0)
(defparameter *task-limit* 10)

(in-package #:cl-autogui)

(defun find-thread-by-name (thread-name)
  (cdr (assoc thread-name
              (mapcar #'(lambda (thread)
                          (cons (bt:thread-name thread)
                                thread))
                      (bt:all-threads))
              :test #'equal)))

(defun stop-report-and-kill-producer (outlock msg)
  (bt:with-lock-held (outlock)
    (format t "~% ~A reported: ~A; stop"
            (bt:thread-name (bt:current-thread))
            msg)
    (finish-output))
  (let ((producer (find-thread-by-name "producer-thread")))
    (when producer
      (bt:destroy-thread producer))))

(defun kill-all-consumers (outlock msg)
  (bt:with-lock-held (outlock)
    (format t "~% ~A reported: ~A; stop all threads"
            (bt:thread-name (bt:current-thread))
            msg)
    (finish-output))
  ;; KILL ALL THREADS!
  (mapcar #'(lambda (pair)
              (bt:destroy-thread (cadr pair)))
          ;; Отфильтровываем всех консюмеров
          (remove-if-not #'car
                         ;; Превращаем его в список кортежей
                         ;; Первый элемент каждого кортежа - является ли поток консюмером
                         (mapcar #'(lambda (th)
                                     (let* ((name (bt:thread-name th))
                                            (bool (equal "consum" (subseq name 0 6))))
                                       (list bool th name)))
                                 ;; Берем список потоков
                                 (bt:all-threads)))))

(in-package #:cl-autogui)

  (in-package  #:cl-autogui)

  (defun analysis (xored-image y-point &optional (border 50))
    "Принимает отксоренное изображение и y-координату  наложения,
     т.е. точку, от которой будет производиться анализ.
     Анализирует кол-во почерневших точек на изображении, возвращает cons-пару типа
     (% черных точек . y-point)"
    (if (null xored-image)
        nil
        (destructuring-bind (height width &optional colors)
            (array-dimensions xored-image)
          ;;(format t "~% y-point ~A height ~A" y-point height)
          (let* ((intesect-height height) ;; высота пересечения
                 (white 0)
                 (black 0)
                 ;; общее кол-во пикселей в области наложения
                 (pix-amount (* intesect-height width)))
            ;; высчитываем максимально допустимое количество белых пикселей
            (setf border (* (float (/ border 100)) pix-amount))
            ;;(format t "~% intesect-height ~A " intesect-height)
            ;; если картинки full-color
            (if colors
                (do ((qy y-point (incf qy)))
                    ((= qy height))
                  ;; если кол-во нечерных пикселей больше 25%
                  (if (> white border)
                      (progn
                        ;; не анализируя дальше, возвращаем nil
                        (return-from analysis))
                      ;; в противном случае анализиуем следующий ряд пикселей
                      (do ((qx 0 (incf qx)))
                          ((= qx width))
                        (when (not (and (eql (aref xored-image qy qx 0) 0)
                                        (eql (aref xored-image qy qx 1) 0)
                                        (eql (aref xored-image qy qx 2) 0)))
                          (incf white)))))
                ;; то же самое для бинарных изображений
                (do ((qy 0 (incf qy)))
                    ((= qy height))
                  (if (> white border)
                      (progn
                        (return-from analysis ))
                      (do ((qx 0 (incf qx)))
                          ((= qx width))
                        (when (not (eql (aref xored-image qy qx) 0))
                          (incf white))))))
            ;; эта часть выполнится только если все циклы выполнены успешно
            ;; считаем кол-во черных пикселей
            (setf black ( - pix-amount white))
            (let ((result (cons (* (float (/ black pix-amount)) 100)
                                (* (float (/ white pix-amount)) 100))))
              ;;(format t " ~% black ~A y-point ~A pixamount ~A" black y-point pix-amount)
              ;; возвращаем кол-во черных пикселей в процентном выражении
              result)))))

;; (block find-best-test
;;   (let* ((arr1 (make-bit-image (binarization (load-png "~/Pictures/img-2"))))
;;          (arr2 (make-bit-image (binarization (load-png "~/Pictures/img-3"))))
;;          (res)
;;          (amount))
;;     (do ((i 0 (incf i)))
;;         ((= i (array-dimension arr1 0)))
;;       (setf amount (analysis (xor-area arr1 arr2 i) i))
;;       (if (car amount)
;;           (setf res (cons (cons amount i) res))))
;;     (format t "~% res ~A" res)
;;     (setf res (find-best res))
;;     (format t "~% best-res ~A" res)
;;     (let ((app-arr (append-image (load-png "~/Pictures/img-2")
;;                                  (load-png "~/Pictures/img-3") (cdr res))))
;;       (destructuring-bind (height width  &rest rest)
;;           (array-dimensions app-arr)
;;         (save-png width height "~/Pictures/area.png" app-arr :grayscale)))))
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
          (let* ((intersect-area (if (> (- height-up y-point) height-down)
                                     height-down
                                     (- height-up y-point)))
                 (new-dims (if (null colors-down)
                               (list intersect-area width-down)
                               (list intersect-area width-down colors-down)))
                 (image-new (make-array new-dims :element-type '(unsigned-byte 8))))
            ;;(format t "~% xor: intersect-area ~A" intersect-area)
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

(defun analize-img-pair (image-up image-down y-points)
    (print "ANALIZE-IMG-PAIR")
    (let* ((cur-results)
           (bit-image-up (make-bit-image image-up))
           (bit-image-down (make-bit-image image-down)))
      (do ((i (length y-points) (- i 1)))
          ((= i 0))
        (let ((y-point (pop y-points)))
          ;; если это первая итерация цикла
          ;; и никаких результатов еще нет
          (if (null cur-results)
              ;; анализируем изображение с текущим y-point
              ;; и допустимым кол-вом белых точек по умолчанию
              (let ((amount (analysis (xor-area bit-image-up
                                                bit-image-down
                                                y-point)
                                      y-point)))
                ;; если какой-то результат получен, пушим его в cur-results
                (when amount
                  (push (cons amount y-point) cur-results)))
              ;; если результаты были, получаем новый порог белых точек
              (let* ((last-result      (car cur-results))
                     (white       (cdr (car last-result)))
                     ;; вызываем анализ с этим порогом
                     (amount (analysis (xor-area bit-image-up
                                                 bit-image-down
                                                 y-point)
                                       y-point white)))
                ;; если какой-то результат получен,
                (when amount
                  ;; записываем в в текущий пулл результатов
                  (push (cons amount y-point) cur-results))))))
      cur-results))
(in-package #:cl-autogui)

(defun find-best (thread-results)
  ;; получаем все результаты от потока
  ;; сортируем
  (let* ((sorted-result
          (sort thread-results
                #'(lambda (a b)
                    (> (car (car a)) (car (car b))))))
         ;; берем лучший из отсортированных
         (best-res (nth 0 sorted-result))
         (i 0))
    (tagbody
     top
     ;; получаем кол-во черных точек и y-point у лучшего результата
     ;; и следующего в списке
       (let ((black-best (car (car best-res)))
             (cur-black (car (car (nth i sorted-result))))
             (cur-y (cdr (nth i sorted-result))))
         ;; если кол-во черных точек в результатах одинаковое
         (if (eql black-best cur-black)
             (progn
               ;; берем новый результат
               ;; это сделано, чтоб если y-point != 0,
               ;; сохранить лучший результат с максимально низким y-point
               ;; так можно будет склеить картинки максимально правильно,
               ;;а не срезать половину
               (setf best-res (nth i sorted-result))
               ;; и при этом y-point = 0
               (if (eql cur-y 0)
                   ;; мы нашли последнюю пару картинок
                     (return-from
                      find-best (values (nth i sorted-result) t))
                   ;; y-point != 0
                   (progn
                     ;; проверяем дальше
                     (incf i)
                     (go top))))
             ;; кол-во черных точек в результатах не одинаковое
             (return-from
              find-best best-res))))))

;; (block find-best-test
;;   (let* ((arr1 (make-bit-image (binarization (load-png "~/Pictures/img-2"))))
;;          (arr2 (make-bit-image (binarization (load-png "~/Pictures/img-3"))))
;;          (amount)
;;          (res))
;;     (do ((i 0 (incf i)))
;;         ((= i (array-dimension arr1 0)))
;;       (setf amount (analysis (xor-area arr1 arr2 i) i))
;;       (if (car amount)
;;           (setf res (cons (cons amount i) res))))
;;     (format t "~% res ~A" res)
;;     (setf res (find-best res))
;;     (format t "~% best-res ~A" res)
;;     (let ((app-arr (append-image (load-png "~/Pictures/img-2")
;;                                  (load-png "~/Pictures/img-3") (cdr res))))
;;       (destructuring-bind (height width  &rest rest)
;;           (array-dimensions app-arr)
;;         (save-png width height "~/Pictures/area.png" app-arr :grayscale)))))

(defun create-roll (path own-cv outlock results-queue-lock)
  (loop
     (bt:with-lock-held (results-queue-lock)
       ;; wait for access
       (bt:condition-wait own-cv results-queue-lock)
       (bt:with-lock-held (outlock)
         (format t "~% create roll is woke"))
       ;; если все сработает верно, то управление в эту строку
       ;; попадет только 1 раз, поэтому не будет попытки удалить несуществующие потоки
       (stop-report-and-kill-producer
        outlock "stop-report-andd-kill-producer: last image!")
       (kill-all-consumers
        outlock "kill-all-consumers: last image!")
       (bt:with-lock-held (outlock)
         (format t "~% all threads are killed"))
       ;; take first img-pair
       (let* ((cur-result (pop *results-queue*))
              (cur-y-point (result-y-point cur-result))
              (cur-image-up (result-image-up cur-result))
              (cur-image-down (result-image-down cur-result))
              ;; append it
              ;; не считаем смещение, потому что на первой склейке его просто нет
              (roll (append-image cur-image-up cur-image-down cur-y-point)))
         ;; do till end of result-queue
         (do ((i (length *results-queue*) (decf i)))
             (( = i 0))
           ;; take img-pair
           (setf cur-result (pop *results-queue*)
                 cur-image-down (result-image-down cur-result)
                 cur-y-point (result-y-point cur-result))
           ;; find height of roll (нам это нужно, чтоб считать смещение)
           (destructuring-bind (height-roll width-roll &optional colors-roll)
               (array-dimensions roll)
             (destructuring-bind (height-up width-up &optional colors-up)
                 (array-dimensions (result-image-up cur-result))
               ;; offset
               ;; поскольку индексация в массивах начинается с 0, то от height
               ;; мы отнимаем 1: если array-dimensions вернула значение 668 для height,
               ;; это означает, что у нас 668 строк с индексацие от 0 до 667,
               ;; а не от 1 до 668. Так мы избежим погрешности в 1 пиксель
               (let* ((difference (- (- height-up 1) cur-y-point))
                      (new-y-point (- height-roll difference)))
                 (bt:with-lock-held (outlock)
                   (format t "~% do: i ~A; height-roll ~A cur-y-point ~A new-y-point ~A"
                           i height-roll cur-y-point new-y-point ))
                 (setf roll (append-image roll cur-image-down new-y-point))))))
         ;; save roll
         (destructuring-bind (height-roll width-roll &optional colors-roll)
             (array-dimensions roll)
           (if colors-roll
               (progn
                 (save-png width-roll height-roll path roll)
                 (return-from create-roll t))
               (progn
                 (bt:with-lock-held (outlock)
                   (format t "~% all the end!"))
                 (save-png width-roll height-roll path roll :grayscale)
                 (return-from create-roll t))))))))


(in-package  #:cl-autogui)

(defstruct result
  black
  white
  y-point
  image-up image-down)

(defun consumer (cv cv-roll task-queue-lock outlock)
  (unless (bt:thread-alive-p (find-thread-by-name "producer-thread"))
    (bt:destroy-thread (bt:current-thread)))
  (bt:with-lock-held (outlock)
    (format t "~% ~A started"
            (bt:thread-name (bt:current-thread)))
    (finish-output))
  (loop (let ((cur-task))
          ;; pop task to cur-task
          (bt:with-lock-held (task-queue-lock)
            (bt:condition-wait cv task-queue-lock)
            (setf cur-task (pop *task-queue*)))
          (if (null cur-task)
              ;; if no task then skip step
              (bt:with-lock-held (outlock)
                (format t "~% ~A reported: no task in queue; skip"
                        (bt:thread-name (bt:current-thread)))
                (finish-output))
              ;; else
              (progn
                (bt:with-lock-held (outlock)
                  (format t "~% ~A woke up for ~A; ~A tasks left, ~A processed"
                          (bt:thread-name (bt:current-thread))
                          (cons (task-image-up-path cur-task)
                                (task-image-down-path cur-task))
                          (length *task-queue*)
                          *task-cnt*)
                  (finish-output))
                ;; analize task and push best results to the queue
                (let* ((cur-results (funcall (task-fn cur-task)
                                             (binarization (task-image-up cur-task))
                                             (binarization (task-image-down cur-task))
                                             (task-y-points cur-task))))
                  ;; find best results after analize
                  (multiple-value-bind (best-res last?)
                    (find-best cur-results)
                    (let ((new-result (make-result
                                       :white (cdr (car best-res))
                                       :black (car (car best-res))
                                       :y-point (cdr best-res)
                                       :image-up (task-image-up cur-task)
                                       :image-down (task-image-down cur-task))))
                      (bt:with-lock-held (task-queue-lock)
                        (setf *results-queue* (append *results-queue* (list new-result))))
                      (bt:with-lock-held (outlock)
                        (format t " ~% thread ~A ; best-res ~A for ~A results ~A;
                                 ~A tasks left"
                                (bt:thread-name (bt:current-thread)) best-res
                                (cons (task-image-up-path cur-task)
                                      (task-image-down-path cur-task))
                                (length *results-queue*) (length *task-queue*))))
                    ;; was it last image?
                    (if last?
                        ;; yes
                        ;; kill all threads
                        (progn
                          (bt:with-lock-held (outlock)
                            (format t " ~% thread ~A: last image!"
                                    (bt:thread-name (bt:current-thread))))
                            (bt:with-lock-held (task-queue-lock)
                              (bt:condition-notify cv-roll)))
                            ;; increment thread-local task-cnt
                            ))))))))


(defun create-threads (num-of-cores)
  (let* ((cv              (bt:make-condition-variable))
         (cv-roll         (bt:make-condition-variable))
         (task-queue-lock (bt:make-lock "task-queue-lock"))
         (results-queue-lock (bt:make-lock "results-queue-lock"))
         (outlock         (bt:make-lock "output-lock"))
         (thread-pool))
    (bt:make-thread (lambda ()
                      (producer cv task-queue-lock))
                    :name "producer-thread")
    (bt:make-thread (lambda ()
                      (create-roll "~/Pictures/roll.png" cv-roll outlock
                      results-queue-lock))
                    :name "roll-thread"
                    :initial-bindings
                    `((*standard-output* . ,*standard-output*)))
    (format t "~%thread 'producer-thread' created")
    (do ((th-idx 0 (incf th-idx)))
        ((= th-idx (- num-of-cores 1)))
      (format t "~%thread 'consumer~A' created" th-idx)
      (push (bt:make-thread (lambda ()
                              (consumer cv cv-roll task-queue-lock outlock))
                            :name (format nil "consumer-~A" th-idx)
                            :initial-bindings
                            `((*standard-output* . ,*standard-output*)
                              (*task-limit*      . ,*task-limit*)))
            thread-pool))
    (values thread-pool task-queue-lock outlock)))

;; теперь ты можешь собрать скрины онлайн

(block producer-consumers-test
(open-browser "/usr/bin/firefox" "https://spb.hh.ru/")
(sleep 8)
(defparameter *clear*
    (multiple-value-bind (thread-pool task-queue-lock outlock)
        (create-threads 3)
      (declare (ignore thread-pool task-queue-lock outlock))
      (when nil
        (print
         (bt:all-threads))))))

(defun producer-test ()
(let* ((cv              (bt:make-condition-variable))
       (task-queue-lock (bt:make-lock "task-queue-lock"))
       (outlock         (bt:make-lock "outlock")))
  (bt:make-thread (lambda ()
                    (producer cv task-queue-lock))
                  :name "producer-thread")
  (loop
       (if (eql (length *task-queue*) 5)
           (progn
           (stop-report-and-kill-producer
            outlock "stop-report-andd-kill-producer: last image!")
           (return-from producer-test t))))))


;; (block producer-test
;;   (open-browser "/usr/bin/firefox" "https://spb.hh.ru/")
;;   (sleep 8)
;;   (producer-test))

;; OUTPUT:
;; thread 'producer-thread' created
;; thread 'consumer0' created
;; thread 'consumer1' created
;; consumer-0 started
;; consumer-1 started consumer-1 started
;; consumer-0 reported: no task in queue; skip
;; consumer-1 woke up for (img-1 . img-2); 0 tasks left, 0 processed
;; "ANALIZE-IMG-PAIR"
;; consumer-0 woke up for (img-2 . img-3); 0 tasks left, 1 processed
;; "ANALIZE-IMG-PAIR"
;; consumer-1 woke up for (img-3 . img-4); 0 tasks left, 2 processed
;; "ANALIZE-IMG-PAIR"
;; consumer-0 woke up for (img-4 . img-5); 0 tasks left, 3 processed
;; "ANALIZE-IMG-PAIR"
;; consumer-1 woke up for (img-5 . img-6); 0 tasks left, 4 processed
;; "ANALIZE-IMG-PAIR"
;; consumer-0 woke up for (img-6 . img-7); 0 tasks left, 5 processed
;; "ANALIZE-IMG-PAIR"
;; consumer-0 reported: task limit has been reached; stop
