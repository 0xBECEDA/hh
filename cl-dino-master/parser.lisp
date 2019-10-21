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
                     (width  *default-width*) (height *default-height*)
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
          (let* ((new-height (if ( > height-up height-down )
                                 (+ height-up y-point)
                                 (+ height-down y-point)))
                 (intersect-area (if ( > (- height-up y-point) height-down)
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
                                 (aref image-down qy qx 3))
                           ))))
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
  
  (defun append-image (image-up image-down y-point)
    (destructuring-bind (height-down width-down &optional colors-down)
        (array-dimensions image-down)
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
  ;;   (let* ((arr1 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300)))
  ;;          (arr2 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300)))
  ;;          (array (append-image arr1 arr2 200)))
  ;;     (destructuring-bind (height width  &rest rest)
  ;;         (array-dimensions array)
  ;;       (save-png width height "~/Pictures/result.png" array :grayscale))))
  
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
          (let* ((intesect-height (- height y-point)) ;; высота пересечения
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
                (do ((qy y-point (incf qy)))
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
  
  ;; (block analysis-test
  ;;   (let* ((arr1 (binarization (load-png "~/Pictures/test-bin.png") 200))
  ;;          (arr2 (binarization (load-png "~/Pictures/test-bin.png") 200))
  ;;          (array (xor-area arr1 arr2 200))
  ;;          (results (cons (analysis
  ;;                          array 200 80)
  ;;                         200)))
  ;;     (format t " ~% results ~A" results)))



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
;; (in-package  #:cl-autogui)
;; 
;; (in-package  #:cl-autogui)
;; 
;; (defstruct result
;;   black
;;   white
;;   y-point
;;   image-up image-down)
;; 
;; (defstruct append-results
;;   append-image)
;; 
;; 
;; (defun get-data (image-up-path image-down-path)
;;   ;; если тасков нет, а занчит, нет и пары изображений
;;   (format t "~% get-data: image-up ~A image-down ~A" image-up-path image-down-path)
;;   ;;(if (eql (fill-pointer tasks) 0)
;;   ;; сделать скриншот
;;   (let ((image-up
;;          (binarization
;;           (x-snapshot :x 440 :y 100
;;                       :width *snap-width* :height *snap-height*))))
;;     ;; провертим экран вниз
;;     (perform-key-action t 117)
;;     (sleep .1)
;;     (perform-key-action nil 117)
;;     (sleep .5)
;;     ;; сделать второй скриншот
;;     (let ((image-down
;;            (binarization (x-snapshot :x 440 :y 100 :width *snap-width*
;;                                      :height *snap-height*))))
;;       ;; сделать таск для них
;;       (create-task image-up image-down image-up-path image-down-path)
;;       ;; сохранить их
;;       (destructuring-bind (height-down width-down)
;;           (array-dimensions image-down)
;;         (save-png width-down height-down image-down-path image-down :grayscale))
;;       (destructuring-bind (height-up width-up)
;;           (array-dimensions image-up)
;;         (save-png width-up height-up image-up-path image-up :grayscale))))
;;   ;; else
;;   ;;(progn
;;   ;; провертим экран вниз
;;   (perform-key-action t 117)
;;   (sleep .1)
;;   (perform-key-action nil 117)
;;   (sleep .5))
;; (in-package #:cl-autogui)
;; 
;; (defun find-best (thread-results)
;;   ;; получаем все результаты от потока
;;   ;; сортируем
;;   (let* ((sorted-result
;;           (sort thread-results
;;                 #'(lambda (a b)
;;                     (> (car (car a)) (car (car b)))))
;;            )
;;          ;; берем лучший из отсортированных
;;          (best-res (nth 0 sorted-result))
;;          (i 0))
;;     (tagbody
;;      top
;;      ;; получаем кол-во черных точек и y-point у лучшего результата
;;      ;; и следующего в списке
;;        (let ((black-best (car (car best-res)))
;;              (cur-black (car (car (nth i sorted-result))))
;;              (cur-y (cdr (nth i sorted-result))))
;;          ;; если кол-во черных точек в результатах одинаковое
;;          (if (eql black-best cur-black)
;;              (progn
;;                ;; берем новый результат
;;                ;; это сделано, чтоб если y-point != 0,
;;                ;; сохранить лучший результат с максимально низким y-point
;;                ;; так можно будет склеить картинки максимально правильно,
;;                ;;а не срезать половину
;;                (setf best-res (nth i sorted-result))
;;                ;; и при этом y-point = 0
;;                (if (eql cur-y 0)
;;                    ;; мы нашли последнюю пару картинок
;;                    (progn
;;                      (setf last? t)
;;                      (return-from
;;                       find-best (nth i sorted-result)))
;;                    ;; y-point != 0
;;                    (progn
;;                      ;; проверяем дальше
;;                      (incf i)
;;                      (go top))))
;;              ;; кол-во черных точек в результатах не одинаковое
;;              (return-from
;;               find-best best-res))))))
;; 
;; 
;; 

(in-package #:cl-autogui)

(defparameter *task-queue* nil)
(defparameter *screen-cnt* 0)
(defparameter *task-cnt* 0)
(defparameter *task-limit* 8)

(defun analize-img-pair ()
  (print "ANALIZE-IMG-PAIR"))

(defun producer (cv lock)
  (labels ((pgdn ()
             (perform-key-action t 117)
             (sleep .1)
             (perform-key-action nil 117)
             (sleep .5))
           (take-screenshot ()
             (binarization
              (x-snapshot :x 440 :y 100
                          :width *snap-width* :height *snap-height*)))
           (save (pic path)
             (destructuring-bind (height width)
                 (array-dimensions pic)
               (save-png width height path pic :grayscale))))
    (tagbody
     repeat
       (let* ((image-up      (take-screenshot))
              (image-up-path (format nil "pic-~A" (incf *screen-cnt*))))
         (save image-up image-up-path)
         (pgdn)
         (let ((image-down      (take-screenshot))
               (image-down-path (format nil "pic-~A" (incf *screen-cnt*))))
           (save image-down image-down-path)
           (destructuring-bind (height-down width-down)
               (array-dimensions image-down)
             ;; add new task to queue
             (bt:with-lock-held (lock)
               (setf *task-queue*
                     (append *task-queue*
                             (list (make-task :y-points (loop
                                                           :for pnt
                                                           :from height-down
                                                           :downto 0
                                                           :collect pnt)
                                              :image-up image-up
                                              :image-down image-down
                                              :image-up-path image-up-path
                                              :image-down-path image-down-path
                                              :fn #'analize-img-pair))))
               ;; send notify
               (bt:condition-notify cv))
             (sleep 2)
             (go repeat)))))))

(defun find-thread-by-name (thread-name)
  (cdr (assoc thread-name
              (mapcar #'(lambda (thread)
                          (cons (bt:thread-name thread)
                                thread))
                      (bt:all-threads))
              :test #'equal)))

(defun consumer (cv lock outlock)
  (flet ((stop (msg)
           (bt:with-lock-held (outlock)
             (format t "~% ~A reported: ~A; stop"
                     (bt:thread-name (bt:current-thread))
                     msg)
             (finish-output))
           (bt:destroy-thread (find-thread-by-name "producer-thread"))))
    (unless (bt:thread-alive-p (find-thread-by-name "producer-thread"))
      (bt:destroy-thread (bt:current-thread)))
    (loop (let ((cur-task))
            ;; pop task to cur-task
            (bt:with-lock-held (lock)
              (bt:condition-wait cv lock)
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
                  ;; some heavy computation
                  (sleep 4)
                  ;; increment thread-local task-cnt
                  (incf *task-cnt*)
                  ;; check task limit
                  (when (> *task-cnt* *task-limit*)
                    (stop "task limit has been reached")
                    (return))
                  ;; check overload
                  (when (> (length *task-queue*) 5)
                    (stop "many tasks in queue")
                    (return))))))))

(defun create-threads (num-of-cores)
  (let* ((cv       (bt:make-condition-variable))
         (lock     (bt:make-lock "task-queue-lock"))
         (outlock  (bt:make-lock "output-lock"))
         (thread-names))
    (bt:make-thread (lambda () (producer cv lock))
                    :name "producer-thread")
    (do ((i 0 (incf i)))
        ((= i (- num-of-cores 1)))
      (let ((t-name (intern (format nil "consumer~A" i))))
        (format t "~%thread '~A' created" t-name)
        ;; (let ((bt:*default-special-bindings*
        ;;        `((tasks . ,tasks)
        ;;          (last? . ,last?)
        ;;          (results . ,results)
        ;;          (append-results . ,append-results))))
        (setf t-name (bt:make-thread (lambda () (consumer cv lock outlock))
                                     :name (format nil "consumer-~A" i)
                                     :initial-bindings
                                     `((*standard-output* . ,*standard-output*)
                                       (*task-cnt*        . ,*task-cnt*)
                                       (*task-limit*      . ,*task-limit*))))
        (push t-name thread-names))
      ;; )
      (values lock outlock thread-names))))

(defparameter *clear*
  (multiple-value-bind (lock outlock thread-names)
      (create-threads 3)
    (lambda ()
      (mapcar #'(lambda (th)
                  (bt:destroy-thread (find-thread-by-name th)))
              thread-names))))

;; (bt:all-threads)

;; (funcall *clear*)

;; OUTPUT:
;; thread 'consumer0' created
;; thread 'consumer1' created
;; consumer-0 reported: no task in queue; skip
;; consumer-1 woke up for (pic-1 . pic-2); 0 tasks left, 0 processed
;; consumer-0 woke up for (pic-3 . pic-4); 0 tasks left, 0 processed
;; consumer-1 woke up for (pic-5 . pic-6); 0 tasks left, 1 processed
;; consumer-0 woke up for (pic-7 . pic-8); 0 tasks left, 1 processed
;; consumer-1 woke up for (pic-9 . pic-10); 0 tasks left, 2 processed
;; consumer-0 woke up for (pic-11 . pic-12); 0 tasks left, 2 processed
;; consumer-1 woke up for (pic-13 . pic-14); 0 tasks left, 3 processed
;; consumer-0 woke up for (pic-15 . pic-16); 0 tasks left, 3 processed
;; consumer-1 woke up for (pic-17 . pic-18); 0 tasks left, 4 processed
;; consumer-0 woke up for (pic-19 . pic-20); 0 tasks left, 4 processed
;; consumer-1 woke up for (pic-21 . pic-22); 0 tasks left, 5 processed
;; consumer-0 woke up for (pic-23 . pic-24); 0 tasks left, 5 processed
;; consumer-1 woke up for (pic-25 . pic-26); 0 tasks left, 6 processed
;; consumer-0 woke up for (pic-27 . pic-28); 0 tasks left, 6 processed
;; consumer-1 woke up for (pic-29 . pic-30); 0 tasks left, 7 processed
;; consumer-0 woke up for (pic-31 . pic-32); 0 tasks left, 7 processed
;; consumer-1 woke up for (pic-33 . pic-34); 0 tasks left, 8 processed
;; consumer-0 woke up for (pic-35 . pic-36); 0 tasks left, 8 processed
;; consumer-1 reported: task limit has been reached; stop
;; consumer-0 reported: task limit has been reached; stop

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
                          (if last?
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
                                        (setf results (append results
                                                              (list new-result))))
                                      ;;(format out "~% results ~A" results)
                                      ;; идем снова брать таск
                                      (go top)))))))))))
          (push t-name thread-names)))
      (values lock
              thread-names))))
(in-package #:cl-autogui)


(defun make-roll-test ()
  (format t "~% make-roll: amount of results ~A"
          (length results))
  (let ((counter 0))
    (tagbody
     append-images
       (format t "~% append-images")
       ;; получаем результаты анализа
       (let* (;; (cur-task (vector-pop results))
              (cur-task (pop results))
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
           (setf append-results (append append-results (list appended-image))))
         ;; результаты аналза кончились?
         ;; (if (not (eql (fill-pointer results) 0))
         (if (not (eql 0 (length results)))
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
             (if last?
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
                              (format t "~% results ~A" (length results))
                              (make-roll-test)
                              (return-from get-area-merge-results t)) ;; TODO: поменять!!!
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
