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
  ;;     (xlib/xtest:fake-button-event d button press?)))

  ;; (defun perform-key-action (press? keycode) ; use xev to get keycode
  ;;   (with-default-display-force (d)
  ;;     (xlib/xtest:fake-key-event d keycode press?)))

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
            ;; (format t "~% intersect-height ~A" intesect-height)
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

  (in-package  #:cl-autogui)



  (defstruct result
    black
    white
    y-point
    image-up image-down)

  (defstruct append-results
    append-image)



  (defstruct task
    (y-points '())
    (image-up nil)
    (image-down nil)
    (image-up-path nil)
    (image-down-path nil))


  (let ((tasks (make-array 100 :fill-pointer 0))
        (last? nil))
    (dotimes (i 100)
      (setf (aref tasks i) (make-task)))
    (let ((results (make-array 100 :fill-pointer 0)))
      (dotimes (i 100)
        (setf (aref results i) (make-result)))
      (let ((append-results (make-array 100 :fill-pointer 0)))
        (dotimes (i 100)
          (setf (aref append-results i) (make-append-results)))

        (defun create-task (image-up image-down &optional image-up-path image-down-path)
          ;;   (format t "~% make-task tasks length ~A " (length tasks))
          (destructuring-bind (height-down width-down &optional colors-down)
              (array-dimensions image-down)
            (let* ((new-task (aref tasks (fill-pointer tasks)))
                   (y-points))
              (do ((y-point 0 (incf y-point)))
                  ((= y-point height-down))
                (setf y-points (cons y-point y-points)))
              ;; Здесь должен быть
              ;; (make-task :y-points ...
              ;; вместо невразумтельного setf
              (setf (task-y-points new-task) y-points
                    (task-image-up new-task) image-up
                    (task-image-down new-task) image-down
                    (task-image-up-path new-task) image-up-path
                    (task-image-down-path new-task) image-down-path)
              ;;(format t "~% new-task ~A" new-task)
              (vector-push new-task tasks))))

          (in-package  #:cl-autogui)


        (defun get-data-ofline (image-up image-down)
              (format t "~% get-data-ofline")
                (create-task (binarization (load-png image-up))
                           (binarization (load-png image-down))))


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

        (in-package  #:cl-autogui)

        (defun find-best (thread-results)
              ;; получаем все результаты от потока
              ;; сортируем
              (let* ((sorted-result
                      (sort thread-results
                            #'(lambda (a b)
                                (> (car (car a)) (car (car b)))))
                       ;; (sort thread-results
                       ;;       #'(lambda (a b)
                       ;;           (> (cdr (car a)) (cdr (car b)))))
                       )
                     ;; берем лучший из отсортированных
                     (best-res (nth 0 sorted-result))
                     (i 0))
                (tagbody
                 top
                   (let ((black-best (car (car best-res)))
                         (cur-black (car (car (nth i sorted-result))))
                         (cur-y (cdr (nth i sorted-result))))
                     ;; если кол-во черных точек в результатах одинаковое
                     (if (eql black-best cur-black)
                         (progn
                           (setf best-res (nth i sorted-result))
                           ;; и при этом y-point = 0
                           (if (eql cur-y 0)
                               ;; мы нашли последнюю пару картинок
                               (progn
                                 (setf last? t)
                                 (return-from
                                  find-best (nth i sorted-result)))
                               ;; y-point != 0
                               (progn
                                 ;; проверяем дальше
                                 (incf i)
                                 (go top))))
                         ;; кол-во черных точек в результатаз не одинаковое
                         (return-from
                          find-best best-res))))))


  (defun make-threads (num-of-cores)
    (let* ((lock (bt:make-lock))
           (thread-names))
      ;; генерим потоки
      (do ((i 0 (incf i)))
          ((= i (- num-of-cores 1)))
        (multiple-value-bind (name value)
            (intern (format nil "thread~A" i))
          (format t "~%  thread ~A" name)
          (in-package  #:cl-autogui)
          (setf name
                (bt:make-thread
                 (lambda ()
                   (with-open-file (out (format nil "thread~A" i) :direction :output
                                        :if-exists :supersede)
                     ;; (format out "~% ~A" tasks)
                     (tagbody
                      top
                        (format out "~%  f-p tasks ~A" (fill-pointer tasks))
                        ;; если таск есть, заюираем немедленно
                        (let* ((cur-task (if (not (eql (fill-pointer tasks) 0))
                                             (bt:with-lock-held (lock)
                                               (vector-pop tasks)))))
                          ;; таск есть?
                          (if (null cur-task)
                              ;; нет
                              nil
                              ;; да
                              (let* ((image-up)
                                     (image-down)
                                     (y-points)
                                     (cur-results))
                                (format out "~% get-area f-p ~A "
                                        (fill-pointer tasks))
                                ;; получаем данные из таска
                                (setf image-up (make-bit-image
                                                (task-image-up cur-task))
                                      image-down
                                      (make-bit-image (task-image-down cur-task))
                                      y-points (task-y-points cur-task))
                                (format out "%  y-points ~A "  y-points)
                                ;; (format out "~% y-point ~A ~% name ~A
                                ;;                ~% image-up ~A ~% image-down ~A"
                                ;;         (car cur-task) name image-up image-down)

                                ;; начинаем анализ
                                (destructuring-bind (height-up width-up)
                                    (array-dimensions image-up)
                                  (do ((i (- height-up 1) (- i 1)))
                                      ((< i 0))
                                    ;; получаем текущий y-point
                                    (let ((y-point (car y-points)))
                                      ;; убираем его из списка y-point-ов
                                      (setf y-points (cdr y-points))
                                      ;; если это первая итерация цикла и нет данных
                                      ;; и никаких результатов еще нет
                                      (if (null cur-results)
                                          ;; анализируем изображение с текущим y-point
                                          ;; и допустимым кол-вом белых точек по умолчанию
                                          (let ((amount (analysis
                                                         (xor-area image-up
                                                                   image-down y-point)
                                                         y-point)))
                                            ;; (format out "~% --- before
                                            ;;                    ~% y-point ~A ~% name ~A
                                            ;;                    ~% amount ~A"
                                            ;;         y-point name amount)
                                            ;; если какой-то результат получен,
                                            (if amount
                                                (progn
                                                  ;; (format out "~% ----
                                                  ;;            ~% y-point ~A ~% name ~A
                                                  ;;             ~% amount ~A
                                                  ;;              %---"
                                                  ;;         y-point name amount)
                                                  (setf cur-results (cons
                                                                     (cons amount y-point)
                                                                     cur-results)))))
                                          ;; если результаты были, получаем новый
                                          ;; порог белых точек
                                          (let* ((last-result (car cur-results))
                                                 (white (cdr (car last-result)))
                                                 ;; вызываем анализ с этим порогом
                                                 (amount (analysis
                                                          (xor-area image-up
                                                                    image-down
                                                                    y-point)
                                                          y-point white)))
                                            ;; (format out "% white ~A" white)
                                            ;; (format out "~% --- before
                                            ;;                    ~% y-point ~A ~% name ~A
                                            ;;                    ~% amount ~A
                                            ;;                     %---"
                                            ;;         y-point name amount)

                                            ;; если какой-то результат получен,
                                            (if amount
                                                ;; записываем в в текущий пулл результатов
                                                ;; (format out "~% amount ~A" amount)
                                                (progn
                                                  ;; (format out "~% ---
                                                  ;;            ~% y-point ~A ~% name ~A
                                                  ;;            ~% amount ~A"
                                                  ;;         y-point name amount)
                                                  (setf cur-results (cons
                                                                     (cons amount
                                                                           y-point)
                                                                     cur-results))
                                                  )))))))
                                ;; сортируем результаты
                                ;; по количеству черных точек
                                ;; от самого выского результата до самого низкого
                                (format out " ~% cur-results ~A" cur-results)
                                (let* ((best-res (find-best cur-results))
                                       (new-result (aref results
                                                         (fill-pointer results))))

                                  (format out "~% sorted-result ~A" best-res)
                                  (setf (result-white new-result) (cdr (car best-res))
                                        (result-black new-result) (car (car best-res))
                                        (result-y-point new-result) (cdr best-res)
                                        (result-image-up new-result)
                                        (task-image-up cur-task)
                                        (result-image-down new-result)
                                        (task-image-down cur-task))
                                  ;; записываем лучший результат
                                  (bt:with-lock-held (lock)
                                    (vector-push new-result results))
                                  ;;(format out "~% results ~A" results)
                                  ;; идем снова брать таск
                                  (go top))))))))))
        ;; сохраняем имя потока
          (setf thread-names (cons name thread-names)))
        )
      (tagbody
       check-threads
         ;; (format t "~% results ~A tasks~A" (length results)
         ;;         (length tasks))
         ;; счетчик живых потоков
         (let ((alive-threads 0))
           (do ((i 0 (incf i)))
               ((= i (length thread-names)))
             ;;(format t "~% nth ~A thread-name ~A" i (nth i thread-names))
             ;; если поток жив
             (if (bt:thread-alive-p (nth i thread-names))
                 ;; (format t "~% alive ~A "(nth i thread-names))
                 ;; инкрементируем счетчик
                 (incf alive-threads)))
           ;;(format t "~% alive threads ~A " alive-threads)
           ;; если живых потоков нет
           (if (eql 0 alive-threads)
               ;; возвращаем результаты
               ;;(progn
               ;;(format t "~% results ~A" results)
               ;;(return-from get-area-merge-results results)
               (return-from make-threads t)
               ;; иначе проверяем снова
               (progn
                 (sleep .5)
                 ;;(format t "~% wait")
                 (go  check-threads)))))))


    (in-package  #:cl-autogui)
  (defun make-roll (num-of-cores)
          (tagbody
           top
             (format t "~% make-roll")
             (format t "~% top f-p results ~A"
                     (fill-pointer results))
             ;; результаты анализа есть?
             (if (eql (fill-pointer results) 0)
                 ;; нет
                 (progn
                   (sleep 10)
                   (if (eql (fill-pointer results) 0)
                       ;; все еще нет
                       (format t "~% no results")
                       (go top)))
                   ;; результаты есть!
                 (tagbody
                  append-images
                    (format t "~% append-images")
                    ;; получаем результаты анализа
                    (let* ((cur-task (vector-pop results))
                           (image-up (result-image-up cur-task))
                           (image-down (result-image-up cur-task))
                           (y-point (result-y-point cur-task)))
                      (destructuring-bind (height-down width-down)
                          (array-dimensions image-down)
                        (destructuring-bind (height-up width-up)
                            (array-dimensions image-up)
                          (format t "~% height-down ~A width-down ~A height-up ~A width-up ~A"
                                  height-down width-down height-up width-up)))
                      (let ((appended-image (append-image image-up image-down y-point)))
                        ;; пушим результаты склейки
                        (vector-push appended-image append-results))
                      ;; результаты аналза кончились?
                      (if (not (eql (fill-pointer results) 0))
                          ;; нет!
                          (go append-images)
                          ;; картинка получилась одна?
                          (if (eql (fill-pointer append-results) 1)
                              ;; да!
                              (let ((roll (vector-pop append-results)))
                                (destructuring-bind (height width)
                                    (array-dimensions roll)
                                  (save-png width height "~/Pictures/resut.png"
                                            roll :grayscale)))
                              ;; нет!
                              ;; делаем таски из новых изображений
                              (let ((flag)
                                    (image-down)
                                    (image-up))
                                (tagbody
                                 do-tasks
                                   (if (null flag)
                                       (progn
                                         (setf image-down (vector-pop append-results))
                                         (setf image-up (vector-pop append-results))
                                         (setf flag 1)
                                         (create-task image-up image-down)
                                         (do ((i 0 (incf i)))
                                             (( = i (fill-pointer tasks)))
                                           (let ((fuck (aref tasks i)))
                                             (format t "~% last y-point ~A"
                                                     (last (task-y-points fuck)))
                                             (destructuring-bind (height-down width-down)
                                                 (array-dimensions (task-image-down fuck))
                                               (destructuring-bind (height-up width-up)
                                                   (array-dimensions (task-image-up fuck))
                                                 (format t "~% task height-down ~A width-down ~A height-up ~A width-up ~A"
                                                         height-down width-down
                                                         height-up width-up))))
                                           )

                                         ;; склеенные картинки кончились?
                                         (if (not (eql (fill-pointer append-results) 0))
                                             ;; нет
                                             (go do-tasks)
                                             ;; да
                                             (progn
                                               (make-threads num-of-cores)
                                               (format t "~% nil go top")
                                               (format t "~% nil f-p ~A"
                                                       (fill-pointer results))
                                               (go top))))
                                       (progn
                                         (setf image-down (aref append-results
                                                                (fill-pointer
                                                                 append-results)))
                                         (setf image-up (vector-pop append-results))
                                         (create-task image-up image-down)
                                         (do ((i 0 (incf i)))
                                             (( = i (fill-pointer tasks)))
                                           (let ((fuck (aref tasks i)))
                                             (format t "~% last y-point ~A"
                                                     (nth 0 (task-y-points fuck)))
                                             (destructuring-bind (height-down width-down)
                                                 (array-dimensions (task-image-down fuck))
                                               (destructuring-bind (height-up width-up)
                                                   (array-dimensions (task-image-up fuck))
                                                 (format t "~% task height-down ~A width-down ~A height-up ~A width-up ~A"
                                                         height-down width-down
                                                         height-up width-up))))
                                           )

                                         ;; склеенные картинки кончились?
                                         (if (not (eql (fill-pointer append-results) 0))
                                             ;; нет
                                             (go do-tasks)
                                             ;; да
                                             (progn
                                               (make-threads num-of-cores)
                                               (format t "~% flag t go top")
                                               (format t "~% t f-p ~A"
                                                       (fill-pointer results))
                                               (go top))))))))))))
             ))


  (let ((cnt 0))
    (defun make-roll-test ()
      (format t "~% make-roll: amount of results ~A"
              (fill-pointer results))
      (tagbody
       append-images
         (format t "~% append-images")
         ;; получаем результаты анализа
         (let* (;; (cur-task (vector-pop results))
                (cur-task (aref results cnt))
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
                         (format nil "~~/Pictures/resut~A.png" cnt)
                         appended-image :grayscale))
             (incf cnt)
             ;; пушим результаты склейки
             (vector-push appended-image append-results))

           ;; результаты аналза кончились?
           ;; (if (not (eql (fill-pointer results) 0))
           (if (not (eql cnt (fill-pointer results)))
               ;; нет!
               (go append-images)
               t)))))


    (defun get-area-merge-results (num-of-cores)
      (let* ((lock (bt:make-lock))
             (thread-names)
             (screen-cnt 0)
             (cnt 0))
        ;; генерим потоки
        (do ((i 0 (incf i)))
            ((= i (- num-of-cores 1)))
          (multiple-value-bind (name value)
              (intern (format nil "thread~A" i))
            (format t "~%  thread ~A" name)
            (in-package  #:cl-autogui)

(setf name
      (bt:make-thread
       (lambda ()
         (with-open-file (out (format nil "thread~A" i) :direction :output
                              :if-exists :supersede)
           ;; (format out "~% ~A" tasks)
           (tagbody
            top
              (format out "~% thread ~A" name)
              ;;(format out "~%  f-p tasks ~A" (fill-pointer tasks))
              ;; если таск есть, заюираем немедленно
              (let* ((cur-task (if (not (eql (fill-pointer tasks) 0))
                                   (bt:with-lock-held (lock)
                                     (vector-pop tasks)))))
                ;; ОЖИДАЕМ ТАСКИ
                (if (null cur-task)
                    (if (eql (fill-pointer tasks) 0)
                        (progn
                          (sleep 6)
                          ;; если тасков так и нет
                          (if (eql (fill-pointer tasks) 0)
                              ;; выход
                              nil
                              ;; иначе идем забирать таск
                              (go top)))
                        ;; таск появился!
                        ;; забираем
                        (go top))
                    ;; ТАСК ЕСТЬ
                    (let* ((image-up)
                           (image-down)
                           (y-points)
                           (cur-results))
                      ;; (format out "~% get-area f-p ~A "
                      ;;         (fill-pointer tasks))
                      ;; получаем данные из таска
                      (setf image-up (make-bit-image
                                      (task-image-up cur-task))
                            image-down (make-bit-image
                                        (task-image-down cur-task))
                            y-points (task-y-points cur-task))

                      ;;(format out "~%  y-points ~A "  y-points)
                      (format out " ~% task image-up ~A image-down ~A"
                              (task-image-up-path cur-task)
                              (task-image-down-path cur-task))
                      (force-output out)
                      ;; (format out "~% y-point ~A ~% name ~A
                      ;;                ~% image-up ~A ~% image-down ~A"
                      ;;         (car cur-task) name image-up image-down)

                      ;; начинаем анализ
                      (do ((i (length y-points) (- i 1)))
                          ((= i 0))
                        ;; получаем текущий y-point
                        (let ((y-point (car y-points)))
                          ;; убираем его из списка y-point-ов
                          (setf y-points (cdr y-points))
                          ;; если это первая итерация цикла и нет данных
                          ;; и никаких результатов еще нет
                          (if (null cur-results)
                              ;; анализируем изображение с текущим y-point
                              ;; и допустимым кол-вом белых точек по умолчанию
                              (let ((amount (analysis
                                             (xor-area image-up
                                                       image-down y-point)
                                             y-point)))
                                ;; (format out "~% --- before
                                ;;                  ~% y-point ~A ~% name ~A
                                ;;                  ~% amount ~A"
                                ;;         y-point name amount)
                                ;; если какой-то результат получен,
                                (if amount
                                    (progn
                                      (format out "~% ----
                                                     ~% IN DO
                                                     ~% y-point ~A ~% image-up ~A
                                                     ~% image-down ~A
                                                     ~% black. white ~A
                                                      % ----"
                                              y-point
                                              (task-image-up-path cur-task)
                                              (task-image-down-path
                                               cur-task)
                                              amount)
                                      (setf cur-results (cons
                                                         (cons amount
                                                               y-point)
                                                         cur-results)))))
                              ;; если результаты были, получаем новый
                              ;; порог белых точек
                              (let* ((last-result (car cur-results))
                                     (white (cdr (car last-result)))
                                     ;; вызываем анализ с этим порогом
                                     (amount (analysis
                                              (xor-area image-up
                                                        image-down
                                                        y-point)
                                              y-point white)))
                                ;;(format out "% white ~A" white)
                                ;; если какой-то результат получен,
                                (if amount
                                    ;; записываем в в текущий пулл результатов
                                    ;; (format out "~% amount ~A" amount)
                                    (progn
                                      (format out "~% ----
                                                     ~% IN DO
                                                     ~% y-point ~A ~% image-up ~A
                                                     ~% image-down ~A
                                                     ~% black. white ~A
                                                      % ----"
                                              y-point
                                              (task-image-up-path cur-task)
                                              (task-image-down-path
                                               cur-task)
                                              amount)
                                      (setf cur-results (cons
                                                         (cons amount
                                                               y-point)
                                                         cur-results))
                                      ))))))
                      ;; сортируем результаты
                      ;; по количеству черных точек
                      ;; от самого выского результата до самого низкого
                      ;;(format out " ~% cur-results ~A" cur-results)
                      (let* ((best-res (find-best cur-results))
                             (new-result (aref results
                                               (fill-pointer results)))
                             (appended-image))

                        ;;(format out "~% sorted-result ~A" best-res)
                        (setf (result-white new-result) (cdr (car best-res))
                              (result-black new-result) (car (car best-res))
                              (result-y-point new-result) (cdr best-res)
                              (result-image-up new-result)
                              (task-image-up cur-task)
                              (result-image-down new-result)
                              (task-image-down cur-task))
                        ;; выводим результат
                        (format out "~% ----
                                                     ~% y-point ~A ~% image-up ~A
                                                     ~% image-down ~A
                                                     ~% amount ~A
                                                      % ----"
                                (result-y-point new-result)
                                (task-image-up-path cur-task)
                                (task-image-down-path cur-task)
                                (result-black new-result))
                        (force-output out)

                        (setf appended-image (append-image
                                              (result-image-up new-result)
                                              (result-image-down new-result)
                                              (result-y-point new-result)))
                        ;; сохраняем склеенную картинку
                        (bt:with-lock-held (lock)
                          (destructuring-bind (height width)
                              (array-dimensions appended-image)
                            (save-png width height
                                      (format nil "~~/Pictures/resut~A.png"
                                              cnt)
                                      appended-image :grayscale)
                            (incf cnt)))
                        ;; записываем результаты в пулл результатов
                        (bt:with-lock-held (lock)
                          (vector-push new-result results))
                        ;;(format out "~% results ~A" results)
                        ;; идем снова брать таск
                        (go top))))))))))
;; сохраняем имя потока
(setf thread-names (cons name thread-names)))
)

          (in-package  #:cl-autogui)

          ;; после того, как создали все потоки и записали их имена,
          ;; скриним экран
          (tagbody
           get-data
             ;;скриним и составляем таски
             (get-data (format nil
                               "~~/Pictures/screen~A.png"
                               screen-cnt)
                       (format nil
                               "~~/Pictures/screen~A.png"
                               (incf screen-cnt)))
             (incf screen-cnt)

             (sleep 8)
             (format t "~% length results ~A" (fill-pointer results))
             ;;(format t "~% length tasks ~A" (fill-pointer tasks))
             ;;(format t "~% length results ~A" (car results))
             (if (eql (fill-pointer results) 0)
                 ;; пока не дойдем до последней пары картинок
                 (go get-data)
                 (let ((f-p (fill-pointer results)))
                   (do ((i 0 (incf i)))
                       (( = i (fill-pointer results)))
                     (let ((struct (aref results i)))
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
                            (let ((alive-threads 0))
                              (do ((i 0 (incf i)))
                                  ((= i (length thread-names)))
                                ;;(format t "~% nth ~A thread-name ~A" i (nth i thread-names))
                                ;; если поток жив
                                (if (bt:thread-alive-p (nth i thread-names))
                                    ;; (format t "~% alive ~A "(nth i thread-names))
                                    ;; инкрементируем счетчик
                                    (incf alive-threads)))
                              ;;(format t "~% alive threads ~A " alive-threads)
                              ;; если живых потоков нет
                              (if (eql 0 alive-threads)
                                  ;; возвращаем результаты
                                  ;;(progn
                                  ;;(format t "~% results ~A" results)
                                  ;;(return-from get-area-merge-results results)
                                  (progn
                                    ;;(make-roll num-of-cores)
                                    ;;(make-roll-test)
                                    (format t "~% results ~A" (fill-pointer results))
                                    (return-from get-area-merge-results t))
                                  ;; иначе проверяем снова
                                  (progn
                                    (sleep .5)
                                    ;;(format t "~% wait")
                                    (go  check-threads))))))
                       (go get-data)
                       )
                   )
                 )
             )
          )))
       (defun demo-get-area-merge-results ()
         (let ((screen-cnt 0))
           (tagbody top
              ;;  (get-data-ofline (format nil
              ;;                           "~~/Pictures/screen~A.png"
              ;;                           screen-cnt)
              ;;                   (format nil
              ;;                           "~~/Pictures/screen~A.png"
              ;;                           (incf screen-cnt)))
              ;; (incf screen-cnt)

              (get-data (format nil
                                "~~/Pictures/screen~A.png"
                                screen-cnt)
                        (format nil
                                "~~/Pictures/screen~A.png"
                                (incf screen-cnt)))
              (incf screen-cnt)

              ;; делаем таски для 3 картинок
              (if (< screen-cnt 8)
                  (go top)
                  (progn
                    (format t "~% demo: amount of tasks ~A" (fill-pointer tasks))
                    (let* ((image-up)
                           (image-down)
                           (y-points)
                           (cur-results)
                           (cur-task))
                      (do ((i 0 (incf i)))
                          (( = i (fill-pointer tasks)))
                        ;; получаем данные из таска
                        (setf cur-task (aref tasks i)
                              image-up (make-bit-image (task-image-up cur-task))
                              image-down (make-bit-image (task-image-down cur-task))
                              y-points (task-y-points cur-task))
                        (do ((i (length y-points) (decf i)))
                            (( = i 0))
                          (let*((y-point (car y-points))
                                (amount (analysis (xor-area image-up image-down y-point)
                                                  y-point)))
                            (setf y-points (cdr y-points))
                            (if amount
                                ;; записываем в текущий пулл результатов
                                (progn
                                  (setf cur-results (cons
                                                     (cons amount
                                                           y-point)
                                                     cur-results))))))
                        ;; (format t "~% cur-results i: ~A
                        ;;          ~% ~A" i cur-results)
                        ;; отсортировали результаты анализа текущего таска
                        (let* ((best-res (find-best cur-results))
                               (new-result (aref results
                                                 (fill-pointer results))))
                          ;; (format t "~% i: ~A best-result ~A" i best-res)

                          (setf (result-white new-result) (cdr (car best-res))
                                (result-black new-result) (car (car best-res))
                                (result-y-point new-result) (cdr best-res)
                                (result-image-up new-result)
                                (task-image-up cur-task)
                                (result-image-down new-result)
                                (task-image-down cur-task))

                          ;; записываем лучший результат
                          (vector-push new-result results)
                          (setf cur-results nil)
                          ))
                      (make-roll-test)
                      ))))))

      ))

(in-package  #:cl-autogui)

  (time
  (block make-task-test
     (open-browser "/usr/bin/firefox" "https://spb.hh.ru/")
     (sleep 8)
     (let ((result (get-area-merge-results 4)))
     )))

  ;; (time
  ;;  (block ofline-demo-test
  ;;    (demo-get-area-merge-results)))

  (time
   (block online-demo-test
     (open-browser "/usr/bin/firefox" "https://spb.hh.ru/")
     (sleep 8)
     (demo-get-area-merge-results)))
