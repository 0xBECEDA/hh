(eval-when (:compile-toplevel :load-toplevel :execute)
  #-clx
  (ql:quickload 'clx)
  #-zpng
  (ql:quickload 'zpng)
  #-cffi
  (ql:quickload 'cffi)
  (ql:quickload "png-read")
  )

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

(defparameter *out-text* "/home/sonja/repo/org/cl-dino-master/out~A")
(defparameter *langs* "rus+eng")
(defparameter *default-width* 1295)
(defparameter *teaser-width* 690)
(defparameter *snap-width* 755)
(defparameter *snap-height* 668)
(defparameter *snap-x* 440)
(defparameter *default-x* 60)
(defparameter *default-y* 37)
(defparameter *new-image-array* nil)
(defparameter *mouse-left* 1)
(defparameter *mouse-middle* 2)
(defparameter *mouse-right* 3)
(defparameter *R-usual* 231)
(defparameter *G-usual* 231)
(defparameter *B-usual* 231)
(defparameter *R-priority* 241)
(defparameter *G-priority* 200)
(defparameter *B-priority* 70)
(defparameter *hh-teaser-url*
  "https://hh.ru/search/vacancy?L_is_autosearch=false&area=2&clusters=true&enable_snippets=true&items_on_page=100&only_with_salary=true&salary=165000&specialization=1.221&page=~A"
  "https://spb.hh.ru/search/vacancy?L_is_autosearch=false&area=1&clusters=true&enable_snippets=true&items_on_page=100&only_with_salary=true&salary=165000&specialization=1.221&page=~A")

(defparameter *browser-path*  "/usr/bin/firefox")


;; (let* ((arr-up (load-png "~/Pictures/test0.png"))
;;        (arr-down (load-png "~/Pictures/test0.png"))
;;        (new-arr (append-xor arr-up arr-down 300)))
;;   (destructuring-bind (height width depth)
;;       (array-dimensions new-arr)
;;     (format t "~% ~A" (count-black new-arr))
;;     (save-png width height "~/Pictures/xor.png" new-arr)))




;; ------------------ append-image BEGIN

(defun append-image (image-up image-down y-point)
  "Принимает 2 массива изображений и высоту,
   где второе изображение будет наложено на первое.
   Изображения должны быть одинаковой ширины
   и иметь одинаковое количество байт на пиксель.
   Возвращает склеенный массив"
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
        ;; смотря что случиться раньше
        (if (null colors-down)  ;; TODO: тут надо проверять цвета первой картинки
            (cycle (0 0 (min height-down y-point) width-down)
                   (setf (aref image-new qy qx)
                         (aref image-up qy qx)))
            ;; else
            (cycle (0 0 (min height-down y-point) width-down)
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

;; ------------------ append-image END


;; ------------------ append-xor BEGIN

(defun append-xor (image-up image-down y-point)
  "Принимает 2 массива изображений и высоту,
   где второе изображение будет наложено на первое
   с помощью XOR.
   Изображения должны быть одинаковой ширины
   и иметь одинаковое количество байт на пиксель.
   Возвращает склеенный массив"
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
          ;; от ее начала до до ее конца (NB: тут отличие от append-image)
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
                       (do ((rz 0 (incf rz)))
                           ((= rz colors-down))
                         (setf (aref image-new new-y qx rz)
                               (logxor (aref image-new new-y qx rz)
                                       (aref image-down qy qx rz)))))
                ;; поправим излишне поксоренный альфа-канал (если он есть)
                ;; но только там где изображения перекрываются (!)
                (when (equal 4 colors-down)
                  (let ((new-y y-point))
                    (cycle (0 0 (- height-up y-point) width-up (incf new-y))
                           (do ((rz 0 (+ colors-down rz)))
                               ((= rz colors-down))
                             ;; (setf (aref image-new new-y qx (+ 3 rz))
                             ;;       #xFF)
                             ;; проверка правильности заксоривания -
                             ;; можно убрать после отладки
                             ;; (setf (aref image-new new-y qx (+ 2 rz)) #xFF)
                             )))))))
        image-new))))

;; (block test-append-xor-fullcolor
;;   (let* ((arr1 (x-snapshot :x 0 :y 0 :width 755 :height 300))
;;          (arr2 (x-snapshot :x 100 :y 100 :width 755 :height 300))
;;          (array (append-xor arr1 arr2 200)))
;;     (destructuring-bind (height width  &rest rest)
;;         (array-dimensions array)
;;       (save-png width height "~/Pictures/result.png" array))))

;; (block test-append-xor-grayscale
;;   (let* ((arr1 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300)))
;;          (arr2 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300)))
;;          (array (append-xor arr1 arr2 200)))
;;     (destructuring-bind (height width  &rest rest)
;;         (array-dimensions array)
;;       (save-png width height "~/Pictures/result.png" array :grayscale))))

;; ------------------ append-xor END


;; ------------------ analysis BEGIN

(defun analysis (xored-image y-point)
  (destructuring-bind (height width &optional colors)
      (array-dimensions xored-image)
    (let ((intesect-height (- height y-point)) ;; высота пересечения
          (black 0))
      (format t "~% intesect-height ~A " intesect-height)
      (macrolet ((cycle ((py px height width)
                         &body body)
                   `(do ((qy ,py (incf qy)))
                        ((= qy ,height))
                      (do ((qx ,px (incf qx)))
                          ((= qx ,width))
                        ,@body))))
        (if colors
            (cycle (y-point 0 height width)
                   (when (and (eql (aref xored-image qy qx 0) 0)
                              (eql (aref xored-image qy qx 1) 0)
                              (eql (aref xored-image qy qx 2) 0))
                     (incf black)))
            ;; else
            (cycle (y-point 0 height width)
                   (when (eql (aref xored-image qy qx) 0)
                     (incf black))))
        (let* ((pix-amount (* intesect-height width))
               (result (float (/ black pix-amount))))
          result)))))

(defun get-merge-results (image-up image-down)
  (do ((vy 0 (incf vy)))
      ((= vy (+ (array-dimension image-up 0)
                (array-dimension image-down 0))))
    (format t "~%: =vy: ~A = ~A"
            vy
            (analysis
             (append-xor image-up image-down vy)
             vy))))

(block test-merge-results-fullcolor
  (time
   (let* ((arr1 (x-snapshot :x 0 :y 0 :width 192 :height 108))
          (arr2 (x-snapshot :x 0 :y 0 :width 192 :height 108)))
     (get-merge-results arr1 arr2))))

;; (block test-merge-results-grayscale
;;   (time
;;    (let* ((arr1 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300)))
;;           (arr2 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300))))
;;      (get-merge-results arr1 arr2))))

;; ------------------ analysis END

;;------------------- BEGIN

    (defun make-bit-image (image-data)
     (destructuring-bind (height width)
         (array-dimensions image-data)
        (let* ((new-width (+ (logior width 7) 1))
               (bit-array (make-array (list height new-width)
                                    :element-type 'bit)))
         (do ((qy 0 (incf qy)))
             ((= qy height))
           (do ((qx 0 (incf qx)))
               ((= qx width))
             ;;(format t "~% y ~A x ~A elt ~A" qy qx (aref image-data qy qx))
             (unless (equal (aref image-data qy qx) 0)
               (setf (bit bit-array qy qx) 1))))
         bit-array)))

;; (block make-bit-image
;;   (x-snapshot :x 0 :y 0 :width 45 :height 20 :path "~/Pictures/test.png")
;; (let ((bit-arr1
;;        (make-bit-image (binarization (load-png "~/Pictures/test.png") 100 )))
;;       )
;;       ;; (bit-arr2
;;       ;;  (make-bit-image (binarization (load-png "~/Pictures/test.png") 100 ))))

;;   (destructuring-bind (height width)
;;       (array-dimensions bit-arr1)
;;     (do ((qy 0 (incf qy)))
;;         ((= qy height))
;;       (format t "~%")
;;       (do ((qx 0 (incf qx)))
;;           ((= qx width))
;;         (format t "~A" (aref bit-arr qy qx)))))

;;   ))


;; (block ttt333
;;   (x-snapshot :x 0 :y 0 :width 45 :height 20 :path "~/Pictures/test.png")
;;   (let* ((arr (binarization (load-png "~/Pictures/test.png") 100 )))
;;   (destructuring-bind (height width)
;;             (array-dimensions arr)
;;           (save-png width height "~/Pictures/test.png" arr :grayscale))))

TEST binarization
(let* ((arr (binarization (load-png "~/Pictures/test0.png") 200)))
  (destructuring-bind (height width)
      (array-dimensions arr)
    ;;(format t "~% h ~A w ~A" height width)
    (save-png width height "~/Pictures/test3.png" arr :grayscale)))

;;------------------- END


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


;; ;; TEST: saving loaded data
;; (let* ((from "~/Pictures/test0.png")
;;        (to   "~/Pictures/test1.png")
;;        (image-data (load-png from)))
;;   (destructuring-bind (height width depth)
;;       (array-dimensions image-data)
;;     (save-png width height to image-data)))

;; ;; TEST: saving screenshot data
;; (let* ((to   "~/Pictures/snap4.png")
;;        (image-data (x-snapshot :x 0 :y 0 :width 20 :height 30)))
;;   (destructuring-bind (height width depth)
;;       (array-dimensions image-data)
;;     (save-png width height to image-data)))



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


;; Ошибка, возникающая когда мы пытаемся прочитать png
;; в котором неизвестно сколько байт на точку
(define-condition unk-png-color-type (error)
  ((color :initarg :color :reader color))
  (:report
   (lambda (condition stream)
     (format stream "Error in LOAD-PNG: unknown color type: ~A"
             (color condition)))))

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

;; ;; TEST: equality screenshot and load-file-data
;; (assert (equalp (progn
;;                   (x-snapshot :path "~/Pictures/snap2.png")
;;                   (load-png "~/Pictures/snap2.png"))
;;                 (x-snapshot)))



;; (let* ((image (load-png "cell.png"))
;;        (image (binarization image)))
;;   (destructuring-bind (dw dh)
;;       (array-dimensions image)
;;     (save-png-gray dw dh "cell1.png" (vectorize-image-gray image))))



(block test
  (open-browser "/usr/bin/firefox"
                 *hh-teaser-url*)
  (sleep 8)
  (do ((i 0 (+ 1 i)))
      ((= i 6))
    (perform-key-action t 116)
    (perform-key-action nil 116))
  (sleep 1)
 (x-snapshot :x 440 :y 100 :width  *snap-width* :height 668
             :path "/home/sonja/Pictures/test0.png"))
;;  (let* ((arr (load-png "~/Pictures/test0.png"))
;;         (array (rewrite-array arr)))
;;     (sleep 1)
;;     (crop-teaser array
;;                  "/home/sonja/Pictures/test0.png")

;;     (let* ((image (load-png "~/Pictures/test0.png"))
;;            (image (binarization image)))
;;       (destructuring-bind (dw dh)
;;           (array-dimensions image)
;;         (save-png-gray dw dh "~/Pictures/test0.png" (vectorize-image-gray image))))

;;     (sleep 1)
;;     ;;(resize "/home/sonja/Pictures/test0.png")
;;     (run-tess "/home/sonja/Pictures/test0.png" "/home/sonja/repo/org/cl-dino-master/out"
;;               *langs*)))

;; (block little-test
;;   ;; (open-browser "/usr/bin/firefox"
;;   ;;               *hh-teaser-url*)
;;   ;; (sleep 8)
;;   (let ((image-array-up (x-snapshot :x 440 :y 100
;;                                     :width *snap-width*
;;                                     :height *snap-height*)))
;;     ;; (sleep 1)
;;     ;; (perform-key-action t 117)
;;     ;; (sleep .1)
;;     ;; (perform-key-action nil 117)
;;     ;; (sleep 1)
;;     (let ((image-array-down
;;            (x-snapshot :x 440 :y 100
;;                        :width *snap-width*
;;                        :height *snap-height*)))
;;       ;; (let ((point (find-row image-array-up image-array-down
;;       ;;                        (/ (array-dimension image-array-up 0) 2))))
;;       ;;  (format t "~% ~A" point)
;;       ;;)))))
;;       ;;(if point
;;       (progn
;;         (let* ((array (append-image image-array-up image-array-down
;;                                     (- *snap-height* 1)))
;;                (width (array-dimension array 1))
;;                (height (array-dimension array 0)))
;;           (save-png width height
;;                     "~/Pictures/result.png"
;;                     (my-vectorize-image
;;                      array)))))))

;;-------------------------------------------------------------------------------------
(defun run-tess (input-image output-text &optional (langs "eng"))
  (let ((proc (sb-ext:run-program "/usr/bin/tesseract"
                                  ;; `(,input-image ,output-text "-l" ,langs)
                                  ;; :input :stream :output :stream)))
                                  `(,input-image ,output-text "-l" ,langs)
                                  :input :stream :output :stream)))
    (if proc
        ;; открываем поток тесеракта на чтение
        (with-open-stream (input (sb-ext:process-input proc))
          ;; открыли поток тесеракта на запись
          (with-open-stream (output (sb-ext:process-output proc))
            (do ((line (read-line output nil 'eof)
                       (read-line output nil 'eof)))
                ((eql line 'eof))
              (format t "~% ~A" line)
              (force-output output))))
        (format t "~% didn't run tesseract"))))

(defun resize (image)
  (let ((proc (sb-ext:run-program
               "/usr/lib/i386-linux-gnu/ImageMagick-6.8.9/bin-Q16/mogrify"
               `("-resize"
                 "1500x502"
                 ,image)
               :input :stream :output :stream)))
    (if proc
        (with-open-stream (input (sb-ext:process-input proc))
          (with-open-stream (output (sb-ext:process-output proc))
            (do ((a-line (read-line output nil 'eof)
                         (read-line output nil 'eof)))
                ((eql a-line 'eof))
              (format t "~A" a-line)
              (force-output output))))
        (format t "~% resize: didn't run ImageMagic"))))

;; принимает массив пикселей картинки, которую должен вырезать
;; и путь для файла, куда сохранит новую картинку
;; путь для image-path должен быть абсолютным
(defun crop-teaser (image-array image-path)
  (multiple-value-bind (border-height x-begin y-begin)
      ;; ищем границы тизера
      (find-border image-array)
    (if (eql border-height 0)
        ;; нулевая высота, встретили блок рекламы
          (return-from crop-teaser -1)
        (if (and (null x-begin) (null y-begin))
            ;; вообще не нашли бордюр, тизеры кончились
              (return-from crop-teaser -2)
            (progn
              (format t "~% ~Ax~A+~A+~A"
                      (array-dimension image-array 1) border-height
                      x-begin y-begin)
              (let ((proc (sb-ext:run-program
                           "/usr/lib/i386-linux-gnu/ImageMagick-6.8.9/bin-Q16/mogrify"
                           `("-crop"
                             ,(format nil "~Ax~A+~A+~A"
                                      (array-dimension image-array 1) border-height
                                      x-begin y-begin)
                             ,image-path)
                           :input :stream :output :stream)))
                (if proc
                    (with-open-stream (input (sb-ext:process-input proc))
                      (with-open-stream (output (sb-ext:process-output proc))
                        (do ((a-line (read-line output nil 'eof)
                                     (read-line output nil 'eof)))
                            ((eql a-line 'eof))
                          (format t "~A" a-line)
                          (force-output output))
                        ;; 0 = успех
                        (return-from crop-teaser 0)))
                    (format t "~% crop-teaser: didn't run ImageMagic"))))))))

(defun find-border (image-array)
  ;; ищем левый верхний угол у вакансии (сначала ищем выделенные)
  (multiple-value-bind (x-begin y-begin)
      (first-point image-array *R-priority* *G-priority* *B-priority*)
    ;; нашли?
    (if (and x-begin y-begin)
        ;; да
        ;; получаем высоту тизера
        (multiple-value-bind (border-height)
            (get-height x-begin y-begin image-array
                      *R-priority* *G-priority* *B-priority*)
          (values border-height x-begin y-begin))
        ;; нет
        ;; ищем левый верхний угол у обычной вакансии
        (multiple-value-bind (x-begin y-begin)
            (first-point image-array *R-usual* *G-usual* *B-usual*)
          ;; нашли?
          (if (and x-begin y-begin)
              ;; да
              ;; проверяем, точно ли это тизер
              (progn
                (format t "~% usual")
                (if (check-width x-begin y-begin image-array *teaser-width*
                                 *R-usual* *G-usual* *B-usual*)
                    ;; да, это тизер
                    (multiple-value-bind (border-height)
                        (get-height x-begin y-begin image-array
                                  *R-usual* *G-usual* *B-usual*)
                      (values border-height x-begin y-begin))
                    ;; нет, это другой элемент
                    (progn
                      (format t "~% find-border: didn't find the border")
                      (values nil nil nil))))
              ;; не нашли даже даже точку вхождения в бордюр
              (progn
                (format t "~% find-border: didn't find the first point")
                (values nil nil nil)))))))

;; ищем верхний левый угол тизера
;; если текущий пиксель = цвет бордюра и следующий за ним справа имеет этот же цвет,
;; точка найдена
(defun first-point (image-array r g b)
  (do ((x 0 (+ x 1)))
      ((= x  (array-dimension image-array 1)))
    (do ((y 0 (+ y 1)))
        ((= y (array-dimension image-array 0))
      (if (and (eql r (aref image-array y x 0))
               (eql g (aref image-array y x 1))
               (eql b (aref image-array y x 2))
               (eql r (aref image-array y (+ x 1) 0))
               (eql g (aref image-array y (+ x 1) 1))
               (eql b (aref image-array y (+ x 1) 2)))
          (return-from first-point (values x y)))))
  ;; если после выполнения циклов точка не найдена
  (return-from first-point (values nil nil))))


;; ищем высоту
(defun get-height (x y image-array r g b)
  (format t "~% x ~A y ~A" x y)
  (let ((height 0))
    (do ((i (+ y 1) (+ i 1)))
        ((= i (array-dimension image-array 0)) height)
      ;; если пиксель экрана имеет цвет бордюра
      (if (and (eql r (aref image-array i x 0))
               (eql g (aref image-array i x 1))
               (eql b (aref image-array i x 2)))
          ;; а соседний справа не имеет
          (if  (and (not (eql r (aref image-array i (+ x 1) 0)))
                    (not (eql g (aref image-array i (+ x 1) 1)))
                    (not (eql b (aref image-array i (+ x 1) 2))))
               ;; увеличиваем высоту
               (setf height (+ height 1))
               ;; в противном случае мы достигли нижней границы = левый нижний угол
               (return-from get-height height))))))

;; принимает X и Y левого угла тизера, массив пикселей скрина
;;
(defun check-width (x y image-array teaser-width r g b)
  (let ((width 0))
    (format t "~% x ~A y ~A" x y)
    (do ((i x (+ i 1)))
        ((= i (array-dimension image-array 1)) width)
      (if (and (eql r (aref image-array y i 0))
               (eql g (aref image-array y i 1))
               (eql b (aref image-array y i 2)))
          (setf width (+ width 1))))
    (if (eql width teaser-width)
        (progn
          (values width))
        (format t "~% check-width: it's not a teaser"))))



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
        (format t "~% resize: didn't run ImageMagic"))))


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
  ;;     "This macro defun a function which witch do mouse or keyboard actions,
  ;; body is called on each action."
  `(defun ,name ,params
     (mapcar
      #'(lambda (action)
          ,@body)
      (mklist ,actions))))

(defun perform-mouse-action (press? button &key x y)
  (and x y (x-move x y))
  (with-default-display-force (d)
    (xtest:fake-button-event d button press?)))

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
                   ((> 0 ,clicks) ,pos) ; scroll up/right
                   ((< 0 ,clicks) ,neg)))) ; scroll down/left
     (dotimes (_ (abs ,clicks))
       (x-click :button button :x ,x :y ,y))))

(defun x-vscroll (clicks &key x y)
  (with-scroll 4 5 clicks x y))

(defun x-scroll (clicks &key x y)
  (x-vscroll clicks :x x :y y))

(defun x-hscroll (clicks &key x y)
  (with-scroll 7 6 clicks x y))

(defun perform-key-action (press? keycode) ; use xev to get keycode
  (with-default-display-force (d)
    (xtest:fake-key-event d keycode press?)))

(macrolet ((def (name actions)
             `(defun-with-actions ,name (keycode)
                  ,actions
                (funcall #'perform-key-action
                         action keycode))))
  (def x-key-down t)
  (def x-key-up nil)
  (def x-press '(t nil)))

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

(multiple-value-bind (default-width default-height) (x-size)
  (defun x-snapshot (&key (x *default-x*) (y *default-y*)
                       (width  *default-width*) (height *default-heght*)
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
            (zpng:data-array image))))))

;; ;; TEST: save screenshot
;; (x-snapshot :path "~/Pictures/snap1.png")


;; (defun start ()
;;   (do ((i 0 (+ 1 i)))
;;       ((= i 6))
;;     (perform-key-action t 116)
;;     (perform-key-action nil 116))
;;   (sleep 1)
;;   (get-images)
;;   (sleep 5)
;;   (setf *image-amount* (*image-amount* -1))
;;   (do ((i 0 (+ i 1)))
;;       ((= i *image-amount*))
;;     (resize `,(format nil
;;                       "/home/sonja/repo/org/cl-dino-master/test~A.png"
;;                       i))
;;     (sleep 1)
;;     (run-tess `,(format nil
;;                         "/home/sonja/repo/org/cl-dino-master/test~A.png"
;;                         i) `,(format nil
;;                                      "/home/sonja/repo/org/cl-dino-master/out~A"
;;                                      i) *langs*)))

;; (defun get-images ()
;;   (let ((crop-marker) (cnt 4))
;;     (do ((j 0 (+ j 1)))
;;         (nil)
;;       (do ((j 0 (+ j 1)))
;;           ((= j cnt))
;;         (perform-key-action t 116)
;;         (perform-key-action nil 116))
;;       (sleep 1)
;;       (x-snapshot :x 440 :width  *snap-width*
;;                   :path `,(format nil "test~A.png" *image-indx*))
;;       (sleep 1)
;;       (setf crop-marker
;;             (crop-teaser *new-image-array*
;;                          `,(format nil
;;                                    "/home/sonja/repo/org/cl-dino-master/test~A.png"
;;                                    *image-indx*)))
;;       (if (eql crop-marker -2)
;;           (progn
;;             (format t "~% get-images: no teasers anymore")
;;             (return-from get-images 1))

;;           (if (eql crop-marker -1)
;;               ;; встретили блок рекламы, пропускаем, не анализируя
;;               (setf cnt 2)
;;               (progn
;;                 (setf *image-indx* (+ *image-indx* 1))
;;                 (setf cnt 4)))))))
