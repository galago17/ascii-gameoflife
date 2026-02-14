(defparameter *grid* 
  (list 
    #(0 1)
    #(1 0)
    #(1 1)
    #(1 2)
    #(2 2)))
(defparameter *deadchar* #\Space)
(defparameter *alivechar* #\O)

(defun txt-grid (file)
  (let ((y 0)
        (x 0)
        (reversed (list))
        (grid (list)))
    (with-open-file (in file)
      (loop for line = (read-line in nil nil) while line do
            (setf reversed (cons line reversed))))
    (loop for line in reversed do
          (loop for char across line do
                (when (equal char *alivechar*)
                  (setf grid (cons (vector x y) grid)))
                (incf x))
          (setf x 0) 
          (incf y))
    grid))

(setf *grid* (txt-grid "pattern.txt"))

(defun getx (coord)
  (elt coord 0))
(defun gety (coord)
  (elt coord 1))

(defun access (coord)
  (find coord *grid* :test #'equalp))

(defun max-x ()
  (reduce #'max (mapcar (lambda (coord) (getx coord)) *grid*)))

(defun max-y ()
  (reduce #'max (mapcar (lambda (coord) (gety coord)) *grid*)))

(defun min-x ()
  (reduce #'min (mapcar (lambda (coord) (getx coord)) *grid*)))

(defun min-y ()
  (reduce #'min (mapcar (lambda (coord) (gety coord)) *grid*)))


(defun get-adjacent (coord)
  (list
    (vector (1- (getx coord)) (1+ (gety coord)))
    (vector (getx coord) (1+ (gety coord)))
    (vector (1+ (getx coord)) (1+ (gety coord)))

    (vector (1- (getx coord)) (gety coord))
    (vector (1+ (getx coord)) (gety coord))

    (vector (1- (getx coord)) (1- (gety coord)))
    (vector (getx coord) (1- (gety coord)))
    (vector (1+ (getx coord)) (1- (gety coord)))))

(defun neighbours (cell)
  (let ((adjacent
          (get-adjacent cell)))
   (remove-if-not #'access adjacent)))

(defun lives-p (coord)
  (let ((ncount (length (neighbours coord))))
    (cond
          ((and (access coord) (or (equal ncount 2) (equal ncount 3)))
           coord)
          ((and (not (access coord)) (equal ncount 3))
           coord)
          ((and (access coord) (< ncount 2))
           nil)
          ((and (access coord) (> ncount 3))
           nil)
          ((access coord)
           coord)
          (t nil))))

(defun generation ()
  (let ((newgrid (list))
        (undead (remove-duplicates (mapcan #'get-adjacent *grid*) :test #'equalp)))
    (loop for coord in *grid* do
          (when (lives-p coord)
            (setf newgrid (cons coord newgrid))))
    (loop for coord in undead do
          (when (lives-p coord)
            (setf newgrid (cons coord newgrid))))
    (setf *grid* (remove-duplicates newgrid :test #'equalp))
    undead))

(defmacro iterate-grid (action &optional action2)
  `(let ((max-x (max-x))
        (max-y (max-y)))
     (loop for y from (1+ max-y) downto (1- (min-y)) do
          (loop for x from (1- (min-x)) to (1+ max-x) do
                ,action)
          ,@(when action2 (list action2)))))
#|
(defun print-grid ()
  (iterate-grid 
    (if (access (vector x y))
                    (princ *alivechar*)
                    (princ *deadchar*))
    (format t "~%")))

|#
(defun print-grid ()
  (loop for y from (max-y) downto (min-y) do
        (if (member y (mapcar #'gety *grid*))
            (progn 
              (loop for x from (min-x) to (max-x) do
                         (if (access (vector x y))
                             (princ *alivechar*)
                             (princ *deadchar*)))
              (format t "~%"))
            (format t "~a~%" (make-string (max-x) :initial-element *deadchar*)))))

(defparameter *lastgrid* (list))
(defparameter *stop* 0) ; generation to stop at
(defparameter *delay* nil)

(defun main (&optional (counter 0))
  (if (and (> *stop* 0) (>= counter *stop*)) (return-from main))
  (when (null *grid*) (return-from main))
  (print-grid)
  ; (format t "~{~a~%~}" *grid*)
  (format t "~a, ~a x ~a, ~a alive" counter (1+ (max-x)) (1+ (max-y)) (length *grid*))
  (if (and (> *stop* 0) (>= counter *stop*)) (return-from main))

  (when *delay* (sleep *delay*)) ; to reduce flickering
  (format t "~c[2J" #\Esc)
  (generation)
  ; (format t "~a~%" counter)
  ; (format t "~a~%" (generation))
  (if (equal *grid* *lastgrid*)
      nil
      (progn
        (when (evenp counter)
              (setf *lastgrid* *grid*))
        (main (1+ counter)))))
