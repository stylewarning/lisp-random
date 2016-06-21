;;;; number-game.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(ql:quickload :alexandria)
(ql:quickload :queues.simple-queue)
(ql:quickload :queues.priority-queue)

(setq *print-circle* t)

(defconstant empty-cell 'empty-cell)

(defstruct cell
  value
  n e s w)

(defun stack (top bottom)
  (unless (null top)
    (setf (cell-s top) bottom))
  (unless (null bottom)
    (setf (cell-n bottom) top)))

(defun join (left right)
  (unless (null left)
    (setf (cell-e left) right))
  (unless (null right)
    (setf (cell-w right) left)))

(defconstant +row-capacity+ 9)

(defun make-row ()
  (make-array +row-capacity+
              :adjustable t
              :fill-pointer 0))

(defun row-filled? (row)
  (= +row-capacity+ (length row)))

(defun add-to-row (row c)
  (if (row-filled? row)
      (error "Trying to add to a filled row.")
      (vector-push c row)))

(defstruct (board (:print-function print-board))
  (moves nil)
  (positions (make-hash-table :test 'eq))
  (layout (let ((a (make-array (round 27 +row-capacity+)
                               :adjustable t
                               :fill-pointer 1)))
            (setf (aref a 0) (make-row))
            a)))

(defun deep-copy-board (board)
  (let ((copies (make-hash-table :test 'eq)))
    (labels ((cell-copy (cell)
               (if (null cell)
                   nil
                   (let ((existing-copy (gethash cell copies)))
                     (cond
                       ((null existing-copy)
                        (let ((copy (copy-cell cell)))
                          (setf (gethash cell copies) copy)
                          (setf (cell-n copy) (cell-copy (cell-n cell))
                                (cell-e copy) (cell-copy (cell-e cell))
                                (cell-s copy) (cell-copy (cell-s cell))
                                (cell-w copy) (cell-copy (cell-w cell)))
                          copy))
                       (t
                        existing-copy)))))
             (row-copy (r)
               (let ((new-r (alexandria:copy-array r)))
                 (map-into new-r #'cell-copy new-r)
                 new-r))
             (layout-copy (l)
               (let ((new-l (alexandria:copy-array l)))
                 (map-into new-l #'row-copy new-l)
                 new-l)))
      (make-board
       :moves (copy-seq (board-moves board))
       :layout (layout-copy (board-layout board))
       :positions (let ((new-hash (make-hash-table :test 'eq)))
                    (maphash (lambda (k v)
                               (setf (gethash (cell-copy k) new-hash) v))
                             (board-positions board))
                    new-hash)))))

(defun ref (board r c)
  (aref (aref (board-layout board) r) c))
(defun (setf ref) (new-value board r c)
  (setf (aref (aref (board-layout board) r) c) new-value))

(defun print-board (board stream depth)
  (declare (ignore depth))
  (print-unreadable-object (board stream :type t :identity t)
    (terpri stream)
    (loop :for row :across (board-layout board) :do
      (loop :for cell :across row :do
        (cond
          ((null cell)
           (write-char #\# stream))
          (t
           (format stream "~D" (cell-value cell))))
        (write-char #\Space stream))
      (terpri stream))))

(defun board-size (b)
  (hash-table-count (board-positions b)))

(defun num-rows (board)
  (length (board-layout board)))

(defun layout-neighbors (board r c)
  "Return N, E, S, and W neighbors."
  (assert (<= 0 r (1- (num-rows board))))
  (assert (<= 0 c (1- (length (aref (board-layout board) r)))))
  (values
   ;; N
   (if (zerop r) nil (ref board (1- r) c))
   ;; E
   (if (= c (1- +row-capacity+))
       (if (= r (1- (num-rows board)))
           nil
           (ref board (1+ r) 0)))
   ;; S
   (if (= r (1- (num-rows board))) nil (ref board (1+ r) c))
   ;; W
   (if (zerop c)
       (if (zerop r)
           nil
           (ref board (1- r) (1- +row-capacity+)))
       (ref board r (1- c)))))

(defun add-to-board (board v)
  (check-type v cell)
  (let* ((l (board-layout board))
         (r (1- (num-rows board)))
         (row (aref l r))
         (c (length row)))
    (cond
      ((row-filled? row)
       (vector-push-extend (make-row) l)
       (add-to-board board v))
      (t
       (add-to-row row v)
       (multiple-value-bind (n e s w)
           (layout-neighbors board r c)
         (stack n v)
         (join  v e)
         (stack v s)
         (join  w v))
       (setf (gethash v (board-positions board)) (cons r c))
       board))))

(defun split-number (v)
  (remove-if #'zerop (multiple-value-list (floor v 10))))

(defun initial-board ()
  (let ((numbers (loop :for v :from 1 :to 19
                       :unless (= v 10)
                         :append (split-number v)))
        (board (make-board)))
    ;; Add the numbers
    (dolist (n numbers)
      (add-to-board board (make-cell :value n)))
    ;; Return the board
    board))

(defun delete-cell (board cell)
  (let ((pos (gethash cell (board-positions board))))
    (assert (not (null pos)) () "Cell not found in board.")
    (setf (ref board (car pos) (cdr pos)) nil)
    (remhash cell (board-positions board))
    (stack (cell-n cell) (cell-s cell))
    (join (cell-w cell) (cell-e cell))
    cell))

(defun map-pairs (f board)
  ;; TODO optimize to only look at each link once.
  (maphash (lambda (k v)
             (declare (ignore v))
             (alexandria:when-let ((n (cell-n k)))
               (funcall f k n))
             (alexandria:when-let ((e (cell-e k)))
               (funcall f k e))
             (alexandria:when-let ((s (cell-s k)))
               (funcall f k s))
             (alexandria:when-let ((w (cell-w k)))
               (funcall f k w)))
           (board-positions board))
  nil)

(defun eliminatable-pair-p (a b)
  (or (= (cell-value a)
         (cell-value b))
      (= 10 (+ (cell-value a)
               (cell-value b)))))

(defun map-moves (f board)
  (flet ((frob (a b)
           (when (eliminatable-pair-p a b)
             (funcall f a b))))
    (map-pairs #'frob board)))

(defun list-moves (board)
  (let ((moves nil))
    (flet ((frob (a b)
             (let ((pos-a (gethash a (board-positions board)))
                   (pos-b (gethash b (board-positions board))))
               (push (list pos-a pos-b) moves))))
      (map-moves #'frob board)
      (nreverse
       (delete-duplicates moves
                          :test (lambda (x y)
                                  (and (eq (first x) (second y))
                                       (eq (second x) (first y)))))))))

(defun execute-move (board pos-a pos-b)
  (let* ((new-board (deep-copy-board board))
         (cell-a (ref new-board (car pos-a) (cdr pos-a)))
         (cell-b (ref new-board (car pos-b) (cdr pos-b))))
    (push (list pos-a pos-b) (board-moves board))
    (delete-cell new-board cell-a)
    (delete-cell new-board cell-b)
    new-board))

(defparameter *min* 1)

(defun game-won-p (board)
  (let* ((num-cells (board-size board))
         (ratio (/ num-cells
                   (reduce #'+ (board-layout board) :key #'length))))
    (when (> *min* ratio)
      (setf *min* ratio)
      (format t "New min: ~A~%~A~%" ratio board))
    (zerop num-cells)))

(defun continue-board (board)
  (let ((new (deep-copy-board board)))
    (loop :for row :across (board-layout board) :do
      (loop :for cell :across row
            :unless (null cell) :do
              (add-to-board new (make-cell :value (cell-value cell)))))
    new))

(defun solve ()
  (let ((q (queues:make-queue ':priority-queue
                              :compare (lambda (a b)
                                         (let ((size-a (board-size a))
                                               (size-b (board-size b)))
                                           (or (< size-a size-b)
                                               (and (= size-a size-b)
                                                    (< (length (board-moves a))
                                                       (length (board-moves b)))))))))
        (n 0))
    (labels ((bfs (board)
               (cond
                 ((game-won-p board)
                  (return-from solve board))
                 (t
                  (let ((possible-moves (list-moves board)))
                    (if (null possible-moves)
                        (queues:qpush q (continue-board board))
                        (mapc (lambda (m)
                                (queues:qpush q (execute-move board
                                                              (first m)
                                                              (second m))))
                              possible-moves)))))))
      (queues:qpush q (initial-board))
      (loop :until (zerop (queues:qsize q)) :do
        (let ((next (queues:qpop q nil)))
          (unless (or (< 25 (length (board-moves next)))
                      ;; Another pruning condition?
                      )
            (incf n)
            (when (zerop (mod n 5000))
              (format t "Tried ~D board with ~D left in queue (~4F%) [~D]~%"
                      n
                      (queues:qsize q)
                      (* 100 (/ (queues:qsize q) n))
                      (board-size next)))
            (bfs next)))))))
