

(ql:quickload :clack)

(clack:stop *)

(defparameter *board* (make-array '(120 30) :initial-element nil))

(defun html-board (board)
  (let ((tox (car (array-dimensions board)))
        (toy (cadr (array-dimensions board)))
        (html (make-array '(0) :element-type 'character
                          :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s html)
      (format s "<table style=\"font-family: monospace; font-size 5px;\" cellspacing=\"0\">~&")
      (do ((y 0 (1+ y)))
          ((= toy y))
        (format s "<tr>")
        (do ((x 0 (1+ x)))
            ((= tox x))
          (format s "<td>~a</td>"
                  (if (aref board x y)
                      "O"
                      "&nbsp;")))
        (format s "<tr>~&"))
      (format s "</table>~&"))
    html))



(defun print-board (board)
  (let ((tox (car (array-dimensions board)))
        (toy (cadr (array-dimensions board))))
    (do ((y 0 (1+ y)))
        ((= toy y))
      (do ((x 0 (1+ x)))
          ((= tox x))
        (format t "~a"
                (if (aref board x y)
                    "O"
                    " ")))
      (format t "~&"))))

(defun limit (v to)
  (cond ((< v 0) (+ v to))
        ((>= v to) (- to v))
        (t v)))


(defun count-neighbors (board px py)
  (let ((tox (car (array-dimensions board)))
        (toy (cadr (array-dimensions board))))
    (-
     (loop for x from (1- px) to (1+ px)
        sum (loop for y from (1- py) to (1+ py)
               sum (if (aref board
                             (limit x tox)
                             (limit y toy))
                       1 0)))
     (if (aref board px py)
         1
         0))))

(defun determine-cell (board px py)
  (let ((nb (count-neighbors board px py)))
    (if (aref board px py)
        (and (> nb 1) (< nb 4))
        (= nb 3))))

(defun randomize-cell (board px py)
  (declare (ignore board)
           (ignore px)
           (ignore py))
  (= 0 (random 2)))

(defun rebuild-board (board func)
  (let ((tox (car (array-dimensions board)))
        (toy (cadr (array-dimensions board))))
    (let ((rv (make-array
               (list tox toy)
               :initial-element nil)))
      (loop for x from 0 to (1- tox)
         do (loop for y from 0 to (1- toy)
               do (setf (aref rv x y) (funcall func board x y))))
      rv)))
    
(defun generation-board (board)
  (rebuild-board board #'determine-cell))

(defun randomize-board (board)
  (rebuild-board board #'randomize-cell))


(defun step-board ()
  (setf *board* (generation-board *board*))
  (print-board *board*))


(defun run-board (steps)
  (dotimes (step steps)  (step-board)))


(defun generate-html (board)
  (let ((html (make-array '(0) :element-type 'character
                          :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s html)
      (format s "<html><head></head><body>
                 <a href=\"randomize\">randomize</a><br/>
                 <a href=\"step\">step</a><br/>
                 ~a</body></html>" (html-board board)))
    html))
      
    



(defun app (env)
  (cond
    ((string= (getf env :path-info) "/favicon.ico")
     '(200
       (:content-type "image/x-icon")
       #p"favicon.ico"))
    ((string= (getf env :path-info) "/env")
     `(200
       (:content-type "text/plain")
       (, (prin1-to-string env))))
    ((string= (getf env :path-info) "/randomize")
     `(200
       (:content-type "text/html")
       (, (progn (setf *board* (randomize-board *board*))
                 (generate-html *board*)))))
    ((string= (getf env :path-info) "/step")
     `(200
       (:content-type "text/html")
       (, (progn (setf *board* (generation-board *board*))
                 (generate-html *board*)))))
    ((string= (getf env :path-info) "/")
     `(200
       (:content-type "text/html")
       (, (generate-html *board*))))
    (t '(404 (:content-type "text/plain") ("Not one little bit found")))))



(clack:clackup #'app)


;; (sb-thread:terminate-thread (car  (sb-thread:list-all-threads)))
