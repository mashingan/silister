(defun null (x) (if x '() t))

(defun map (f lst)
  (if (null lst)
    '()
    (cons (f (car lst)) (map f (cdr lst)))))
