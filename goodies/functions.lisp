(defun id(x) x)
(defmacro drop-if (what from-where &key (key #'id) (test #'eql))
  `(setf ,from-where (remove ,what ,from-where :key ,key :test ,test)))
