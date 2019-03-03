(cl:defpackage :trivial-docstring
  (:use :cl)
  (:export #:docstring))
(cl:in-package :trivial-docstring)


(defmacro docstring (object-def &body docstring)
  (flet ((%transform-object-def ()
           (if (listp object-def)
               (case (first object-def)
                 (cl:class
                  `(cl:find-class ',(second object-def)))
                 (cl:method
                  (destructuring-bind (qualifiers specializers)
                      (loop for (el . rest) on (cddr object-def)
                            until (null rest)
                            collect el into qualifiers
                            finally (return (list qualifiers el)))
                    `(cl:find-method #',(second object-def)
                                     ',qualifiers ',specializers)))
                 (t object-def))
               object-def)))
    (if (null docstring)
        `(documentation ,(%transform-object-def) t)
        `(setf (documentation ,(%transform-object-def) t) ,@docstring))))


(docstring (macro-function 'docstring)
  "Macro for attaching and retrieving documentation strings from objects")
