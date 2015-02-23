(in-package :lispbot.http-plugins)

(defclass google-plugin (plugin)
  ()
  (:default-initargs :name "google"))

(defmethod help ((pl google-plugin))
  (declare (ignore pl))
  (reply "google {words*} : search google for words"))

(defparameter *google-url* "http://www.google.com/search")

(defparameter *user-agent*
  "Mozilla/5.0 (X11; Linux x86_64; rv:31.0) Gecko/20100101 Firefox/31.0 Iceweasel/31.4.0")

(defun get-google-matches (search)
  (let* ((html (drakma:http-request *google-url*
                                    :parameters `(("q" . ,search))
                                    :user-agent *user-agent*))
         (document (chtml:parse html (cxml-stp:make-builder)))
         (res nil))
    (stp:do-recursively (elem document)
      (when (and (typep elem 'stp:element)
                 (equal (stp:local-name elem) "h3")
                 (equal (stp:attribute-value elem "class") "r"))
        (alexandria:when-let
            (a (stp:find-child-if
                (lambda (a) (and (typep a 'stp:element)
                                 (equal (stp:local-name a) "a")))
                elem))
          (push (cons (stp:string-value a)
                      (stp:attribute-value a "href")) res))))
    (reverse res)))

(defcommand google ((pl google-plugin) arg1 &rest args)
  (declare (ignore pl))
  (let* ((str (format nil "\"~a\"~{ \"~a\"~}" arg1 args))
         (res (get-google-matches str)))
    (if res
        (reply (format nil "~a - ~a"
                       (car (first res))
                       (cdr (first res))))
        (reply (format nil "nothing found for ~a" str)))))
