(require 'url)
(require 'json)
(require 'popup)

(defun basic-auth-token (username password)
  (format " Basic %s" (base64-encode-string (format "%s:%s" username password))))

(setq
  username "dev"
  password "dev")

(defvar jenkins-api-url "http://localhost:8080/job/test1/api/json")
(defvar jenkins-api-url1 "http://localhost:8080/job/test job 1/api/json")
(defvar jenkins-api-url2 "http://localhost:8080/api/json")

;; https://stackoverflow.com/questions/1664202/emacs-lisp-evaluate-variable-in-alist
;; https://stackoverflow.com/questions/15485833/emacs-lisp-evaluate-variable-in-alist
;; https://stackoverflow.com/questions/19774603/convert-alist-to-from-regular-list-in-elisp

(defun handle-response ()
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (search-forward "{")
    ;; (print (buffer-substring (1- (point)) (point-max)))
    (let ((json-object-type 'hash-table))
      (let ((data (json-read-from-string (buffer-substring (1- (point)) (point-max)))))
        (print (gethash "jobs" data))
        (let ((jobs (gethash "jobs" data)))
          (let ((names (mapcar (lambda(job)
                                 (format "%s - %s" (gethash "name" job) (gethash "color" job))) jobs)))
            ;; (print names)
            ;; (popup-menu* '("Foo" "Bar"))
            (popup-menu* '("Foo" "Bar"))
            (print names)
            ;; (popup-menu* names)
            ;; '(names)
            )
          )
        )
      )
    ))



(defun show-popup ()
  (interactive)
  (get-dashboard-details "true"))


(defun get-dashboard-details(pretty)
      (let* ((url-request-method "GET")
            (x (basic-auth-token username password))
            (arg-stuff
             (concat "?pretty=" (url-hexify-string pretty)))
            (url-request-extra-headers
             `(("Authorization" . ,x)
               ("Content-Type" . "application/json"))))
        (print (length x))
        (url-retrieve (concat jenkins-api-url2 arg-stuff)
                      (lambda (status)
                        (handle-response)
                        ))))

;; (get-dashboard-details "true")
(provide 'show-popup)
