;; (let ((result '()))
;;   (dolist (elt
;;            (directory-files (expand-file-name "mylist/" user-emacs-directory) nil "\.el$")
;;            result)
;;     (setq result (cons (cons (file-name-base elt) t) result)))
;;   (setq result (reverse result)))
(let ((lisplist '(
                  (init_dired . t)
                  (init_calendar . t)
                  (init_org . t)
                  (init_org_gtd . nil)
                  (init_prg . t)
                  )))
  (dolist (elt lisplist)
    (when (cdr elt) (require (car elt)))))

(provide 'init_index)