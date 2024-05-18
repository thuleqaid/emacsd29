(defvar extra-exec-path-flag nil)
(defvar extra-exec-path-list '()
  "add additional paths if any executable does not exist

 Usage:
 1. set the whole value of extra-exec-path-list before running current file
 2. call function to append element of extra-exec-path-list by need

 Example:
 (setq extra-exec-path-list '(
                            ; ((executable_1 executable_2 ...) path_1 path_2 ...)
                              (() \"C:\\\\foobar\\\\bin\")
                              ((\"dot\") \"C:\\\\Graphviz\\\\bin\")
                              ((\"gcc\" \"cmake\") \"C:\\\\mingw\\\\bin\" \"C:\\\\cmake\\\\bin\")
                              ))
                               ; (path_1 path_2 ...)          (executable_1 executable_2 ...)
 (thuleqaid/add-extra-exec-path '(\"C:\\\\Graphviz\\\\bin\") '(\"dot\"))
")
(defun thuleqaid/add-extra-exec-path (pathlist &optional exelist)
  (let (
        (pair (cons exelist pathlist))
        (flag t)
        )
    (setq extra-exec-path-list (cons pair extra-exec-path-list))
    (when extra-exec-path-flag
      (dolist (exec_item (car pair))
        (unless (executable-find exec_item)
          (setq flag (and flag nil)))
        )
      (unless flag
        (let ((path_sep (path-separator))
              (path_val (getenv "PATH"))
              )
          (dolist (path_item (cdr pair))
            (when (file-directory-p path_item)
              (setq path_val (concat path_item path_sep path_val)
                    exec-path (cons (expand-file-name path_item) exec-path))
              )
            )
          (setenv "PATH" path_val)
          )
        )
      )
    )
  )
(add-hook 'after-init-hook
          (lambda ()
            (when extra-exec-path-list
              (let ((path_sep (path-separator))
                    (path_val (getenv "PATH"))
                    (flag t)
                    )
                (dolist (pair extra-exec-path-list)
                  (setq flag t)
                  (dolist (exec_item (car pair))
                    (unless (executable-find exec_item)
                      (setq flag (and flag nil)))
                    )
                  (unless flag
                    (dolist (path_item (cdr pair))
                      (when (file-directory-p path_item)
                        (setq path_val (concat path_item path_sep path_val)
                              exec-path (cons (expand-file-name path_item) exec-path))
                        )
                      )
                    )
                  )
                (setenv "PATH" path_val)))
            (setq extra-exec-path-flag t)
            )
          )

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
                  (init_macro . t)
                  (init_prg . t)
                  )))
  (dolist (elt lisplist)
    (when (cdr elt) (require (car elt)))))

(provide 'init_index)