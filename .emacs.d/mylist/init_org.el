
(defun mysetting/coding-system (func coding)
  "Set coding system for external program"
  (eval-expression
   `(defadvice ,func (around ,(make-symbol (format "coding-system/%s/%s" func coding)) activate)
      (let ((coding-system-for-read ',coding)
            (coding-system-for-write ',coding))
        ad-do-it
        )
      )
   )
  )
(defun mysetting/org-confirm-babel-evaluate (lang body)
  (if (member lang '("plantuml" "dot")) nil t)) ; don't ask for plantuml, graphviz

(use-package org :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . nil)
     (haskell . nil)
     (latex . nil)
     (ledger . nil)
     (ocaml . nil)
     (octave . nil)
     (plantuml . t)
     (python . t)
     (ruby . nil)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . nil)
     (sqlite . nil)))
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  :custom
  (org-support-shift-select t)
  (org-use-sub-superscripts nil))
(use-package org-compat :defer t
  :custom
  (org-catch-invisible-edits 'show))
(use-package org-list :defer t
  :custom
  (org-list-demote-modify-bullet
  '(("+" . "-") ("-" . "+") ("*" . "+"))))
(use-package ox :defer t
  :custom
  (org-export-coding-system 'utf-8)
  (org-export-with-sub-superscripts nil))
(use-package ox-html :defer t
  :custom
  (org-html-validation-link nil))
(use-package ob-core :defer t
  :config
  (mysetting/coding-system 'org-babel-execute-src-block 'utf-8)
  :custom
  (org-confirm-babel-evaluate 'mysetting/org-confirm-babel-evaluate))
(use-package ob-ditaa :defer t
  :custom
  (org-ditaa-jar-path (expand-file-name "ditaa.jar" user-emacs-directory)))
(use-package ob-plantuml :defer t
  :custom
  (org-plantuml-jar-path (expand-file-name "plantuml.jar" user-emacs-directory)))

;; (define-key global-map (kbd "C-c l") 'org-store-link)
;; (define-key global-map (kbd "C-c L") 'org-id-store-link)

(add-hook 'org-mode-hook 'buffer-face-mode)

(defun thuleqaid/org-babel-image-format ()
  (interactive)
  (save-excursion
    (let ((image-format "png"))
      (setq image-format (read-from-minibuffer "Image Format(png/svg): ")) ; read target image format
      (goto-char (point-min)) ; replace gnuplot script for output mode
      (while (re-search-forward "^\\(\\s *set term \\)\\(\\S +\\) " nil t)
        (replace-match (format "\\1%s " image-format)))
      (goto-char (point-min)) ; replace outfile name
      (while (re-search-forward "^\\(\\s *#\\+begin_src \\(?:plantuml\\|gnuplot\\|dot\\) :file \\S +\\.\\)\\(\\S +\\) " nil t)
        (replace-match (format "\\1%s " image-format)))
      )))
(defun thuleqaid/org-babel-batch-src ()
  (interactive)
  (save-excursion
    (major-mode-suspend)
    (org-mode)
    (goto-char (point-min)) ; replace outfile name
    (while (re-search-forward "^\\(\\s *#\\+begin_src \\(?:plantuml\\|gnuplot\\|dot\\) :file \\S +\\.\\)\\(\\S +\\) " nil t)
      (let* ((info (org-babel-get-src-block-info))
             (params (nth 2 info))
             (file (cdr (assq :file params)))
             (fullpath (expand-file-name file))
             (dirname (directory-file-name (file-name-directory fullpath))))
        (unless (file-accessible-directory-p dirname)
          (make-directory dirname))
        (org-babel-execute-src-block))
      )
    (major-mode-restore)
    ))
;; (defun thuleqaid/org-insert-sub-file ()
;;   (interactive)
;;   (let ((curdir (file-name-directory (buffer-file-name)))
;;         (curdt (format-time-string "%Y%m%d%H%M%S"))
;;         (subdesc (read-string "Description: "))
;;         )
;;     (make-directory curdt curdir)
;;     (org-insert-link nil (format "file:%s.org" curdt) subdesc)
;;     ))
;; (defun thuleqaid/org-clear-sub-dir ()
;;   (interactive)
;;   (let* ((curdir (file-name-directory (buffer-file-name)))
;;          (filelist (directory-files curdir t))
;;          (dirlist '())
;;          (orglist '())
;;          (rmdirlist '())
;;          curfile
;;          )
;;     ;; sepearte dirs and org files
;;     (dolist (curfile filelist)
;;       (if (file-directory-p curfile)
;;           (unless (string-match "/\.\.\?$" curfile)
;;             (setq dirlist (cons curfile dirlist)))
;;         (when (string-match "\.org$" curfile)
;;           (setq orglist (cons curfile orglist))))
;;       )
;;     ;; find empty dir without same name org file
;;     (dolist (curfile dirlist)
;;       (unless (> (length (directory-files curfile t)) 2)
;;         (unless (member (format "%s.org" curfile) orglist)
;;           (setq rmdirlist (cons curfile rmdirlist))
;;           )
;;         )
;;       )
;;     ;; delete unused dirs
;;     (dolist (curfile rmdirlist)
;;       (delete-directory curfile)
;;       )
;;     ))

(provide 'init_org)