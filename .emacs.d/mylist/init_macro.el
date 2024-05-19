;; 文本处理宏框架

(defun thuleqaid/textmacro(func)
  "全文/指定范围内文本处理框架

按行提取文本，调用传入的函数参数生成新的文本，然后替换原文本

使用示例：
(defun text-actor-XXX (list)
  (reverse list))
(defun text-macro-XXX()
  (interactive)
  (thuleqaid/textmacro 'text-actor-XXX)
  )
"
  (let (
        (line1 (line-number-at-pos (if (use-region-p) (region-beginning) (point-min))))
        (line2 (line-number-at-pos (if (use-region-p) (region-end) (point-max))))
        (lines '())
        pos1 pos2
        )
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line1))
      (setq pos1 (point))
      (dotimes (i (1+ (- line2 line1)))
        (setq lines (cons (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines))
        (forward-line 1)
        (setq pos2 (point))
        )
      (setq lines (reverse lines))
      ;; (prin1 lines)
      )
    (setq lines (funcall func lines))
    (delete-region pos1 pos2)
    (insert (string-join lines "\n"))
    (insert "\n")
    )
  )

(defun thuleqaid/text-macro-load ()
  "加载文本处理模块"
  (interactive)
  (let ((prompt "Macro file to load: ")
        (initial-input nil)
        (action nil)
        (choices '())
        res)
    (dolist (elt
             (directory-files (expand-file-name "mylist/macros" user-emacs-directory) nil "\.el$")
             choices)
      (setq choices (cons (cons (file-name-base elt) t) choices)))
    (setq res
          (pcase (if (eq projectile-completion-system 'auto)
                     (cond
                      ((bound-and-true-p ido-mode)  'ido)
                      ((bound-and-true-p helm-mode) 'helm)
                      ((bound-and-true-p ivy-mode)  'ivy)
                      (t 'default))
                   projectile-completion-system)
            ('default (completing-read prompt choices nil nil initial-input))
            ('ido (ido-completing-read prompt choices nil nil initial-input))
            ('helm
             (if (and (fboundp 'helm)
                      (fboundp 'helm-make-source))
                 (helm :sources
                       (helm-make-source "Projectile" 'helm-source-sync
                                         :candidates choices
                                         :action (if action
                                                     (prog1 action
                                                       (setq action nil))
                                                   #'identity))
                       :prompt prompt
                       :input initial-input
                       :buffer "*helm-projectile*")
               (user-error "Please install helm")))
            ('ivy
             (if (fboundp 'ivy-read)
                 (ivy-read prompt choices
                           :initial-input initial-input
                           :action (prog1 action
                                     (setq action nil))
                           :caller 'projectile-completing-read)
               (user-error "Please install ivy")))
            (_ (funcall projectile-completion-system prompt choices))))
    ;; (if action
    ;;     (funcall action res)
    ;;   res)
    (load (format "macros/%s" res))
    )
  )

(provide 'init_macro)