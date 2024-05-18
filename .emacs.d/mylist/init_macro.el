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

(provide 'init_macro)