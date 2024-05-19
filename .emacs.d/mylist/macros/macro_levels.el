(defun text-actor-levels (lines)
  (let ((pat "^\\([ \t]*\\)")
        (prefix "")
        (prefixlen 0)
        (levels '())
        (contents '())
        (outlines '())
        ;; (marks '("    " "+---" "|   " "+---"))
        (marks '("　　" "┣━" "┃　" "┗━"))
        indent indentlen indentpat
        tmp i j k l val
        )
    ;; 提取第1行前面的空白作为Prefix
    (setq tmp (string-trim-right (car lines)))
    (when (string-match pat tmp)
      (setq prefix (match-string 1 tmp)
            prefixlen (length prefix))
      )
    (setq levels (cons 0 levels)
          contents (cons (substring tmp prefixlen nil) contents)
          tmp (string-trim-right (nth 1 lines)))
    (when (string-prefix-p prefix tmp)
      ;; 提取第2行去掉Prefix后，前面的空白作为Indent
      (setq tmp (substring tmp prefixlen nil))
      (when (string-match pat tmp)
        (setq indent (match-string 1 tmp)
              indentlen (length indent)
              indentpat (format "^\\(%s\\)+" (regexp-quote indent))
              )
        (setq levels (cons 1 levels)
              contents (cons (substring tmp indentlen nil) contents)
              i 2)
        ;; 遍历第3~N行
        (while (< i (length lines))
          (setq tmp (string-trim-right (nth i lines)))
          (if (string-prefix-p prefix tmp)
              (progn
                (setq tmp (substring tmp prefixlen nil)
                      i (1+ i))
                (if (string-match indentpat tmp)
                    (setq levels (cons (/ (length (match-string 0 tmp)) indentlen) levels)
                          contents (cons (substring tmp (* (car levels) indentlen) nil) contents)
                          )
                  ;; 不满足Indent时退出
                  (setq i (length lines))
                  )
                )
            ;; 不满足Prefix时退出
            (setq i (length lines))
            )
          )
        )
      )
    (if (<= (length levels) 1)
        lines
      (progn
        (setq levels (reverse levels)
              contents (reverse contents)
              outlines (cons (format "%s%s" prefix (car contents)) outlines))
        ;; 计算每行前的Mark
        (setq l 1
              tmp "")
        (while (< l (length levels))
          (setq i 1
                val (nth l levels))
          ;; 计算第l行第i层的Mark
          (while (<= i val)
            ;; 假设是相同Level的最后一个
            (setq k (if (= i val) 3 0)
                  j (1+ l))
            (while (< j (length levels))
              (if (= (nth j levels) i)
                  ;; 下方找到相同Level的
                  (setq k (if (= i val) 1 2)
                        j (length levels))
                (when (< (nth j levels) i)
                  ;; 下方找到更深Level的
                  (setq k (if (= i val) 3 0)
                        j (length levels))
                  )
                )
              (setq j (1+ j))
              )
            (setq tmp (concat tmp (nth k marks))
                  i (1+ i))
            )
          (message "%s%s" tmp (nth l contents))
          (setq outlines (cons (format "%s%s%s" prefix tmp (nth l contents)) outlines)
                l (1+ l)
                tmp "")
          )
        (while (< l (length lines))
          (setq outlines (cons (nth l lines) outlines)
                l (1+ l))
          )
        (reverse outlines)
        )
      )
    )
  )

(defun text-macro-levels()
  "根据缩进绘制目录结构图

输入文本格式：
aaa
    bbb
        ccc
        ddd
    eee
    fff
        ggg
输出文本格式：
aaa
┣━bbb
┃　┣━ccc
┃　┗━ddd
┣━eee
┗━fff
　　┗━ggg
"
  (interactive)
  (thuleqaid/textmacro 'text-actor-levels)
  )
