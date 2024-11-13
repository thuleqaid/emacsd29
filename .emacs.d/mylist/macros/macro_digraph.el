(defun text-actor-digraph (lines arg)
  (let ((pat "^\\([ \t]*\\)")
        (prefix "")
        (prefixlen 0)
        (levels '())
        (contents '())
        indent indentlen indentpat
        tmp i
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
      (let ((pairs '())
            (cstacks '())
            curlevel curcontent outlines
            )
        (setq levels (reverse levels)
              contents (reverse contents)
              cstacks (cons (cons (pop levels) (string-trim (pop contents))) cstacks))
        (while (> (length levels) 0)
          (setq curlevel (pop levels)
                curcontent (string-trim (pop contents)))
          (while (<= curlevel (caar cstacks))
            (pop cstacks)
            )
          (add-to-list 'pairs (cons (cdar cstacks) curcontent))
          (push (cons curlevel curcontent) cstacks)
          )
        (if arg
            (setq outlines (mapcar (lambda(item) (format "%s\"%s\" -> \"%s\";" prefix (car item) (cdr item))) pairs))
          (setq outlines (mapcar (lambda(item) (format "%s\"%s\" -> \"%s\";" prefix (cdr item) (car item))) pairs))
          )
        (push (format "%s}" prefix) outlines)
        (setq outlines (reverse outlines))
        (push (format "%sdigraph G {" prefix) outlines)
        (push (format "%s// dot inputfile -Tpng -o outputfile" prefix) outlines)
        )
      )
    )
  )

(defun text-macro-digraph(&optional arg)
  "根据缩进生成Graphviz的dot文件digraph文本

输入文本格式：
aaa
    bbb
        ccc
        ddd
    eee
    fff
        ggg
输出文本格式：
digraph G {
bbb -> aaa;
ccc -> bbb;
ddd -> bbb;
eee -> aaa;
fff -> aaa;
ggg -> fff;
}
"
  (interactive "P")
  (thuleqaid/textmacro 'text-actor-digraph arg)
  )
