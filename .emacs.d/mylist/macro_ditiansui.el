(defun text-actor-ditiansui (list)
  (let (
        (flag nil)
        (count1 0)
        (count2 0)
        (part1 '("note:"))
        (part2 '())
        )
    (dotimes (i (length list))
      (unless (string-blank-p (nth i list))
        (if flag
            ;; 案例部分
            (progn
              (setq count2 (1+ count2))
              (when (= 1 count2)
                (setq count1 (1+ count1)
                      part2 (cons (format "\n## 例%d" count1) part2)
                      )
                )
              (cond
               ((= 1 count2) (setq part2 (cons (format "八字：%s" (string-trim (nth i list))) part2)))
               ((= 2 count2) (setq part2 (cons (format "大运：%s" (string-trim (nth i list))) part2)))
               ((= 3 count2) (setq part2 (cons (format "原著：%s\n理解：" (string-trim (nth i list))) part2)
                                   count2 0
                                   ))
               )
              )
          ;; 原文摘录部分
          (if (string-prefix-p "====" (nth i list))
              ;; 遇到两部分的分隔
              (setq flag t)
            (setq part1 (cons (format "  - %s" (string-trim (nth i list))) part1))
            )
          )
        )
      )
    (when (<= (length part1) 1)
      (setq part1 (cons "  - N/A" part1))
      )
    (setq part1 (cons "---" part1)
          part1 (reverse part1)
          part1 (cons (format "count: %d" count1) part1)
          part1 (cons "---" part1)
          part2 (reverse part2)
          part1 (append part1 part2)
          )
    part1
    )
  )
(defun text-macro-ditiansui()
  "解析滴天髓文本

输入文本格式：
foobar

==================

甲子 甲子 甲子 甲子
甲子 甲子 甲子 甲子 甲子 甲子
description

乙丑 乙丑 乙丑 乙丑
乙丑 乙丑 乙丑 乙丑 乙丑 乙丑
description

输出文本格式：
---
count: 2
note:
  - foobar
---

## 例1
八字：甲子 甲子 甲子 甲子
大运：甲子 甲子 甲子 甲子 甲子 甲子
原著：description
理解：

## 例2
八字：乙丑 乙丑 乙丑 乙丑
大运：乙丑 乙丑 乙丑 乙丑 乙丑 乙丑
原著：description
理解：

"
  (interactive)
  (thuleqaid/textmacro 'text-actor-ditiansui)
  )
