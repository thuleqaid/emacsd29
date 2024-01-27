(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-visited-mode t)
 '(column-number-mode t)
 '(delete-selection-mode t)
 '(display-line-numbers t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(global-auto-revert-mode t)
 '(global-hl-line-mode t)
 '(global-whitespace-mode t)
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(package-archives
   '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
     ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
 '(package-selected-packages
   '(markdown-mode highlight-symbol iedit multiple-cursors tiny treemacs-projectile yasnippet-snippets htmlize yasnippet unicad marginalia orderless vertico undo-tree rg which-key company))
 '(prog-mode-hook
   '(flymake-mode prettify-symbols-mode electric-pair-mode hs-minor-mode show-paren-mode))
 '(recentf-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(use-short-answers t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((t nil))))

(put 'narrow-to-region 'disabled nil)
(global-set-key (kbd "C-,") 'set-mark-command)
(global-set-key (kbd "C-x C-,") 'pop-global-mark)

;; 安装Package: unicad undo-tree marginalia orderless vertico which-key

;; 设置默认编码
(prefer-coding-system 'utf-8)
;; 自动检测文字编码
(use-package unicad :defer t
  :if (package-installed-p 'unicad)
  :hook (after-init . unicad-mode))


;; <C-x u>显示树状历史
(use-package undo-tree :defer t
  :if (package-installed-p 'undo-tree)
  :custom (undo-tree-auto-save-history nil)
  :hook (after-init . global-undo-tree-mode))

;; minibuffer补全
;; 1. 竖式展开小缓冲区
(use-package vertico
  :if (package-installed-p 'vertico)
  :custom (verticle-cycle t)
  :config (vertico-mode))
;; 2. 给各个选项条目添加注解
(use-package marginalia
  :if (package-installed-p 'marginalia)
  :config (marginalia-mode))
;; 3. 乱序补全（过滤出包含不限顺序的多个关键词的条目）
(use-package orderless
  :if (package-installed-p 'orderless)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))
;; 4. 使用<M-x>执行命令时，提示绑定的快捷键
(use-package which-key :defer t
  :if (package-installed-p 'which-key)
  :custom (which-key-idle-delay 0.5)
  :hook (after-init . which-key-mode))


; (dolist (elt (directory-files (expand-file-name "mylist/" user-emacs-directory) t "\.el$")) (load-file elt))
(add-to-list 'load-path (expand-file-name "mylist/" user-emacs-directory))
(require 'init_index)


;; Info视图
;; 比较有如下文档结构：
;; 1
;; 1.6
;; 1.6.1
;; 1.6.2
;; 1.7    <--当前正在查看
;; 1.7.1
;; 1.8
;;
;; <b>,<e>            跳到当前章节的开始/结束位置
;; <SPC>,<Backspace>  在向下/向上翻页，已经处于当前章节的结束/开始位置时，相当于<]>/<[>
;; <]>,<[>            跳到下一个/上一个章节，这里是1.7.1和1.6.2
;; <n>,<p>            跳到同级别下一个/上一个章节，这里是1.8和1.6
;; <u>,<^>            跳到父章节，这是是章节1
;; <Tab>,<S-Tab>      在当前章节内的链接文字之间跳转
;; <Ret>              跳转到光标所在链接的指向位置


;; 书签
;; <C-x r m>          设置书签
;; <C-x r b>          跳转书签
;; <C-x r l>          显示所有书签


;; 目录级变量
;;<M-x> add-dir-local-variable
