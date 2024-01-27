;; 安装Package: markdown-mode highlight-symbol iedit multiple-cursors tiny treemacs-projectile yasnippet-snippets yasnippet rg company

(use-package markdown-mode
  :if (package-installed-p 'markdown-mode)
  :init (setq markdown-command '("pandoc" "--from=markdown" "--to=html5")))

;; 自动补全
;; <M-/>     尝试根据上下文补全
;; <Tab>     自动补全Company
;; <S-Tab>   自动扩展Snippet
;; <C-Tab>   扩展左侧格式：m[<range start:=0>][<separator:= >]<range end>[Lisp expr]|[format expr]
(global-set-key (kbd "M-/") 'hippie-expand)
(use-package company :defer t
  :if (package-installed-p 'company)
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1) ; 只需敲1个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t) ;; 给选项编号 (按快捷键<M-1>/<M-2>等等来进行选择)
  ;; (setq company-transformers '(company-sort-by-occurrence)) ; 根据选择的频率进行排序
  (setq company-selection-wrap-around t))
(use-package yasnippet :defer t
  :if (package-installed-p 'yasnippet)
  :hook
  (prog-mode . yas-minor-mode)
  (org-mode . yas-minor-mode)
  (markdown-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  ;; add company-yasnippet to company-backends
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
	backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  :bind
  (:map yas-minor-mode-map ("S-<tab>" . yas-expand))
  :after company)
(use-package yasnippet-snippets
  :if (package-installed-p 'yasnippet-snippets)
  :after yasnippet)
;; 按指定规律扩展文字
;; <M-x> tiny-helper  提示方式逐步输入格式中各参数
(use-package tiny :defer t
  :if (package-installed-p 'tiny)
  :hook (after-init . (lambda () (global-set-key (kbd "C-<tab>") 'tiny-expand))))

;; <C-x 0>删除当前窗口
;; <C-x 1>最大化当前窗口（删除其它窗口）
;; <C-x 2>在下方新建窗口
;; <C-x 3>在右侧新建窗口
;; <C-o>选择另一个的窗口（超过2个窗口时，提示窗口序号）
(use-package ace-window
  :if (package-installed-p 'ace-window)
  :bind (("C-x o" . 'ace-window)))

;; ripgrep插件
;; Prefix: <C-c s>
;; 在检索结果中直接修改文件（配合iedit）
;;; 1. 使用rg检索，切换到检索结果Buffer
;;; 2. <C-c C-p>进入wgrep模式
;;; 3. 光标移动到需要修改的单词上，<C-;>进入iedit模式
;;; 4. 批量修改，<C-;>退出iedit模式
;;; 5. <C-x C-s>/<C-c C-k>保存/放弃变更，并退出wgrep模式
;; .rgignore文件示例（影响ripgrep，可保存在.projectile文件同目录）
;;; /foo/bar/abc
;;; *.html
;;; tests
(use-package rg :defer t
  :if (package-installed-p 'rg)
  :hook (after-init . rg-enable-default-bindings))

;; 用于组织多个不同的命令
(use-package hydra
  :if (package-installed-p 'hydra))

;; 目录树（可以切换Workspace，一个Workspace包括多个Project）
;; <C-x t t>  Toggle目录树
;; <M-0>      在目录树和文件Buffer之间切换
;; 在目录树窗口中：
;;   Prefix: <C-c C-w> Workspace相关子命令
;;   Prefix: <C-c C-p> Project相关子命令
;;   <C-c C-p p>可以将Projectile自动识别的项目加到当前Workspace中
;;   <C-c C-w e>可以编辑org文件形式的Workspace/Project列表，Project名称前加上`COMMENT`可以隐藏Project
(use-package treemacs :defer t
  :if (package-installed-p 'treemacs)
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; 自动识别项目根目录（.git目录、.projectile文件等方式）
;; .projectile文件示例（影响projectile中查找文件，首先使用"+"规则筛选范围，然后使用"-"规则进一步筛选）
;;; +/foo/bar
;;; -/foo/bar/abc
;;; -*.html
;;; -tests
;; Prefix: <C-c p>
(use-package projectile :defer t
  :if (package-installed-p 'projectile)
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))
(use-package treemacs-projectile
  :if (package-installed-p 'treemacs-projectile)
  :after (treemacs projectile))

(use-package multiple-cursors
  :if (package-installed-p 'multiple-cursors)
  :init
  (defhydra hydra-multiple-cursors (:exit nil :hint nil)
    "
  Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
  ------------------------------------------------------------------
   [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
   [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
   [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
   [_|_] Align with input CHAR       [Click] Cursor at point"
      ("l" mc/edit-lines :exit t)
      ("a" mc/mark-all-like-this :exit t)
      ("n" mc/mark-next-like-this)
      ("N" mc/skip-to-next-like-this)
      ("M-n" mc/unmark-next-like-this)
      ("p" mc/mark-previous-like-this)
      ("P" mc/skip-to-previous-like-this)
      ("M-p" mc/unmark-previous-like-this)
      ("|" mc/vertical-align)
      ("s" mc/mark-all-in-region-regexp :exit t)
      ("0" mc/insert-numbers :exit t)
      ("A" mc/insert-letters :exit t)
      ("<mouse-1>" mc/add-cursor-on-click)
      ;; Help with click recognition in this hydra
      ("<down-mouse-1>" ignore)
      ("<drag-mouse-1>" ignore)
      ("q" nil))
  :after hydra
  :bind
  (("C-c m" . hydra-multiple-cursors/body)
   ("C-S-<mouse-1>" . mc/toggle-cursor-on-click)))
;; <M-s h .>  高亮当前Symbol
;; <M-s h p>  高亮指定短语
;; <M-s h r>  高亮指定正则表达式
;; <M-s h u>  选取要取消的高亮
(use-package highlight-symbol :defer t
  :if (package-installed-p 'highlight-symbol)
  :hook
  (prog-mode . highlight-symbol-mode)
  (org-mode . highlight-symbol-mode))
;; <C-;>  用于代码重构，同步修改所有Symbol名称
(use-package iedit
  :if (package-installed-p 'iedit))
;; <C-'>直接跳转到指定匹配位置（<C-s>/<C-r>搜索过程中使用）
;;      跳转前输入<?>，显示可用的高级操作
(use-package avy
  :if (package-installed-p 'avy)
  ; :bind (("C-j" . 'avy-goto-char-timer))
  :config (avy-setup-default))

;; python环境准备
;;   pip install 'python-language-server[all]'
(add-hook 'python-mode-hook 'eglot-ensure)

(provide 'init_prg)