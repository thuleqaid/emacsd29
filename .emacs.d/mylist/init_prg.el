;; 安装Package: ggtags smart-mode-line markdown-mode highlight-symbol iedit multiple-cursors tiny treemacs-projectile yasnippet-snippets yasnippet rg company

(use-package smart-mode-line :defer t
  :if (package-installed-p 'smart-mode-line)
  :init
  (setq sml/no-confirm-load-theme t)  ; avoid asking when startup
  (sml/setup)
  :config
  (setq rm-blacklist
    (format "^ \\(%s\\)$"
      (mapconcat #'identity
        '("Projectile.*" "company.*" "Undo-Tree" "yas" "WK" "WS" "ElDoc" "Abbrev" "Unicad" "hl-s" "Flymake.*" "hs" "GG")
         "\\|"))))

(use-package markdown-mode :defer t
  :if (package-installed-p 'markdown-mode)
  :init (when (executable-find "pandoc")
          (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))))

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
(use-package yasnippet-snippets :defer t
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
(use-package ace-window :defer t
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
  :if (and (package-installed-p 'rg) (executable-find "rg"))
  :hook (after-init . rg-enable-default-bindings)
  :config
  (defun grep-analysis-extract-result-item()
    (let ((pos (line-beginning-position))
          )
      (if-let (msg (get-text-property pos 'compilation-message))
          (let* ((loc (compilation--message->loc msg))
                 (lineno (cadr loc))
                 (text (buffer-substring-no-properties pos (line-end-position)))
                 )
            (if rg-group-result
                ;; Line Col Code
                (setq text (concat
                            (substring-no-properties (caar (nth 2 loc))) "\t"
                            (replace-regexp-in-string
                             "^ *\\([0-9]+\\) +\\([0-9]+\\) +"
                             "\\1\t"
                             (string-clean-whitespace text))))
              ;; File:Line:Code
              (setq text
                    (replace-regexp-in-string
                     "^\\(.+\\):\\([0-9]+\\): *"
                     "\\1\t\\2\t"
                     (string-clean-whitespace text)))
              )
            text
            )
        nil
        )
      )
    )
  (defun thuleqaid/copy-rg-result()
    "Extract rg result"
    (interactive)
    (save-excursion
      (let ((text "")
            (not-moved 0)
            )
        (goto-char (point-min))
        (while (= not-moved 0)
          (when-let (data (grep-analysis-extract-result-item))
            (setq text (concat text "\n" data))
            )
          (setq not-moved (forward-line))
          )
        (setq text (string-trim-left text))
        (kill-new text)
        (message "Copied")
        )
      )
    )
  (when (sqlite-available-p)
    (setq rg-group-result nil)
    (defvar memdb nil "sqlite db handle")
    (defconst grep-analysis-file "grep_analysis.db" "sqlite db file")
    (defun struct-serialize (obj)
      "serialize record into string"
      (when (recordp obj)
        (let* ((type (type-of obj))
               (slots (cl-struct-slot-info type))
               (slotcnt (1- (length slots)))
               (prefix (format "%s-" (symbol-name type)))
               (out '())
               )
          (while (> slotcnt 0)
            (let* ((slot (car (nth slotcnt slots)))
                   (value (funcall (intern (concat prefix (symbol-name slot))) obj))
                   )
              (setq out (plist-put out slot value)
                    slotcnt (1- slotcnt)
                    )
              )
            )
          (json-encode-plist out)
          )
        )
      )
    (defun struct-deserialize (text type &optional constructor)
      "deserialize string back to object"
      (let* ((plistobj (json-parse-string text :null-object nil :object-type 'plist))
             (constructor (or constructor (intern (format "make-%s" (symbol-name type)))))
             )
        (apply constructor plistobj)
        )
      )
    (defun grep-analysis-create-db ()
      (let* ((dbfile (expand-file-name grep-analysis-file (rg-search-dir rg-cur-search)))
             (dbexists (file-exists-p dbfile))
             (dbattach (yes-or-no-p "Attach old db file?"))
             )
        (when (and dbexists (not dbattach))
          (delete-file dbfile)
          )
        (setq memdb (sqlite-open dbfile))
        (sqlite-execute memdb "create table if not exists search (id integer primary key autoincrement, title text not null unique, dump text default \"\");")
        (sqlite-execute memdb "create table if not exists result (id integer primary key autoincrement, code text not null unique);")
        (sqlite-execute memdb "create table if not exists search_result (sid integer, rid integer, judge text not null default \"\", reason text not null default \"\", foreign key(sid) references search(id), foreign key(rid) references result(id));")
        (when dbattach
          (thuleqaid/analysis-filter (get-buffer (rg-buffer-name)) nil)
          )
        )
      )
    (defun grep-analysis-search-insert (title &optional text)
      (if-let (sid (car (grep-analysis-search-find title)))
          sid
        (let ((cnt (sqlite-execute memdb "insert into search(title, dump) values (?,?)" (list title (or text ""))))
              )
          (if (> cnt 0)
              (caar (sqlite-execute memdb "select id from search order by id desc limit ?" (list cnt)))
            nil
            )
          )
        )
      )
    (defun grep-analysis-search-find (title)
      (if-let (ret (sqlite-select memdb "select id, dump from search where title=?" (list title)))
          (car ret)
        nil
        )
      )
    (defun grep-analysis-search-find2 ()
      (if-let (ret (sqlite-select memdb "select id, title from search where dump=?" (list (struct-serialize rg-cur-search))))
          (car ret)
        nil
        )
      )
    (defun grep-analysis-search-update (title text)
      (sqlite-execute memdb "update search set dump=? where title=?" (list text title))
      )
    (defun grep-analysis-search-all-titles ()
      (if-let (ret (sqlite-select memdb "select title from search"))
          (flatten-list ret)
        nil
        )
      )
    (defun grep-analysis-search-unbind-titles ()
      (if-let (ret (sqlite-select memdb "select title from search where dump=\"\""))
          (flatten-list ret)
        nil
        )
      )
    (defun grep-analysis-search-summary-levels (curlevel search searchs)
      (cond ((> (nth 4 search) 0) (list (list (nth 1 search) -1 -1 curlevel (nth 5 search))))
            ((= (nth 2 search) 0) (list (list (nth 1 search) 0 0 curlevel (nth 5 search))))
            (t (let ((subtitles (flatten-list (sqlite-select memdb "select distinct judge from search_result where (judge not in (\"OK\", \"NG\", \"Ignore\", \"\")) and sid=?" (list (car search)))))
                     (out (list (list (nth 1 search) (nth 2 search) (nth 3 search) curlevel (nth 5 search))))
                     )
                 (setf (nth 4 search) 1)
                 (dolist (subtitle subtitles)
                   (when-let (subsearch (seq-find (lambda(item) (string= (nth 1 item) subtitle)) searchs nil))
                     (setq out (append out (grep-analysis-search-summary-levels (1+ curlevel) subsearch searchs))))
                   )
                 out
                 )
               )
            )
      )
    (defun grep-analysis-search-summary ()
      (let* ((searchs (sqlite-select memdb "select s.id, s.title, (select count(*) from search_result sr where sr.sid = s.id) as total,  (select count(*) from search_result sr2 where sr2.sid = s.id and sr2.judge!=\"\") as part, 0, s.dump as used from search s group by s.id;"))
             (out '())
             (search (seq-find (lambda(item) (and (> (nth 2 item) 0) (<= (nth 4 item) 0))) searchs nil))
             )
        (while search
          (setq out (append out (grep-analysis-search-summary-levels 0 search searchs)))
          (setq search (seq-find (lambda(item) (and (> (nth 2 item) 0) (<= (nth 4 item) 0))) searchs nil))
          )
        out
        )
      )
    (defun grep-analysis-result-insert (sid code)
      (let ((rid (grep-analysis-result-find code))
            )
        (unless rid ; code does not exist
          (let ((cnt (sqlite-execute memdb "insert into result(code) values (?)" (list code)))
                )
            (setq rid (if (> cnt 0) (caar (sqlite-execute memdb "select id from result order by id desc limit ?" (list cnt))) nil))
            )
          )
        (when rid
          (sqlite-execute memdb "insert into search_result(sid,rid) values(?,?)" (list sid rid))
          )
        )
      )
    (defun grep-analysis-result-find (code)
      (if-let (ret (sqlite-select memdb "select id from result where code=?" (list code)))
          (caar ret)
        nil
        )
      )
    (defun grep-analysis-judge-find (sid rid)
      (if-let (ret (sqlite-select memdb "select judge, reason from search_result where sid=? and rid=?" (list sid rid)))
          (car ret)
        nil
        )
      )
    (defun grep-analysis-judge-update(sid rid judge &optional reason)
      (sqlite-execute memdb "update search_result set judge=?, reason=? where sid=? and rid=?" (list judge (or reason "") sid rid))
      )
    (defun grep-analysis-lisp-attach ()
      (unless memdb (grep-analysis-create-db))
      (if-let (title (grep-analysis-search-find2))
          (car title)
        (let* ((titles (grep-analysis-search-all-titles))
               (titles (cons (rg-search-pattern rg-cur-search) titles))
               (newtitle (string-trim (completing-read "Input Grep Title: " titles nil 'confirm)))
               (sitem (grep-analysis-search-find newtitle))
               (sid (if sitem (car sitem) nil))
               )
          (unless (and sitem (> (length (nth 1 sitem)) 0))
            (if sitem
                (progn
                  (setq sid (car sitem))
                  (grep-analysis-search-update newtitle (struct-serialize rg-cur-search))
                  )
              (setq sid (grep-analysis-search-insert newtitle (struct-serialize rg-cur-search)))
              )
            (save-excursion
              (let ((not-moved 0)
                    )
                (goto-char (point-min))
                (while (= not-moved 0)
                  (when-let (data (grep-analysis-extract-result-item))
                    (grep-analysis-result-insert sid data)
                    )
                  (setq not-moved (forward-line))
                  )
                )
              )
            )
          sid
          )
        )
      )
    (defun thuleqaid/analysis-new()
      "Clear analysis history"
      (interactive)
      (when memdb (sqlite-close memdb))
      (setq memdb nil)
      )
    (defun thuleqaid/analysis-comment()
      "Comment current result item"
      (interactive)
      (when-let (data (grep-analysis-extract-result-item))
        (let* ((sid (grep-analysis-lisp-attach))
               (rid (grep-analysis-result-find data))
               (curjudge (grep-analysis-judge-find sid rid))
               (choices (grep-analysis-search-all-titles))
               (choices (append (list "OK" "NG" "Ignore") choices))
               (newjudge (string-trim (completing-read (format "Judge[%s]: " (car curjudge)) choices nil 'confirm)))
               (newreason (string-trim (read-string "Reason: " (nth 1 curjudge))))
               (text (regexp-quote (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
               )
          (unhighlight-regexp text)
          (grep-analysis-judge-update sid rid newjudge newreason)
          (cond ((string= newjudge "OK") (highlight-regexp text 'hl-line))
                ((string= newjudge "NG") (highlight-regexp text 'hi-pink))
                ((string= newjudge "Ignore") (highlight-regexp text 'mode-line))
                ((string= newjudge ""))
                (t (progn
                     (grep-analysis-search-insert newjudge)
                     (highlight-regexp text 'hi-blue)))
                )
          )
        )
      )
    (defun thuleqaid/analysis-batch-comment()
      "Comment result items in the selected range"
      (interactive)
      (when (use-region-p)
        (save-excursion
          (let* ((sid (grep-analysis-lisp-attach))
                 (pos1 (region-beginning))
                 (pos2 (region-end))
                 (choices (grep-analysis-search-all-titles))
                 (choices (append (list "OK" "NG" "Ignore") choices))
                 (judge (string-trim (completing-read "Judge: " choices nil 'confirm)))
                 (reason (string-trim (read-string "Reason: " )))
                 (not-moved 0)
                 (insert-flag t)
                 )
            (goto-char pos1)
            (while (and (= not-moved 0) (<= (point) pos2))
              (when-let (data (grep-analysis-extract-result-item))
                (let ((rid (grep-analysis-result-find data))
                      (text (regexp-quote (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                      )
                  (unhighlight-regexp text)
                  (grep-analysis-judge-update sid rid judge reason)
                  (cond ((string= newjudge "OK") (highlight-regexp text 'hl-line))
                        ((string= newjudge "NG") (highlight-regexp text 'hi-pink))
                        ((string= newjudge "Ignore") (highlight-regexp text 'mode-line))
                        ((string= newjudge ""))
                        (t (progn
                             (when insert-flag
                               (grep-analysis-search-insert judge)
                               (setq insert-flag nil)
                               )
                             (highlight-regexp text 'hi-blue)))
                        )
                  )
                )
              (setq not-moved (forward-line))
              )
            )
          )
        )
      )
    (defun thuleqaid/analysis-filter(buffer _)
      (when memdb
        (when-let (title (grep-analysis-search-find2))
          (with-current-buffer buffer
            (save-excursion
              (let* ((text "")
                     (not-moved 0)
                     (sid (car title))
                     (records (sqlite-select memdb "select r.code, sr.judge from search_result sr join result r on sr.rid=r.id where sr.sid=?;" (list sid)))
                     )
                (goto-char (point-min))
                (while (= not-moved 0)
                  (when-let (data (grep-analysis-extract-result-item))
                    (let ((record (seq-find (lambda(item) (string= data (car item))) records))
                          (text (regexp-quote (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                          )
                      (cond ((string= (nth 1 record) "OK") (highlight-regexp text 'hl-line))
                            ((string= (nth 1 record) "NG") (highlight-regexp text 'hi-pink))
                            ((string= (nth 1 record) "Ignore") (highlight-regexp text 'mode-line))
                            ((string= (nth 1 record) ""))
                            (t (highlight-regexp text 'hi-blue))
                            )
                      )
                    )
                  (setq not-moved (forward-line))
                  )
                )
              )
            )
          )
        )
      )
    (defun thuleqaid/analysis-list()
      "List all search"
      (interactive)
      (when memdb
        (let* ((searchs (grep-analysis-search-summary))
               (searchcnt (length searchs))
               (idx-width (length (format "%d" searchcnt)))
               (out "Grep List:")
               search
               )
          (dotimes (idx searchcnt)
            (setq search (nth idx searchs))
            (if (>= (nth 1 search) 0)
                (setq out (concat out "\n" (format (format "%%0%dd. %%s%%s[%%d/%%d]" idx-width) idx (make-string (nth 3 search) 32) (car search) (nth 2 search) (nth 1 search))))
              (setq out (concat out "\n" (format (format "%%0%dd. %%s%%s" idx-width) idx (make-string (nth 3 search) 32) (car search))))
              )
            )
          (setq out (concat out "\n\nRe-run grep:"))
          (let ((choice (read-number out))
                title
                )
            (when (< choice searchcnt)
              (setq title (nth choice searchs)
                    )
              (if (> (length (nth 4 title)) 0)
                  (progn
                    (setq rg-cur-search (struct-deserialize (nth 4 title) 'rg-search 'rg-search-create))
                    (rg-rerun t)
                    )
                (progn
                  (kill-new (car title))
                  (message "Copied")
                  )
                )
              )
            )
          )
        )
      )
    (defun thuleqaid/analysis-report()
      "List all search"
      (interactive)
      (when memdb
        (let* ((searchs (grep-analysis-search-summary))
               (searchcnt (length searchs))
               (out "")
               title search sid items
               )
          (dotimes (idx searchcnt)
            (setq search (nth idx searchs))
            (when (>= (nth 1 search) 0)
              (setq title (car search)
                    out (concat out (format "Key:\t%s\nJudge\tFile\tLine\tCode\tReason\n" title))
                    sid (car (grep-analysis-search-find title))
                    items (sqlite-select memdb "select sr.judge, r.code, sr.reason from search_result sr join result r on sr.rid=r.id where sr.sid=?" (list sid))
                    )
              (dolist (item items)
                (setq out (concat out (format "%s\t%s\t%s\n" (car item) (nth 1 item) (nth 2 item))))
                )
              (setq out (concat out "\n\n"))
              )
            )
          (kill-new out)
          (message "Report Copied")
          )
        )
      )
    (add-to-list 'rg-finish-functions 'thuleqaid/analysis-filter)
    (define-key rg-mode-map (kbd "C-c a n") 'thuleqaid/analysis-new)
    (define-key rg-mode-map (kbd "C-c a l") 'thuleqaid/analysis-list)
    (define-key rg-mode-map (kbd "C-c a b") 'thuleqaid/analysis-batch-comment)
    (define-key rg-mode-map (kbd "C-c a c") 'thuleqaid/analysis-comment)
    (define-key rg-mode-map (kbd "C-c a r") 'thuleqaid/analysis-report)
    )
  :bind
  (:map rg-mode-map
        ("C-c c" . thuleqaid/copy-rg-result))
  )

;; 用于组织多个不同的命令
(use-package hydra :defer t
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
(use-package treemacs-projectile :defer t
  :if (package-installed-p 'treemacs-projectile)
  :after (treemacs projectile))

;; 需要手动调整~/.emacs.d/.mc-lists.el文件中的run-once和run-for-all数组
(use-package multiple-cursors :defer t
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
;; <M-s h .>  高亮/取消高亮当前Symbol
;; <M-s h U>  取消所有高亮
;; <M-s h p>  高亮指定短语
;; <M-s h r>  高亮指定正则表达式
;; <M-s h u>  选取要取消的高亮
(use-package highlight-symbol :defer t
  :if (package-installed-p 'highlight-symbol)
  :hook
  (prog-mode . highlight-symbol-mode)
  (org-mode . highlight-symbol-mode)
  :bind
  (("M-s h U" . highlight-symbol-remove-all))
  :config
  (defun highlight-symbol-count (&optional symbol)
    (interactive)
    )
  )
;; <C-;>  用于代码重构，同步修改所有Symbol名称
(use-package iedit :defer t
  :if (package-installed-p 'iedit))
;; <C-'>直接跳转到指定匹配位置（<C-s>/<C-r>搜索过程中使用）
;;      跳转前输入<?>，显示可用的高级操作
(use-package avy :defer t
  :if (package-installed-p 'avy)
  ; :bind (("C-j" . 'avy-goto-char-timer))
  :config (avy-setup-default))

(use-package ggtags :defer t
  :if (and (package-installed-p 'ggtags) (executable-find "gtags"))
  :hook (c-mode . ggtags-mode)
  :config
  (defun thuleqaid/gtags-generate ()
    (interactive)
    (when (and (package-installed-p 'projectile) (executable-find "rg") (not (string= (projectile-project-name) "-")))
      (let* ((default-directory (projectile-acquire-root))
             (rgignore (expand-file-name ".rgignore" default-directory))
             )
        (when (file-exists-p rgignore)
          (with-current-buffer (get-buffer-create " __FILE_LIST__" t)
            (erase-buffer)
            ;; rg -l . --hidden > filelist.txt
            (call-process "rg" nil t nil "-l" "." "--hidden")
            ;; gtags -f filelist.txt
            (call-process-region (point-min) (point-max) "gtags" nil t nil "-f" "-")
            (kill-buffer)
            )
          )
        )
      )
    )
  )

;; <C-M-f>/<C-M-b> 匹配括号之间跳转
;; <C-M-h>         选中当前函数
;; <C-M-a>/<C-M-e> 跳转到函数头/尾
;; 显示当前函数名
(add-hook 'prog-mode-hook 'which-function-mode)
(defun thuleqaid/prg-mono-font ()
  (when (string= system-type "windows-nt")
    (defface fate-buffer-face '((t :family "MS Gothic" :foundry "outline" :slant normal :weight regular :height 120 :width normal)) "Prg buffer mono font")
    (buffer-face-set 'fate-buffer-face)
    )
  )
(add-hook 'prog-mode-hook 'thuleqaid/prg-mono-font)

(use-package eglot :defer t
  :hook
  (python-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  )

;; python环境准备
;;   pip install 'python-language-server[all]'
;; c/c++环境准备
;;   https://github.com/mstorsjo/llvm-mingw/releases
(provide 'init_prg)