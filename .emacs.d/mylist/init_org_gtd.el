;; GTD工作流
;1. 使用<C-c c>快速记录ToDo项或者Note项到note.org文件中
;2. 定期查看note.org文件，使用<C-c C-w>将note.org中的内容移动到todolist.org中的Action/Information/Reserved之一下面
;3. 对于某一条Todo项，使用<C-c C-s>/<C-c C-d>设置计划日期/Deadline，使用<C-c t>修改状态，使用<C-c C-c>设置Tag
;4. archive finished items in gtdfile

;; Agenda视图（<C-c a a>）
;; <d>,<w>,<v t>,<v m>  显示日/周/双周/月视图
;; <.>,<f>,<b>,<j>      跳转到今天/当前视图的上一页/下一页/指定日期
;; <g>                  刷新当前视图

(defun mysetting/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(defun mysetting/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))
(defun mysetting/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))
(defun mysetting/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(use-package org :defer t
  :init (setq org-agenda-path (expand-file-name "agenda/" user-emacs-directory))
  :config
  (org-clock-persistence-insinuate)
  (let ((agenda-file (expand-file-name "todolist.org" org-agenda-path))
        (note-file (expand-file-name "notes.org" org-agenda-path)))
    (setq org-agenda-files (list agenda-file note-file))
    (setq org-default-notes-file note-file)
    (unless (file-exists-p org-agenda-path)
      (make-directory org-agenda-path))
    (unless (file-exists-p agenda-file)
      (append-to-file "# -*- mode:org; coding:utf-8 -*-\n\n" nil agenda-file)
      (append-to-file "* Action\n\n" nil agenda-file)
      (append-to-file "* Information\n\n" nil agenda-file)
      (append-to-file "* Reserved\n\n" nil agenda-file)
      (append-to-file "* org-mode configuration\n" nil agenda-file)
      (append-to-file "#+STARTUP: overview\n" nil agenda-file)
      (append-to-file "#+STARTUP: hidestars\n" nil agenda-file)
      (append-to-file "#+STARTUP: logdone\n" nil agenda-file)
      (append-to-file "#+COLUMNS: %38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM{Total}\n" nil agenda-file)
      (append-to-file "#+PROPERTY: Effort_ALL 0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 8:00\n" nil agenda-file)
      (append-to-file "#+TAGS: OVERALL(o) PROJECT(p)\n" nil agenda-file)
      (append-to-file "#+SEQ_TODO: TODO(t) STARTED(s) WAITING(w) APPT(a) | DONE(d) CANCELLED(c) DEFERRED(f)\n" nil agenda-file)
      )
    (unless (file-exists-p note-file)
      (append-to-file "# -*- mode:org; coding:utf-8 -*-\n" nil note-file)
      )
    )
  :custom
  (org-log-done t)
  (org-edit-timestamp-down-means-later t)
  (org-fast-tag-selection-single-key 'expert)
  (org-tags-column 80)
  (org-log-into-drawer t))
(use-package org-refile :defer t
  :custom
  (org-refile-use-cache nil)
  (org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
  (org-refile-target-verify-function 'mysetting/verify-refile-target)
  (org-refile-use-outline-path t)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-outline-path-complete-in-steps nil))
(use-package org-agenda :defer t
  :config
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry)
  :custom
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  (org-agenda-compact-blocks t)
  (org-agenda-sticky t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 'day)
  (org-agenda-sorting-strategy
  '((agenda habit-down time-up user-defined-up effort-up category-keep)
    (todo category-up effort-up)
    (tags category-up effort-up)
    (search category-up)))
  (org-agenda-window-setup 'current-window)
  (org-agenda-include-diary t)
  (org-agenda-custom-commands
  `(("N" "Notes" tags "NOTE"
     ((org-agenda-overriding-header "Notes")
      (org-tags-match-list-sublevels t)))
    ("g" "GTD"
     ((agenda "" nil)
      ;; (agenda "" ((org-agenda-span 7)
      ;;             (org-agenda-sorting-strategy
      ;;              (quote ((agenda time-up priority-down tag-up))))
      ;;             (org-deadline-warning-days 0)))
      (tags-todo "PROJECT"
                 ((org-agenda-overriding-header "Project")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled 'future)
                  (org-agenda-sorting-strategy '(category-keep))))
      (tags-todo "OVERALL"
                 ((org-agenda-overriding-header "Overall")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled 'future)
                  (org-agenda-sorting-strategy '(category-keep))))
      (todo "APPT"
                 ((org-agenda-overriding-header "Appointment")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled 'future)
                  (org-agenda-sorting-strategy '(category-keep))))
      (todo "WAITING"
                 ((org-agenda-overriding-header "Waiting")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled 'future)
                  (org-agenda-sorting-strategy '(category-keep))))
      ))
    )))
(use-package org-archive :defer t
  :custom
  (org-archive-mark-done nil)
  (org-archive-location "%s_archive::* Archive"))
(use-package org-capture :defer t
  :custom
  (org-capture-templates
  `(("t" "todo" entry (file "")  ; "" => org-default-notes-file
     "* TODO %?\n%U\n" :clock-resume t)
    ("n" "note" entry (file "")
     "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
    )))
(use-package org-clock :defer t
  :config
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)
  (add-hook 'org-clock-out-hook 'mysetting/remove-empty-drawer-on-clock-out 'append)
  :custom
  (org-clock-persist t)
  (org-clock-in-resume t)
  (org-clock-into-drawer t)
  (org-clock-out-remove-zero-time-clocks t))

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)

(add-hook 'org-agenda-mode-hook 'hl-line-mode)
(add-hook 'org-clock-in-hook 'mysetting/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'mysetting/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'mysetting/hide-org-clock-from-header-line)

(org-agenda-to-appt)

(provide 'init_org_gtd)