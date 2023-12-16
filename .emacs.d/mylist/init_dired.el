;; 进入方法
;; 1. <C-x d>
;; 2. <C-x C-f> 输入目录路径
;; 3. 在文件Buffer中，<C-x C-j>打开当前文件所在的目录
;; 退出方法
;; <q>     退出当前Dired Buffer（存在于后台中，还可以切换回来）
;; 移动命令
;; 1. <p>,<n> 上/下一行
;; 2. <<>,<>> 上/下一个目录
;; 3. <^>     父目录
;; 4. <a>     在当前Buffer中跳转目录
;; 5. <Enter> 相当于<C-x C-f>当前条目（会新建Buffer）
;; 6. <X>     使用系统默认应用程序打开当前条目
;; 7. <+>     新建子目录
;; 标记命令
;; 1. <m>     标记文件
;; 2. <u>,<U> 取消当前/全部标记
;; 3. <d>,<x> 标记为删除/删除含删除标记的文件
;; 4. <C>     复制当前文件/被标记的文件
;; 5. <D>     删除当前文件/被标记的文件
;; 6. <R>     移动当前文件/被标记的文件
;; 7. <* %>   使用正则表达示标记文件/子目录
;; 8. <* s>   标记所有文件/子目录
;; 9. <* />   标记所有子目录

(put 'dired-find-alternate-file 'disabled nil)
(use-package dired :defer t
  :config
  (define-key dired-mode-map "X"
    (lambda () (interactive)
      (let* ((file-list (dired-get-marked-files))
             (file-count (length file-list))
             (current-file (dired-get-filename))
             (open-list (if (or (<= file-count 0) (> file-count 5))
                            (list current-file)
                          file-list)))
        (cond
         ((string-equal system-type "windows-nt")
          (mapc
           (lambda (fPath)
             (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) file-list))
         ((string-equal system-type "darwin")
          (mapc
           (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  file-list))
         ((string-equal system-type "gnu/linux")
          (mapc
           (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) file-list)))
        )))
  (define-key dired-mode-map "^"
    (lambda() (interactive) (find-alternate-file "..")))
  :custom
  (dired-recursive-deletes 'top))

(provide 'init_dired)