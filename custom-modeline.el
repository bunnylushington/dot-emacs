(defun ii/nano-modeline-maybe-set-project ()
  (let ((project (projectile-project-name)))
    (if (> (length project) 1)
        (propertize (concat " ◊ " project) 'face 'italic))))

(defun ii/nano-modeline-default-mode ()
  (if lsp-mode
      (ii/nano-modeline-default-mode-lsp)
    (ii/nano-modeline-default-mode-nolsp)))

(defun ii/buffer-position ()
  "Escape the % that %p (sometimes) provides."
  (let ((pos (format-mode-line "%l:%c %p")))
    (if (s-ends-with-p "%" pos)
        (concat pos "%")
      pos)))

(defun ii/default-face (str)
  (propertize str 'face 'nano-face-header-default))

(defun ii/nano-modeline-default-mode-nolsp ()
  "Default modeline, LSP not enabled."
  (let* ((buffer-name (if (or (fancy-narrow-active-p) (buffer-narrowed-p))
                          (format-mode-line "%b…")
                        (format-mode-line "%b")))
         (mode-name   (nano-mode-name))
         (project     (ii/nano-modeline-maybe-set-project))
         (branch      (vc-branch))
         (position    (ii/buffer-position))
         (b-title     (if branch
                          (concat ", " (propertize branch 'face 'italic)))))
    (ii/nano-modeline-compose (nano-modeline-status)
                              buffer-name
                              (concat "(" mode-name b-title ")" project)
                              position)))

(defun ii/nano-modeline-default-mode-lsp ()
  "Default modeline, steal components from lsp-headerline."
  (let* ((mode-name   (nano-mode-name))
         (project     (ii/nano-modeline-maybe-set-project))
         (branch      (vc-branch))
         (position    (ii/buffer-position))
         (b-title     (if branch
                          (concat ", " (propertize branch 'face 'italic)))))
    (ii/nano-modeline-compose-lsp
     (nano-modeline-status)
     (let* ((filename  (lsp-headerline--build-file-string))
            (buffername (if (eq filename "")
                            (ii/default-face (format-mode-line "%b"))
                          filename))
            (indicator  (if (or (fancy-narrow-active-p) (buffer-narrowed-p))
                            (ii/default-face "…") "")))
       (concat buffername indicator))
     (concat " (" mode-name b-title ", LSP)" project)
     (lsp-headerline--build-symbol-string)
     position)))


(defun ii/nano-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (space-up       +0.15)
         (space-down     -0.20)
	     (prefix (cond ((string= status "RO")
			            (propertize (if (window-dedicated-p)" -- " " RO ")
                                    'face 'nano-face-header-popout))
                       ((string= status "**")
			            (propertize (if (window-dedicated-p)" -- " " ** ")
                                    'face 'nano-face-header-critical))
                       ((string= status "RW")
			            (propertize (if (window-dedicated-p)" -- " " RW ")
                                    'face 'nano-face-header-faded))
                       (t (propertize status 'face 'nano-face-header-popout))))
         (left (concat
                (propertize " "  'face 'nano-face-header-default
			                'display `(raise ,space-up))
                (propertize name 'face 'nano-face-header-strong)
                (propertize " "  'face 'nano-face-header-default
			                'display `(raise ,space-down))
		        (propertize primary 'face 'nano-face-header-default)))
         (right (concat secondary " "))
         (length-right (if (s-ends-with-p "%" secondary)
                           (- (length right) 1)
                         (length right)))
         (available-width (- (window-total-width)
			                 (length prefix) (length left) length-right
			                 (/ (window-right-divider-width) char-width)))
	     (available-width (max 1 available-width)))
    (concat prefix
	        left
	        (propertize (make-string available-width ?\ )
                        'face 'nano-face-header-default)
	        (propertize right 'face `(:inherit nano-face-header-default
                                               :foreground ,nano-color-faded)))))

(defun ii/nano-modeline-compose-lsp (status name primary secondary position)
  "Custom replacement for nano-modeline-compose."
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (space-up       +0.15)
         (space-down     -0.20)
         (end-space (propertize " "
                                'face 'nano-face-header-default
                                'display `(raise ,space-up)))
         (prefix (cond ((string= status "RO")
                        (propertize (if (window-dedicated-p)" -- " " RO ")
                                    'face 'nano-face-header-popout))
                       ((string= status "**")
                        (propertize (if (window-dedicated-p)" -- " " ** ")
                                    'face 'nano-face-header-critical))
                       ((string= status "RW")
                        (propertize (if (window-dedicated-p)" -- " " RW ")
                                    'face 'nano-face-header-faded))
                       (t (propertize status 'face 'nano-face-header-popout))))
         (left (concat
                (propertize " "
                            'face 'nano-face-header-default
                            'display `(raise ,space-up))
                (ii/hack-lsp-symbol-string name)
                (propertize " "
                            'face 'nano-face-header-default
                            'display `(raise ,space-down))
                (ii/default-face primary)))
         (right (concat
                 (ii/hack-lsp-symbol-string secondary)
                 (propertize (concat " " position)
                             'face `(:inherit nano-face-header-default
                                              :foreground ,nano-color-faded))))
         ;; nn%% (percentage escaped) is one character too long and
         ;; throws off the width.  go ahead and account for that here:
         (length-right (if (s-ends-with-p "%" right)
                           (- (length right) 1)
                         (length right)))
         (available-width (- (window-total-width)
                             (length prefix) (length left) length-right
                             (length end-space)
                             (/ (window-right-divider-width) char-width)))
         (available-width (max 1 available-width)))
    (concat prefix
            left
            (ii/default-face (make-string available-width ?\ ))
            right
            end-space)))


(defun ii/hack-lsp-symbol-string (symbol)
  "Reformat the symbol string for Nano's header line.

Specifically:
  - remove any pre- and post-fix space
  - replace un-prop'd space around the separator with default space."
  (if (or (not symbol) (= 0 (length symbol)))
      ""
    (let* ((trimmed (s-trim symbol))
           (space (ii/default-face " "))
           (unspaced (replace-regexp-in-string
                      " +\\([^ ]+\\) +"
                      (concat space "\\1" space)
                      trimmed)))
      unspaced)))

(fset 'nano-modeline-default-mode 'ii/nano-modeline-default-mode)

;;;;
