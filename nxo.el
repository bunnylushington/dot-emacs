;;; nxo.el --- Utility functions for NXO projects

;;; Commentary:
;;
;; This library provides some functionality on top of vterm and
;; projectile for managing Erlang Nitrogen/NXO projects.  In
;; particular, there are functions meant to aid in navigating SQL
;; statements in .sql and .eqlite files as well as managing the Docker
;; stack the NXO application is deployed with.

;;; Code:

(require 'vterm)
(require 'projectile)
(require 's)
(require 'ido)
(require 'grep)
(require 'thingatpt)
(require 'browse-url)
(require 'yaml)
(require 'tempo)

(defgroup nxo-mode nil
  "Customization group for nxo-mode."
  :group 'convenience)

(defcustom nxo-mode-command-prefix "H-n"
  "Prefix for nxo-mode."
  :group 'nxo-mode)

;;; This could probably be done in lisp but (a) that seems like more
;;; work and (b) I bet it'dq be a lot slower.
(defcustom nxo-eqlite-parse-command
  "grep -n \"^-- :\" %s | cut -d : -f 1,3 -"
  "Command to snarf out named eqlite statements.

%s represents the full file path."
  :type 'string
  :group 'nxo-mode)

(defcustom nxo-find-file-placement 'frame
  "Where to open new buffers."
  :group 'nxo-mode
  :type '(radio
          (const :tag "Frame" frame)
          (const :tag "Window" window)
          (const :tag "Tab" tab)
          (const :tag "In Place" t)))

(defvar nxo-mode-command-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") 'nxo/front-end-browse)
    (define-key map (kbd "d") 'nxo/project-db)
    (define-key map (kbd "e") 'nxo/project-shell)
    (define-key map (kbd "f") 'nxo/sql-find-occurance)
    (define-key map (kbd "i") 'nxo/sql-insert-name)
    (define-key map (kbd "j") 'nxo/sql-jump)
    (define-key map (kbd "t") 'nxo/project-term)
    (define-key map (kbd "q") 'nxo/project-stop)
    (define-key map (kbd "r") 'nxo/sql-return-from)
    (define-key map (kbd "s") 'nxo/project-start)
    (define-key map (kbd "t") 'nxo/project-term)
    (define-key map (kbd "H-t d") 'nxo/template-dtl)
    (define-key map (kbd "H-t h") 'nxo/template-html)
    (define-key map (kbd "H-t p") 'nxo/template-page)
    (define-key map (kbd "H-; c")
      'nxo/code-comment-template-directive-region)
    (define-key map (kbd "H-; u")
      'nxo/code-uncomment-template-directive-region)
    map)
  "Keymap for nxo-mode commands.")

(defvar nxo-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd nxo-mode-command-prefix) nxo-mode-command-keymap)
    (easy-menu-define nxo-mode-menu map
      "Menu for NXO"
      '("NXO"
        ["Start Containers" nxo/project-start]
        ["Stop Containers" nxo/project-stop]
        "--"
        ["Project Shell" nxo/project-shell]
        ["Project DB Console" nxo/project-db]
        ["Project Terminal" nxo/project-term]
        "--"
        ["Jump to SQL Definition" nxo/sql-jump]
        ["Return from Definition" nxo/sql-return-from]
        ["Insert SQL Statement Name" nxo/sql-insert-name]
        ["Find SQL Reference in Code" nxo/sql-find-occurance]
        "--"
        ["New HTML Template" nxo/template-html]
        ["New DTL Template" nxo/template-dtl]
        ["New Nitrogen Page" nxo/template-page]
        "--"
        ["Comment Page Directive"
         nxo/code-comment-template-directive-region]
        ["Uncomment Page Directive"
         nxo/code-uncomment-template-directive-region]
        "--"
        ["Browse to Project" nxo/front-end-browse]))
    map)
  "Keymap for `nxo-mode'.")

(define-minor-mode nxo-mode
  "Helpers for working with an NXO project."
  :global true
  :lighter " nxo"
  :keymap nxo-mode-keymap)

;;; ------------------------------ Code Manipulations
(defun nxo/code-comment-template-directive-region (start end)
  "Comment out a [[[ ... ]]] construction in an HTML template."
  ;;; expanding the region after the first replacement is required
  ;;; because the replacement is one character longer than the
  ;;; original.
  (interactive "r")
  (replace-string-in-region "[[[" "<!--" start end)
  (replace-string-in-region "]]]" "-->" start (1+ end)))

(defun nxo/code-uncomment-template-directive-region (start end)
  "Uncomment a [[[ ... ]]] construction in an HTML tempalte."
  (interactive "r")
  (replace-string-in-region "<!--" "[[[" start end)
  (replace-string-in-region "-->" "]]]" start end))


;;; ------------------------------ SQL File Management

(defun nxo/sql-jump (all-statements stmt)
  (interactive
   (let* ((all-statements (nxo/sql-statements))
          (names (nxo/all-sql-statements all-statements))
          (tap (thing-at-point 'symbol))
          (default (if (and tap (member tap names)) tap nil))
          (stmt (ido-completing-read "Statement: " names
                                     nil t nil nil default)))
     (list all-statements stmt)))
  (let* ((entry (assoc stmt all-statements))
         (file (caadr entry))
         (line (cdadr entry)))
    (nxo/find-file file)
    (goto-line line)))

(defun nxo/sql-return-from ()
  "Jump back after visiting SQL file."
  (interactive)
  (pop-to-buffer (other-buffer (current-buffer) t)))

(defun nxo/sql-find-occurance (stmt)
  "Find an occurance of the names SQL query in the Erlang code."
  (interactive
   (let* ((names (nxo/all-sql-statements (nxo/sql-statements)))
          (stmt (ido-completing-read "Find statement: " names)))
     (list stmt)))
  (let ((root (concat (projectile-project-root) "src"))
        (regexp (concat "(" stmt)))
  (rgrep regexp "*.erl" root nil)))

(defun nxo/sql-insert-name (stmt names)
  "Insert the selected SQL statement name.

If the statement is not present, offer to create it and allow the
user to choose which eqlite file to append it to."
  (interactive
   (let* ((all-statements (nxo/sql-statements))
          (names (nxo/all-sql-statements all-statements))
          (tap (thing-at-point 'symbol))
          (default (if tap tap nil))
          (stmt (ido-completing-read "Statement: " names
                                     nil nil default)))
     (list stmt names)))
  (if (member stmt names)
      (progn
        (nxo/kill-word-at-point)
        (insert stmt))
    (nxo/offer-to-create-stmt stmt)))

(defun nxo/offer-to-create-stmt (stmt)
  "Offer to create the SQL Statement"
  (if (y-or-n-p (concat "Create new SQL statement " stmt))
      (let* ((candidates (nxo/find-eqlite-files))
             (file-stem (ido-completing-read "eqlite file: " candidates))
             (file-path (concat (projectile-project-root) file-stem)))
        (nxo/find-file file-path)
        (goto-char (point-max))
        (insert (concat "\n-- :" stmt "\n")))
    (message "SQL Insert Cancelled.")))

(defun nxo/kill-word-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        (kill-region (car bounds) (cdr bounds)))))

(defun nxo/all-sql-statements (all-sql-files)
  (mapcar 'car all-sql-files))

(defun nxo/find-sql-files ()
  (interactive)
  (directory-files-recursively
   (projectile-project-root) ".+\\.eqlite$\\|.+\\.sql$"))

(defun nxo/find-eqlite-files ()
  "List the available eqlite files in the project.

This list comes back with the shortest (i.e., the most likely)
paths first and the projectile-project-root removed.)"
  (interactive)
  (let* ((root (projectile-project-root))
         (candidates (directory-files-recursively root ".+\\.eqlite$")))
    (mapcar (lambda (x) (s-chop-prefix root x))
            (sort candidates (lambda (x y) (< (length x) (length y)))))))

(defun nxo/sql-statements ()
  "Generate a completion list of sql-statements in the NXO project."
  (interactive)
  (let* ((file-list (nxo/find-sql-files))
         (eqlite-statements (nxo/find-eqlite-statements file-list))
         (sql-statements (nxo/find-sql-statements file-list))
         (ddl-statements (nxo/find-ddl-statements file-list)))
    (append eqlite-statements sql-statements ddl-statements)))

(defun nxo/find-sql-statements (file-list)
  "Find the names of all the single statement SQL files."
  (let ((result (list)))
    (dolist (file file-list result)
      (if (nxo/sql-file-p file)
          (let* ((token (file-name-base file))
                 (lineno 1)
                 (entry (nxo/make-sqltags-entry token file lineno)))
            (setq result (cons (list entry) result)))))))

(defun nxo/find-eqlite-statements (file-list)
  "Find all the named statements in the eqlite files."
  (let ((result (list)))
    (dolist (file file-list result)
      (if (nxo/eqlite-file-p file)
          (setq result (append (nxo/parse-eqlite-file file) result))))))

(defun nxo/parse-eqlite-file (file)
  "Find all the named statements in an eqlite file."
  (interactive)
  (let* ((cmd (format nxo-eqlite-parse-command file))
         (res (shell-command-to-string cmd))
         (lst (s-split "\n" res t))
         (result (list)))
    (dolist (slug lst result)
      (let* ((split (s-split ":" slug))
             (lineno (string-to-number (car split)))
             (token (cadr split))
             (entry (nxo/make-sqltags-entry token file lineno)))
        (setq result (append (list entry) result))))))

(defun nxo/find-ddl-statements (file-list)
  "Find all the CREATE {TABLE,VIEW} statements in DDL files."
  '())

(defun nxo/make-sqltags-entry (token path line)
  "A sqltags list entry constructor."
  (list token (cons path line)))

(defmacro nxo/is-sql-file-type (path suffix pathpart)
  `(and (s-ends-with-p ,suffix ,path)
        (s-contains-p ,pathpart ,path)))

(defun nxo/sql-file-p (path)
  (nxo/is-sql-file-type path ".sql" "/sql/"))

(defun nxo/ddl-file-p (path)
  (nxo/is-sql-file-type path ".sql" "/ddl/"))

(defun nxo/eqlite-file-p (path)
  (nxo/is-sql-file-type path ".eqlite" "/sql/"))


;;; ------------------------------ Container/VTerm Management
(defun nxo/find-project-ctl ()
  "If <root>/bin/<project ctl> exists, return the filepath.

It is assumed that the <project ctl> file has the same name as
the project.  This is true using the NXO template."
  (interactive)
  (let ((root (projectile-project-root))
        (name (projectile-project-name)))
    (if (and name root)
        (let ((executable (concat root "bin/" name)))
          (if (file-executable-p executable)
              executable
            nil))
      nil)))

(defmacro nxo/project-command (cmd)
  (let ((ctl (gensym))
        (new-dir (gensym))
        (existing-default-dir (gensym)))
    `(let* ((,ctl (nxo/find-project-ctl))
            (,existing-default-dir default-directory)
            (,new-dir (projectile-project-root)))
       (if ,ctl
           (progn
             (cd ,new-dir)
             (shell-command (concat ,ctl " " ,cmd))
             (cd ,existing-default-dir))
         (message "Project control not found.")))))

(defun nxo/project-start ()
  "Start the NXO project's Docker swarm stack."
  (interactive)
  (nxo/project-command "start"))

(defun nxo/project-stop ()
  "Stop the NXO project's Docker swarm stack."
  (interactive)
  (nxo/project-command "stop"))

(defmacro nxo/vterm-project-command (command)
  (let ((project-name (gensym))
        (ctl (gensym))
        (buffer-name (gensym))
        (full-command (gensym)))
    `(let ((,ctl (nxo/find-project-ctl)))
       (if ,ctl
           (let* ((,project-name (projectile-project-name))
                  (,full-command (concat ,ctl " " ,command))
                  (,buffer-name (concat ,project-name " [" ,command "]")))
             (if (get-buffer ,buffer-name)
                 (pop-to-buffer ,buffer-name)
               (nxo/run-in-vterm ,full-command ,buffer-name)))
         (message "Project control not found.")))))

(defun nxo/project-term ()
  "Open or pop to the project terminal."
  (interactive)
  (let ((project-term (concat (projectile-project-name) " Terminal")))
    (if (get-buffer project-term)
        (pop-to-buffer project-term)
      (let ((cmd (concat "cd " (projectile-project-root))))
        (nxo/run-in-vterm cmd project-term)))))


(defun nxo/project-shell ()
  "Open the NXO project shell in a vterm buffer."
  (interactive)
  (nxo/vterm-project-command "shell"))

(defun nxo/project-db ()
  "Open the NXO project DB console in a vterm buffer."
  (interactive)
  (nxo/vterm-project-command "db"))

;;; See https://tinyurl.com/y2f9g83p
(defun nxo/run-in-vterm (command buf)
  "Execute string COMMAND in a new vterm BUF."
  (interactive)
  (message (concat "Running command: " command))
  (with-current-buffer (vterm buf)
    (vterm-send-string command)
    (vterm-send-return)))

;;; ------------------------------ TEMPLATE CREATION
(defmacro nxo/priv-subdir (subdir)
  `(concat (projectile-project-root)
           (file-name-as-directory "priv")
           (file-name-as-directory ,subdir)))

(defmacro nxo/find-files-with-extension (dir ext)
  `(directory-files-recursively ,dir (concat ".+\\." ,ext "$")))

(defmacro nxo/template-names (files base ext)
  `(mapcar (lambda (f) (s-chop-suffix ,ext (s-chop-prefix ,base f)))
           ,files))

(defun nxo/template-html-dir ()
  (nxo/priv-subdir "templates"))

(defun nxo/template-html-files ()
  (nxo/find-files-with-extension (nxo/template-html-dir) "html"))

(defun nxo/template-html-names ()
  (nxo/template-names
   (nxo/template-html-files) (nxo/template-html-dir) ".html"))

(defun nxo/template-html (&optional name)
  "Create a new NXO HTML template."
  (interactive)
  (let* ((html (if name name
                 (ido-completing-read "Template name (.html assumed): "
                                      (nxo/template-html-names)
                                      nil nil
                                      (nxo/template-from-buffer-name))))
         (file (if (s-ends-with-p ".html" html) html (concat html ".html")))
         (path (concat (nxo/template-html-dir) file)))
    (if (file-exists-p path)
        (nxo/find-file path)
      (progn
        (nxo/find-file path)
        (tempo-template-nxo-html-stub)
        (save-buffer)))))

(defun nxo/template-dtl-dir ()
  (nxo/priv-subdir "dtl"))

(defun nxo/template-dtl-files ()
  (nxo/find-files-with-extension (nxo/template-dtl-dir) "dtl"))

(defun nxo/template-dtl-names ()
  (nxo/template-names
   (nxo/template-dtl-files) (nxo/template-dtl-dir) ".dtl"))

(defun nxo/template-dtl (&optional name)
  "Pop to or create a new NXO DTL template."
  (interactive)
  (let* ((dtl (if name name
                (ido-completing-read "Template name (.dtl assumed): "
                                     (nxo/template-dtl-names)
                                     nil nil
                                     (nxo/template-from-buffer-name))))
         (file (if s-ends-with-p ".dtl" dtl) dtl (concat dtl ".dtl"))
         (path (concat (nxo/template-dtl-dir) file)))
    (nxo/find-file path)))

(defun nxo/template-from-buffer-name ()
  (interactive)
  (let ((page-parts (s-split-up-to "page_" (buffer-name) 2)))
    (if (= (length page-parts) 2)
        (file-name-sans-extension (cadr page-parts))
      "")))


;;; ------------------------------ page_X.erl manipulation

(defun nxo/template-page (&optional name)
  "Pop to or create a new NXO page_*.erl."
  (interactive)
  (let* ((page (if name name
                 (ido-completing-read "Page name: "
                                      (nxo/page-names))))
         (file-prefix (if (s-starts-with-p "page_" page)
                          page
                        (concat "page_" page)))
         (file-suffix (if (s-ends-with-p ".erl" file-prefix)
                          file-prefix
                        (concat file-prefix ".erl")))
         (path (concat (nxo/page-path) file-suffix)))
    (if (file-exists-p path)
        (nxo/find-file path)
      (progn
        (nxo/find-file path)
        (tempo-template-nxo-page)
        (save-buffer)
        (let ((source-buffer (current-buffer))
              (html-template
               (concat (nxo/template-from-buffer-name) ".html")))
          (if (y-or-n-p "Create corresponding HTML Template? ")
              (progn
                (nxo/template-html html-template)
                (pop-to-buffer source-buffer))))))))

(defun nxo/page-names ()
  (let* ((base (concat (projectile-project-root)
                      (file-name-as-directory "src")))
         (files (directory-files-recursively base "^page_.+\\.erl$")))
    (mapcar (lambda (f) (file-name-sans-extension
                         (cadr (s-split-up-to "page_" f 2)))) files)))
(defun nxo/page-path ()
  (concat (projectile-project-root)
          (file-name-as-directory "src")
          (file-name-as-directory "pages")))

(defun nxo/page-name ()
  (let ((parts (s-split-up-to "page_" (buffer-name) 2)))
    (if (= (length parts) 2)
        (file-name-sans-extension (cadr parts))
      "index")))

(defun nxo/page-module ()
  (file-name-base (buffer-name)))

;;; ------------------------------ BROWSE FE
(defun nxo/yaml-read-file (file)
  (let ((yaml-string (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
    (yaml-parse-string yaml-string :sequence-type 'list)))

(defun nxo/front-end-port ()
    (let* ((compose (concat (projectile-project-root) "docker-compose.yml"))
           (data (nxo/yaml-read-file compose))
           (ports (gethash 'ports (gethash 'fe (gethash 'services data)))))
      (if (listp ports)
          (car (s-split ":" (car ports))))))

(defun nxo/front-end-url ()
  (concat "http://localhost:" (nxo/front-end-port)))

(defun nxo/front-end-browse ()
  (interactive)
  (browse-url (nxo/front-end-url)))


(defun nxo/find-file (file)
  (let* ((type nxo-find-file-placement)
         (ff (cond ((equal type 'frame) 'find-file-other-frame)
                   ((equal type 'window) 'find-file-other-window)
                   ((equal type 'tab) 'find-file-other-tab)
                   (t 'find-file))))
    (funcall ff file)))

;;; ------------------------------ TEMPO TEMPLATES

(tempo-define-template
 "nxo-html-stub"
 '("<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <title>[[[page:title()]]]</title>
    [[[ nxo_view:header() ]]]
  </head>

  <body>
    [[[ nxo_view:menu() ]]]
    <div class=\"container\">
      [[[page:body()]]]
    </div>


    [[[ nxo_view:footer() ]]]
  </body>
</html>")
   nil
   "Insert a new HTML template.")

(tempo-define-template
 "nxo-page"
 '("-module(" (nxo/page-module) ")." n
   "-include(\"" (projectile-project-name) ".hrl\")." n
   "-export([main/0, title/0, body/0, event/1, button/1])." n
   n
   "-security(none)." n
   "-postback_security(none)." n
   "-origin_security(none)." n
   n
   "main() -> #template{file=nxo:template(\"" (nxo/page-name) ".html\")}." n
   n
   "title() -> []." n
   n
   "body() -> []." n
   n
   "event(_) ->" n>
   "ok." n
   n
   "button(_) ->" n>
   "button." n)
 nil
 "Insert a new NXO Page template.")


(provide 'nxo-mode)

;;; nxo.el ends here
