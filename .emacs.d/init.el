; Define the directory of this file so that hard paths aren't referenced
; everywhere.
(defconst emacs-config-directory (file-name-directory load-file-name))

; Convenience function to get full path of file in this directory
; (or subdirectory).
(defun expand-config-file-name (name)
  (concat emacs-config-directory "/" name))

; Convenience function to add a directory under `./packages` to the
; `load-path`
(defun add-package-directory (directory)
  (add-to-list 'load-path (expand-config-file-name (concat "packages/" directory))))
  
(let ((packages '(
    color-theme
    js2-mode
    magit
  )))
  (setq package-archives
    '(("gnu" . "http://elpa.gnu.org/packages/")
      ("marmalade" . "http://marmalade-repo.org/packages/")))
  (package-initialize)
  (dolist (package-name packages)
    (when (not (package-installed-p package-name))
      (package-refresh-contents)
      (package-install package-name))))

(dolist (path '(
    "color-theme-vibrant-ink"
    "evil"
    "key-chord"
  ))
  (add-package-directory path))

; Load each `.el` file in `init.d`. 
(dolist (file (directory-files (expand-config-file-name "init.d/") t "\\.el$"))
  (load file)) 
