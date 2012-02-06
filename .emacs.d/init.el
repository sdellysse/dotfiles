; for `map`
(require 'cl)

; Define the directory of this file so that hard paths aren't referenced
; everywhere.
(when (not (boundp 'emacs-config-directory))
  (defconst emacs-config-directory (file-name-directory load-file-name)))

; Convenience function to get full path of file in this directory
; (or subdirectory).
(defun expand-config-file-name (name)
  (concat emacs-config-directory "/" name))

; Convenience function to add a directory under `./packages` to the
; `load-path`
(defun add-package-directory (directory)
  (add-to-list 'load-path (expand-config-file-name (concat "packages/" directory))))

(when (equal system-type 'windows-nt)
  (push "c:/cygwin/bin" exec-path))
  
    (let ((packages '(
        apache-mode
        cedet
        color-theme
        crontab-mode
        csv-mode
        cygwin-mount
        jabber
        js2-mode
        magit
        paredit
	smex
      )))
      (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("marmalade" . "http://marmalade-repo.org/packages/")))
      (package-initialize)
      (dolist (package-name packages)
        (when (not (package-installed-p package-name))
          (package-refresh-contents)
          (package-install package-name))))

(map nil #'add-package-directory '(
  "color-theme-vibrant-ink"
  "evil"
  "key-chord"
  "pi-php-mode"
  "twittering-mode"
))

; Load each `.el` file in `init.d`. 
(map nil #'load (directory-files (expand-config-file-name "init.d/") t "\\.el$"))
