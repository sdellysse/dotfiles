;; Start an Emacs server instance (I think. Check this one out for more details)
(server-start)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))

(require 'my-font)
(require 'my-defaults)
