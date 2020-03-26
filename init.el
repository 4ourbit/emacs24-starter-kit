;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; http://orgmode.org/worg/org-contrib/babel/intro.html

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;; remember this directory
(setq starter-kit-dir
      (file-name-directory (or load-file-name (buffer-file-name))))

;; The GC can easily double startup time, so we suppress it at startup by
;; enforcing a high GC threshold. When idling GC is immediately triggered
;; and a low threshold is set.
(let ((gcmh-dir (expand-file-name "src/gcmh" starter-kit-dir)))
  (add-to-list 'load-path gcmh-dir)
  (require 'gcmh)
  (gcmh-mode 1))

(define-key input-decode-map (kbd "M-?") (kbd "C-h"))

(define-key input-decode-map (kbd "C-c C-k") (kbd "C-c C-k"))
(define-key input-decode-map (kbd "C-x C-k") (kbd "C-x C-k"))
(define-key input-decode-map (kbd "C-k") (kbd "<up>"))
(define-key input-decode-map (kbd "C-c C-j") (kbd "C-c C-j"))
(define-key input-decode-map (kbd "C-x C-j") (kbd "C-x C-j"))
(define-key input-decode-map (kbd "C-j") (kbd "<down>"))
(define-key input-decode-map (kbd "C-c C-l") (kbd "C-c C-l"))
(define-key input-decode-map (kbd "C-x C-l") (kbd "C-x C-l"))
(define-key input-decode-map (kbd "C-l") (kbd "C-<left>"))
(define-key input-decode-map (kbd "C-c C-ö") (kbd "C-c C-ö"))
(define-key input-decode-map (kbd "C-x C-ö") (kbd "C-x C-ö"))
(define-key input-decode-map (kbd "C-ö") (kbd "C-<right>"))
(define-key input-decode-map (kbd "C-c C-;") (kbd "C-c C-;"))
(define-key input-decode-map (kbd "C-x C-;") (kbd "C-x C-;"))
(define-key input-decode-map (kbd "C-;") (kbd "C-<right>"))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Translation-Keymaps.html
;; 22.14.1 Interaction with normal keymaps
;; ..., it is better to avoid binding commands to key sequences where the end of
;; the key sequence is a prefix of a key translation. The main such problematic
;; suffixes/prefixes are <ESC>, M-O (which is really <ESC> O) and M-[ (which is
;; really <ESC> [).

(define-key input-decode-map (kbd "C-ü") (kbd "<ESC>"))
;; This is where C-[ is located at in de-de keyboard layout replace it with the
;; en-us standard behavior to block it from a future use in user-level key
;; binding maps, in case of switching keyboard layouts.

(define-key input-decode-map (kbd "C-M-j") (kbd "<next>"))
(define-key input-decode-map (kbd "C-M-k") (kbd "<prior>"))
(define-key input-decode-map (kbd "C-M-l") (kbd "<home>"))
(define-key input-decode-map (kbd "C-M-ö") (kbd "<end>"))
(define-key input-decode-map (kbd "C-M-;") (kbd "<end>"))

(define-key input-decode-map (kbd "C-h") (kbd "<backspace>"))

(add-to-list 'load-path (locate-user-emacs-file "src/"))

(with-eval-after-load 'org
  (org-babel-load-file (expand-file-name "starter-kit-org.org" starter-kit-dir)))

;; load Org-mode from source when the ORG_HOME environment variable is set
(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                  (file-directory-p (expand-file-name "lisp"
                                                      (getenv "ORG_HOME"))))
       '(require 'org))
    ;; load up the starter kit
    (org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))))

;;; init.el ends here
