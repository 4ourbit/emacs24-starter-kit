;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; http://orgmode.org/worg/org-contrib/babel/intro.html

(define-key input-decode-map (kbd "M-?") (kbd "C-h"))
(define-key input-decode-map (kbd "C-k") (kbd "<up>"))
(define-key input-decode-map (kbd "C-j") (kbd "<down>"))
(define-key input-decode-map (kbd "C-l") (kbd "C-<left>"))
(define-key input-decode-map (kbd "C-ö") (kbd "C-<right>"))

(define-key input-decode-map (kbd "C-M-j") (kbd "<next>"))
(define-key input-decode-map (kbd "C-M-k") (kbd "<prior>"))
(define-key input-decode-map (kbd "C-M-ö") (kbd "<end>"))
(define-key input-decode-map (kbd "C-M-l") (kbd "<home>"))
(define-key input-decode-map (kbd "C-h") (kbd "<backspace>"))
;; (define-key input-decode-map (kbd "C-M-h") (kbd "C-<backspace>"))
;; (define-key my-keys-minor-mode-map (kbd "C-M-h") 'backward-kill-word)
;; (define-key my-keys-minor-mode-map (kbd "M-ö") 'backward-kill-word)
;; (define-key input-decode-map (kbd "C-m") (kbd "<return>"))

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
    ;; remember this directory
    (setq starter-kit-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                  (file-directory-p (expand-file-name "lisp"
                                                      (getenv "ORG_HOME"))))
       '(require 'org))
    ;; load up the starter kit
    (org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))))

;;; init.el ends here
