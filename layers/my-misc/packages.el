;;; packages.el --- my-misc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-misc-packages
    '(
      ;; package names go here
      lispy

      youdao-dictionary
      helm-gtags
      helm-github-stars
      ))

;; List of packages to exclude.
(setq my-misc-excluded-packages '())

;; For each package, define a function my-misc/init-<package-name>
;;
;; (defun my-misc/init-my-package ()
;;   "Initialize my package"
;;   )
;;

(defun my-misc/post-init-youdao-dictionary ()
  (evil-leader/set-key "oy" 'youdao-dictionary-search-at-point+))

(defun my-misc/post-init-helm-gtags ()
  (progn
    (spacemacs/helm-gtags-define-keys-for-mode 'emacs-lisp-mode)
    (spacemacs/helm-gtags-define-keys-for-mode 'clojure-mode)
    (spacemacs/helm-gtags-define-keys-for-mode 'python-mode)
     ))

(defun my-misc/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config
    (progn
      (setq helm-github-stars-username "driftcrow")
      (evil-leader/set-key "og" 'helm-github-stars)
      (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache"))))


(defun my-misc/init-lispy ()
  "Initialize lispy"
  (use-package lispy
    :defer t
    :diminish (lispy-mode)
    :config
    (progn
      (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline))
    :init
    (progn
      ;; (define-key evil-insert-state-map (kbd "C-y") 'lispy-yank)
      ;; (define-key evil-insert-state-map (kbd "C-d") 'lispy-delete)
      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1))))))

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
