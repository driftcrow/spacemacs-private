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
      ;; lispy                             ; conflic with imenu

      youdao-dictionary
      helm-gtags
      helm-github-stars

      elfeed
      4clojure
      midje-mode
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
  (use-package helm-gtags
    :diminish helm-gtags-mode
    :defer
    :config
    (progn
      (spacemacs/helm-gtags-define-keys-for-mode 'emacs-lisp-mode)
      (spacemacs/helm-gtags-define-keys-for-mode 'clojure-mode)
      (spacemacs/helm-gtags-define-keys-for-mode 'python-mode)
      (evil-make-overriding-map helm-gtags-mode-map 'normal)
      (add-hook 'helm-gtags-mode-hook #'evil-normalize-keymaps))))


(defun my-misc/init-helm-github-stars ()
  (use-package helm-github-stars
    :init
    (evil-leader/set-key "ag" 'helm-github-stars)
    :config
    (progn
      (setq helm-github-stars-username "driftcrow")
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

(defun my-misc/init-4clojure ()
  (use-package 4clojure
    :init
    (progn
      (spacemacs/declare-prefix "o4" "4clojure")
      (evil-leader/set-key "o4q" '4clojure-open-question)
      (evil-leader/set-key "o4n" '4clojure-next-question)
      (evil-leader/set-key "o4p" '4clojure-previous-question)
      (evil-leader/set-key "o4c" '4clojure-check-answers)
      )))

(defun my-misc/init-midje-mode ()
  (use-package midje-mode
    :defer t
    :init
    (add-hook 'clojure-mode-hook 'midje-mode)))

(defun my-misc/init-elfeed ()
  (use-package elfeed
    :init
    (evil-leader/set-key "ae" 'elfeed)
    :config
    (progn
      (setq elfeed-feeds
            '("http://nullprogram.com/feed/"
              "http://z.caudate.me/rss/"
              "http://irreal.org/blog/?feed=rss2"
              "http://feeds.feedburner.com/LostInTheTriangles"
              "http://tonybai.com/feed/"
              "http://planet.emacsen.org/atom.xml"
              "http://feeds.feedburner.com/emacsblog"
              "http://blog.binchen.org/rss.xml"
              "http://oremacs.com/atom.xml"
              "http://blog.gemserk.com/feed/"
              ;; "http://www.masteringemacs.org/feed/"
              "http://t-machine.org/index.php/feed/"
              "http://gameenginebook.blogspot.com/feeds/posts/default"
              ;; "http://feeds.feedburner.com/ruanyifeng"
              "http://coolshell.cn/feed"
              "http://blog.devtang.com/atom.xml"
              "http://emacsist.com/rss"
              "http://puntoblogspot.blogspot.com/feeds/2507074905876002529/comments/default"
              "http://angelic-sedition.github.io/atom.xml"))

      ;; (evilify elfeed-search-mode elfeed-search-mode-map)
      ;; (spacemacs|evilify-map elfeed-search-mode-map
      ;;   :mode elfeed-search-mode
      ;;   :bindings
      ;;   "G" 'elfeed-update
      ;;   "g" 'elfeed-search-update--force)

      (defun elfeed-mark-all-as-read ()
        (interactive)
        (mark-whole-buffer)
        (elfeed-search-untag-all-unread))

      (define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)

      (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
        "Insert the yanked text from x-selection to kill ring"
        (kill-new (x-get-selection)))

      (ad-activate 'elfeed-show-yank))))
