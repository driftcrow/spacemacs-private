;;; packages.el --- liubin layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <liubin@AMBIT-PC>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `liubin-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `liubin/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `liubin/pre-init-PACKAGE' and/or
;;   `liubin/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst liubin-packages
  '(
    ;; package names go here
    ;; lispy                             ; conflic with imenu
    unicode-fonts
    discover-my-major

    youdao-dictionary
    helm-gtags
    helm-github-stars

    4clojure
    midje-mode
    hydra

    elfeed

    cal-china-x
    chinese-wbim

    beacon
    (org :location built-in)
    org-bullets
    ox-reveal
    org-mac-link
    ;; worf
    org-download
    graphviz-dot-mode
    )
  "The list of Lisp packages required by the liubin layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun liubin/init-unicode-fonts ()
  (use-package unicode-fonts
    :init
    (unicode-fonts-setup)))

(defun liubin/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "hdM") 'discover-my-major)

      (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map))))

(defun liubin/post-init-youdao-dictionary ()
  (evil-leader/set-key "oy" 'youdao-dictionary-search-at-point+))

(defun liubin/post-init-helm-gtags ()
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


(defun liubin/init-helm-github-stars ()
  (use-package helm-github-stars
    :init
    (evil-leader/set-key "ag" 'helm-github-stars)
    :config
    (progn
      (setq helm-github-stars-username "driftcrow")
      (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache"))))


(defun liubin/init-lispy ()
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

(defun liubin/init-4clojure ()
  (use-package 4clojure
    :init
    (progn
      (spacemacs/declare-prefix "o4" "4clojure")
      (evil-leader/set-key "o4q" '4clojure-open-question)
      (evil-leader/set-key "o4n" '4clojure-next-question)
      (evil-leader/set-key "o4p" '4clojure-previous-question)
      (evil-leader/set-key "o4c" '4clojure-check-answers)
      )))

(defun liubin/init-midje-mode ()
  (use-package midje-mode
    :defer t
    :init
    (add-hook 'clojure-mode-hook 'midje-mode)))

(defun liubin/post-init-hydra ()
  (use-package hydra
    :config
    (progn
      ;; major mode hydra is really cool, don't need to switch mode anymore
      ;; C-c [a-z] and s-[a-z] is very quick to pressed even in emacs-state and F1-F9 is also the same
      ;; If the command will change the buffer, they should be put in these groups.
      ;; otherwise, use which-key + spacems + user defined key mappings in evil normal mode
      (defhydra hydra-yasnippet (:color blue :hint nil)
        "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
        ("d" yas-load-directory)
        ("e" yas-activate-extra-mode)
        ("i" yas-insert-snippet)
        ("f" yas-visit-snippet-file :color blue)
        ("n" yas-new-snippet)
        ("t" yas-tryout-snippet)
        ("l" yas-describe-tables)
        ("g" yas/global-mode)
        ("m" yas/minor-mode)
        ("a" yas-reload-all))

      (spacemacs/set-leader-keys "os" 'hydra-yasnippet/body)

      (defhydra hydra-apropos (:color blue)
        "Apropos"
        ("a" apropos "apropos")
        ("c" apropos-command "cmd")
        ("d" apropos-documentation "doc")
        ("e" apropos-value "val")
        ("l" apropos-library "lib")
        ("o" apropos-user-option "option")
        ("u" apropos-user-option "option")
        ("v" apropos-variable "var")
        ("i" info-apropos "info")
        ("t" tags-apropos "tags")
        ("z" hydra-customize-apropos/body "customize"))

      (defhydra hydra-customize-apropos (:color blue)
        "Apropos (customize)"
        ("a" customize-apropos "apropos")
        ("f" customize-apropos-faces "faces")
        ("g" customize-apropos-groups "groups")
        ("o" customize-apropos-options "options"))

      (spacemacs/set-leader-keys "ha" 'hydra-apropos/body)

      )))

(defun liubin/post-init-chinese-wbim ()
  (progn
    ;; [[http://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings][keymap - How to override major mode bindings - Emacs Stack Exchange]]
    (bind-key* ";" 'chinese-wbim-insert-ascii)
    (setq chinese-wbim-punc-translate-p nil)
    (spacemacs/declare-prefix "ot" "Toggle")
    (spacemacs/set-leader-keys
      "otp" 'chinese-wbim-punc-translate-toggle)
    (setq chinese-wbim-wb-use-gbk t)
    (add-hook 'chinese-wbim-wb-load-hook
              (lambda ()
                (let ((map (chinese-wbim-mode-map)))
                  (define-key map "-" 'chinese-wbim-previous-page)
                  (define-key map "=" 'chinese-wbim-next-page))))
    ))

(defun liubin/init-elfeed ()
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

(defun liubin/init-cal-china-x ()
  "Initialize my package"
  (use-package cal-china-x
    :init
    (evil-leader/set-key "aC" 'calendar)
    :config
    (progn
      (setq mark-holidays-in-calendar t)
      ;; (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
      ;; (setq calendar-holidays cal-china-x-important-holidays)
      (setq calendar-remove-frame-by-deleting t)
      (setq mark-diary-entries-in-calendar t) ; 标记calendar上有diary的日期
      ;; (setq mark-holidays-in-calendar nil) ; 为了突出有diary的日期，calendar上不标记节日
      ;; (setq view-calendar-holidays-initially nil) ; 打开calendar的时候不显示一堆节日

      ;; (setq calendar-load-hook
      ;;       '(lambda ()
      ;;          (set-face-foreground 'diary-face "skyblue")
      ;;          (set-face-background 'holiday-face "slate blue")
      ;;          (set-face-foreground 'holiday-face "white")))

      (setq holiday-driftcrow-holidays
            '(;;公历节日
              (holiday-fixed 1 1   "元旦")
              (holiday-fixed 2 14  "情人节")
              (holiday-fixed 3 8   "妇女节")
              (holiday-fixed 3 14  "白色情人节")
              (holiday-fixed 4 1   "愚人节")
              (holiday-fixed 5 1   "劳动节")
              (holiday-float 5 0 2 "母亲节")
              (holiday-fixed 6 1   "儿童节")
              (holiday-float 6 0 3 "父亲节")
              (holiday-fixed 9 10  "教师节")
              (holiday-fixed 10 1  "国庆节")
              (holiday-fixed 12 25 "圣诞节")
              ;; 农历节日
              (holiday-lunar 1 1   "春节" 0)
              (holiday-lunar 1 2   "春节" 0)
              (holiday-lunar 1 3   "春节" 0)
              (holiday-lunar 1 15  "元宵节" 0)
              (holiday-solar-term  "清明" "清明节")
              (holiday-lunar 5 5   "端午节" 0)
              (holiday-lunar 8 15  "中秋节" 0)
              ;; 生日
              (holiday-lunar 7 1   "Mum's birthday")
              (holiday-lunar 9 24  "Dad's birthday")
              (holiday-lunar 9 13  "Jzh's birthday") ; ???
              (holiday-lunar 8 4   "Yfq's birthday")
              (holiday-lunar 2 11  "Pianer's birthday")
              (holiday-lunar 4 23  "Yanzi's birthday")
              (holiday-lunar 6 11  "T3's birthday")
              (holiday-lunar 4 3   "Liubin's birthday")
              (holiday-lunar 11 15 "Baby's birthday")
              (holiday-lunar 12 7  "Jiao's birthday")
              (holiday-fixed 12 27 "Baby's birthday-w")
              ))

      (setq calendar-holidays holiday-driftcrow-holidays)

      ;;日历基本配置
      ;;设置我所在地方的经纬度，calendar里有个功能是日月食的预测，和你的经纬度相联系的。
      (setq calendar-latitude +39.9)
      (setq calendar-longitude +116.4)

      (setq calendar-location-name "Huai-hua")

      ;; firstday of a week
      (setq calendar-week-start-day 1)

      ;; diary setting
      (setq diary-file "~/org/diary")


      ;;打开calendar自动打开节日和生日列表
      (setq calendar-view-holidays-initially-flag t)
      (setq calendar-mark-holidays-flag t)
      )
    )
  )



(defun liubin/init-org-mac-link ()
  (use-package org-mac-link
    :init
    (add-hook 'org-mode-hook (lambda ()
                               (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))))

(defun liubin/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :init
    (progn
      (setq org-reveal-root (concat dotspacemacs-directory "reveal-js"))))) ; TODO::change for useful

(defun liubin/init-org-download ()
  (use-package org-download
    :defer t
    :init
    (org-download-enable)))

(defun liubin/init-beacon ()
  (use-package beacon
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "otb"))))

(defun liubin/post-init-org ()
  (with-eval-after-load 'org
    (progn
      (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)
      (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

      (setq org-directory "~/org")
      (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
      (setq org-default-works-file (expand-file-name "works.org" org-directory))
      (setq org-default-journal-file (expand-file-name "journal.org.gpg" org-directory))
      (setq org-passwords-file (expand-file-name "passwords.org.gpg" org-directory))
      (setq org-agenda-files  (list org-directory
                                    ;; (expand-file-name "works" org-directory)
                                    ))

      (setq org-agenda-inhibit-startup t)       ;; ~50x speedup
      (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
      (setq org-agenda-window-setup 'current-window)
      (setq org-log-done t)
      (setq org-log-into-drawer t)

      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/@)")
                    (sequence "WAITING(w@/!)" "SOMEDAY(S)"  "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))


      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-targets
            '((nil :maxlevel . 4)
              (org-agenda-files :maxlevel . 4)))

      ;; http://stackoverflow.com/questions/25437069/how-can-i-mark-org-habits-as-done-in-the-past#

      (defun org-todo-custom-date (&optional arg)
        "Like org-todo-yesterday, but prompt the user for a date. The time
of change will be 23:59 on that day"
        (interactive "P")
        (let* ((hour (nth 2 (decode-time
                             (org-current-time))))
               (daysback (- (date-to-day (current-time-string)) (org-time-string-to-absolute (org-read-date))))
               (org-extend-today-until (+ 1 (* 24 (- daysback 1)) hour))
               (org-use-effective-time t)) ; use the adjusted timestamp for logging
          (if (eq major-mode 'org-agenda-mode)
              (org-agenda-todo arg)
            (org-todo arg))))
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode "t" 'org-todo-yesterday)
      ;; Make org files behave in ediff
      ;; unfold the entire org file within ediff
      (add-hook 'ediff-prepare-buffer-hook 'f-ediff-prepare-buffer-hook-setup)
      (defun f-ediff-prepare-buffer-hook-setup ()
        ;; specific modes
        (cond ((eq major-mode 'org-mode)
               (visible-mode 1))
              ))

      ;; 加密文章
      ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
      (require 'org-crypt)
      ;; 當被加密的部份要存入硬碟時，自動加密回去
      (org-crypt-use-before-save-magic)
      ;; 設定要加密的 tag 標籤為 secret
      (setq org-crypt-tag-matcher "secret")
      ;; 避免 secret 這個 tag 被子項目繼承
      ;; (但是子項目還是會被加密喔)
      (setq org-tags-exclude-from-inheritance (quote ("secret")))
      ;; 用於加密的 GPG 金鑰
      ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
      (setq org-crypt-key "hhdslb@gmail.com")


      ;; Org-clock
      ;; Change task state to STARTED when clocking in
      (setq org-clock-in-switch-to-state "STARTED")
      ;; Save clock data and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      ;; Removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line


      ;; the %i would copy the selected text into the template
      ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;;add multi-file journal
      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline  org-default-notes-file "Daily Tasks")
               "* TODO %?\n  %i\n"
               :empty-lines 1)
              ("n" "Note" entry (file+headline  org-default-notes-file "Quick notes")
               "*  %? :NOTE:\n%U\n%a\n"
               :empty-lines 1)
              ("b" "Blog Ideas" entry (file+headline  org-default-notes-file "Blog Ideas")
               "* TODO %?\n  %i\n %U"
               :empty-lines 1)
              ("w" "Works Log"
               entry (file+datetree+prompt org-default-works-file )
               "* %?\nEntered on %U\n"
               :empty-lines 1)
              ("h" "Habit" entry (file (expand-file-name "habit.org" org-directory))
               "* TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:END:\n")
              ("j" "Journal Entry"
               entry (file+datetree+prompt org-default-journal-file )
               "* %?\nEntered on %U\n"
               :kill-buffer
               :empty-lines 1)
              ("p" "password" entry (file+headline org-passwords-file "Password")
               "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p"
               :kill-buffer)
              ))

      (setq org-tags-match-list-sublevels nil)
      ;;An entry without a cookie is treated just like priority ' B '.
      ;;So when create new task, they are default 重要且紧急
      (setq org-agenda-custom-commands
            '(
              ("w" . "任务安排")
              ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
              ("wb" "重要且不紧急的任务" tags-todo "+PRIORITY=\"B\"")
              ("wc" "不重要且不紧急的任务" tags-todo "+PRIORITY=\"C\"")
              ("b" "Blog" tags-todo "BLOG")
              ("p" . "项目安排")
              ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"netmanager\"")
              ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"liubin\"")
              ("W" "Weekly Review"
               ((stuck "")            ;; review stuck projects as designated by org-stuck-projects
                (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                ))
              ("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("D" "Daily Action List"
               ((agenda "" ((org-agenda-ndays 1)
                            (org-agenda-sorting-strategy
                             (quote ((agenda time-up priority-down tag-up) )))
                            (org-deadline-warning-days 0)
                            ))))))

      (defun org-summary-todo (n-done n-not-done)
        "Switch entry to DONE when all subentries are done, to TODO otherwise."
        (let (org-log-done org-log-states) ; turn off logging
          (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

      (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

      ;; used by org-clock-sum-today-by-tags
      (defun filter-by-tags ()
        (let ((head-tags (org-get-tags-at)))
          (member current-tag head-tags)))

      (defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
        (interactive "P")
        (let* ((timerange-numeric-value (prefix-numeric-value timerange))
               (files (org-add-archive-files (org-agenda-files)))
               (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                               "LIFE" "PROJECT" "OTHER"))
               (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
               (output-string "")
               (tstart (or tstart
                           (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                           (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                           (org-time-today)))
               (tend (or tend
                         (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                         (+ tstart 86400)))
               h m file item prompt donesomething)
          (while (setq file (pop files))
            (setq org-agenda-buffer (if (file-exists-p file)
                                        (org-get-agenda-file-buffer file)
                                      (error "No such file %s" file)))
            (with-current-buffer org-agenda-buffer
              (dolist (current-tag include-tags)
                (org-clock-sum tstart tend 'filter-by-tags)
                (setcdr (assoc current-tag tags-time-alist)
                        (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
          (while (setq item (pop tags-time-alist))
            (unless (equal (cdr item) 0)
              (setq donesomething t)
              (setq h (/ (cdr item) 60)
                    m (- (cdr item) (* 60 h)))
              (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
          (unless donesomething
            (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
          (unless noinsert
            (insert output-string))
          output-string))


      ;; http://wenshanren.org/?p=327
      ;; change it to helm
      (defun liubin/org-insert-src-block (src-code-type)
        "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
        (interactive
         (let ((src-code-types
                '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
                  "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
                  "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
                  "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
                  "scheme" "sqlite")))
           (list (ido-completing-read "Source code type: " src-code-types))))
        (progn
          (newline-and-indent)
          (insert (format "#+BEGIN_SRC %s\n" src-code-type))
          (newline-and-indent)
          (insert "#+END_SRC\n")
          (previous-line 2)
          (org-edit-src-code)))

      (add-hook 'org-mode-hook '(lambda ()
                                  ;; keybinding for editing source code blocks
                                  ;; keybinding for inserting code blocks
                                  (local-set-key (kbd "C-c i s")
                                                 'liubin/org-insert-src-block)
                                  ))

      (require 'ox-publish)
      (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{geometry}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\geometry{a4paper, textwidth=6.5in, textheight=10in,marginparsep=7pt, marginparwidth=.6in}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                        ("\\section{%s}" . "\\section*{%s}")
                                        ("\\subsection{%s}" . "\\subsection*{%s}")
                                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      ;; {{ export org-mode in Chinese into PDF
      ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
      ;; and you need install texlive-xetex on different platforms
      ;; To install texlive-xetex:
      ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
      ;; }}
      (setq org-latex-default-class "ctexart")
      (setq org-latex-pdf-process
            '(
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "rm -fr %b.out %b.log %b.tex auto"))

      (setq org-latex-listings t)
      ;; improve org babel

      (org-babel-do-load-languages
       (quote org-babel-load-languages)
       (quote ((emacs-lisp . t)
               (dot . t)
               (ditaa . t)
               (R . t)
               (python . t)
               (ruby . t)
               (gnuplot . t)
               (clojure . t)
               (sql . t)
               (sh . t)
               (js . t)
               (css . t)
               (org . t)
               (plantuml . t)
               (latex . t))))

      (setq org-plantuml-jar-path
            (expand-file-name "~/.spacemacs.d/plantuml.jar"))
      (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")
      ) ))

(defun liubin/post-init-org-bullets ()
  ;; (setq org-bullets-bullet-list '("✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▲" "●" ))
  (setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))
  (setq org-ellipsis "➥...");; ⚡➥⤵ ≫
  ;; (setq org-bullets-bullet-list '("🐉" "🐠" "🐬" "🐤"))
  )

(defun liubin/init-graphviz-dot-mode ()
    (use-package graphviz-dot-mode
      :init
       ))
;;; packages.el ends here
