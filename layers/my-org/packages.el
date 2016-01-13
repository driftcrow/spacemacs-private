;;; packages.el --- my-org Layer packages File for Spacemacs
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
(setq my-org-packages
    '(
      ;; package names go here
      ;; org
      (org :location built-in)
      org-bullets
      ox-reveal
      org-mac-link
      ;; worf
      org-download
      ))

;; List of packages to exclude.
(setq my-org-excluded-packages '())

;; For each package, define a function my-org/init-<package-name>
;;
;; (defun my-org/init-my-package ()
;;   "Initialize my package"
;;   )
(defun my-org/init-org-mac-link ()
  (use-package org-mac-link
    :init
    (add-hook 'org-mode-hook (lambda ()
                               (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))))

(defun my-org/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :init
    (progn
      (setq org-reveal-root "file:///Users/liubin/.emacs.d/reveal-js")))) ; TODO::change for useful

(defun my-org/init-org-download ()
  (use-package org-download
    :defer t
    :init
    (org-download-enable)))

(defun my-org/post-init-org ()
  (progn
    (setq org-directory "~/org")
    (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
    (setq org-default-works-file (expand-file-name "works.org" org-directory))
    (setq org-default-journal-file (expand-file-name "journal.org.gpg" org-directory))
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
             "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
            ("j" "Journal Entry"
             entry (file+datetree+prompt org-default-journal-file )
             "* %?\nEntered on %U\n"
             :kill-buffer
             :empty-lines 1)))

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
    (defun my-org/org-insert-src-block (src-code-type)
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
                                               'my-org/org-insert-src-block)
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
             (sh . t)
             (ledger . t)
             (org . t)
             (plantuml . t)
             (latex . t))))

    (setq org-plantuml-jar-path
          (expand-file-name "~/.spacemacs.d/plantuml.jar"))
    (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")
    ))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
