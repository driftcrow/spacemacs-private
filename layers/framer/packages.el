;;; packages.el --- framer Layer packages File for Spacemacs
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
(setq framer-packages
    '(
      frame-cmds
      ))

;; List of packages to exclude.
(setq framer-excluded-packages '())

;; For each package, define a function framer/init-<package-name>
;;
(defun framer/init-frame-cmds ()
  ;;set frame full height and 86 columns wide
  ;;and position at screen left
  (defun frame-resize-l ()
    "set frame full height and 86 columns wide and position at screen left"
    (interactive)
    (set-frame-width (selected-frame) 86)
    (maximize-frame-vertically)
    (set-frame-position (selected-frame) 0 0)
    )

  ;;set frame full height and 86 columns wide
  ;;and position at screen right
  (defun frame-resize-r ()
    "set frame full height and 86 columns wide and position at screen right"
    (interactive)
    (set-frame-width (selected-frame) 86)
    (maximize-frame-vertically)
    (set-frame-position (selected-frame) (- (display-pixel-width) (frame-pixel-width)) 0)
    )

  ;;set frame full height and 86 columns wide
  ;;and position at screen right of left hand screen in 2 monitor display
  ;;assumes monitors are same resolution
  (defun frame-resize-r2 ()
    "set frame full height and 86 columns wide and position at screen right of left hand screen in 2 monitor display assumes monitors are same resolution"
    (interactive)
    (set-frame-width (selected-frame) 86)
    (maximize-frame-vertically)
    (set-frame-position nil (- (/ (display-pixel-width) 2) (frame-pixel-width)) 0)
    )

  (evil-leader/set-key
    "F2"  'tile-frames-vertically
    "F3"  'tile-frames-horizontally
    "F <SPC>"  'show-frame
    "Fc"  'spacemacs/frame-killer
    "FC"  'delete-other-frames
    "FH"  'toggle-max-frame-horizontally
    "F <S-left>"  'evil-window-move-far-left
    "Fh"  'frame-resize-l
    "F <left>"  'evil-window-left
    "FJ"  'evil-window-move-very-bottom
    "F <S-down>"  'evil-window-move-very-bottom
    "Fj"  'evil-window-down
    "F <down>"  'evil-window-down
    "FK"  'evil-window-move-very-top
    "F <S-up>"  'evil-window-move-very-top
    "Fk"  'evil-window-up
    "F <up>"  'evil-window-up
    "FL"  'evil-window-move-far-right
    "F <S-right>"  'evil-window-move-far-right
    "Fl"  'frame-resize-r
    "F <right>"  'evil-window-right
    "Fm"  'toggle-frame-maximized
    "Fo"  'other-frame
    "Fs"  'create-frame-tiled-vertically
    "F-"  'create-frame-tiled-vertically
    "Fv"  'create-frame-tiled-horizontally
    "FV"  'toggle-max-frame-vertically
    "F/"  'create-frame-tiled-horizontally
    "F="  'tile-frames-horizontally)


;; Frame Manipulation Micro State

(defun spacemacs/shrink-window-horizontally (delta)
  "Wrap `spacemacs/shrink-window-horizontally'."
  (interactive "p")
  (shrink-window delta t))

(defun spacemacs/shrink-window (delta)
  "Wrap `spacemacs/shrink-window'."
  (interactive "p")
  (shrink-window delta))

(defun spacemacs/enlarge-window (delta)
  "Wrap `spacemacs/enlarge-window'."
  (interactive "p")
  (enlarge-window delta))

(defun spacemacs/enlarge-window-horizontally (delta)
  "Wrap `spacemacs/enlarge-window-horizontally'."
  (interactive "p")
  (enlarge-window delta t))

(defun framer//manipulation-full-doc ()
  "Full documentation for window manipulation micro-state."
  "
  [?]                       display this help
  [0,9]                     go to numbered window
  [-] [/] [s] [v] [S] [V]   split windows below|right and focus
  [c] [C]                   close current|other windows
  [g]                       toggle golden-ratio
  [h] [j] [k] [l]           go to left|bottom|top|right
  [H] [J] [K] [L]           move windows to far/very left|bottom|top|right
  [[] []] [{] [}]           shrink/enlarge horizontally and vertically respectively
  [o] [w]                   other frame|window
  [R]                       rotate windows
  [u] [U]                   restore previous|next window layout")

(defun framer//manipulation-move-doc ()
  "Help string for moving between windows"
  (concat "[h] [j] [k] [l] to move focus, "
          "[H] [J] [K] [L] to move window, "
          "[R]otate windows, other [f]rame, other [w]indow"))

(defun framer//manipulation-resize-doc ()
  "Dynamic help string when resizing windows."
  (format
   (concat "[%sx%s] Resize window: [[] []] shrink/enlarge horizontally, "
           "[{] [}] shrink/enlarge vertically.")
   (window-total-width) (window-total-height)))

(defun framer//manipulation-split-doc ()
  "Help string for moving between windows"
  (concat "[-], [s] to split horizontally,  [/], [v] to split vertically, "
          "[S], [V] to split and focus"))

(defun framer//manipulation-number-doc ()
  "Help string for selecting window with number."
  (format "(selected window #%s) press [0,9] to select the corresponding numbered window."
          (window-numbering-get-number-string)))

(defun framer//manipulation-layout-doc ()
  "Help string for layout manipulation"
  (concat "[c]lose window, [C]lose other windows, "
          "[u]ndo window layout, [U] redo window layout."))

(defun framer//manipulation-gratio-doc ()
  "Help string for golden ratio"
  (format "(golden-ration %s) toggle with [g]"
          (if (symbol-value golden-ratio-mode) "enabled" "disabled")))

(spacemacs|define-micro-state frame-manipulation
  :doc "[?] for help"
  :evil-leader "F."
  :use-minibuffer t
  :bindings
  ("?" nil                                   :doc (framer//manipulation-full-doc))
  ("0" select-window-0                       :doc (framer//manipulation-number-doc))
  ("1" select-window-1                       :doc (framer//manipulation-number-doc))
  ("2" select-window-2                       :doc (framer//manipulation-number-doc))
  ("3" select-window-3                       :doc (framer//manipulation-number-doc))
  ("4" select-window-4                       :doc (framer//manipulation-number-doc))
  ("5" select-window-5                       :doc (framer//manipulation-number-doc))
  ("6" select-window-6                       :doc (framer//manipulation-number-doc))
  ("7" select-window-7                       :doc (framer//manipulation-number-doc))
  ("8" select-window-8                       :doc (framer//manipulation-number-doc))
  ("9" select-window-9                       :doc (framer//manipulation-number-doc))
  ("-" split-window-below-and-focus          :doc (framer//manipulation-split-doc))
  ("/" split-window-right-and-focus          :doc (framer//manipulation-split-doc))
  ("[" shrink-frame-horizontally             :doc (framer//manipulation-resize-doc))
  ("]" enlarge-frame-horizontally            :doc (framer//manipulation-resize-doc))
  ("{" spacemacs/shrink-window               :doc (framer//manipulation-resize-doc))
  ("}" spacemacs/enlarge-window              :doc (framer//manipulation-resize-doc))
  ("c" delete-window                         :doc (framer//manipulation-layout-doc))
  ("C" delete-other-windows                  :doc (framer//manipulation-layout-doc))
  ("g" spacemacs/toggle-golden-ratio         :doc (framer//manipulation-gratio-doc))
  ("h" evil-window-left                      :doc (framer//manipulation-move-doc))
  ("<left>" evil-window-left                 :doc (framer//manipulation-move-doc))
  ("j" evil-window-down                      :doc (framer//manipulation-move-doc))
  ("<down>" evil-window-down                 :doc (framer//manipulation-move-doc))
  ("k" evil-window-up                        :doc (framer//manipulation-move-doc))
  ("<up>" evil-window-up                     :doc (framer//manipulation-move-doc))
  ("l" evil-window-right                     :doc (framer//manipulation-move-doc))
  ("<right>" evil-window-right               :doc (framer//manipulation-move-doc))
  ("H" evil-window-move-far-left             :doc (framer//manipulation-move-doc))
  ("<S-left>" evil-window-move-far-left      :doc (framer//manipulation-move-doc))
  ("J" evil-window-move-very-bottom          :doc (framer//manipulation-move-doc))
  ("<S-down>" evil-window-move-very-bottom   :doc (framer//manipulation-move-doc))
  ("K" evil-window-move-very-top             :doc (framer//manipulation-move-doc))
  ("<S-up>" evil-window-move-very-top        :doc (framer//manipulation-move-doc))
  ("L" evil-window-move-far-right            :doc (framer//manipulation-move-doc))
  ("<S-right>" evil-window-move-far-right    :doc (framer//manipulation-move-doc))
  ("o" other-frame                           :doc (framer//manipulation-move-doc))
  ("R" spacemacs/rotate-windows              :doc (framer//manipulation-move-doc))
  ("s" split-window-below                    :doc (framer//manipulation-split-doc))
  ("S" split-window-below-and-focus          :doc (framer//manipulation-split-doc))
  ("u" winner-undo                           :doc (framer//manipulation-layout-doc))
  ("U" winner-redo                           :doc (framer//manipulation-layout-doc))
  ("v" split-window-right                    :doc (framer//manipulation-split-doc))
  ("V" split-window-right-and-focus          :doc (framer//manipulation-split-doc))
  ("w" other-window                          :doc (framer//manipulation-move-doc)))

;; end of Frame Manipulation Micro State


  )



;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
