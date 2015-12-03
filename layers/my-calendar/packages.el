;;; packages.el --- my-calendar Layer packages File for Spacemacs
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
(setq my-calendar-packages
    '(
      ;; package names go here
      cal-china-x
      ))

;; List of packages to exclude.
(setq my-calendar-excluded-packages '())

;; For each package, define a function my-calendar/init-<package-name>
;;
(defun my-calendar/init-cal-china-x ()
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
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
