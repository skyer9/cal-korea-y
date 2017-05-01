;;; cal-korea-y.el --- Utilities for Korean lunar calendar -*- coding: utf-8 -*-

;; Copyright (C) 2017  Lee San

;; Author: Lee San <skyer9@gmail.com>
;; Keywords: calendar, lisp, local, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

;; in ~/diary file

;; %%(diary-solar-term "소한") 소한(小寒)
;; %%(diary-solar-term "대한") 대한(大寒)
;; %%(diary-solar-term "입춘") 입춘(立春)
;; %%(diary-solar-term "우수") 우수(雨水)
;; %%(diary-solar-term "경칩") 경칩(驚蟄)
;; %%(diary-solar-term "춘분") 춘분(春分)
;; %%(diary-solar-term "청명") 청명(淸明)
;; %%(diary-solar-term "곡우") 곡우(谷雨)
;; %%(diary-solar-term "입하") 입하(立夏)
;; %%(diary-solar-term "소만") 소만(小滿)
;; %%(diary-solar-term "망종") 망종(芒種)
;; %%(diary-solar-term "하지") 하지(夏至)
;; %%(diary-solar-term "소서") 소서(小暑)
;; %%(diary-solar-term "대서") 대서(大暑)
;; %%(diary-solar-term "입추") 입추(立秋)
;; %%(diary-solar-term "처서") 처서(處暑)
;; %%(diary-solar-term "백로") 백로(白露)
;; %%(diary-solar-term "추분") 추분(秋分)
;; %%(diary-solar-term "한로") 한로(寒露)
;; %%(diary-solar-term "상강") 상강(霜降)
;; %%(diary-solar-term "입동") 입동(立冬)
;; %%(diary-solar-term "소설") 소설(小雪)
;; %%(diary-solar-term "대설") 대설(大雪)
;; %%(diary-solar-term "동지") 동지(冬至)
;; %%(diary-korean-lunar-anniversary 5 5) 생일

(require 'cal-china)
(require 'cal-china-x)

(defconst cal-korea-y-dir (if load-file-name
                              (file-name-directory load-file-name)
                            ""))

;; 한국 타임존 설정(KST = UTC+09:00)
(setq calendar-chinese-time-zone (* 9 60))

(setq calendar-chinese-standard-time-zone-name "KST")
(setq calendar-chinese-daylight-time-zone-name "KDT")

;; 양력 휴일
(defconst holiday-korean-holidays
  '((holiday-fixed 1 1          "신정")
    (holiday-fixed 3 1          "3.1절")
    (holiday-fixed 5 1          "근로자의날")
    (holiday-fixed 5 5          "어린이날")
    (holiday-fixed 6 6          "현충일")
    (holiday-fixed 8 15         "광복절")
    (holiday-fixed 10 3         "개천절")
    (holiday-fixed 10 9         "한글날")
    (holiday-fixed 12 25        "성탄절"))
  "Pre-define Korean public holidays.")

;; 한국 음력 휴일
(defconst holiday-korean-lunar-holidays
  '(
    (holiday-korean-lunar  1  0  "설날")
    (holiday-korean-lunar  1  1  "설날")
    (holiday-korean-lunar  1  2  "설날")
    (holiday-korean-lunar  4  8  "석가탄신일")
    (holiday-korean-lunar  8 14  "추석")
    (holiday-korean-lunar  8 15  "추석")
    (holiday-korean-lunar  8 16  "추석"))
  "Pre-define Korean public lunar holidays.")

;; 60갑자
(setq calendar-chinese-celestial-stem
      ["갑" "을" "병" "정" "무"
       "기" "경" "신" "임" "계"]
      calendar-chinese-terrestrial-branch
      ["자" "축" "인" "묘"
       "진" "사" "오" "미"
       "신" "유" "술" "해"])

;; 월
(setq calendar-month-name-array
      ["01월" "02월" "03월" "04월" "05월" "06월"
       "07월" "08월" "09월" "10월" "11월" "12월"]
   calendar-month-header '(format "%d년 %.2d월" year month))

;; 요일
(setq calendar-day-name-array ["일요일" "월요일" "화요일" "수요일"
			       "목요일" "금요일" "토요일"]
      calendar-day-header-array calendar-day-name-array
      calendar-day-abbrev-array [ "일" "월" "화" "수"
				  "목" "금" "토" ])

;; 날짜 표시형식
(setq calendar-date-display-form
      '((format "%4s-%.2d-%.2d %s"
		year
		(string-to-number month)
		(string-to-number day)
		dayname)))

(setq calendar-mode-line-format
      (list
       (calendar-mode-line-entry 'calendar-scroll-right "previous month" "<<")
       '(calendar-date-string date nil)
       '(cal-korea-y-date-string date)
       (calendar-mode-line-entry 'calendar-scroll-left "next month" ">>")))

;; 한국 휴일을 표시한다.
(setq calendar-holidays holiday-korean-holidays)

;; 한국 음력 휴일을 표시한다.
(setq calendar-holidays
      (append calendar-holidays holiday-korean-lunar-holidays))

(defun holiday-korean-lunar (month day string)
  "Wrapper function for `holiday-chinese'."
  (holiday-chinese month day string))

(defun diary-korean-lunar-anniversary (month day &optional year mark)
  "Wrapper function for `diary-chinese-anniversary'."
  (diary-chinese-anniversary month day year mark))

(defun cal-korea-y-date-string (date)
  (let* ((cn-date (calendar-chinese-from-absolute
                   (calendar-absolute-from-gregorian date)))
         (cn-year  (cadr   cn-date))
         (cn-month (cl-caddr  cn-date))
         (cn-day   (cl-cadddr cn-date)))
    (format "음력 : %s년 %.2d월%s %.2d일"
            (calendar-chinese-sexagesimal-name cn-year)
            (floor cn-month)
            (if (integerp cn-month) "" "(윤달)")
            cn-day)))

(defconst cal-korea-y-solar-term-name
  ["소한" "대한" "입춘" "우수" "경칩" "춘분"
   "청명" "곡우" "입하" "소만" "망종" "하지"
   "소서" "대서" "입추" "처서" "백로" "추분"
   "한로" "상강" "입동" "소설" "대설" "동지"]
  "24 solar terms(in korean).")

(defvar cal-korea-y-solar-term-alist nil) ; e.g., '(((9 23 2013) "추분") ...)
(defvar cal-korea-y-solar-term-year nil)

(defun cal-korea-y-solar-term-alist-new (year)
  "Return a solar-term alist for YEAR."
  ;; use cached values (china time zone +0800)
  (let ((cached-jieqi-file (expand-file-name (concat cal-korea-y-dir "/solar.txt"))))
    (if (and (> year 1900)
             (< year 2101)
             (file-exists-p cached-jieqi-file))
        (let ((solar-term-alist '())
              (year (number-to-string year)))
          (with-temp-buffer
            (insert-file-contents cached-jieqi-file)
            (goto-char (point-min))
            (while (search-forward year nil t 1)
              (let* ((str (buffer-substring (line-beginning-position) (line-end-position)))
                     (lst (split-string str))
                     (jieqi (nth 0 lst))
                     (y (string-to-number (nth 1 lst)))
                     (m (string-to-number (nth 2 lst)))
                     (d (string-to-number (nth 3 lst))))
                (setq solar-term-alist (cons (cons (list m d y) jieqi)
                                             solar-term-alist)))))
          solar-term-alist)
      ;; calculation may have one day difference.
      (cl-loop for i from 0 upto 23

             for date = (cal-china-x-next-solar-term `(1 1 ,year))
             then (setq date (cal-china-x-next-solar-term date))

             with solar-term-alist = '()

             collect (cons date (aref cal-korea-y-solar-term-name i))
             into solar-term-alist

             finally return solar-term-alist))))

(defun cal-korea-y-sync-solar-term (year)
  "Sync `cal-korea-y-solar-term-alist' and `cal-korea-y-solar-term-year' to YEAR."
  (unless (and cal-korea-y-solar-term-year
               (= cal-korea-y-solar-term-year year))
      (setq cal-korea-y-solar-term-alist
            (cal-korea-y-solar-term-alist-new year))
      (setq cal-korea-y-solar-term-year
            (calendar-extract-year
             (caar cal-korea-y-solar-term-alist)))))

(defun diary-solar-term (solar-term &optional mark)
  "A holiday(STR) on SOLAR-TERM day."
  (let* (
         (c-date (or date (calendar-current-date)))
         (y (calendar-extract-year c-date))
         (m (calendar-extract-month c-date))
         (d (calendar-extract-day c-date)))
    (cal-korea-y-sync-solar-term y)
    (let ((l cal-korea-y-solar-term-alist)
          date)
      (dolist (i l)
        (when (string= (cdr i) solar-term)
          (setq l '()
                date (car i))))
      (let (
            (s-m (car date))
            (s-d (cadr date)))
        (if (and
             (equal s-m m)
             (equal s-d d))
            (cons mark (replace-regexp-in-string "[\t ]+" "" entry)))))))

(provide 'cal-korea-y)
;;; cal-korea-y.el ends here
