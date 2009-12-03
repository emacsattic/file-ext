;;; grep-ext.el --- tools for seeking text in files and edit this files

;; Author: Mirko Link <MirkoLink@t-online.de or LINK@kba-planeta.de>
;; Maintainer: KBA
;; $Revision: 1.3 $
;; Keywords: grep, tools

;; Copyright (C) 2000 KBA

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; folgende zusätzliche Funktionen sind zur Zeit unterstützt:
;;  - Sicherungsdateien in einem festen Verszeichnis
;;  - Suchen über mehrere Dateien: `sr' oder `search-about-files'
;;  - eine Zeile als Bookmark setzen `set-bookmark-line' (Tastenkombination: C-M-j)

;; Revision 1.4:
;; - Ersetzen über mehrere Dateien erstellt
;; - bereits geöffnete Buffer bleiben offen (auch bei der Suche über mehrere Dateien

(defgroup file-extend nil
  "Erweiterungen des emacs-Befehlssatzes zur Arbeit mit Dateien."
  :prefix "file-ext-"
  :group 'files)

(defcustom file-ext-find-location "c:/emacs-20.7/kba-ext/bin/find.exe -name "
  "Dateiname, -pfad und Parameter für das Linuxprogramm `find'."
  :type 'string
  :group 'file-extend)

(defcustom file-ext-backup-directory "c:/emacs-20.7/backup/"
  "Verzeichnis für Sicherungsdateien."
  :type 'string
  :group 'file-extend)

(defcustom file-ext-file-history (list
				  "k:/software/sps/"
				  "d:/software/sps/"
				  "d:/work/"
				  "c:/windows/desktop/aktenkoffer/"
				  "d:/emacs.src/emacs-ext/"
				  "d:/emacs.src/info/"
				  "d:/emacs.src/lisp/"
				  "c:/emacs-20.7/lisp/"
				  "c:/emacs-20.7/lisp/default.el")
  "Pfad- und Dateiangaben für die Historyliste des find-file-Befehls."
  :type 'alist
  :group 'file-extend)

(setq file-name-history file-ext-file-history)

;Tastenbelegungen
(global-set-key "\M-\C-j" 'set-bookmark-line)

(defvar overlay-orginal nil "Markiert den gefundenen Eintrag.")
(defvar search-directory () "Verzeichnis, in dem nach Dateien gesucht wird.")
(defvar search-files () "Dateien, nach denen gesucht wurde.")
(defvar search-expression () "regulaerer Ausdruck, nach dem gesucht wurde.")
(defvar founded-files-alist () "Liste von Dateien, die dann durchsucht werden sollen.")
(defvar founded-positions () "Liste von gefundenen Positionen vom Typ: (Datei (Positionen))")

; Überschreiben der Funktion make-backup-file-name, um die Autosave-Dateien in einem festen Verzeichnis abzulegen
(defun make-backup-file-name (file)
  "Create the non-numeric backup file name for FILE..."
  (interactive "FDatei: ")
  (let (lastpos)
    (while (string-match "/" file)
      (setq lastpos (match-end 0))
      (setq file (substring file (match-end 0))))
    (concat file-ext-backup-directory file)))

(defalias 'sr 'search-about-files)

(defun replace-about-files ()
  "Interactives Programm zum Ersetzen von Strings in mehreren Dateien."
  (interactive)
  (create-file-list
   (read-from-minibuffer "Dateien: ")
   (read-from-minibuffer "Startverzeichnis: ")
   (y-or-n-p "Unterverzeichnisse durchsuchen ?"))
  (replace-at-founded-files
   (read-from-minibuffer "Suchausdruck: ")
   (read-from-minibuffer "Ersatz: ")))

(defun replace-at-founded-files (regexpr ersatz)
  "Sucht in `founded-files-alist' nach dem regulaeren Ausdruck regexpr und ersetzt ihn durch den ersatz."
  (save-window-excursion
    (let ((index 0)
	  outbuf
	  file
	  exists
	  (zaehler 0)
	  pos)
      (if (< 0 (length founded-files-alist))
 	  ()
 	(create-file-list))
      (cd search-directory)
      (while (< index (length founded-files-alist))
 	(setq file (nth index founded-files-alist))
 	(setq exists (find-buffer-visiting file))
 	(setq outbuf (find-file file))
 	(set-buffer outbuf)
 	(message "Die Datei %s wird nach Ausdruck %s durchsucht." file regexpr)
 	(save-excursion
          (goto-char 0)
 	  (while (re-search-forward regexpr () t)
	    (set-mark (match-beginning 0))
	    (goto-char (match-end 0))
	    (if (not (y-or-n-p "Replace ?"))
 		()
 	      (replace-match ersatz t t)
 	      (setq zaehler (1+ zaehler))))
 	(setq index (1+ index))
 	(save-buffer)
 	(if exists
 	    ()
 	  (kill-buffer outbuf))))
      (message "Es wurden %d Ausdrücke ersetzt!" zaehler))))

(defun search-about-files ()
  "Interactives Programm zum Suchen nach einem regulaeren Ausdruck in mehreren Dateien."
  (interactive)
  (create-file-list
   (read-from-minibuffer "Dateien: ")
   (read-from-minibuffer "Startverzeichnis: ")
   (y-or-n-p "Unterverzeichnisse durchsuchen ?"))
  (search-at-founded-files
   (read-from-minibuffer "Ausdruck: "))
  (show-founded-positions))

;; erstens:  alle Dateien in einer Liste schreiben
(defun create-file-list (&optional file dir no-subdir)
  "Erstellt eine Liste `founded-files-alist' mit gefundenen Dateien."
  (save-window-excursion
    (message "Die Dateien werden zusammengestellt.")
    (let (files
	  command
	  outbuf)
      (if file
	  (setq files file)
	(setq files (read-from-minibuffer "Zu suchende Dateien: ")))
      (setq search-files files)
      (if dir
	  (progn
	    (setq search-directory dir)
	    (cd-absolute search-directory))
	(setq search-directory default-directory))
      (setq outbuf (get-buffer-create "*find-results*"))
      (set-buffer outbuf)
      (setq buffer-read-only nil)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (buffer-enable-undo (current-buffer))
      (setq command (concat file-ext-find-location files))
      (shell-command command outbuf)
      (set-buffer outbuf)
      (goto-char (point-min))
      (let (file-name
	    last-file-name)
	(setq file-name (substring (thing-at-point 'line) 0 (- (length (thing-at-point 'line)) 1)))
	(setq founded-files-alist (list file-name))
	(while (not (string= file-name last-file-name))
	  (forward-line 1)
	  (setq last-file-name file-name)
	  (setq file-name (substring (thing-at-point 'line) 0 (- (length (thing-at-point 'line)) 1)))
	  (if (string= file-name last-file-name)
	      ()
	    (setq founded-files-alist (cons file-name
					    founded-files-alist)))))
      (kill-buffer outbuf))))

;; zweitens: Liste durchgehen (zum Beispiel wie bei TAGS !?)

(defun search-at-founded-files (regexpr)
  "Sucht in `founded-files-alist' nach dem regulaeren Ausdruck regexpr. Das Ergebnis wird in die Variable `founded-positions' geschrieben."
  (setq search-expression regexpr)
  (save-window-excursion
    (let ((index 0)
	  outbuf
	  file
	  exists
	  pos)
      (if (< 0 (length founded-files-alist))
	  ()
	(create-file-list))
      (global-font-lock-mode -1)
      (cd search-directory)
      (setq founded-positions nil)
      (while (< index (length founded-files-alist))
	(setq pos nil)
	(setq file (nth index founded-files-alist))
	(setq exists (find-buffer-visiting file))
	(setq outbuf (find-file file))
	(message "Die Datei %s wird nach Ausdruck %s durchsucht." file regexpr)
	(set-buffer outbuf)
	(save-excursion
	  (goto-char 0)
	  (while (re-search-forward regexpr () t)
	    (setq pos (cons (point)
			    pos))))
	(if (> (length pos) 0)
	    (setq founded-positions (cons (list file (reverse pos))
					  founded-positions))
	  ())
	(setq index (+ 1 index))
	(if exists
	    ()
	  (kill-buffer outbuf)))
      (global-font-lock-mode 1))))

;; drittens: Ergebnis anzeigen und mit Tastenkürzeln bestücken
(defun show-founded-positions ()
  "Zeigt die Positionen der gefundenen Einträge."
  (let (outbuf)
    (setq outbuf (get-buffer-create "*expressions*"))
    (switch-to-buffer-other-window outbuf)
    (setq buffer-read-only nil)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (buffer-enable-undo (current-buffer))
    (let ((index1 0)
	  (index2 0)
	  (alle 0)
	  file)
      (while (< index1 (length founded-positions))
	(setq file (nth 0 (nth index1 founded-positions)))
	(setq index2 0)
	(while (< index2 (length (nth 1 (nth index1 founded-positions))))
	  (insert (format "%d" (nth index2 (nth 1 (nth index1 founded-positions)))) ":" file "\n")
	  (setq alle (+ 1 alle))
	  (setq index2 (+ 1 index2)))
	(setq index1 (+ 1 index1)))
      (goto-char (point-min))
      (insert "Directory:  " search-directory "\n")
      (insert "Files:      " search-files "\n")
      (insert "Expression: " search-expression "\n")
      (insert "gefunden:   " (format "%d" alle) " Eintraege\n\n")
      (set-buffer-modified-p nil)))
  (use-local-map file-ext-map))

(defun show-detail-other-window ()
  "Zeigt die gefundene Position in einem anderen Fenster an."
  (interactive)
  (let ((buf (buffer-name)))
    (save-excursion
      (let ((number (progn (beginning-of-line)
			   (string-to-number (thing-at-point 'word))))
	    (file (progn (search-forward ":")
			 (buffer-substring-no-properties (point) (progn (end-of-line)
									(point))))))
	(cd search-directory)
	(find-file-other-window file)
	(if (overlayp overlay-orginal)
	    (delete-overlay overlay-orginal)
	  ())
	(setq overlay-orginal (make-overlay (- number (length search-expression)) number))
	(overlay-put overlay-orginal 'face 'highlight)
	(goto-char number)))
    (switch-to-buffer-other-window buf)))

(defvar file-ext-map (make-sparse-keymap) "erweiterte Tasten")
(define-key file-ext-map [(control o)] 'show-detail-other-window)

(defun set-bookmark-line ()
  "Setzt einen Bookmark und gibt als Default-Namen den Zeileninhalt vor."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[^ \t].*$")
    (bookmark-set (read-from-minibuffer "Set bookmark: " (match-string 0)))))

(provide 'file-ext)