(eval-when-compile (require 'cl-lib))

(require 'gamegrid)

(defvar dino-use-glyphs-flag t
  "Non-nil means use glyphs when available.")


(defvar dino-use-color-flag t
  "Non-nil means use color when available.")

(defvar dino-buffer-name "*Dino*"
  "Name used for Snake buffer.")


(defvar dino-buffer-width 100
  "Width of used portion of buffer.")

(defvar dino-buffer-height 33
  "Height of used portion of buffer.")

(defvar dino-width 100
  "Width of playing area.")

(defvar dino-height 33
  "Height of playing area.")

(defvar dino-initial-length 5
  "Initial length of snake.")

(defvar dino-initial-x 10
  "Initial X position of snake.")

(defvar dino-initial-y 10
  "Initial Y position of snake.")

(defvar dino-initial-velocity-x 1
  "Initial X velocity of snake.")

(defvar dino-initial-velocity-y 0
  "Initial Y velocity of snake.")

(defvar dino-tick-period 0.2
  "The default time taken for the snake to advance one square.")

(defvar dino-mode-hook nil
  "Hook run upon starting Snake.")

(defvar dino-score-x 0
  "X position of score.")

(defvar dino-score-y snake-height
  "Y position of score.")

(defvar dino-score-file "dino-scores"
  "File for holding high scores.")

(defvar dino-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defvar dino-snake-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    (((glyph color-x) [1 1 0])
     (color-tty "yellow"))))

(defvar dino-dot-options
  '(((glyph colorize)
     (t ?\*))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [1 0 0])
     (color-tty "red"))))

(defvar dino-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "white"))))

(defvar dino-space-options
  '(((t ?\040))
    nil
    nil))

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst dino-blank	0)
(defconst dino-snake	1)
(defconst dino-dot	2)
(defconst dino-border	3)
(defconst dino-space	4)

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local dino-length 0)
(defvar-local dino-velocity-x 1)
(defvar-local dino-velocity-y 0)
(defvar-local dino-positions nil)
(defvar-local dino-score 0)
(defvar-local dino-paused nil)
(defvar-local dino-moved-p nil)
(defvar-local dino-velocity-queue nil)

(defvar-keymap dino-mode-map
  :doc "Keymap for Dino games."
  :name 'dino-mode-map
  "n"       #'dino-start-game
  "q"       #'dino-end-game
  "p"       #'dino-pause-game

  "<left>"  #'dino-move-left
  "<right>" #'dino-move-right
  "<up>"    #'dino-move-up
  "<down>"  #'dino-move-down

  "C-b"     #'dino-move-left
  "C-f"     #'dino-move-right
  "C-p"     #'dino-move-up
  "C-n"     #'dino-move-down)

(defvar-keymap snake-null-map
  :doc "Keymap for finished Snake games."
  :name 'snake-null-map
  "n"       #'dino-start-game
  "q"       #'quit-window)

(defun dino-display-options ()
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
	    (cond ((= c dino-blank)
		   dino-blank-options)
                  ((= c dino-snake)
		   dino-snake-options)
                  ((= c dino-dot)
		   dino-dot-options)
                  ((= c dino-border)
		   dino-border-options)
                  ((= c dino-space)
		   dino-space-options)
                  (t
		   '(nil nil nil)))))
    options))

(defun dino-update-score ()
  (let* ((string (format "Score:  %05d" dino-score))
	 (len (length string)))
    (dotimes (x len)
      (gamegrid-set-cell (+ dino-score-x x)
			 dino-score-y
			 (aref string x)))))


