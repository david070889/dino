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

(defvar dino-score-y dino-height
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

(defvar dino-dino-options
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
(defconst dino-dino	1)
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

(defvar-keymap dino-null-map
  :doc "Keymap for finished Snake games."
  :name 'dino-null-map
  "n"       #'dino-start-game
  "q"       #'quit-window)

(defun dino-display-options ()
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
	    (cond ((= c dino-blank)
		   dino-blank-options)
                  ((= c dino-dino)
		   dino-dino-options)
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
(defun dino-init-buffer ()
  (gamegrid-init-buffer dino-buffer-width
			dino-buffer-height
			dino-space)
  (let ((buffer-read-only nil))
    (dotimes (y dino-height)
      (dotimes (x dino-width)
        (gamegrid-set-cell x y dino-border)))
    (cl-loop for y from 1 to (- dino-height 2) do
             (cl-loop for x from 1 to (- dino-width 2) do
                      (gamegrid-set-cell x y dino-blank)))))
(defun dino-reset-game ()
  (gamegrid-kill-timer)
  (dino-init-buffer)
  (setq dino-length		dino-initial-length
	dino-velocity-x	dino-initial-velocity-x
	dino-velocity-y	dino-initial-velocity-y
	dino-positions		nil
	dino-score		0
	dino-paused		nil
	dino-moved-p           nil
	dino-velocity-queue    nil)
  (let ((x dino-initial-x)
	(y dino-initial-y))
    (dotimes (_ dino-length)
      (gamegrid-set-cell x y dino-dino)
      (setq dino-positions (cons (vector x y) dino-positions))
      (cl-incf x dino-velocity-x)
      (cl-incf y dino-velocity-y)))
  (dino-update-score))

(defun dino-set-dot ()
  (let ((x (random dino-width))
	(y (random dino-height)))
    (while (not (= (gamegrid-get-cell x y) dino-blank))
      (setq x (random dino-width))
      (setq y (random dino-height)))
    (gamegrid-set-cell x y dino-dot)))

(defun dino-update-game (dino-buffer)
  "Called on each clock tick.
Advances the snake one square, testing for collision.
Argument SNAKE-BUFFER is the name of the buffer."
  (when (and (not dino-paused)
	     (eq (current-buffer) dino-buffer))
    (dino-update-velocity)
    (let* ((pos (car dino-positions))
	   (x (+ (aref pos 0) dino-velocity-x))
	   (y (+ (aref pos 1) dino-velocity-y))
	   (c (gamegrid-get-cell x y)))
      (if (or (= c dino-border)
	      (= c dino-dino))
	  (dino-end-game)
	(cond ((= c dino-dot)
	       (cl-incf dino-length)
	       (cl-incf dino-score)
	       (dino-update-score)
	       (dino-set-dot))
	      (t
	       (let* ((last-cons (nthcdr (- dino-length 2)
					 dino-positions))
		      (tail-pos (cadr last-cons))
		      (x0 (aref tail-pos 0))
		      (y0 (aref tail-pos 1)))
		 (gamegrid-set-cell x0 y0 dino-blank)
		 (setcdr last-cons nil))))
	(gamegrid-set-cell x y dino-dino)
	(setq dino-positions
	      (cons (vector x y) dino-positions))
	(setq dino-moved-p nil)))))

(defun dino-update-velocity ()
  (unless dino-moved-p
    (if dino-velocity-queue
	(let ((new-vel (car (last dino-velocity-queue))))
	  (setq dino-velocity-x (car new-vel)
		dino-velocity-y (cadr new-vel))
	  (setq dino-velocity-queue
		(nreverse (cdr (nreverse dino-velocity-queue))))))
    (setq dino-moved-p t)))

(defun dino-final-x-velocity ()
  (or (caar dino-velocity-queue)
      dino-velocity-x))

(defun dino-final-y-velocity ()
  (or (cadr (car dino-velocity-queue))
      dino-velocity-y))

(defun dino-move-left ()
  "Make the snake move left."
  (interactive nil dino-mode)
  (when (zerop (dino-final-x-velocity))
    (push '(-1 0) dino-velocity-queue)))

(defun dino-move-right ()
  "Make the snake move right."
  (interactive nil dino-mode)
  (when (zerop (dino-final-x-velocity))
    (push '(1 0) dino-velocity-queue)))

(defun dino-move-up ()
  "Make the snake move up."
  (interactive nil dino-mode)
  (when (zerop (dino-final-y-velocity))
    (push '(0 -1) dino-velocity-queue)))

(defun dino-move-down ()
  "Make the snake move down."
  (interactive nil dino-mode)
  (when (zerop (dino-final-y-velocity))
    (push '(0 1) dino-velocity-queue)))

(defun dino-end-game ()
  "Terminate the current game."
  (interactive nil dino-mode)
  (gamegrid-kill-timer)
  (use-local-map dino-null-map)
  (gamegrid-add-score dino-score-file dino-score))

(defun dino-start-game ()
  "Start a new game of Snake."
  (interactive nil dino-mode)
  (dino-reset-game)
  (dino-set-dot)
  (use-local-map dino-mode-map)
  (gamegrid-start-timer dino-tick-period 'dino-update-game))

(defun dino-pause-game ()
  "Pause (or resume) the current game."
  (interactive nil dino-mode)
  (setq dino-paused (not dino-paused))
  (message (and dino-paused "Game paused (press p to resume)")))

(defun dino-active-p ()
  (eq (current-local-map) dino-mode-map))

(put 'dino-mode 'mode-class 'special)

(define-derived-mode dino-mode special-mode "Dino"
  "A mode for playing Snake."
  :interactive nil

  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)

  (use-local-map dino-null-map)

  (setq gamegrid-use-glyphs dino-use-glyphs-flag)
  (setq gamegrid-use-color dino-use-color-flag)

  (gamegrid-init (dino-display-options)))

;;;###autoload
(defun dino ()
  "Play the Snake game.
Move the snake around without colliding with its tail or with the border.

Eating dots causes the snake to get longer.

Snake mode keybindings:
   \\<snake-mode-map>
\\[snake-start-game]	Starts a new game of Snake
\\[snake-end-game]	Terminates the current game
\\[snake-pause-game]	Pauses (or resumes) the current game
\\[snake-move-left]	Makes the snake move left
\\[snake-move-right]	Makes the snake move right
\\[snake-move-up]	Makes the snake move up
\\[snake-move-down]	Makes the snake move down"
  (interactive)

  (switch-to-buffer dino-buffer-name)
  (gamegrid-kill-timer)
  (dino-mode)
  (dino-start-game))

(provide 'dino)

;;; snake.el ends here


