(defvar bespoke-scala//fix-brace-last-key nil)
(defun bespoke-scala/fix-brace ()
  ;; (message "this key: %s" (this-command-keys-vector))
  (when (and (not (eql bespoke-scala//fix-brace-last-key ?\{))
         (= 1 (length (this-command-keys-vector)))
             (eql (elt (this-command-keys-vector) 0)
                  ?\C-m)
             (save-excursion
               (beginning-of-line)
               (looking-at " *\\} *$")))
    (save-excursion
      (insert-char ?\n)
      (funcall indent-line-function)))
  (setq bespoke-scala//fix-brace-last-key (elt (this-command-keys-vector) 0)))

(defun bespoke-scala/init ()
  (add-hook 'post-self-insert-hook #'bespoke-scala/fix-brace nil 'local)
  (electric-indent-local-mode 0))

(defmacro bespoke-scala//on-last-nonempty-line (&rest thunk)
  `(save-excursion
     (forward-line -1)
     (while (and (not (bobp))
                 (looking-at "^ *$"))
       (forward-line -1))
     (progn ,@thunk)))

(defun bespoke-scala/compute-ws-indent ()
  (bespoke-scala//on-last-nonempty-line
    (back-to-indentation)
    (let ((ci (current-column)))
      (end-of-line)
      (skip-chars-backward "\s")
      (let ((adjustment
             (if (looking-back (rx (or "[" "{" "("
                                       "=" "=>" "=>>" "<-"
                                       ":"
                                       )))
                 +1
               (if (and (looking-back (rx word-boundary
                                          (or
                                           "if" "while" "for" "match" "try"
                                           "then" "else" "do" "finally" "yield" "case"
                                          )))
                        (progn (goto-char (match-beginning 0))
                               (skip-chars-backward "\s")
                               (not (looking-back "end"))))
                   +1
                 0))))
        (+ ci (* adjustment scala-indent:step)))
      )
    ))

(defvar bespoke-scala//ws-indent-last-line nil)
(defvar bespoke-scala//ws-indent-last-command nil)
(defun bespoke-scala/ws-indent (&optional direction)
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (bespoke-scala/compute-ws-indent))
        (d (if (< 0 (or direction 1)) 1 -1)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      ;; TODO match indent
      (when (or (looking-at (rx (or "]" "}" ")" "end")))
                (and (looking-at "case")
                     (bespoke-scala//on-last-nonempty-line
                       (back-to-indentation)
                       (and (not (looking-at "case"))
                            (progn
                              (end-of-line)
                              (skip-chars-backward "\s")
                              (not (looking-back (rx (or "{" "match")))))))))
        (setq need (- need scala-indent:step)))
      (if (and (eql (line-number-at-pos) bespoke-scala//ws-indent-last-line)
               (eq last-command bespoke-scala//ws-indent-last-command))
          (indent-to (+ ci (* d scala-indent:step)))
        (indent-to need)))
    (if (< (current-column) (current-indentation))
        (forward-to-indentation 0))
    (setq bespoke-scala//ws-indent-last-command this-command
          bespoke-scala//ws-indent-last-line (line-number-at-pos)
    )
    ))

(defun bespoke-scala/ws-indent-backwards ()
  (interactive "*")
  (bespoke-scala/ws-indent -1))

(defun bespoke-scala/toggle-indent ()
  (interactive)
  (when (eq major-mode 'scala-mode)
    (if (eq indent-line-function #'scala-indent:indent-line)
        (progn
          (message "Toggle indent: relative to previous line")
          (setq indent-line-function #'bespoke-scala/ws-indent)
          (remove-hook 'post-self-insert-hook #'scala-indent:indent-on-special-words 'local))

      (progn
        (message "Toggle indent: Scala indent")
        (setq indent-line-function #'scala-indent:indent-line)
        (add-hook 'post-self-insert-hook #'scala-indent:indent-on-special-words 'local)))))

(provide 'bespoke-scala-mode)
;;; bespoke-scala-mode.el ends here
