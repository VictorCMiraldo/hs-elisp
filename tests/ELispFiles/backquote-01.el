(defun magit-discard-files (sections)
  (let ((auto-revert-verbose nil)
        (inhibit-magit-refresh t)
        (status (magit-file-status))
        delete resurrect rename discard resolve)
    (dolist (section sections)
      (let ((file (magit-section-value section)))
        (pcase (cons (pcase (magit-diff-type section)
                       (`staged ?X)
                       (`unstaged ?Y)
                       (`untracked ?Z))
                     (magit-file-status file status))
          ((or `(?Z ?? ??) `(?Z ?! ?!)) (push file delete))
          ((or `(?Z ?D ? ) `(,_ ?D ?D)) (push file delete))
          ((or `(,_ ?U ,_) `(,_ ,_ ?U)) (push file resolve))
          (`(,_ ?A ?A)                  (push file resolve))
          (`(?X ?M ,(or ?  ?M ?D)) (push section discard))
          (`(?Y ,_         ?M    ) (push section discard))
          (`(?X ?A ,(or ?  ?M ?D)) (push file delete))
          (`(?X ?C ,(or ?  ?M ?D)) (push file delete))
          (`(?X ?D ,(or ?  ?M   )) (push file resurrect))
          (`(?Y ,_            ?D ) (push file resurrect))
          (`(?X ?R ,(or ?  ?M ?D)) (push section rename)))))
    (magit-discard-files--resurrect (nreverse resurrect))
    (magit-discard-files--delete    (nreverse delete))
    (magit-discard-files--rename    (nreverse rename))
    (magit-discard-files--discard   (nreverse discard))
    (when resolve
      (let ((inhibit-magit-refresh t))
        (dolist (file (nreverse resolve))
          (magit-checkout-stage file (magit-checkout-read-stage file))))))
  (magit-refresh))
