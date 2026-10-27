;;; ================================================================
;;; DLX.lsp
;;; Native DLI -> optional continuous DCO workflow
;;;
;;; Command: DLX
;;;
;;; Workflow:
;;;   1. Type DLX.
;;;   2. AutoCAD immediately runs native DIMLINEAR.
;;;   3. After the linear dimension is complete:
;;;        C     = switch to native DIMCONTINUE
;;;        Enter = finish DLX without continuing
;;;        Esc   = finish DLX
;;;   4. DIMCONTINUE remains active and can create any number of
;;;      continued dimensions.
;;;   5. Press Enter once to finish DIMCONTINUE and end DLX.
;;;      DLX automatically supplies AutoCAD's normally required
;;;      second Enter.
;;;   6. Type DLX again to start another linear dimension.
;;;
;;; This file intentionally uses native AutoCAD commands so OSNAP,
;;; dynamic input, polar tracking, and dimension previews are retained.
;;; It does not use FBOUNDP or Express Tools.
;;; ================================================================

(vl-load-com)

(setq *DLX:Version* "2.1.0")


;;; Return T only when ENAME is a live dimension entity.
(defun DLX:Dimension-p (ename / data)
  (and
    ename
    (setq data (entget ename))
    (= "DIMENSION" (cdr (assoc 0 data)))
  )
)


;;; Find the newest dimension created after MARK.
(defun DLX:NewestDimensionAfter (mark / item newest)
  (setq item
    (if mark
      (entnext mark)
      (entnext)
    )
  )

  (while item
    (if (DLX:Dimension-p item)
      (setq newest item)
    )
    (setq item (entnext item))
  )

  newest
)


;;; Keep passing native user input to the active AutoCAD command.
(defun DLX:RunActiveCommand ()
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )
)


;;; Run one native DIMLINEAR and return its new dimension, if any.
(defun DLX:RunDLI (/ mark)
  (setq mark (entlast))

  (command "_.DIMLINEAR")
  (DLX:RunActiveCommand)

  (DLX:NewestDimensionAfter mark)
)


;;; Wait after DLI.
;;; Returns CONTINUE or END.
(defun DLX:WaitAfterDLI (/ event kind value result)
  (prompt
    "\nDLX: DLI complete | C=Continue (DCO)  Enter=End  Esc=End"
  )

  (setq result nil)

  (while (null result)
    ;; Bit 2 returns all keys. Bit 8 returns Esc instead of raising
    ;; a console-break error at this controller prompt.
    (setq event (grread T 10 0))
    (setq kind  (car event))
    (setq value (cadr event))

    (if (= kind 2)
      (cond
        ((or (= value 67) (= value 99)) ; C or c
         (setq result "CONTINUE")
        )
        ((or (= value 13) (= value 27)) ; Enter or Esc
         (setq result "END")
        )
      )
    )
  )

  result
)


;;; Run native DIMCONTINUE continuously.
;;;
;;; No dimension entity is explicitly selected here. AutoCAD therefore
;;; uses the last-created DLI and continues from its second extension
;;; line (the DLI endpoint), which is the required direction.
;;;
;;; Native DIMCONTINUE normally needs two Enter inputs to finish:
;;; the first opens its Select Base Dimension prompt and the second
;;; exits. This loop detects an input that created no new entity and
;;; automatically supplies the second Enter.
(defun DLX:RunDCO (/ beforeInput)
  (prompt
    "\nDLX: DCO active | Pick continuation points; press Enter once to finish."
  )

  (command "_.DIMCONTINUE")

  (while (> (getvar "CMDACTIVE") 0)
    (setq beforeInput (entlast))
    (command pause)

    ;; A point that creates a continued dimension changes ENTLAST.
    ;; If the command remains active without creating an entity, the
    ;; user pressed Enter at the point prompt. AutoCAD is now at its
    ;; Select Base Dimension prompt, so one automatic Enter exits it.
    (if
      (and
        (> (getvar "CMDACTIVE") 0)
        (equal beforeInput (entlast))
      )
      (command "")
    )
  )
)


;;; Main command.
(defun c:DLX
  (
    /
    *error*
    oldCmdecho
    oldModeMacro
    baseDim
    action
  )

  (setq oldCmdecho   (getvar "CMDECHO"))
  (setq oldModeMacro (getvar "MODEMACRO"))

  (defun *error* (message)
    (redraw)
    (setvar "CMDECHO" oldCmdecho)
    (setvar "MODEMACRO" oldModeMacro)

    (if
      (and
        message
        (not
          (member
            message
            '(
              "Function cancelled"
              "quit / exit abort"
              "console break"
            )
          )
        )
      )
      (prompt (strcat "\nDLX error: " message))
    )

    (prompt "\nDLX ended.")
    (princ)
  )

  (setvar "CMDECHO" 0)
  (setvar
    "MODEMACRO"
    "DLX [DLI] | Complete the linear dimension"
  )

  (prompt
    (strcat
      "\nDLX " *DLX:Version*
      " | DLI first; press C afterward for continuous DCO."
    )
  )

  ;; Every invocation of DLX starts with one native linear dimension.
  (setq baseDim (DLX:RunDLI))

  (if (DLX:Dimension-p baseDim)
    (progn
      (setvar
        "MODEMACRO"
        "DLX | C=Continue (DCO) Enter=End Esc=End"
      )

      (setq action (DLX:WaitAfterDLI))

      (if (= action "CONTINUE")
        (progn
          (setvar
            "MODEMACRO"
            "DLX [DCO] | Enter once to finish"
          )
          (DLX:RunDCO)
        )
      )
    )
    (prompt "\nDLX: No linear dimension was created.")
  )

  (redraw)
  (setvar "CMDECHO" oldCmdecho)
  (setvar "MODEMACRO" oldModeMacro)

  (prompt "\nDLX ended. Type DLX again for a new linear dimension.")
  (princ)
)


(prompt "\nDLX 2.1 loaded. Type DLX to start.")
(princ)
