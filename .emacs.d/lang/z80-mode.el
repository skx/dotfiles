(defvar z80-mode-hook nil)
(defvar z80-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for z80 major mode")
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.z80\\'" . z80-mode))
(defconst z80-font-lock-keywords-1
  (list
   '("\\<\\(ADC\\|ADD\\|AND\\|BIT\\|CALL\\|CCF\\|CP\\|CPD\\|CPDR\\|CPIR\\|CPI\\|CPL\\|DAA\\|DEC\\|DI\\|DJNZ\\|EI\\|EX\\|EXX\\|HALT\\|IM\\|IN\\|INC\\|IND\\|INDR\\|INI\\|INIR\\|JP\\|JR\\|LD\\|LDD\\|LDDR\\|LDI\\|LDIR\\|NEG\\|NOP\\|MLT\\|OR\\|OTDM\\|OTDMR\\|OTDR\\|OTIM\\|OTIMR\\|OTIR\\|OUT\\|OUTD\\|OUTI\\|POP\\|PUSH\\|RES\\|RET\\|RETI\\|RETN\\|RL\\|RLA\\|RLC\\|RLCA\\|RLD\\|RR\\|RRA\\|RRC\\|RRCA\\|RRD\\|RST\\|SBC\\|SCF\\|SET\\|SLA\\|SLP\\|SRA\\|SRL\\|SUB\\|TST\\|TSTIO\\|XOR\\|adc\\|add\\|and\\|bit\\|call\\|ccf\\|cp\\|cpd\\|cpdr\\|cpir\\|cpi\\|cpl\\|daa\\|dec\\|di\\|djnz\\|ei\\|ex\\|exx\\|halt\\|im\\|in\\|inc\\|ind\\|indr\\|ini\\|inir\\|jp\\|jr\\|ld\\|ldd\\|lddr\\|ldi\\|ldir\\|neg\\|nop\\|mlt\\|or\\|otdm\\|otdmr\\|otdr\\|otim\\|otimr\\|otir\\|out\\|outd\\|outi\\|pop\\|push\\|res\\|ret\\|reti\\|retn\\|rl\\|rla\\|rlc\\|rlca\\|rld\\|rr\\|rra\\|rrc\\|rrca\\|rrd\\|rst\\|sbc\\|scf\\|set\\|sla\\|slp\\|sra\\|srl\\|sub\\|tst\\|tstio\\|xor\\|A\\|B\\|C\\|D\\|E\\|H\\|L\\|AF\\|BC\\|DE\\|HL\\|IX\\|IY\\|SP\\|PC\\|a\\|b\\|c\\|d\\|e\\|h\\|l\\|af\\|bc\\|de\\|hl\\|ix\\|iy\\|sp\\|pc\\|\\|NC\\|C\\|M\\|nc\\|c\\|m\\|Z\\|z\\|NZ\\|nz\\|\\|[Pp]\\([Oo]*\\|[Ee]*\\)\\|BCALL\\|bcall\\)\\>" . font-lock-builtin-face)
   '("\\(\\w*:\\)"  . font-lock-variable-name-face))
  "Minimal highlighting expressions for z80 mode")
(defconst z80-font-lock-keywords-2
  (append z80-font-lock-keywords-1
     (list
      '("\\<\\(\\([0-9][0-9A-Fa-f]*[Hh]\\|\\(0[Xx]\\|[0-9]\\|\\$[0-9A-Fa-f]\\)[0-9A-Fa-f]*\\)\\|[01][01]*[Bb]\\|%[01][01]*\\|[0-9]*\\)\\>" . font-lock-constant-face)
      '("\\(\\$\\)" . font-lock-function-name-face)))
  "Additional Keywords to highlight in z80 mode")
(defconst z80-font-lock-keywords-3
  (append z80-font-lock-keywords-2
     (list
      '("\\(\\.\\w*\\|#\\w*\\)" . font-lock-preprocessor-face)))
  "Balls-out highlighting in z80 mode")
(defvar z80-font-lock-keywords z80-font-lock-keywords-3
  "Default highlighting expressions for z80 mode")
(defun z80-indent-line ()
  "Indent current line as z80 code"
  (beginning-of-line)
  (let ((not-indented t) cur-indent)
    (if (looking-at "^\\w*:")
   (setq cur-indent 0)
      (if (looking-at "^[ \t]* [ \t]*")
     (setq cur-indent (+ (current-indentation) default-tab-width))
   (save-excursion
     (while not-indented
       (forward-line -1)
       (when (bobp)
         (setq not-indented nil)
         (setq cur-indent default-tab-width))
       (unless (or (looking-at "^[ \t]*$") (looking-at "^\\w*:")) ; Makes sure you aren't looking at blank lines or labels
         (if (looking-at "^[ \t]*[pP][oO][pP][ \t]")
        (progn
          (setq cur-indent (- (current-indentation) default-tab-width))
          (setq not-indented nil))
      (if (looking-at "^[ \t]*[pP][uU][Ss][Hh][ \t]")
          (progn
            (setq cur-indent (+ (current-indentation) default-tab-width))
            (setq not-indented nil))
        (when (looking-at "^[ \t][ \t]*\\w")
          (setq cur-indent (current-indentation))
          (setq not-indented nil)))))))))
    (if cur-indent
   (indent-line-to cur-indent)
      (indent-line-to 0)))) ; If we didn't see an indentation hint, then allow no indentation
(defvar z80-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# "w" st)
    (modify-syntax-entry ?. "w" st)
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\t "-" st)
    st)
  "Syntax table for z80-mode")
(defun z80-mode ()
  "Major mode for editing Zilog Z80 ASM files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table z80-mode-syntax-table)
  (use-local-map z80-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(z80-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'z80-indent-line)
  (setq major-mode 'z80-mode)
  (setq mode-name "Z80")
  (run-hooks 'z80-mode-hook))
(provide 'z80-mode)
