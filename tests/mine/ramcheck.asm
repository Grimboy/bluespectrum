;; ram-check
L11DA:  LD      H,D             ; Transfer the top value to the HL register
        LD      L,E             ; pair.

;; RAM-FILL
L11DC:  LD      (HL),#0x02        ; Load memory with $02 - red ink on black paper.
        DEC     HL              ; Decrement memory address.
        CP      H               ; Have we reached ROM - $3F ?
        JR      NZ,L11DC        ; Back to RAM-FILL if not.

;; RAM-READ
L11E2:  AND     A               ; Clear carry - prepare to subtract.
        SBC     HL,DE           ; subtract and add back setting
        ADD     HL,DE           ; carry when back at start.
        INC     HL              ; and increment for next iteration.
        JR      NC,L11EF        ; forward to RAM-DONE if we've got back to
                                ; starting point with no errors.

        DEC     (HL)            ; decrement to 1.
        JR      Z,L11EF         ; forward to RAM-DONE if faulty.

        DEC     (HL)            ; decrement to zero.
        JR      Z,L11E2         ; back to RAM-READ if zero flag was set.

;; RAM-DONE
L11EF:  DEC     HL              ; step back to last valid location.
        EXX                     ; regardless of state, set up possibly
                                ; stored system variables in case from NEW.
        LD      (0x5CB4),BC      ; insert P-RAMT.
        LD      (0x5C38),DE      ; insert RASP/PIP.
        LD      (0x5C7B),HL      ; insert UDG.
        EXX                     ; switch in main set.
        INC     B               ; now test if we arrived here from NEW.
        JR      Z,L1219         ; forward to RAM-SET if we did.

;   This section applies to START only.

        LD      (0x5CB4),HL      ; set P-RAMT to the highest working RAM
                                ; address.
        LD      DE,#0x3EAF        ; address of last byte of 'U' bitmap in ROM.
        LD      BC,#0x00A8        ; there are 21 user defined graphics.
        EX      DE,HL           ; switch pointers and make the UDGs a
        LDDR                    ; copy of the standard characters A - U.
        EX      DE,HL           ; switch the pointer to HL.
        INC     HL              ; update to start of 'A' in RAM.
        LD      (0x5C7B),HL      ; make UDG system variable address the first
                                ; bitmap.
        DEC     HL              ; point at RAMTOP again.

        LD      BC,#0x0040        ; set the values of
        LD      (0x5C38),BC      ; the PIP and RASP system variables.

;   The NEW command path rejoins here.

;; RAM-SET
L1219:  LD      (0x5CB2),HL      ; set system variable RAMTOP to HL.
