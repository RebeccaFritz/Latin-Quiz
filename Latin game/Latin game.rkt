(require 2htdp/batch-io)
(require racket/list)

; Latin Game
; A quiz on Latin declension and conjugation endings
; Has an option for just the endings or actual declension
; Embeded memorization videos or pronunciation recordings
; hint option fills if the word you are on

; What will change?
; * the background table will change depending on if it is a declension or conjugation
; * the set of words you need to match will change
; * the color of the words will change when you have them correct (or wrong if you ask for help)

;-----------------------------------------------------------
; Menu Structure
; * title - a string
; * buttons - list of Button structures
(define-struct Menu [title buttons])

; Includes all information relavent to creating buttons
; * image - an image of the button
; * location - a posn
(define-struct Button [image location])

;Make World State and Structures
; * ciw -> list of InputWord structures (another structure)
; * form_info -> a structure with all the form information
; * score -> how many words the player has correct
; * grid -> grid structure with all the grid details
; * b1? and b2? -> is the button pressed
; * cursor_location -> a posn representing the last location the mouse clicked
(define-struct WS [ciw Form_Info grid area_clicked])

; Includes all the information about the current forms the player is matching
; * prompt - a string with the prompt being displayed for the player
; * type - a string with the values "decelension" or "conjugation"
; * list_forms - list of Form structures
(define-struct Form_Info [prompt type list_forms])

; Used to create a list of forms the player is trying to match
; * word - word or ending
; * color - color of the word
; * location - posn with the location of the word
(define-struct Form [word color])

; Used to create a list of the player's words
; * word - word
; * location - posn with the location of the word
(define-struct InputWord [word location])

; Grid Details
; * gridoutline - current grid outline in use
; * lohl - list of horizontal lines
; * lovl - list of vertical lines
; * headers - list of header words
(define-struct Grid [outline lohl lovl headers])

;-----------------------------------------------------

; filename -> list of Forms
; turns a list of strings into a list of Form structures
; [word color location]
(define (forms-from-file filename)
  (local [(define lof (read-words/line filename))]
    (map lof->form lof)))

; String -> Form structure
; turns a string into a Form structure
(define (lof->form los)
  (make-Form (first los) 'green))

;-----------------------------------------------------------

; Globals

; Background
(define HEIGHT 800)
(define WIDTH 600)
(define BACKGROUND (rectangle WIDTH HEIGHT 'solid 'LightCyan))
(define MENU_BACKGROUND (rectangle (* WIDTH 2) HEIGHT 'solid 'LightCyan))

; Menu
(define MAIN_MENU (make-Menu "Main Menu" (list (make-Button (overlay (text "Noun Declension" 24 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 130 150))
                                               (make-Button (overlay (text "Verb Conjugation" 24 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 365 150))
                                               (make-Button (overlay (text "Demonstratives" 24 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 600 150))
                                               (make-Button (overlay (text "Special Verbs" 24 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 835 150))
                                               (make-Button (overlay (text "Misc" 24 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 1070 150))
                                               
                                               ; Noun Declension Options
                                               (make-Button (overlay (text "1st Declension Nouns" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 130 210))
                                               (make-Button (overlay (text "2nd Declension Nouns" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 130 270))
                                               (make-Button (overlay (text "3rd Declension Nouns" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 130 330))
                                               (make-Button (overlay (text "4th Declension Nouns" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 130 390))
                                               (make-Button (overlay (text "5th Declension Nouns" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 130 450))
                                               
                                               ; Verb Conjugation Options
                                               (make-Button (overlay (text "1st Conjugation Verbs" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 365 210))
                                               (make-Button (overlay (text "2nd Conjugation Verbs" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 365 270))
                                               (make-Button (overlay (text "3rd Conjugation Verbs" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 365 330))
                                               (make-Button (overlay (text "3rd Conjugation -iō Verbs" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 365 390))
                                               (make-Button (overlay (text "4th Conjugation Verbs" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 365 450))

                                               ; Demonstratives
                                               (make-Button (overlay (text "hic, haec, hoc" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 600 210))
                                               (make-Button (overlay (text "ille, illa, illud" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 600 270))
                                               (make-Button (overlay (text "quī, quae, quod (Relative)" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 600 330))
                                               (make-Button (overlay (text "quis, quis, quid (Interrogative)" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 600 390))
                                               (make-Button (overlay (text "ipse, ipsa, ipsum (Intensive)" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 600 450))
                                               (make-Button (overlay (text "is, ea, id" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 600 510))
                                               (make-Button (overlay (text "īdem, eadem, idem" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 600 570))
                                               (make-Button (overlay (text "ego, meī (Personal)" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 600 630))
                                               (make-Button (overlay (text "tū, tuī (Personal)" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 600 690))
                                               (make-Button (overlay (text "--, suī (Reflexive)" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 600 750))
                                               

                                               ; Special Verb Options
                                               (make-Button (overlay (text "sum, esse, fuī, futūrum" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 835 210))
                                               (make-Button (overlay (text "possum, posse, potuī" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 835 270))
                                               (make-Button (overlay (text "volō, velle, voluī" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 835 330))
                                               (make-Button (overlay (text "nōlō, nōlle, nōluī" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 835 390))
                                               (make-Button (overlay (text "mālō, mālle, māluī" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 835 450))
                                               (make-Button (overlay (text "eō, īre, iī, itum" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 835 510))
                                               (make-Button (overlay (text "ferō, ferre, tulī, lātum" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 835 570))
                                               (make-Button (overlay (text "fīō, fierī, factus sum" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 835 630))

                                               ; Misc
                                               (make-Button (overlay (text "Numerals" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 1070 210))
                                               (make-Button (overlay (text "Adjectives" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 1070 270))
                                               (make-Button (overlay (text "Adverbs" 12 'black)
                                                                     (rectangle 200 50 'outline 'black))
                                                            (make-posn 1070 330)))))

; Defining Globals for the Declensions Chart
(define DGRIDWIDTH (- WIDTH 100))
(define DGRIDHEIGHT (- HEIGHT 100))
;Horizontal and Vertical lines for the declensions Grid
(define HLINED (list (list (make-posn 0 100) (make-posn DGRIDWIDTH 100))
                     (list (make-posn 0 200) (make-posn DGRIDWIDTH 200))
                     (list (make-posn 0 300) (make-posn DGRIDWIDTH 300))
                     (list (make-posn 0 400) (make-posn DGRIDWIDTH 400))
                     (list (make-posn 0 500) (make-posn DGRIDWIDTH 500))
                     (list (make-posn 0 600) (make-posn DGRIDWIDTH 600))))
(define VLINED (list (list (make-posn (/ DGRIDWIDTH 3) 0) (make-posn (/ DGRIDWIDTH 3) DGRIDHEIGHT))
                     (list (make-posn (* DGRIDWIDTH (/ 2 3)) 0) (make-posn (* DGRIDWIDTH (/ 2 3)) DGRIDHEIGHT))))
; outline for the declensions Grid
(define DECOUTLINE (overlay/align "right" "bottom"
                                   (rectangle DGRIDWIDTH 100 'solid 'PaleTurquoise)
                                   (rectangle DGRIDWIDTH DGRIDHEIGHT 'outline 'black)))
; text textx texty
(define DECHEADERLIST (list (list (text "Singular" 24 'black) 300 100)
                            (list (text "Plural" 24 'black) 465 100)
                            (list (text "Nominative" 24 'black) 130 200)
                            (list (text "Genitive" 24 'black) 130 300)
                            (list (text "Dative" 24 'black) 130 400)
                            (list (text "Accusative" 24 'black) 130 500)
                            (list (text "Ablative" 24 'black) 130 600)
                            (list (text "Vocative" 24 'black) 130 700)))
; Declension grid structure
(define decgridstruct (make-Grid DECOUTLINE VLINED HLINED DECHEADERLIST))
; Declension lists
(define 1STDECEND (make-Form_Info "1st Declension Noun Endings"
                                  "Declension"
                                  (forms-from-file "1st Declension Noun Endings.txt")))
(define 1STDECEX (make-Form_Info "First Declension Feminine Nouns:\nnauta, nautae (sailor)"
                                 "Declension"
                                  (forms-from-file "1st Declension Noun Example.txt")))
(define 2NDDECMASCEND (make-Form_Info "2nd Declension Noun Masculine Endings"
                                   "Declension"
                                   (forms-from-file "2nd Declension Noun Masculine Endings.txt")))
(define 2NDDECMASCEX (make-Form_Info "2nd Declension Masculine Nouns:\nager, agrī (farm)"
                                   "Declension"
                                   (forms-from-file "2nd Declension Noun Masculine Example.txt")))
(define 2NDDECNEUEND (make-Form_Info "2nd Declension Noun Neuter Endings"
                                  "Declension"
                                  (forms-from-file "2nd Declension Noun Neuter Endings.txt")))
(define 2NDDECNEUEX (make-Form_Info "2nd Declension Neuter Noun:\noppidum, oppidī (town)"
                                  "Declension"
                                  (forms-from-file "2nd Declension Noun Neuter Example.txt")))
(define 3RDDECEND (make-Form_Info "3rd Declension Noun\n Masculine and Feminine Endings"
                               "Declension"
                               (forms-from-file "3rd Declension Noun Endings.txt")))
(define 3RDDECEX (make-Form_Info "3rd Declension Masculine Noun:\ndux, ducis (leader)"
                               "Declension"
                               (forms-from-file "3rd Declension Noun Example.txt")))
(define 3RDISTEMEND (make-Form_Info "3rd Declension Noun\n Masculine and Feminine i-stem Endings"
                               "Declension"
                               (forms-from-file "3rd Declension i-stem Endings.txt")))
(define 3RDISTEMEX (make-Form_Info "3rd Declension Feminine i-stem Noun:\npars, partis (part)"
                               "Declension"
                               (forms-from-file "3rd Declension i-stem Example.txt")))
(define 3RDDECNEUEND (make-Form_Info "3rd Declension Noun Neuter Endings"
                                  "Declension"
                                  (forms-from-file "3rd Declension Noun Neuter Endings.txt")))
(define 3RDDECNEUEX (make-Form_Info "3rd Declension Neuter Noun:\nlūmen, lūminis (light)"
                                  "Declension"
                                  (forms-from-file "3rd Declension Noun Neuter Example.txt")))
(define 3RDISTEMNEUEND (make-Form_Info "3rd Declension Noun Neuter i-stem Endings"
                                  "Declension"
                                  (forms-from-file "3rd Declension Neuter i-stem Endings.txt")))
(define 3RDISTEMNEUEX (make-Form_Info "3rd Declension Neuter i-stem Noun:\nmare, maris (sea)"
                                  "Declension"
                                  (forms-from-file "3rd Declension Neuter i-stem Example.txt")))
(define 4THDECMASCEND (make-Form_Info "4th Declension Noun Masculine Endings"
                                   "Declension"
                                   (forms-from-file "4th Declension Noun Masculine Endings.txt")))
(define 4THDECMASCEX (make-Form_Info "4th Declension Masculine Noun:\nimpetus, impetūs (attack)"
                                   "Declension"
                                   (forms-from-file "4th Declension Noun Masculine Example.txt")))
(define 4THDECNEUEND (make-Form_Info "4th Declension Noun Neuter Endings"
                                  "Declension"
                                  (forms-from-file "4th Declension Noun Neuter Endings.txt")))
(define 4THDECNEUEX (make-Form_Info "4th Declension Neuter Noun:\ngenū, genūs (knee)"
                                  "Declension"
                                  (forms-from-file "4th Declension Noun Neuter Example.txt")))
(define 5THDECEND (make-Form_Info "5th Declension Noun Feminine Endings"
                               "Declension"
                               (forms-from-file "5th Declension Noun Endings.txt")))
(define 5THDECEX (make-Form_Info "5th Declension Feminine Noun:\nrēs, reī (thing)"
                               "Declension"
                               (forms-from-file "5th Declension Noun Example.txt")))
(define 1ST_DEC_LIST (list 1STDECEND 1STDECEX))
(define 2ND_DEC_LIST (list 2NDDECMASCEND 2NDDECMASCEX 2NDDECNEUEND 2NDDECNEUEX))
(define 3RD_DEC_LIST (list 3RDDECEND 3RDDECEX 3RDISTEMEND 3RDISTEMEX 3RDDECNEUEND 3RDDECNEUEX 3RDISTEMNEUEND 3RDISTEMNEUEX))
(define 4TH_DEC_LIST (list 4THDECMASCEND 4THDECMASCEX 4THDECNEUEND 4THDECNEUEX))
(define 5TH_DEC_LIST (list 5THDECEND 5THDECEX))
(define DEC_LIST (append 1ST_DEC_LIST 2ND_DEC_LIST 3RD_DEC_LIST 4TH_DEC_LIST 5TH_DEC_LIST))
  
; Declensions locations
(define LOCATIOND1 (make-posn 295 200))
(define LOCATIOND2 (make-posn 295 300))
(define LOCATIOND3 (make-posn 295 400))
(define LOCATIOND4 (make-posn 295 500))
(define LOCATIOND5 (make-posn 295 600))
(define LOCATIOND6 (make-posn 295 700))
(define LOCATIOND7 (make-posn 465 200))
(define LOCATIOND8 (make-posn 465 300))
(define LOCATIOND9 (make-posn 465 400))
(define LOCATIOND10 (make-posn 465 500))
(define LOCATIOND11 (make-posn 465 600))
(define LOCATIOND12 (make-posn 465 700))
(define DEC_LOCATIONS (list LOCATIOND1 LOCATIOND2 LOCATIOND3 LOCATIOND4 LOCATIOND5 LOCATIOND6 LOCATIOND7 LOCATIOND8 LOCATIOND9 LOCATIOND10 LOCATIOND11 LOCATIOND12))
(define DEC_WORDS (list (make-InputWord "" LOCATIOND1)
                         (make-InputWord "" LOCATIOND2)
                         (make-InputWord "" LOCATIOND3)
                         (make-InputWord "" LOCATIOND4)
                         (make-InputWord "" LOCATIOND5)
                         (make-InputWord "" LOCATIOND6)
                         (make-InputWord "" LOCATIOND7)
                         (make-InputWord "" LOCATIOND8)
                         (make-InputWord "" LOCATIOND9)
                         (make-InputWord "" LOCATIOND10)
                         (make-InputWord "" LOCATIOND11)
                         (make-InputWord "" LOCATIOND12)))

;Defining Globals for the Conjugation Chart
(define CGRIDWIDTH 500)
(define CGRIDHEIGHT 400)
; Horizontal and Vertical lines for the conjugation grid
(define HLINEC (list (list (make-posn 0 100) (make-posn CGRIDWIDTH 100))
                     (list (make-posn 0 200) (make-posn CGRIDWIDTH 200))
                     (list (make-posn 0 300) (make-posn CGRIDWIDTH 300))))
(define VLINEC (list (list (make-posn (/ CGRIDWIDTH 3) 0) (make-posn (/ CGRIDWIDTH 3) CGRIDHEIGHT))
                     (list (make-posn (* CGRIDWIDTH (/ 2 3)) 0) (make-posn (* CGRIDWIDTH (/ 2 3)) CGRIDHEIGHT))))
; Outline for the Conjugation Chart
(define CONJOUTLINE (rectangle CGRIDWIDTH CGRIDHEIGHT 'outline 'black))
; List of headers for the conjugation chart
; text textx texty
(define CONJHEADERLIST (list (list (text "Singular" 24 'black) 300 250)
                            (list (text "Plural" 24 'black) 465 250)
                            (list (text "First" 24 'black) 130 350)
                            (list (text "Second" 24 'black) 130 450)
                            (list (text "Third" 24 'black) 130 550)))
; Conjugation grid structure
(define conjgridstruct (make-Grid CONJOUTLINE VLINEC HLINEC CONJHEADERLIST))
; Conjugaton text locations
(define LOCATIONC1 (make-posn 295 350))
(define LOCATIONC2 (make-posn 295 450))
(define LOCATIONC3 (make-posn 295 550))
(define LOCATIONC4 (make-posn 465 350))
(define LOCATIONC5 (make-posn 465 450))
(define LOCATIONC6 (make-posn 465 550))
(define CONJ_LOCATIONS (list LOCATIONC1 LOCATIONC2 LOCATIONC3 LOCATIONC4 LOCATIONC5 LOCATIONC6))
(define CONJ_WORDS (list (make-InputWord "" LOCATIONC1)
                         (make-InputWord "" LOCATIONC2)
                         (make-InputWord "" LOCATIONC3)
                         (make-InputWord "" LOCATIONC4)
                         (make-InputWord "" LOCATIONC5)
                         (make-InputWord "" LOCATIONC6)))
; Conjugation Form lists
(define ACTINDICEND (make-Form_Info "Active Indicative Endings"
                               "Conjugation"
                               (forms-from-file "Present System Active Endings.txt")))
(define 1STCONJPRESACTINDIC (make-Form_Info "1st Conjugation Present Active Indicative:\nlaudō, laudāre, laudāvī, laudātum (to praise)"
                               "Conjugation"
                               (forms-from-file "Present Active 1st Conjugation Example.txt")))
(define 2NDCONJPRESACTINDIC (make-Form_Info "2nd Conjugation Present Active Indicative:\nmoneō, monēre, monuī, monitum (to warn)"
                               "Conjugation"
                               (forms-from-file "Present Active 2nd Conjugation Example.txt")))
(define 3RDCONJPRESACTINDIC (make-Form_Info "3rd Conjugation Present Active Indicative:\nagō, agere, ēgī, āctum (to drive, lead, do, act)"
                               "Conjugation"
                               (forms-from-file "Present Active 3rd Conjugation Example.txt")))
(define 3RDCONJIOPRESACTINDIC (make-Form_Info "3rd -iō Conjugation Present Active Indicative:\ncapiō, capere, cēpī, captum\n(to take, capture, seize, get)"
                               "Conjugation"
                               (forms-from-file "Present Active 3rd -iō Conjugation Example.txt")))
(define 4THCONJPRESACTINDIC (make-Form_Info "4th Conjugation Present Active Indicative:\naudiō, audīre, audīvī, audītum (to hear, listen to)"
                               "Conjugation"
                               (forms-from-file "Present Active 4th Conjugation Example.txt")))
(define 1ST_CONJ_LIST (list 1STCONJPRESACTINDIC))
(define 2ND_CONJ_LIST (list 2NDCONJPRESACTINDIC))
(define 3RD_CONJ_LIST (list 3RDCONJPRESACTINDIC))
(define 3RD_IO_CONJ_LIST (list 3RDCONJIOPRESACTINDIC))
(define 4TH_CONJ_LIST (list 4THCONJPRESACTINDIC))
(define CONJ_LIST (append 1ST_CONJ_LIST
                          2ND_CONJ_LIST
                          3RD_CONJ_LIST 3RD_IO_CONJ_LIST
                          4TH_CONJ_LIST
                          (list ACTINDICEND)))

; Buttons
(define BUTTONWIDTH 250)
(define BUTTONHEIGHT 100)
(define HELPBUTTON (overlay (text "?" 24 'black) (square 50 'outline 'black)))
(define BUTTONS (place-image HELPBUTTON 550 50                            
                             (place-image (overlay (text "Menu" 24 'black)
                                                   (rectangle BUTTONWIDTH BUTTONHEIGHT 'outline 'black))
                                          (/ WIDTH 2) (* 7 (/ HEIGHT 8)) BACKGROUND)))

;------------------------------------------------------------------------------

; Worldstate -> Image 
(define (render ws)
  (cond [(WS? ws) (local [(define grid (WS-grid ws))]
                    (beside (overlay (draw-words (WS-ciw ws) (Form_Info-list_forms (WS-Form_Info ws))) (drawgrid grid))
                            (place-image (drawprompt (Form_Info-prompt (WS-Form_Info ws)))
                                         (/ (image-width BACKGROUND) 2) (/ (image-height BACKGROUND) 2)
                                         BUTTONS)))]
        [(Menu? ws) (draw-menu-buttons ws (draw-title ws MENU_BACKGROUND))]))

; List of InputWord Structures, List of Form Structures -> Image
; draws the users text on screen
(define (draw-words ciw lof)
  (local [(define decwords? (= (length ciw) 12))
          (define txt_size 24)]
    (cond [(or (empty? ciw) (empty? lof)) empty-image]
          [decwords? (place-images (list (word_or_form (InputWord-word (first ciw)) (Form-word (first lof)))
                                         (word_or_form (InputWord-word (second ciw)) (Form-word (second lof)))
                                         (word_or_form (InputWord-word (third ciw)) (Form-word (third lof)))
                                         (word_or_form (InputWord-word (fourth ciw)) (Form-word (fourth lof)))
                                         (word_or_form (InputWord-word (fifth ciw)) (Form-word (fifth lof)))
                                         (word_or_form (InputWord-word (sixth ciw)) (Form-word (sixth lof)))
                                         (word_or_form (InputWord-word (seventh ciw)) (Form-word (seventh lof)))
                                         (word_or_form (InputWord-word (eighth ciw)) (Form-word (eighth lof)))
                                         (word_or_form (InputWord-word (ninth ciw)) (Form-word (ninth lof)))
                                         (word_or_form (InputWord-word (tenth ciw)) (Form-word (tenth lof)))
                                         (word_or_form (InputWord-word (list-ref ciw 10)) (Form-word (list-ref lof 10)))
                                         (word_or_form (InputWord-word (list-ref ciw 11)) (Form-word (list-ref lof 11))))
                                   DEC_LOCATIONS
                                   (rectangle WIDTH HEIGHT 'solid 'transparent))]
          [else (place-images (list (word_or_form (InputWord-word (first ciw)) (Form-word (first lof)))
                                    (word_or_form (InputWord-word (second ciw)) (Form-word (second lof)))
                                    (word_or_form (InputWord-word (third ciw)) (Form-word (third lof)))
                                    (word_or_form (InputWord-word (fourth ciw)) (Form-word (fourth lof)))
                                    (word_or_form (InputWord-word (fifth ciw)) (Form-word (fifth lof)))
                                    (word_or_form (InputWord-word (sixth ciw)) (Form-word (sixth lof))))
                              CONJ_LOCATIONS
                              (rectangle WIDTH HEIGHT 'solid 'transparent))])))

; String, String -> String
; returns the Form string if the InputWord string is the same as the Form string
; returns the InputWord string if they are different
(define (word_or_form wordstring formstring)
  (local [(define txt_size 24)]
    (if (string=? wordstring formstring) (text formstring txt_size 'green) (text wordstring txt_size 'black))))

; String -> image
; Draws the prompt text on the board
(define (drawprompt prompt)
  (text prompt 24 'black))

; structure -> image
; draws one of the grids on the background
(define (drawgrid grid)
  (addtext (Grid-headers grid)
           (overlay (addlines (addlines (Grid-outline grid)
                                        (Grid-lohl grid))
                              (Grid-lovl grid))
                    BACKGROUND)))

; list of (list text number number), img -> img
; adds text to the charts
(define (addtext list img)
  (foldl placeonetext img list))

; (list text number number), img -> img
(define (placeonetext li acc)
  (local [(define text (first li))
          (define x (second li))
          (define y (first (rest (rest li))))]
    (place-image text x y acc)))

; image, list of list of posns -> image
; adds lines to the charts
(define (addlines bkgrnd lol)
  (foldl (lambda (li acc)
           (add-line acc
                     (posn-x (first li))
                     (posn-y (first li))
                     (posn-x (first (rest li)))
                     (posn-y (first (rest li)))
                     'black))
         bkgrnd
         lol))

; Worldstate, image -> image
; Draws the title on the menu screen
(define (draw-title ws img) (place-image (text (Menu-title ws) 48 'black) WIDTH 50 img))

; WorldState, image -> image
; draws the buttons on the menu screen
(define (draw-menu-buttons ws img)
  (foldr (lambda (li acc) (place-image (Button-image li) (posn-x (Button-location li)) (posn-y (Button-location li)) acc))
         img
         (Menu-buttons ws)))
  
; Worldstate, Mouse-x, Mouse-y, Mouse event -> Worldstate
; checks to see if the buttons on screen are clicked
(define (mouse-handler ws mx my evt)
  (cond [(WS? ws) (if (string=? "button-up" evt)
                      (return_to_menu (update-area_clicked (give-hint ws mx my) mx my) mx my)
                      ws)]
        [(Menu? ws) (if (string=? "button-up" evt)
                        (change-ws ws mx my)
                        ws)]))

; WorldState, Mouse-x, Mouse-y -> Worldstate
; returns to the menu if the menu button is pressed
(define (return_to_menu ws mx my)
  (if (and (<= 775 mx 1025) (<= 650 my 750)) MAIN_MENU ws))

; Worldstate, Mouse-x, Mouse-y -> Worldstate
; changes the area_clicked value when the mouse is used
(define (update-area_clicked ws mx my)
  (make-WS (WS-ciw ws) (WS-Form_Info ws) (WS-grid ws) (make-posn mx my)))

; Worldstate, Mouse-x, Mouse-y -> Worldstate
; gives a hint when the ? button is pressed
(define (give-hint ws mx my)
  (if (and (< 1125 mx 1175) (< 25 my 75))
      (make-WS (update-hint ws (WS-ciw ws) (Form_Info-list_forms (WS-Form_Info ws)))
               (WS-Form_Info ws)
               (WS-grid ws)
               (WS-area_clicked ws))
      ws))

; Worldstate, list of InputWord, list of Forms -> list of InputWord
; updates the ciw with the proper answer
(define (update-hint ws ciw list_forms)
  (local [(define location (determine_location (WS-area_clicked ws) (WS-grid ws)))]
           (map (lambda (li)
                  (if (and (= (posn-x location) (posn-x (InputWord-location li)))
                           (= (posn-y location) (posn-y (InputWord-location li))))
                      (make-InputWord (answer ciw list_forms location)
                                      (InputWord-location li))
                      li))
                ciw)))

; list of InputWord, list of Forms, location -> string
; finds the proper answer in the list of Forms
(define (answer ciw list_forms location)
  (if (and (= (posn-x location) (posn-x (InputWord-location (first ciw))))
           (= (posn-y location) (posn-y (InputWord-location (first ciw)))))
      (Form-word (first list_forms))
      (answer (rest ciw) (rest list_forms) location)))

; Worldstate, Mouse-x, Mouse-y -> Worldstate
; generates a new world state based on the option selected from the menu 
(define (change-ws ws mx my)
  (local [(define (dec_list->ws dec_list) (make-WS DEC_WORDS (list-ref dec_list (random 0 (length dec_list))) decgridstruct (make-posn 0 0)))
          (define (conj_list->ws conj_list) (make-WS CONJ_WORDS (list-ref conj_list (random 0 (length conj_list))) conjgridstruct (make-posn 0 0)))]
  (cond [(and (<= 30 mx 230) (<= 125 my 175)) ;Random Noun Declension
         (dec_list->ws DEC_LIST)]
        [(and (<= 20 mx 230) (<= 185 my 235)) ;1st Declension Nouns
         (dec_list->ws 1ST_DEC_LIST)]
        [(and (<= 20 mx 230) (<= 245 my 295)) ;2nd Declension Nouns
         (dec_list->ws 2ND_DEC_LIST)]
        [(and (<= 20 mx 230) (<= 305 my 355)) ;3rd Declension Nouns
         (dec_list->ws 3RD_DEC_LIST)]
        [(and (<= 20 mx 230) (<= 365 my 415)) ;4th Declension Nouns
         (dec_list->ws 4TH_DEC_LIST)]
        [(and (<= 20 mx 230) (<= 425 my 575)) ;5th Declension Nouns
         (dec_list->ws 5TH_DEC_LIST)]
        [(and (<= 265 mx 465) (<= 125 my 175)) ;Random Verb Conjugation
         (conj_list->ws CONJ_LIST)] 
        [else ws])))

; Worldstate, Key -> Worldstate
; if a key is hit and the mouse has clicked in the proper area then type in that location
; [ciw lof score grid prompt b1? b2? area_clicked]
(define (key-handler ws key); ws)
  (cond [(WS? ws) (local [(define location (determine_location (WS-area_clicked ws) (WS-grid ws)))]
                    (if (posn? location)
                        (make-WS (update_word key (WS-ciw ws) location)
                                 (WS-Form_Info ws)
                                 (WS-grid ws)
                                 (next_location (WS-area_clicked ws) (Form_Info-type (WS-Form_Info ws)) key))
                        ws))]
        [(Menu? ws) ws]))

; Posn, Grid -> Posn or Boolean
(define (determine_location area_clicked grid)
  (local [(define mx (posn-x area_clicked))
          (define my (posn-y area_clicked))]
    (if (decgrid? (Grid-outline grid))
        (dec_location mx my)
        (conj_location mx my))))

; Number, Number -> Posn
; returns one of the text locations for typing, or false if the user did not click near one  
(define (dec_location mx my)
  (foldr (lambda (location acc)
        (if (and (< (- mx 50) (posn-x location) (+ mx 50))
                   (< (- my 25) (posn-y location) (+ my 25)))
              location
              acc)) #false DEC_LOCATIONS))

; Number, Number -> Posn
; returns one of the text locations for typing, or false if the user did not click near one  
(define (conj_location mx my)
  (foldr (lambda (location acc)
        (if (and (< (- mx 50) (posn-x location) (+ mx 50))
                   (< (- my 25) (posn-y location) (+ my 25)))
              location
              acc)) #false CONJ_LOCATIONS))

; Image -> boolean
; determines if the image corresponds to the declesion grid
(define (decgrid? outline)
  (= (image-height outline) (image-height DECOUTLINE)))

; Key, String, Posn -> String
; updates the current input word at that location when a key is pressed
(define (update_word key ciw location)
  (local [(define valid_inputs (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "A" "E" "I" "O" "U" "-" "/"))
          (define valid? (foldr (lambda (li acc) (if (string=? key li) #true acc)) #false valid_inputs))
          (define proper_key (key->macron key))]
    (cond [valid?
           (map (lambda (li)
                  (if (and (= (posn-x location) (posn-x (InputWord-location li)))
                           (= (posn-y location) (posn-y (InputWord-location li))))
                      (make-InputWord (string-append (InputWord-word li) proper_key)
                                      (InputWord-location li))
                      li))
                ciw)]
          [(string=? key "\b")
           (map (lambda (li)
                  (if (and (= (posn-x location) (posn-x (InputWord-location li)))
                           (= (posn-y location) (posn-y (InputWord-location li))))
                      (make-InputWord (substring (InputWord-word li) 0 (- (string-length (InputWord-word li)) 1))
                                      (InputWord-location li))
                      li))
                ciw)]
           [else ciw])))

; String -> String
; if the string is a capital letter A, E, I, O, or U if turns it into a macron vowel
; if not if just returns the string
(define (key->macron key)
  (cond [(string=? key "A") "ā"]
        [(string=? key "E") "ē"]
        [(string=? key "I") "ī"]
        [(string=? key "O") "ō"]
        [(string=? key "U") "ū"]
        [else key]))

; posn, string, string -> posn
; if tab is pressed, this function returns a new posn corresponding to the next space where the user can type
(define (next_location current_location type key)
  (if (string=? key "\t")
      (local [(define x (posn-x current_location))
              (define y (posn-y current_location))]
        (cond [(and (and (< (- x 100) 295 (+ x 100))
                         (< (- y 50) 700 (+ y 50)))
                    (string=? type "Declension"))
               (make-posn (+ x 170) 200)]
              [(and (and (< (- x 100) 295 (+ x 100))
                         (< (- y 50) 550 (+ y 50)))
                    (string=? type "Conjugation"))
               (make-posn (+ x 170) 350)]
              [else (make-posn x (+ y 100))]))
      current_location))

(big-bang MAIN_MENU
  (to-draw render)
  (on-mouse mouse-handler)
  (on-key key-handler))
 
