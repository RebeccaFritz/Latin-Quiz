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

;Make World State and Structures
; * ciw -> list of current input words (another structure)
; * form_info -> a structure with all the form information
; * score -> how many words the player has correct
; * grid -> grid structure with all the grid details
; * b1? and b2? -> is the button pressed
; * cursor_location -> a posn representing the last location the mouse clicked
(define-struct WS [ciw Form_Info score grid b1? b2? area_clicked])

;Includes all the information about the current forms the player is matching
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
(define DEC_LIST (list 1STDECEND 1STDECEX
                       2NDDECMASCEND 2NDDECMASCEX 2NDDECNEUEND 2NDDECNEUEX
                       3RDDECEND 3RDDECEX 3RDISTEMEND 3RDISTEMEX 3RDDECNEUEND 3RDDECNEUEX 3RDISTEMNEUEND 3RDISTEMNEUEX
                       4THDECMASCEND 4THDECMASCEX 4THDECNEUEND 4THDECNEUEX
                       5THDECEND 5THDECEX))
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
(define 1STCONJACTINDIC (make-Form_Info "1st Conjugation Active Indicative"
                               "Conjugation"
                               empty))
(define 2NDCONJACTINDIC (make-Form_Info "2nd Conjugation Active Indicative"
                               "Conjugation"
                               empty))
(define 3RDCONJACTINDIC (make-Form_Info "3rd Conjugation Active Indicative"
                               "Conjugation"
                               empty))
(define 4THCONJACTINDIC (make-Form_Info "4th Conjugation Active Indicative"
                               "Conjugation"
                               empty))
(define CONJ_LIST (list 1STCONJACTINDIC 2NDCONJACTINDIC 3RDCONJACTINDIC 4THCONJACTINDIC))

; Buttons
(define B1LOCATION (make-posn (/ WIDTH 4) (* 7 (/ HEIGHT 8))))
(define B2LOCATION (make-posn (* (/ WIDTH 4) 3) (posn-y B1LOCATION)))
(define BUTTONWIDTH 250)
(define BUTTONHEIGHT 100)
(define BUTTONS (place-image (overlay (rectangle BUTTONWIDTH BUTTONHEIGHT 'outline 'black)
                                      (text "Noun Declension" 24 'black)) (posn-x B1LOCATION) (posn-y B1LOCATION)
                                                                          (place-image (overlay (text "Verb Conjugation" 24 'black)
                                                                                                (rectangle BUTTONWIDTH BUTTONHEIGHT 'outline 'black))
                                                                                       (posn-x B2LOCATION) (posn-y B2LOCATION) BACKGROUND)))
(define B1P (place-image (overlay (rectangle BUTTONWIDTH BUTTONHEIGHT 'outline 'black)
                                  (text "Noun Declension" 24 'black)
                                  (rectangle BUTTONWIDTH BUTTONHEIGHT 'solid 'PaleTurquoise))
                         (posn-x B1LOCATION) (posn-y B1LOCATION)
                         (place-image (overlay (text "Verb Conjugation" 24 'black)
                                               (rectangle BUTTONWIDTH BUTTONHEIGHT 'outline 'black))
                                      (posn-x B2LOCATION) (posn-y B2LOCATION) BACKGROUND)))
(define B2P (place-image (overlay (rectangle BUTTONWIDTH BUTTONHEIGHT 'outline 'black)
                                  (text "Noun Declension" 24 'black))
                         (posn-x B1LOCATION) (posn-y B1LOCATION)
                         (place-image (overlay (text "Verb Conjugation" 24 'black)
                                               (rectangle BUTTONWIDTH BUTTONHEIGHT 'outline 'black)
                                               (rectangle BUTTONWIDTH BUTTONHEIGHT 'solid 'PaleTurquoise))
                                      (posn-x B2LOCATION) (posn-y B2LOCATION) BACKGROUND)))

;------------------------------------------------------------------------------

; Worldstate -> Image 
(define (render ws)
  (local [(define grid (WS-grid ws))]
    (beside (overlay (draw-words (WS-ciw ws) (Form_Info-list_forms (WS-Form_Info ws))) (drawgrid grid))
            (place-image (drawprompt (Form_Info-prompt (WS-Form_Info ws)))
             (/ (image-width BACKGROUND) 2) (/ (image-height BACKGROUND) 2)
                     (drawbuttons (WS-b1? ws) (WS-b2? ws))))))

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
          [else (place-images (list (text (InputWord-word (first ciw)) txt_size 'black)
                                    (text (InputWord-word (second ciw)) txt_size 'black)
                                    (text (InputWord-word (third ciw)) txt_size 'black)
                                    (text (InputWord-word (fourth ciw)) txt_size 'black)
                                    (text (InputWord-word (fifth ciw)) txt_size 'black)
                                    (text (InputWord-word (sixth ciw)) txt_size 'black))
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

; Boolean, boolean -> image
; Draws the buttons on the board with a darker color if they are pressed
(define (drawbuttons b1? b2?)
  (cond
    [b1? B1P]
    [b2? B2P]
    [else BUTTONS]))

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




; Worldstate -> Worldstate
(define (tock ws) ws)


; Worldstate, Mouse-x, Mouse-y, Mouse event -> Worldstate
; checks to see if the buttons on screen are clicked
(define (mouse-handler ws mx my evt)
  (if (string=? "button-up" evt)
      (update-area_clicked (change-button-color (change-chart ws mx my) mx my) mx my)
      ws))

; Worldstate, Mouse-x, Mouse-y -> Worldstate
; changes the area_clicked value when the mouse is used
(define (update-area_clicked ws mx my)
  (make-WS (WS-ciw ws) (WS-Form_Info ws) (WS-score ws) (WS-grid ws) (WS-b1? ws) (WS-b2? ws) (make-posn mx my)))

; Worldstate, Mouse-x, Mouse-y -> Worldstate
; changes the color of the buttons when they are clicked
(define (change-button-color ws mx my)
  (local [(define b1p (make-WS DEC_WORDS (list-ref DEC_LIST (random 0 (length DEC_LIST))) (WS-score ws) (WS-grid ws) #t #f (WS-area_clicked ws)))
          (define b2p (make-WS CONJ_WORDS (list-ref CONJ_LIST (random 0 (length CONJ_LIST))) (WS-score ws) (WS-grid ws) #f #t (WS-area_clicked ws)))]
    (cond [(b1-clicked? mx my) b1p]
          [(b2-clicked? mx my) b2p]
          [else ws])))

; Worldstate, Mouse-x, Mouse-y -> Worldstate
; changes the chart being displayed when a button is pressed
(define (change-chart ws mx my)
  (local [(define declension_chart (make-WS (WS-ciw ws) (WS-Form_Info ws) (WS-score ws) decgridstruct (WS-b1? ws) (WS-b2? ws) (WS-area_clicked ws)))
          (define conjugation_chart (make-WS (WS-ciw ws) (WS-Form_Info ws) (WS-score ws) conjgridstruct (WS-b1? ws) (WS-b2? ws) (WS-area_clicked ws)))]
    (cond [(b1-clicked? mx my) declension_chart]
          [(b2-clicked? mx my) conjugation_chart]
          [else ws])))

; Mouse-x, Mouse-y -> Boolean
; consumes the location of the mouse and returns true if it is over the first button (Declensions)
(define (b1-clicked? mx my)
  (local [(define b1x (+ 600 (posn-x B1LOCATION)))
          (define b1y (posn-y B1LOCATION))
          (define b2x (+ 600 (posn-x B2LOCATION)))
          (define b2y (posn-y B2LOCATION))]
    (and (<= (- b1x (/ BUTTONWIDTH 2))
             mx
             (+ b1x (/ BUTTONWIDTH 2)))
         (<= (- b1y (/ BUTTONHEIGHT 2))
             my
             (+ b1y (/ BUTTONHEIGHT 2))))))

; Mouse-x, Mouse-y -> Boolean
; consumes the location of the mouse and returns true if it is over the second button (Conjugations)
(define (b2-clicked? mx my)
  (local [(define b1x (+ 600 (posn-x B1LOCATION)))
          (define b1y (posn-y B1LOCATION))
          (define b2x (+ 600 (posn-x B2LOCATION)))
          (define b2y (posn-y B2LOCATION))]
    (and (<= (- b2x (/ BUTTONWIDTH 2))
             mx
             (+ b2x (/ BUTTONWIDTH 2)))
         (<= (- b2y (/ BUTTONHEIGHT 2))
             my
             (+ b2y (/ BUTTONHEIGHT 2))))))




; Worldstate, Key -> Worldstate
; if a key is hit and the mouse has clicked in the proper area then type in that location
; [ciw lof score grid prompt b1? b2? area_clicked]
(define (key-handler ws key); ws)
  (local [(define location (determine_location (WS-area_clicked ws) (WS-grid ws)))]
    (if (posn? location)
        (make-WS (update_word key (WS-ciw ws) location)
                 (WS-Form_Info ws)
                 (WS-score ws)
                 (WS-grid ws)
                 (WS-b1? ws)
                 (WS-b2? ws)
                 (WS-area_clicked ws))
       ws)))

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
  


; Initial Worldstate
; (define-struct WS [ciw lof score grid b1? b2?])
(define initial-ws (make-WS DEC_WORDS
                            (make-Form_Info "Welcome! Please press 'Noun Declension'\nor 'Verb Conjugation' to begin." "intro" empty)
                            0
                            decgridstruct
                            #f
                            #f
                            (make-posn 0 0)))

(big-bang initial-ws
  (to-draw render)
  (on-tick tock)
  (on-mouse mouse-handler)
  (on-key key-handler))
  
