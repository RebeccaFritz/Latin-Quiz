(require 2htdp/batch-io)

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
; * ciw -> current input words (another structure)
; * lof -> list of forms / endings (is another structure)
; * score -> how many words the player has correct
; * grid -> grid structure with all the grid details
; * prompt -> a string with the prompt being displayed for the player
; * b1? and b2? -> is the button pressed
(define-struct WS [ciw lof score grid prompt b1? b2?])

; Used to create a list of forms the player is trying to match
; * woe - word or ending
; * color - color of the word
; * location - posn with the location of the word
(define-struct Forms [woe color location])

; Used to create a list of the player's words
; * pw - player word
; * color - color of the word
; * location - posn with the location of the word
(define-struct InputWord [pw location])

; Grid Details
; * gridoutline - current grid outline in use
; * lohl - list of horizontal lines
; * lovl - list of vertical lines
; * headers - list of header words
(define-struct Grid [outline lohl lovl headers])

;-----------------------------------------------------

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
(define 1STDEC (read-lines "1st Declension Noun Endings.txt"))
(define 2NDDECMASC (read-lines "2nd Declension Noun Masculine Endings.txt"))
(define 2NDDECNEU (read-lines "2nd Declension Noun Neuter Endings.txt"))
(define 3RDDEC (read-lines "3rd Declension Noun Endings.txt"))
(define 3RDDECNEU (read-lines "3rd Declension Noun Neuter Endings.txt"))
(define 4THDECMASC (read-lines "4th Declension Noun Masculine Endings.txt"))
(define 4THDECNEU (read-lines "4th Declension Noun Neuter Endings.txt"))
(define 5THDEC (read-lines "5th Declension Noun Endings.txt"))

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
    (beside (drawgrid grid)
            (place-image (drawprompt (WS-prompt ws)) (/ (image-width BACKGROUND) 2) (/ (image-height BACKGROUND) 2)
                     (drawbuttons (WS-b1? ws) (WS-b2? ws))))))

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
      (change-button-color (change-chart (change-prompt ws mx my evt) mx my evt) mx my evt)
      ws))

; Worldstate, Mouse-x, Mouse-y, Mouse event -> Worldstate
; changes the color of the buttons when they are clicked
(define (change-button-color ws mx my evt)
  (local [(define b1p (make-WS (WS-ciw ws) (WS-lof ws) (WS-score ws) (WS-grid ws) (WS-prompt ws) #t #f))
          (define b2p (make-WS (WS-ciw ws) (WS-lof ws) (WS-score ws) (WS-grid ws) (WS-prompt ws) #f #t))]
    (cond [(b1-clicked? mx my) b1p]
          [(b2-clicked? mx my) b2p]
          [else ws])))

; Worldstate, Mouse-x, Mouse-y, Mouse event -> Worldstate
; changes the chart being displayed when a button is pressed
(define (change-chart ws mx my evt)
  (local [(define declension_chart (make-WS (WS-ciw ws) (WS-lof ws) (WS-score ws) decgridstruct (WS-prompt ws) (WS-b1? ws) (WS-b2? ws)))
          (define conjugation_chart (make-WS (WS-ciw ws) (WS-lof ws) (WS-score ws) conjgridstruct (WS-prompt ws) (WS-b1? ws) (WS-b2? ws)))]
    (cond [(b1-clicked? mx my) declension_chart]
          [(b2-clicked? mx my) conjugation_chart]
          [else ws])))

; Worldstate, Mouse-x, Mouse-y, Mouse event -> Worldstate
; changes the prompt being displayed when a button is pressed NON FUNCTIONAL
(define (change-prompt ws mx my evt)
  (local [(define declension_prompt (make-WS (WS-ciw ws) (WS-lof ws) (WS-score ws) (WS-grid ws) "Please Decline" (WS-b1? ws) (WS-b2? ws)))
          (define conjugation_prompt (make-WS (WS-ciw ws) (WS-lof ws) (WS-score ws) (WS-grid ws) "Please Conjugate" (WS-b1? ws) (WS-b2? ws)))]
    (cond [(b1-clicked? mx my) declension_prompt]
          [(b2-clicked? mx my) conjugation_prompt]
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
(define (key-handler ws key) ws)

; Initial Worldstate
; (define-struct WS [ciw lof score grid b1? b2?])
(define initial-ws (make-WS empty empty 0 conjgridstruct "Welcome! Please press 'Noun Declension'\nor 'Verb Conjugation' to begin." #f #f))

(big-bang initial-ws
  (to-draw render)
  (on-tick tock)
  (on-mouse mouse-handler)
  (on-key key-handler))
  
