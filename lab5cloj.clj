(ns lab5cloj
  (:use [clojure.data])
  )
;izmantoju vektorus, kur saglabat rindu vertibas, un no tiem izveidoju sifru, vai atsifreju vertibu
;encrypt funkcijai
(defn encrypt-help [sifret cipher lists step dir]           ;funkcija izveido vektoru, kura elementi ir sifra rindas (strada ar 3 vai vairak rindam)
  (if (> (count sifret) 0)
    (recur
      (subs sifret 1)                               ;sifret nonem pirmo burtu
      cipher                                                ;nemaina vertibu
      (assoc lists step (str (nth lists step) (subs sifret 0 1))) ;vektora elementam step pievieno pirmo burtu
      (if (and (< step cipher) (= dir 1)) (inc step) (if (> step 0) (dec step) (inc step))) ;vektora elements, kuram pievieno
      (if (and (> step (- cipher 3)) (= dir 1)) -1 (if (and (< step 1) (= dir -1)) 1 dir)) ;vektora pievienosanas virziens
      )
    lists)                                                  ;atgriez rindu rezultatu vektora
  )

(defn encrypt-helptwo [sifret lists]                        ;funkcija izveido 2 rindu vektoru, ja cipher = 2
  (if (> (count sifret) 0)
    (if (= (count sifret) 1)
     (recur
      (subs sifret 1)                                       ;nonem pirmo burtu
      (assoc lists 0 (str (nth lists 0) (subs sifret 0 1))) ;vektora 0 elem pievieno 1. burtu
      );recur
     (recur
       (subs sifret 2)                                       ;nonem pirmos 2 burtus
       (assoc (assoc lists 0 (str (nth lists 0) (subs sifret 0 1))) 1 (str (nth lists 1) (subs sifret 1 2))) ;vektora 0 elem pievieno 1. burtu, vektora 1. elem pievieno 2. burtu
       );recur
     )
    lists)                                                  ;atgriez vektoru
  )

(defn initialise-empty-vector [cipher lists]                ;funkcija tuksa vektora izveidei ar n elementiem
  (if (> cipher 0)
    (recur (- cipher 1) (conj lists ""))
    lists
    )
  )

(defn encrypt [sifret cipher]
  (if (> cipher 2)
    (clojure.string/join (encrypt-help (clojure.string/replace sifret #" " "_") cipher (initialise-empty-vector cipher []) 0 1)) ;replace nomaina atstarpes ar "_"
    (if (= cipher 2)
      (clojure.string/join (encrypt-helptwo (clojure.string/replace sifret #" " "_") ["" ""]))
      (clojure.string/replace sifret #" " "_"))
    )
  )

;decrypt funkcijai
(defn decrypt-to-vector-helper [atsifret kopa rem-cipher cikls solis remains lists] ;funkcija ievieto vidus limenus vektora
  ;atsifret
  (if (< (inc solis) rem-cipher)
   (recur ;formula pievieno atlikumu +2/1/0 attiecigajai rindai, kam nepieciesams ja ir atlikums
    (subs atsifret (+ cikls (if (and (> remains 0) (= solis 0)) 1 (if (> remains 0) (max 1 (int (/ remains (- rem-cipher solis)))) 0)))) ;recur nonemot pirmo rindu
    kopa                                                    ;nelietots
    rem-cipher
    cikls
    (inc solis)
    (- remains (if (and (> remains 0) (= solis 0)) 1 (if (> remains 0) (max 1 (int (/ remains (- rem-cipher solis)))) 0))) ;atlikumam nonem, ko pievienoja rindai
    (assoc lists solis (subs atsifret 0 (+ cikls (if (and (> remains 0) (= solis 0)) 1 (if (> remains 0) (max 1 (int (/ remains (- rem-cipher solis)))) 0))))) ;vektora pievieno rindu
    );recur
   (assoc lists solis atsifret)                             ;atgriez pabeigtu vektoru
   )
  )

(defn decrypt-vector-string [lists solis virziens rezultats]         ;funkcija parveido vektoru teksta
  (if (not (= "" (nth lists solis)));cikls lidz atrod tuksu vertibu vektora
    (recur
      (assoc lists solis (subs (nth lists solis) 1))        ;nonem vektora n elementam pirmo burtu
      (if (and (= solis 0) (= virziens -1)) (inc solis) (+ solis virziens)) ;solis + virziens, un out of bounds drosiba
      (if (= (+ solis 2) (count lists)) -1 (if (<= solis 1) 1 virziens)) ;meklesanas virziens
      (str rezultats (subs (nth lists solis) 0 1))          ;rezultata tekstam pievieno vektora n elementa pirmo burtu
      );recur
    rezultats)
  )

(defn decrypt-to-vector [atsifret cipher remains cikls]     ;funkcija izveido vektora pirmo un pedejo elementu, un izsauc citu funkciju ja ir vidus limeni
  (let [lists-rez (if (= cipher 2)                          ;vektors, ja cipher = 2
                    [(subs atsifret 0 (+ (/ (count atsifret) 2) remains)) ;vektora 1. elements
                     (subs atsifret (+ (/ (count atsifret) 2) remains))] ;vektora 2. elements
                    ;else vektors, ja cipher > 2
                     (conj (reduce conj [(subs atsifret 0 (+ (/ (count atsifret) cikls) (if (> remains 0) 1 0)))] ;vektora pirmais elements
                                  (decrypt-to-vector-helper  ;vektora videjas rindas
                                     (subs atsifret (+ (/ (count atsifret) cikls) (if (> remains 0) 1 0)) ;nonem pirmo elementu
                                            (- (count atsifret) (- (/ (count atsifret) cikls) (if (and (> remains 0)(< remains cipher)) 1 0))))  ;nonem pedejo elementu
                                     (count                 ;padoto simbolu kopejais skaits
                                       (subs atsifret (+ (/ (count atsifret) cikls) (if (> remains 0) 1 0))
                                       (- (count atsifret) (- (/ (count atsifret) cikls) (if (and (> remains 0)(< remains cipher)) 1 0)))))
                                     (- cipher 2)           ;padod atlikuso rindu skaitu
                                     (int (* (int (/ (count atsifret) (- (* cipher 2) 2))) 2)) ;padod videjas rindas garumu bez atlikuma
                                     0                      ;padod sola nr
                                     (if (> remains 0) (if (and (> remains 0) (> remains cipher)) (- remains 2) (dec remains)) 0) ;padod atlikumu, nonemot pirma un pedeja elementa izmantoto
                                     (initialise-empty-vector (- cipher 2) []) ;padod tuksu vektoru ar n elementiem
                                     ))
                           (subs atsifret (- (count atsifret) (- (/ (count atsifret) cikls) (if (and (> remains 0) (< remains cipher)) 1 0))))) ;vektora pedejais elements
                    )
        ] lists-rez)
  )

(defn decrypt [atsifret cipher]
  (clojure.string/replace                                   ;funkcija lai mainitu "_" ar atstarpem
    (if (> cipher 1)
      (decrypt-vector-string                                ;funkcijai padod vektoru, ko atsifret
        (decrypt-to-vector atsifret cipher (rem (count atsifret) (- (* cipher 2) 2)) (- (* cipher 2) 2)) ;funkcija kas izveido vektoru
        0                                                   ;solis
        1                                                   ;virziens
        ""                                                  ;rezultats
        );true
      atsifret
      );if
    #"_" " ");clojure.string/replace
  )

;izsaukt funkcijas
(def crypted-cypher 4)
(def crypted-string "I REALLY LIKE PUZZLES")
(def crypted (encrypt crypted-string crypted-cypher))
(println (str "Ievadits: " crypted-string " \t Rindu skaits \"zogla\": " crypted-cypher))
(println (str "Sifrets: " crypted))
(println (str "Atsifrets: " (decrypt crypted crypted-cypher)))

;(println (* (int (/ (count crypted) (- (* crypted-cypher 2) 2))) 2))
;(println (rem (count crypted) (- (* crypted-cypher 2) 2)))
;(println (conj (reduce conj ["1"] ["2"]) "3"))
;(println (conj (reduce conj [(if true "1")] ["2"]) "3"))
;(println (reduce conj ["1"] [nil]))
;(def vektors ["viens" "divi" "tris" ""])                       ;try assoc (assoc [1 2 3] 1 5) => [1 5 3]
;(println (clojure.string/join vektors))
;(println vektors)
;(println (first vektors))
;(println (subs (first vektors) 1))                          ;nonemt pirmo burtu
;(println (subs (first vektors) 0 1))                        ;pirmais burts
;(println (nth vektors 1))                                   ;atrast n elementu
;(println (assoc vektors 1 "divi+"))                         ;nomainit n elementu
;(println (assoc vektors 1 (str "new_" (nth vektors 1))))    ;pievienot n elementam "new_"
;(def vektorstwo (encrypt-empty-vector 4 []))
;(println (assoc vektorstwo 0 (str (nth vektorstwo 0) "_new")))
;(println (assoc vektorstwo 3 (str (nth vektorstwo 3) "_new")))
