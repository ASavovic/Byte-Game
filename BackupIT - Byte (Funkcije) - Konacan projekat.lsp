(defun stampajtablu (stanje boja)
(stampajrez)
(stampajbrojeve)
(stampajstanje stanje slova)
(stampajsledeceg boja)
)

(defun stampajstanje (stanje slova)

(format t "~%  ")
(stampaj stanje 0 (car slova))
(format t "~%~a " (car slova))
(stampaj stanje 1 (car slova))
(format t "~%  ")
(stampaj stanje 2 (car slova))
(cond 
((= 8 n) (if (/= (length (cdr slova)) 0) (stampajstanje (cddddr(cddddr stanje)) (cdr slova))))
(t (if (/= (length (cdr slova)) 0) (stampajstanje (cddr(cddddr(cddddr stanje))) (cdr slova))))
)
)

(defun stampajsledeceg (boja)
(format t "~%Igrac na potezu: ~a" boja)
)


(defun stampajrez ()
(format t "Rezultat: (~a   :   ~a) ~%" rez1 rez2)
)

(defun stampajbrojeve ()
(if (= 8 n) (format t "     1      2      3      4      5      6      7      8") (format t "     1      2      3      4      5      6      7      8      9      10"))
)

(defun stampaj (stanje red slovo)
(
cond ((null stanje) '())
((and (equalp slovo (caar stanje)) (= (mod red 3) 0))(format t " ~a ~a ~a " (nth 0 (car(cdr(cdr(car stanje)))))
(nth 1 (car(cdr(cdr(car stanje))))) (nth 2 (car(cdr(cdr(car stanje)))))
)(stampaj (cdr stanje) red slovo))
((and (equalp slovo (caar stanje)) (= (mod red 3) 1))(format t " ~a ~a ~a " (nth 3 (car(cdr(cdr(car stanje)))))
(nth 4 (car(cdr(cdr(car stanje))))) (nth 5 (car(cdr(cdr(car stanje)))))
)(stampaj (cdr stanje) red slovo))
((and (equalp slovo (caar stanje)) (= (mod red 3) 2))(format t " ~a ~a ~a " (nth 6 (car(cdr(cdr(car stanje)))))
(nth 7 (car(cdr(cdr(car stanje))))) (nth 8 (car(cdr(cdr(car stanje)))))
)(stampaj (cdr stanje) red slovo))
)
)


(defun unesi(stanje igrac boja)
(cond ((equalp igrac 'r) (napravipoteze stanje igrac boja))
  (t
    (format t "~%Unesite potez (npr: ((E 3) (F 4) 0)): ")
      (setq potez (read))
        ;;(setq figura(if boja 'X 'O))
        ;;(if (equalp boja 'X) (setq figura t)) (setq figura '()))
        (postavi stanje potez igrac boja)
  )
  )
)
	
(defun postavi(stanje potez igrac boja)
(cond((null stanje) '())
((null potez) '())
(t 
	(setq elm1 (nth(- (+ (cadar potez)(* n (rednibrojslova slova (caar potez)))) 1) stanje))
	(setq elm2 (nth(- (+ (cadadr potez)(* n (rednibrojslova slova (caadr potez)))) 1) stanje))
	(if (and (validacijastartpoz elm1 potez boja) (validacijaciljpoz elm1 elm2 potez))
	
	(izvrsipotez stanje potez elm1 elm2 igrac boja)
	(unesi stanje igrac boja)
	)
)
))

(defun izvrsipotez(stanje potez elm1 elm2 igrac boja)
(cond((null stanje) '())
(t (setq h2 (visinapolja (reverse (caddr elm2)) 0))
	(setq hprenos (visinapolja (reverse (caddr elm1)) (caddr potez)))
	(prebacielemente potez (reverse (caddr elm1)) (reverse (caddr elm2)) hprenos h2)
	(setq stanje (zameni-nth stanje (- (+ (cadar potez)(* n (rednibrojslova slova (caar potez)))) 1) (list (caar potez) (cadar potez) (reverse pom3))))
	(setq stanje (zameni-nth stanje (- (+ (cadadr potez)(* n (rednibrojslova slova (caadr potez)))) 1) (list (caadr potez) (cadadr potez) (reverse pom2))))
	(if (equalp igrac 'r) (setq igrac 'c) (setq igrac 'r))
	(if (equalp boja 'X) (setq boja 'O) (setq boja 'X))
	(proverapobede stanje potez igrac boja)
)
)
)

(defun zameni-nth (lista n elem)
  (cond 
    ((null lista) '())
    ((eq n 0) (cons elem (cdr lista)))
    (t (cons(car lista) (zameni-nth (cdr lista) (- n 1) elem)))))
	
(defun prebacielemente(potez el1 el2 hprenos h2)
(cond((= hprenos 0) '())
(t
	(prebacielemente potez el1 el2 (- hprenos 1) h2)
	(rotatef (nth (-(+ hprenos (caddr potez)) 1) el1) (nth (- (+ h2 hprenos) 1) el2)) 
	(setq pom3 el1)
	(setq pom2 el2)
)
))

(defun validacijastartpoz (elm potez figura)
(cond ((null elm) '())
(t 
	(setq figuratrenutnogigraca (equalp figura (nth (caddr potez) (reverse (caddr elm)))));;da li je na prosledjenoj visini figura trenutnog igraca
	(setq praznopolje (not(equalp "." (nth 8 (caddr elm)))));;da li je prazno polje
	(setq crnopolje (equalp "." (nth 0 (caddr elm)))) ;;da li je crno polje
	
	(if(not figuratrenutnogigraca) (format t "Potez nije validan jer nije moguce pomeriti token koji nije vase boje."))	
	(if(not praznopolje) (format t "Potez nije validan jer nije moguce pomeriti figuru sa praznog polja."))
	(if(not crnopolje) (format t "Potez nije validan jer nije moguce pomeriti tokene sa praznog polja."))
	(and figuratrenutnogigraca (and praznopolje crnopolje))
	)
)
)

(defun validacijaciljpoz (elm1 elm2 potez)
(cond ((null elm2) '())
(t(setq h1 (visinapolja (reverse(caddr elm1))(caddr potez)))
	(setq h2 (visinapolja (reverse(caddr elm2)) '0))
	(setq zbirveciod8 (>= 8 (+ h1 h2)))
	(setq ceostekod0 (equalp 0 (caddr potez)))
	;(setq h1veceodh2 (if(not (and (= h2 0) ceostekod0))(if(not (and (= h1 1)(= h2 1))) (> h2 h1) t) t))
	
	(setq praznopolje (if (not ceostekod0) (not(equalp "." (nth 8 (caddr elm2))))t));;da li je prazno polje
	
	(setq crnopolje (equalp "." (nth 0 (caddr elm2)))) ;;da li je crno polje
	(setq prvired (not (equal (caadr potez) 'A)))
	(if (= n 8) (setq zadnjired (not (equal (caadr potez) 'H))) (setq zadnjired (not (equal (caadr potez) 'J))))
	(if (not zbirveciod8) (format t "Potez nije validan jer bi br. elemenata kule na odredistu bio veci od 8. "))
	;(if (not h1veceodh2) (format t "Potez nije validan jer je br. tokena koji se prenose veci ili jednak br. tokena na odredistu."))
	(if (not praznopolje) (format t "Potez nije validan jer nije moguce pomeriti figuru na prazno polje."))
	(if (not crnopolje) (format t "Potez nije validan jer nije moguce pomeriti tokene na belo polje."))
	(if (not zadnjired) (format t "Potez nije validan jer se ide na poslednji red."))
	(if (not prvired) (format t "Potez nije validan jer se ide na prvi red."))
	(and(and(and zbirveciod8 (and t (and praznopolje crnopolje))) prvired)zadnjired)
)))

(defun rednibrojslova (lista slovo)
(
  cond ((null lista) '0)
  ((not (equalp slovo (car lista))) (+ 1 (rednibrojslova (cdr lista) slovo)))
  (t '0)
)
)

(defun visinapolja (lista pocpoz)
(
  cond ((equalp "." (car lista)) '0) ;; dobar cond
  ((> pocpoz 0)(visinapolja (cdr lista) (- pocpoz 1)))
  ((<= pocpoz 0) (+ 1 (visinapolja (cdr lista) pocpoz))

)))



(defun postavistanje (n)
(cond ((not(or (= 8 n) (= 10 n))) (format t "~%Vredno za n mora biti 8 ili 10! Unesite ponovo!"))
((= 8 n) (setq stanje '((A 1 ("." "." "." "." "." "." "." "." ".")) (A 2 (" " " " " " " " " " " " " " " " " ")) (A 3 ("." "." "." "." "." "." "." "." ".")) (A 4 (" " " " " " " " " " " " " " " " " ")) (A 5 ("." "." "." "." "." "." "." "." ".")) (A 6 (" " " " " " " " " " " " " " " " " ")) (A 7 ("." "." "." "." "." "." "." "." ".")) (A 8 (" " " " " " " " " " " " " " " " " "))
(B 1 (" " " " " " " " " " " " " " " " " ")) (B 2 ("." "." "." "." "." "." "." "." X)) (B 3 (" " " " " " " " " " " " " " " " " ")) (B 4 ("." "." "." "." "." "." "." "." X)) (B 5 (" " " " " " " " " " " " " " " " " ")) (B 6 ("." "." "." "." "." "." "." "." X)) (B 7 (" " " " " " " " " " " " " " " " " ")) (B 8 ("." "." "." "." "." "." "." "." X))
(C 1 ("." "." "." "." "." "." "." "." O)) (C 2 (" " " " " " " " " " " " " " " " " ")) (C 3 ("." "." "." "." "." "." "." "." O)) (C 4 (" " " " " " " " " " " " " " " " " ")) (C 5 ("." "." "." "." "." "." "." "." O)) (C 6 (" " " " " " " " " " " " " " " " " ")) (C 7 ("." "." "." "." "." "." "." "." O)) (C 8 (" " " " " " " " " " " " " " " " " "))
(D 1 (" " " " " " " " " " " " " " " " " ")) (D 2 ("." "." "." "." "." "." "." "." X)) (D 3 (" " " " " " " " " " " " " " " " " ")) (D 4 ("." "." "." "." "." "." "." "." X)) (D 5 (" " " " " " " " " " " " " " " " " ")) (D 6 ("." "." "." "." "." "." "." "." X)) (D 7 (" " " " " " " " " " " " " " " " " ")) (D 8 ("." "." "." "." "." "." "." "." X))
(E 1 ("." "." "." "." "." "." "." "." O)) (E 2 (" " " " " " " " " " " " " " " " " ")) (E 3 ("." "." "." "." "." "." "." "." O)) (E 4 (" " " " " " " " " " " " " " " " " ")) (E 5 ("." "." "." "." "." "." "." "." O)) (E 6 (" " " " " " " " " " " " " " " " " ")) (E 7 ("." "." "." "." "." "." "." "." O)) (E 8 (" " " " " " " " " " " " " " " " " "))
(F 1 (" " " " " " " " " " " " " " " " " ")) (F 2 ("." "." "." "." "." "." "." "." X)) (F 3 (" " " " " " " " " " " " " " " " " ")) (F 4 ("." "." "." "." "." "." "." "." X)) (F 5 (" " " " " " " " " " " " " " " " " ")) (F 6 ("." "." "." "." "." "." "." "." X)) (F 7 (" " " " " " " " " " " " " " " " " ")) (F 8 ("." "." "." "." "." "." "." "." X))
(G 1 ("." "." "." "." "." "." "." "." O)) (G 2 (" " " " " " " " " " " " " " " " " ")) (G 3 ("." "." "." "." "." "." "." "." O)) (G 4 (" " " " " " " " " " " " " " " " " ")) (G 5 ("." "." "." "." "." "." "." "." O)) (G 6 (" " " " " " " " " " " " " " " " " ")) (G 7 ("." "." "." "." "." "." "." "." O)) (G 8 (" " " " " " " " " " " " " " " " " "))
(H 1 (" " " " " " " " " " " " " " " " " ")) (H 2 ("." "." "." "." "." "." "." "." ".")) (H 3 (" " " " " " " " " " " " " " " " " ")) (H 4 ("." "." "." "." "." "." "." "." ".")) (H 5 (" " " " " " " " " " " " " " " " " ")) (H 6 ("." "." "." "." "." "." "." "." ".")) (H 7 (" " " " " " " " " " " " " " " " " ")) (H 8 ("." "." "." "." "." "." "." "." "."))
)) (setq slova '(A B C D E F G H)))
((= 10 n) (setq stanje '((A 1 ("." "." "." "." "." "." "." "." ".")) (A 2 (" " " " " " " " " " " " " " " " " ")) (A 3 ("." "." "." "." "." "." "." "." ".")) (A 4 (" " " " " " " " " " " " " " " " " ")) (A 5 ("." "." "." "." "." "." "." "." ".")) (A 6 (" " " " " " " " " " " " " " " " " ")) (A 7 ("." "." "." "." "." "." "." "." ".")) (A 8 (" " " " " " " " " " " " " " " " " ")) (A 9 ("." "." "." "." "." "." "." "." ".")) (A 10 (" " " " " " " " " " " " " " " " " "))
(B 1 (" " " " " " " " " " " " " " " " " ")) (B 2 ("." "." "." "." "." "." "." "." X)) (B 3 (" " " " " " " " " " " " " " " " " ")) (B 4 ("." "." "." "." "." "." "." "." X)) (B 5 (" " " " " " " " " " " " " " " " " ")) (B 6 ("." "." "." "." "." "." "." "." X)) (B 7 (" " " " " " " " " " " " " " " " " ")) (B 8 ("." "." "." "." "." "." "." "." X)) (B 9 (" " " " " " " " " " " " " " " " " ")) (B 10 ("." "." "." "." "." "." "." "." X))
(C 1 ("." "." "." "." "." "." "." "." O)) (C 2 (" " " " " " " " " " " " " " " " " ")) (C 3 ("." "." "." "." "." "." "." "." O)) (C 4 (" " " " " " " " " " " " " " " " " ")) (C 5 ("." "." "." "." "." "." "." "." O)) (C 6 (" " " " " " " " " " " " " " " " " ")) (C 7 ("." "." "." "." "." "." "." "." O)) (C 8 (" " " " " " " " " " " " " " " " " ")) (C 9 ("." "." "." "." "." "." "." "." O)) (C 10 (" " " " " " " " " " " " " " " " " "))
(D 1 (" " " " " " " " " " " " " " " " " ")) (D 2 ("." "." "." "." "." "." "." "." X)) (D 3 (" " " " " " " " " " " " " " " " " ")) (D 4 ("." "." "." "." "." "." "." "." X)) (D 5 (" " " " " " " " " " " " " " " " " ")) (D 6 ("." "." "." "." "." "." "." "." X)) (D 7 (" " " " " " " " " " " " " " " " " ")) (D 8 ("." "." "." "." "." "." "." "." X)) (D 9 (" " " " " " " " " " " " " " " " " ")) (D 10 ("." "." "." "." "." "." "." "." X))
(E 1 ("." "." "." "." "." "." "." "." O)) (E 2 (" " " " " " " " " " " " " " " " " ")) (E 3 ("." "." "." "." "." "." "." "." O)) (E 4 (" " " " " " " " " " " " " " " " " ")) (E 5 ("." "." "." "." "." "." "." "." O)) (E 6 (" " " " " " " " " " " " " " " " " ")) (E 7 ("." "." "." "." "." "." "." "." O)) (E 8 (" " " " " " " " " " " " " " " " " ")) (E 9 ("." "." "." "." "." "." "." "." O)) (E 10 (" " " " " " " " " " " " " " " " " "))
(F 1 (" " " " " " " " " " " " " " " " " ")) (F 2 ("." "." "." "." "." "." "." "." X)) (F 3 (" " " " " " " " " " " " " " " " " ")) (F 4 ("." "." "." "." "." "." "." "." X)) (F 5 (" " " " " " " " " " " " " " " " " ")) (F 6 ("." "." "." "." "." "." "." "." X)) (F 7 (" " " " " " " " " " " " " " " " " ")) (F 8 ("." "." "." "." "." "." "." "." X)) (F 9 (" " " " " " " " " " " " " " " " " ")) (F 10 ("." "." "." "." "." "." "." "." X))
(G 1 ("." "." "." "." "." "." "." "." O)) (G 2 (" " " " " " " " " " " " " " " " " ")) (G 3 ("." "." "." "." "." "." "." "." O)) (G 4 (" " " " " " " " " " " " " " " " " ")) (G 5 ("." "." "." "." "." "." "." "." O)) (G 6 (" " " " " " " " " " " " " " " " " ")) (G 7 ("." "." "." "." "." "." "." "." O)) (G 8 (" " " " " " " " " " " " " " " " " ")) (G 9 ("." "." "." "." "." "." "." "." O)) (G 10 (" " " " " " " " " " " " " " " " " "))
(H 1 (" " " " " " " " " " " " " " " " " ")) (H 2 ("." "." "." "." "." "." "." "." X)) (H 3 (" " " " " " " " " " " " " " " " " ")) (H 4 ("." "." "." "." "." "." "." "." X)) (H 5 (" " " " " " " " " " " " " " " " " ")) (H 6 ("." "." "." "." "." "." "." "." X)) (H 7 (" " " " " " " " " " " " " " " " " ")) (H 8 ("." "." "." "." "." "." "." "." X)) (H 9 (" " " " " " " " " " " " " " " " " ")) (H 10 ("." "." "." "." "." "." "." "." X))
(I 1 ("." "." "." "." "." "." "." "." O)) (I 2 (" " " " " " " " " " " " " " " " " ")) (I 3 ("." "." "." "." "." "." "." "." O)) (I 4 (" " " " " " " " " " " " " " " " " ")) (I 5 ("." "." "." "." "." "." "." "." O)) (I 6 (" " " " " " " " " " " " " " " " " ")) (I 7 ("." "." "." "." "." "." "." "." O)) (I 8 (" " " " " " " " " " " " " " " " " ")) (I 9 ("." "." "." "." "." "." "." "." O)) (I 10 (" " " " " " " " " " " " " " " " " "))
(J 1 (" " " " " " " " " " " " " " " " " ")) (J 2 ("." "." "." "." "." "." "." "." ".")) (J 3 (" " " " " " " " " " " " " " " " " ")) (J 4 ("." "." "." "." "." "." "." "." ".")) (J 5 (" " " " " " " " " " " " " " " " " ")) (J 6 ("." "." "." "." "." "." "." "." ".")) (J 7 (" " " " " " " " " " " " " " " " " ")) (J 8 ("." "." "." "." "." "." "." "." ".")) (J 9 (" " " " " " " " " " " " " " " " " ")) (J 10 ("." "." "." "." "." "." "." "." "."))
)) (setq slova '(A B C D E F G H I J))))
)

(defun byte-game ()
	(format t "~%Unesite velicinu table(8 ili 10):")
	(setq n (read))

	(format t "~%Unesite r ako racunar igra prvi (r ili c):")
	(setq igrac (read))
	(setq prviigrac igrac)
	(setq boja 'X)
	(setq rez1 0)
	(setq rez2 0)
	(postavistanje n)
	(stampajtablu stanje boja)
	(unesi stanje igrac boja)
)

(defun proverapobede (stanje potez igrac boja)
	(setq zadnjiel (nth 1 (caddr (nth(- (+ (cadadr potez)(* n (rednibrojslova slova (caadr potez)))) 1) stanje)))) ;;predzadnja figura
  	(
	cond
	  ((equalp 'X zadnjiel) (setq rez1 (+ rez1 1)))
	  ((equalp 'O zadnjiel) (setq rez2 (+ rez2 1)))
	)
	(if (not (equalp zadnjiel ".")) (setq stanje (zameni-nth stanje (- (+ (cadadr potez)(* n (rednibrojslova slova (caadr potez)))) 1) (list (caadr potez) (cadadr potez) '("." "." "." "." "." "." "." "." ".")))))
	(cond 
		((= 8 n) 
			(cond 
				((= rez1 2) (format t "Pobedio je igrac ~a (X - bele boje)" (if(equalp prviigrac 'c) 'Covek 'Racunar)) (abort))
				((= rez2 2) (format t "Pobedio je igrac ~a (O - crne boje)" (if(equalp prviigrac 'r) 'Covek 'Racunar)) (abort))
			))
		((= 10 n)
			(cond 
				((= rez1 3) (format t "Pobedio je igrac ~a (X - bele boje)" (if(equalp prviigrac 'c) 'Covek 'Racunar)) (abort))
				((= rez2 3) (format t "Pobedio je igrac ~a (O - crne boje)" (if(equalp prviigrac 'r) 'Covek 'Racunar)) (abort))
			))
	)
	(stampajtablu stanje boja)
	(napravipoteze stanje igrac boja)
)

(defun napravipoteze (stanje igrac boja)
(setq stanje2 stanje)
(setq stanje2 (if (= n 8)(cdr (cddddr (cddddr stanje2)))(cdddr (cddddr (cddddr stanje2)))))
(setq stanje2 (if (= n 8)(cdr (cddddr (cddddr (reverse stanje2))))(cdddr (cddddr (cddddr (reverse stanje2))))))
(setq stanje2 (reverse stanje2))
(setq stanje2 (ocististanje stanje2))
(setq potezi '())
(formirajlistupoteza stanje stanje2 igrac boja)
(sledecinapotezu stanje igrac boja)
)

(defun ocististanje (stanje2)
(cond 
	((equalp (cdr stanje2) '()) (list (car stanje2)))
	((and (equalp " " (caaddr (cadr stanje2))) (equalp " " (caaddr (caddr stanje2)))) (setq stanje2 (cons (car stanje2) (ocististanje (cdddr stanje2)))))
	((equalp "." (caaddr (cadr stanje2))) (setq stanje2 (cons (car stanje2) (ocististanje (cdr stanje2)))))
	(t  (setq stanje2 (cons (car stanje2) (ocististanje (cddr stanje2)))) )
)
)

(defun validacijastartpoz1 (elm potez figura)
(cond ((null elm) '())
(t 
	;(setq figuratrenutnogigraca (equalp figura (nth (caddr potez) (reverse (caddr elm)))));;da li je na prosledjenoj visini figura trenutnog igraca
	(setq praznopolje (not(equalp "." (nth 8 (caddr elm)))));;da li je prazno polje
	(setq crnopolje (equalp "." (nth 0 (caddr elm)))) ;;da li je crno polje
	;(and figuratrenutnogigraca (and praznopolje crnopolje))
	(and praznopolje crnopolje)
	)
)
)

(defun validacijaciljpoz1 (elm1 elm2 potez)
(cond ((null elm2) '())
(t(setq h1 (visinapolja (reverse(caddr elm1))(caddr potez)))
	(setq h2 (visinapolja (reverse(caddr elm2)) '0))
	(setq zbirveciod8 (>= 8 (+ h1 h2)))
	(setq ceostekod0 (equalp 0 (caddr potez)))
	(setq h1veceodh2 (if(not (and (= h2 0) ceostekod0))(if(not (and (= h1 1)(= h2 1))) (> h2 h1) t) t))
	
	;;(setq praznopolje (if (not ceostekod0) (not(equalp "." (nth 8 (caddr elm2))))t));;da li je prazno polje
	
	(setq crnopolje (equalp "." (nth 0 (caddr elm2)))) ;;da li je crno polje

	(setq prvired (not (equal (caadr potez) 'A)))
	(if (= n 8) (setq zadnjired (not (equal (caadr potez) 'H))) (setq zadnjired (not (equal (caadr potez) 'J))))

	(and zbirveciod8 (and h1veceodh2 (and crnopolje (and prvired zadnjired )))))
)))


(defun formirajlistupoteza (stanje stanje2 igrac boja)
(cond 
	((null stanje2) '())
	((not (null (member boja (caddar stanje2))))
	;; K1   K2
	;;    X
	;; K3   K4
	;;P je potez
	;; ovaj cond samo kreira koeficijente
	(cond 
		((= (mod (+ (* n (- (rednibrojslova slova (caar stanje2)) 1)) (- (cadar stanje2) 2)) n) (- n 2))
			(setq k1 (nth (+ (* n (- (rednibrojslova slova (caar stanje2)) 1)) (- (cadar stanje2) 2)) stanje ))
			(setq k3 (nth (+ (* n (+ (rednibrojslova slova (caar stanje2)) 1)) (- (cadar stanje2) 2)) stanje ))
			(setq k2 '())
			(setq k4 '())
		)
		((= (mod (+ (* n (- (rednibrojslova slova (caar stanje2)) 1)) (- (cadar stanje2) 2)) n) (- n 1))
			(setq k2 (nth (+ (* n (- (rednibrojslova slova (caar stanje2)) 1)) (cadar stanje2)) stanje ))
			(setq k4 (nth (+ (* n (+ (rednibrojslova slova (caar stanje2)) 1)) (cadar stanje2)) stanje ))
			(setq k1 '())
			(setq k3 '())
		)
		(t 
			(setq k1 (nth (+ (* n (- (rednibrojslova slova (caar stanje2)) 1)) (- (cadar stanje2) 2)) stanje ))
			(setq k2 (nth (+ (* n (- (rednibrojslova slova (caar stanje2)) 1)) (cadar stanje2)) stanje ))
			(setq k3 (nth (+ (* n (+ (rednibrojslova slova (caar stanje2)) 1)) (- (cadar stanje2) 2)) stanje ))
			(setq k4 (nth (+ (* n (+ (rednibrojslova slova (caar stanje2)) 1)) (cadar stanje2)) stanje ))
		) 
	)
		;;(list (list (caar stanje2) (cadar stanje2)) (list (car k1) (cadr k1)) 0)
		(if (null k1) (setq p1 '()) (setq p1 (list (list (caar stanje2) (cadar stanje2)) (list (car k1) (cadr k1)) 0)))
		(if (null k2) (setq p2 '()) (setq p2 (list (list (caar stanje2) (cadar stanje2)) (list (car k2) (cadr k2)) 0)))
		(if (null k3) (setq p3 '()) (setq p3 (list (list (caar stanje2) (cadar stanje2)) (list (car k3) (cadr k3)) 0)))
		(if (null k4) (setq p4 '()) (setq p4 (list (list (caar stanje2) (cadar stanje2)) (list (car k4) (cadr k4)) 0)))
		;; (validacijastartpoz1 elm1 potez boja) (validacijaciljpoz1 elm1 elm2 potez))
		;;potezi je lista u kojoj se formiraju potezi
		(if (not (null p2))(if (and (validacijastartpoz1 (car stanje2) p1 boja) (validacijaciljpoz1 (car stanje2) k1 p1)) (setq potezi (append potezi (list p1)))))
		;(if (not (null p2))(if (validacijaciljpoz1 (car stanje2) k1 p1) (setq potezi (append potezi (list p1)))))
		(if (not (null p2))(if (and (validacijastartpoz1 (car stanje2) p2 boja) (validacijaciljpoz1 (car stanje2) k2 p2)) (setq potezi (append potezi (list p2)))))
		(if (not (null p3))(if (and (validacijastartpoz1 (car stanje2) p3 boja) (validacijaciljpoz1 (car stanje2) k3 p3)) (setq potezi (append potezi (list p3)))))
		(if (not (null p4))(if (and (validacijastartpoz1 (car stanje2) p4 boja) (validacijaciljpoz1 (car stanje2) k4 p4)) (setq potezi (append potezi (list p4)))))
		;(setq potezi (append potezi (list p1)))
		;(setq potezi (append potezi (list p2)))
		;(setq potezi (append potezi (list p3)))
		;(setq potezi (append potezi (list p4)))
		(formirajlistupoteza stanje (cdr stanje2) igrac boja))
	(t (formirajlistupoteza stanje (cdr stanje2) igrac boja))
)
))

(defun min-max(node depth A B maximizingplayer)
  (cond ((or (= depth 0) (terminalnicvor node))
  (if (atom (car node)) (heuristika node) (heuristika (car node))))
  (t(if maximizingplayer
      (let ((value (list -9999999 '())))
        (dolist (child (children node))
          (setq pomocnap (min-max child (1- depth) A B nil))
          
          (if (< (car value) (car pomocnap))(setq value pomocnap))
          (if (< (car A) (car value)) (setq A value))
          (when (<= (car B) (car A))
            ;;B cut-off
            (return)))
        value)
      (let ((value (list 9999999 '())))
        (dolist (child (children node))
          (setq pomocnap2 (min-max child (1- depth) A B t))
          (if (> (car value) (car pomocnap2 ))(setq value pomocnap2))
          (if (> (car A) (car value)) (setq A value))
          (when (<= (car B) (car A))
            ;;A cut-off
            (return)))
        value)))))



(defun terminalnicvor(node)
    (if (atom (car node)) t '())
)


(defun children(node)
    (list (car node) (cadr node))
)       

(defun heuristika(node)
;;kod
;;//
;;//
     (setq vrednost (random 20) )
;;kod
	(setq node1 (zameni-nth node 0 vrednost))
	node1
)

(defun split (given-list cnt &optional (result-left nil))
  (if (= cnt 0)
    (if (null result-left)
      given-list
      (cons result-left (list given-list)))
    (if (null given-list)
      result-left
      (split (cdr given-list) (- cnt 1) (append result-left (list (car given-list)))))))
	  
(defun splithalf (l)
	(split l (car (list (ceiling (length l) 2)))) 
)


(defun stablo (potezi) 
    (cond
		((null potezi) '())
		(t 
			(let* 
				((levi (car (splithalf potezi))) (desni (cadr (splithalf potezi))))
				(cond 
					((null (cddr potezi)) (list (list 0 levi) (list 0 desni)))
					((null (cdr desni)) (list (stablo levi) (list (list 0 desni) (list 0 desni))))
					(t (list (stablo levi) (stablo desni))) 
				)
			)
		)
	)
)


(defun dubina (l) 
	(cond
		((atom l) -1)
		(t (+ (dubina (car l)) 1))
	)
)

(defun sledecinapotezu(stanje igrac boja)
(cond ((equalp igrac 'c) (unesi stanje igrac boja))
	(t
	(setq stablotrazenja (stablo potezi))
	(setq dubinast (dubina stablotrazenja))
	(setq potezracunara (min-max stablotrazenja dubinast (list -9999999 '()) (list 9999999 '()) t))
	(format t "~%Potez ~a je odigrao racunar.~%" (caadr potezracunara))
	(postavi stanje (caadr potezracunara) igrac boja)
	)
))

(defun true-var? (s) 
  (if (symbolp s)
      (equal #\? (char (symbol-name s) 0))
    nil))

(defun var? (s) 
  (if (symbolp s)
      (let ((c (char (symbol-name s) 0)))
        (or (equal c #\?) (equal c #\%)))
    nil))

(defun func? (s) 
  (if (symbolp s)
      (equal #\= (char (symbol-name s) 0))
    nil))

(defun predefined-predicate? (s)
  (if (symbolp s)
      (equal #\! (char (symbol-name s) 0))
    nil))

(defun const? (s)
  (not (or (var? s) (func? s))))

(defun func-of (f x)
  (cond
   ((null f) 
    t)
   ((atom f)
    (equal f x))
   (t
    (or (func-of (car f) x) (func-of (cdr f) x)))))

(defun has-var (f)
  (cond
   ((null f) 
    nil)
   ((atom f)
    (var? f))
   (t
    (or (has-var (car f)) (has-var (cdr f))))))

(defun rule-consequence (r)
  (car (last r)))

(defun rule-premises (r)
  (let ((p (cadr r)))
    (if (and (listp p) (equal (car p) 'and))
        (cdr p)
      (list p))))
      
(defun format-query (q)
  (if (and (listp q) (equal (car q) 'and))
      (cdr q)
    (list q))) 

(defun evaluate-predicate (p ls)
  (if (has-var p) nil  
    (if (eval p) 
        (list ls) 
      nil))) 

(defparameter *FACTS* nil)
(defparameter *RULES* nil)
(defparameter *MAXDEPTH* 10)

(defun prepare-knowledge (lr lf maxdepth)
  (setq *FACTS* lf *RULES* (fix-rules lr) *MAXDEPTH* maxdepth))

(defun count-results (q)
  (length (infer- (format-query q) '(nil) 0)))

(defun infer (q)
  (filter-results (infer- (format-query q) '(nil) 0)))

(defun fix-rules (lr)
  (if (null lr) nil
    (cons (fix-rule (car lr)) (fix-rules (cdr lr)))))

(defun fix-rule (r)
  (let ((ls (make-rule-ls r nil)))
    (apply-ls r ls)))

(defun make-rule-ls (r ls)
  (cond
   ((null r)
    ls)
   ((var? r)
    (let ((a (assoc r ls)))
      (if (null a)
          (cons (list r (gensym "%")) ls)
        ls)))
   ((atom r)
    ls)   
   (t
    (make-rule-ls (cdr r) 
                  (make-rule-ls (car r) ls)))))

(defun filter-results (lls)
  (if (null lls) nil
    (cons (filter-result (car lls)) (filter-results (cdr lls)))))

(defun filter-result (ls)
  (if (null ls) nil
    (if (true-var? (caar ls))
        (cons (car ls) (filter-result (cdr ls)))
      (filter-result (cdr ls)))))

(defun infer- (lq lls depth)
  (if (null lq) lls
    (let ((lls-n (infer-q (car lq) lls depth)))
      (if (null lls-n) nil
        (infer- (cdr lq) lls-n depth)))))

(defun infer-q (q lls depth)
  (if (null lls) nil
    (let ((lls-n (infer-q-ls q (car lls) depth)))
      (if (null lls-n)
          (infer-q q (cdr lls) depth)
        (append lls-n (infer-q q (cdr lls) depth))))))

(defun infer-q-ls (q ls depth)
  (if (predefined-predicate? (car q))
      (evaluate-predicate (apply-ls q ls) ls)
    (if (< depth *MAXDEPTH*)
        (append (infer-q-ls-lf q *FACTS* ls) (infer-q-ls-lr q *RULES* ls depth))
      (infer-q-ls-lf q *FACTS* ls))))
      
(defun infer-q-ls-lf (q lf ls)
  (if (null lf) nil
    (let ((ls-n (infer-q-ls-f q (car lf) ls)))
      (if (null ls-n)
          (infer-q-ls-lf q (cdr lf) ls)
        (if (null (car ls-n)) ls-n
          (append ls-n (infer-q-ls-lf q (cdr lf) ls)))))))

(defun infer-q-ls-f (q f ls)
  (if (= (length q) (length f)) 
      (infer-q-ls-f- q f ls)
    nil))

(defun infer-q-ls-f- (q f ls)
  (if (null q) (list ls)
    (let ((nq (apply-and-eval (car q) ls)) (nf (car f)))
      (if (var? nq) 
          (infer-q-ls-f- (cdr q) (cdr f) (append ls (list (list nq nf))))
        (if (equal nq nf) 
            (infer-q-ls-f- (cdr q) (cdr f) ls)
          nil)))))
          
(defun infer-q-ls-lr (q lr ls depth)
  (if (null lr) nil
    (let ((ls-n (infer-q-ls-r q (car lr) ls depth)))
      (if (null ls-n)
          (infer-q-ls-lr q (cdr lr) ls depth)
        (if (null (car ls-n)) ls-n
          (append ls-n (infer-q-ls-lr q (cdr lr) ls depth)))))))

(defun infer-q-ls-r (q r ls depth)
  (let ((c (rule-consequence r)))
    (if (= (length q) (length c))
        (let ((lsc (unify q c nil ls)))
          (if (null lsc) nil
            (infer- (apply-ls (rule-premises r) (car lsc)) (cdr lsc) (1+ depth))))
      nil)))

(defun unify (q c uls ls)
  (if (or (null q) (null c))
      (if (and (null q) (null c)) (list uls ls) nil)
    (let ((eq (car q)) (ec (car c)))
      (cond
       ((equal eq ec)
        (unify (cdr q) (cdr c) uls ls))
       ((var? eq)
        (cond
         ((var? ec)
          (let ((a (assoc ec uls)))
            (cond
             ((null a)              
              (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
             ((equal (cadr a) eq)
              (unify (cdr q) (cdr c) uls ls))
             (t
              nil))))
         ((func? ec)
          nil)
         (t ;; const
          (let ((a (assoc eq ls)))
            (cond
             ((null a)
              (unify (cdr q) (cdr c) uls (cons (list eq ec) ls)))
             ((equal (cadr a) ec)
              (unify (cdr q) (cdr c) uls ls))
             (t 
              nil))))))
       ((func? eq)
        (cond
         ((var? ec)
          (if (func-of eq ec) nil
            (let ((a (assoc ec uls)))
              (cond
               ((null a)              
                (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
               ((equal (cadr a) eq)
                (unify (cdr q) (cdr c) uls ls))
               (t
                nil)))))
         ((func? ec)
          nil)
         (t ;; const
          (let ((f (apply-ls eq ls)))
            (if (has-var f) nil
              (if (equal (eval f) ec)
                  (unify (cdr q) (cdr c) uls ls)
                nil))))))
       (t ;; const
        (cond
         ((var? ec)
          (let ((a (assoc ec uls)))
            (cond
             ((null a)              
              (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
             ((equal (cadr a) eq)
              (unify (cdr q) (cdr c) uls ls))
             (t
              nil))))
         (t ;; func or const
          nil)))))))

(defun apply-and-eval (x ls)
  (if (var? x)
      (apply-ls x ls)
    (if (and (listp x) (func? (car x)))
        (eval (apply-ls x ls)) 
      x)))

(defun apply-ls (x ls)
  (cond
   ((null x)
    x)
   ((var? x)
    (let ((ax (assoc x ls)))
      (if (null ax) x
        (cadr ax))))
   ((atom x)
    x)
   (t
    (cons (apply-ls (car x) ls) (apply-ls (cdr x) ls)))))

    (byte-game)