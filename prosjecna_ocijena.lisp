
; Funkcija za unos ocjena, potrebno stisnuti enter nakon svake ocjene
(defun unos-ocjena ()
  (format *query-io* "Unesite ocjene odvojene razmakom - pritisnite Enter (završite unos sa 'kraj'): ")
  (let ((ocjene '()) ; ocjene su tipa list
        (unos ""))
    (loop
     (setf unos (read-line))
     (when (string= unos "kraj")
       (return))
     (let ((ocjena (parse-integer unos :junk-allowed t)))
       (if (and ocjena (<= 1 ocjena 5))
           (push ocjena ocjene)
           (format *query-io* "Molimo unesite ocjenu u rasponu od 1 do 5. Ponovite unos.~%")))) ;ukoliko je ocjena veca od 5 onda upozorava i dozvoljava ponovni unos podataka
    ocjene))

; Izracunavanje prosjecne ocjene ucenika
(defun izracunaj-prosjek (ocjene)
  (if ocjene
      (if (member 1 ocjene) ; Ukoliko postoji ocjena 1 onda je ucenik pao i samim tim zavrsio razred/polugodiste sa ocjenom 1
          1
          (/ (apply #'+ ocjene) (length ocjene)))
      0))

; Izracunavanje najvise ocjene ucenika
(defun najvisa-ocjena (ocjene)
  (if ocjene
      (apply #'max ocjene)
      0))

; Izracunavanje najnize ocjene ucenika
(defun najniza-ocjena (ocjene)
  (if ocjene
      (apply #'min ocjene)
      0))

; Funckija vraca najcesce unijetu ocjenu
(defun najcesca-ocjena (ocjene)
  (if ocjene
      (car (maximally-occurring-element ocjene))
      nil))

; Funkcija koja pretrazuje najcesce unesenu ocjenu i vraca funkciji najcesca-ocjena
(defun maximally-occurring-element (lst)
  (if (endp lst)
      nil
      (let* ((counts (make-hash-table)) ;make-hash-table je funkcija koja omogucava brz pristup podacima kroz key-value parove
             (max-elem (car lst))
             (max-count 1))
        (dolist (elem lst)
          (let ((count (gethash elem counts 0))) ;gethash funkcija vraca element koji ima vrijednost counts-a a ukoliko je nil onda ce vratiti nulti element
            (setf (gethash elem counts) (1+ count))
            (when (> count max-count)
              (setf max-elem elem
                    max-count count))))
        (list max-elem max-count))))


; main funkcija koja poziva funkciju unos-ocjene koja vraca podatke u ocjene. Nakon zavrsenog unosa i ispunjenog uslova za kraj funkcija nastavlja sa sledecim linijama koda koje pozivaju funkcije za dobijanje zeljenog rezultata oko ocjena i formatiraju ih sa ispisom u konzoli 
(defun main ()
  (let ((ocjene (unos-ocjena)))
    (format t "Prosjek ocjena: ~2,2f~%" (izracunaj-prosjek ocjene))
    (format t "Najviša ocjena: ~a~%" (najvisa-ocjena ocjene))
    (format t "Najniža ocjena: ~a~%" (najniza-ocjena ocjene))
    (format t "Najčešća ocjena: ~a~%" (najcesca-ocjena ocjene))))

; Pokretanje koda
(main)