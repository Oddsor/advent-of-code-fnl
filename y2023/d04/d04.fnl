(local {: slurp : split : filter : remove : empty? : keep : first : sum}
       (require :utils))

(local testdata "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(local realdata (slurp :input.txt))

(fn line->card->match [xs]
  (let [xss (split "[:|]" xs)
        [cardns winningns drawns] (keep (fn [x]
                                          (icollect [k (string.gmatch x "%d+")]
                                            (tonumber k)))
                                        xss)]
    {:card (first cardns)
     :matches (length (keep (fn [d]
                              (let [matches (filter (fn [x] (= x d)) winningns)]
                                (if (empty? matches) nil d)))
                            drawns))}))

(fn match->points [x]
  (match x
    0 0
    1 1
    (where x (> x 1)) (* 2 (match->points (- x 1)))))

(fn cards->matches [text]
  (->> text
       (split "\n")
       (remove empty?)
       (keep line->card->match)))

(fn part-1 [cards->matches]
  (accumulate [x 0 _ v (ipairs (icollect [_ v (ipairs cards->matches)]
                                 (match->points (. v :matches))))]
    (+ x v)))

(print :Run :Answer)
(print "Test 1" (-> testdata cards->matches part-1))
(print "Real 1" (-> realdata cards->matches part-1))

(fn part-2 [cards->matches]
  (let [xs (icollect [k _ (pairs cards->matches)] k)
        max-card (math.max (table.unpack xs))
        acc (collect [k _ (pairs cards->matches)] k 1)]
    (each [current-card (ipairs xs)]
      (let [cardmatches (. (. cards->matches current-card) :matches)
            cards-to-increment (fcollect [i (+ current-card 1) (math.min max-card
                                                                         (+ current-card
                                                                            cardmatches))]
                                 i)]
        (each [_ card-num (ipairs cards-to-increment)]
          (tset acc card-num (+ (. acc card-num) (. acc current-card))))))
    (sum acc)))

(print "Test 2" (-> testdata cards->matches part-2))
(print "Real 2" (-> realdata cards->matches part-2))

