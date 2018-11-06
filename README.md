```
$ echo '
(let [square (fn [x]
               (* x x))
      apply-and-increment (fn [f x]
                            (+ 1 (f x)))]
  (apply-and-increment square 4))
' | cargo run
25
```
