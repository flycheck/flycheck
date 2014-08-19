Module SyntaxError.

  Fixpoint evenb (n:nat) : bool :=
    match n with
      | O => true
       S O => false
      | S (S n') => evenb n'
    end.

End SyntaxError.
