Module Error.

  Fixpoint evenb (n:nat) : bool :=
    match n with
      | O => true
      | S O => false
      | S (S n') => 1
    end.

End Error.
