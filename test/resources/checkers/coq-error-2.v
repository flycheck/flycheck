Section A_declared.
  Variables (A : Set) (P Q : A -> Prop) (R : A -> A -> Prop).
  Theorem all_perm : (forall a b : A, R a b) -> forall a b : A, R b a.
  Proof ((fun (R : A -> A -> Prop) (a:A) (b:A) => R b a) R).
End Section A.
