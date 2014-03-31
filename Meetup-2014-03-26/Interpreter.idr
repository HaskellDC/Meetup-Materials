module Interpreter

%default total

infixr 6 ==>

data Ty = BOOL | (==>) Ty Ty

%name Ty t,u

typeOf : Ty -> Type
typeOf BOOL = Bool
typeOf (t ==> u) = typeOf t -> typeOf u

using (n : Nat, ctxt : Vect n Ty)
  data Term : Vect n Ty -> Ty -> Type where
    T : Term ctxt BOOL
    F : Term ctxt BOOL
    IF : Term ctxt BOOL -> Term ctxt t -> Term ctxt t -> Term ctxt t

    App : Term ctxt (t ==> u) -> Term ctxt t -> Term ctxt u
    Lam : Term (t::ctxt) u -> Term ctxt (t ==> u)
    Var : (i : Fin n) -> Term ctxt (index i ctxt)

  %name Term tm, tm', tm''

  not : Term ctxt (BOOL ==> BOOL)
  not = Lam $ IF (Var 0) F T

  choose : Term ctxt (BOOL ==> BOOL ==> BOOL ==> BOOL)
  choose = Lam $ Lam $ Lam $ IF (Var 2) (Var 1) (Var 0)

  not' : Term ctxt (BOOL ==> BOOL)
  not' = Lam (App (App (App choose (Var 0)) F) T)

  data Env : Vect n Ty -> Type where
    Nil : Env []
    (::) : typeOf t -> Env ctxt -> Env (t::ctxt)

  lookup : (i : Fin n) -> (env : Env ctxt) -> typeOf (index i ctxt)
  lookup fZ (x :: y) = x
  lookup (fS x) (y :: z) = lookup x z

  eval : Term ctxt t -> Env ctxt -> typeOf t
  eval T env = True
  eval F env = False
  eval (IF tm tm' tm'') env = if eval tm env then eval tm' env else eval tm'' env
  eval (App f x) env = (eval f env) (eval x env)
  eval (Lam tm) env = \ x => eval tm (x :: env)
  eval (Var i) env = lookup i env


  
