% Da elBedaya
s(T) --> wff_expr(T, 0).

% 3araf tarteeb precedence levels
prec('&', 1).
prec('|', 2).
prec('=>', 3).
prec('<=>', 4).

% parse b tarteebprecedence handling
wff_expr(T, P) -->
    wff_unary(U),
    wff_expr_rest(U, P, T).

wff_expr_rest(L, P, T) -->
    operator(Op), { prec(Op, Q), Q > P },
    wff_expr(R, Q),
    { combine(Op, L, R, NewL) },
    wff_expr_rest(NewL, P, T).
wff_expr_rest(T, _, T) --> [].

% Parse elunary operators
wff_unary(neg(SubTree)) --> ['~'], wff_expr(SubTree, 0).
wff_unary(quant(Quant, Var, SubTree)) --> 
    quantifier(Quant), 
    variable(Var), 
    ['('], wff_expr(SubTree, 0), [')'].
wff_unary(paren(SubTree)) --> ['('], wff_expr(SubTree, 0), [')'].
wff_unary(atom(Predicate, Terms)) --> 
    predicate(Predicate), 
    ['('], terms(Terms), [')'].

% 3araf elrules for parsing atoms
predicate(P) --> [P], { member(P, ['P', 'Q', 'R', 'S', 'T']) }.

% 3araf elrules for parsing terms
terms(T) --> term_list(T).
term_list([T]) --> term(T).
term_list([T|Ts]) --> term(T), ['*'], term_list(Ts).

% 3araf elrules le parsing elterms
term(variable(V)) --> [V], { member(V, ['x', 'y', 'z', 'w', 't']) }.
term(constant(C)) --> [C], { member(C, ['J', 'K', 'L', 'M', 'N']) }.

%3araf elparsing variables and constants
variable(V) --> [V], { member(V, ['x', 'y', 'z', 'w', 't']) }.
constant(C) --> [C], { member(C, ['J', 'K', 'L', 'M', 'N']) }.

% 3araf elquantifiers
quantifier('A') --> ['A'].
quantifier('E') --> ['E'].

% 3araf eloperators
operator(Op) --> [Op], { member(Op, ['&', '|', '=>', '<=>']) }.

% gama3 based 3al operator
combine('&', Left, Right, conj(Left, Right)).
combine('|', Left, Right, disj(Left, Right)).
combine('=>', Left, Right, impl(Left, Right)).
combine('<=>', Left, Right, bicon(Left, Right)).
