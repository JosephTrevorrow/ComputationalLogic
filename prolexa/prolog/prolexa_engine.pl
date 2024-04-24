%:- module(prolexa_engine,
%	[
%		prove_question/3,		% main question-answering engine
%		explain_question/3,		% extended version that constructs a proof tree
%		known_rule/2,			% test if a rule can be deduced from stored rules
%		all_rules/1,			% collect all stored rules 
%		all_answers/2,			% everything that can be proved about a particular Proper Noun
%	]).

:- consult(library).

%%% SANITY NOTES
%
% The name of a predicate includes its name and arity, e.g. pred/3
% A cut (!) is used to prevent backtracking, ensuring that once a rule is found, the rule will continue
%	until an atom is reached, then backtrack to the next rule.
%

%%% INITIAL FEATURES NOTES

%% HOW PROVE_QUESTION /3 WORKS
% findall(R,prolexa:stored_rule(SessionId,R),Rulebase) - 
%	find all rules in the session given the sessionID and stores in Rulebase
% AND
% (prove_rb(Query,Rulebase) -
%	prove the query given the rulebase. If it can, then the code following -> is executed
% IF PROVED
% transform(Query,Clauses) -
%	transform the query into clauses
% phrase(sentence(Clauses),AnswerAtomList) -
%	convert the list of clauses into a list of atoms that form the answer
% atomics_to_string(AnswerAtomList," ",Answer) -
%	convert the list of atoms into a string
% ELSE
% Answer = 'Sorry, I don\'t think this is the case' - (does the obvious)
% )

%% HOW EXPLAIN_QUESTION /3 WORKS
% The same as prove_question/3, but with an additional argument, Proof, which is 
% 	an accumulator for proofs to generate a proof tree

%% HOW PSTEP2MESSAGE /2 WORKS
% it just goes through each element in the list and prints it

%% HOW KNOWN_RULE /2 WORKS
% findall(R,prolexa:stored_rule(SessionId,R),Rulebase) -
%	find all rules in the session given the sessionID and stores in Rulebase
% AND
% try((numbervars(Rule,0,_) -


%% HOW add_body_to_rulebase WORKS
% if the body is a conjunction, then it adds the first part of the 
% conjunction to the Rulebase
% it uses a cut to prevent backtracking, i.e. there are multiple rules
% (2 definitions of the func) - one for conjunctions and one for single atoms
% the first rule is recursively caled for each element of the pair A,B (all A's then all B's)
% When it reaches the end of the A's (where a is not a pair, its an atom)
% (to start its backtracking through the A list) it then calls the
% seconary rule, which adds the rule to the rulebase as a fact (A:-true)

%% HOW PROVE_RB /4 WORKS
% 1. if the query is true, then it returns the proof accumulator

% 2. if the query is a conjunction (A,B), then it finds a clause A:-C in the rulebase, appends C to B
% and then recursively calls itself with the new query (C,B) and the rulebase

% 3. if the query is a single atom, then it finds a clause A:-B in the rulebase, and 
% 	recursively calls itself with B as the new query. This is the base case for the recursion?

% 4. top-level version that ignores proof


%%% Main question-answering engine adapted from nl_shell.pl %%%

prove_question(Query,SessionId,Answer):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = 'Sorry, I don\'t think this is the case'
	).	

% two-argument version that can be used in maplist/3 (see all_answers/2)
prove_question(Query,Answer):-
	findall(R,prolexa:stored_rule(_SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = ""
	).	


%%% Extended version of prove_question/3 that constructs a proof tree %%%
explain_question(Query,SessionId,Answer):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase,[],Proof) ->
		maplist(pstep2message,Proof,Msg),
		phrase(sentence1([(Query:-true)]),L),
		atomic_list_concat([therefore|L]," ",Last),
		append(Msg,[Last],Messages),
		atomic_list_concat(Messages,"; ",Answer)
	; Answer = 'Sorry, I don\'t think this is the case'
	).

% convert proof step to message
pstep2message(p(_,Rule),Message):-
	rule2message(Rule,Message).
pstep2message(n(Fact),Message):-
	rule2message([(Fact:-true)],FM),
	atomic_list_concat(['It is not known that',FM]," ",Message).


%%% test if a rule can be deduced from stored rules %%%
known_rule([Rule],SessionId):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	try((numbervars(Rule,0,_),
	     Rule=(H:-B),
	     add_body_to_rulebase(B,Rulebase,RB2),
	     prove_rb(H,RB2)
	   )).

% add body to rulebase by recursively looking at every pair, to extract atoms 
% 	and add them to the rulebase as facts
add_body_to_rulebase((A,B),Rs0,Rs):-!,
	add_body_to_rulebase(A,Rs0,Rs1),
	add_body_to_rulebase(B,Rs1,Rs).
add_body_to_rulebase(A,Rs0,[[(A:-true)]|Rs0]).


%%% meta-interpreter that constructs proofs %%%

% 3d argument is accumulator for proofs


%%%% TERRIBLE PROVE_RB CHANGE ATTEMPTING NEGATION FIRST TRY %%%%
% Case for when query is true
%prove_rb(true,_Rulebase,P,P):-!.
% Case for when query is conjunction e.g. 'animal(tweety),can_fly(tweety)', this might return something
% like 'can_fly(tweety)' if it found the first part as true (conj_append gets rid of the true part)
% 	in this example the single atom rule would be called. conj_append sees true as the identity, so it combines the rule with true, which is just the rule
%prove_rb((A,B),Rulebase,P0,P):-!,
%	find_clause((A:-C),Rule,Rulebase),
%	conj_append(C,B,D),
%    prove_rb(D,Rulebase,[p((A,B),Rule)|P0],P).
% Case for when query is single atom e.g. 'bird(tweety)'
%prove_rb(A,Rulebase,P0,P):-
%    find_clause((A:-B),Rule,Rulebase),
%	prove_rb(B,Rulebase,[p(A,Rule)|P0],P).
% top-level version that ignores proof
%prove_rb(Q,RB):-
%	prove_rb(Q,RB,[],_P).




% 3d argument is accumulator for proofs
prove_rb(true,_Rulebase,P,P):-!.
prove_rb((A,B),Rulebase,P0,P):-!,
	find_clause((A:-C),Rule,Rulebase),
	conj_append(C,B,D),
    prove_rb(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_rb(A,Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
	prove_rb(B,Rulebase,[p(A,Rule)|P0],P).
	
% This is our negation function. It is the same as the above, but with the not function added
% 	which is the case for when query is single atom e.g. 'bird(tweety)'
prove_rb(not B, Rulebase, P0, P):- 
	find_clause((A:-B), Rule, Rulebase),
	prove_rb(not A, Rulebase, [p(not B, Rule)|P0], P).

% top-level version that ignores proof
prove_rb(Q,RB):-
	prove_rb(Q,RB,[],_P).

%%% Utilities from nl_shell.pl %%%

find_clause(Clause,Rule,[Rule|_Rules]):-
	copy_term(Rule,[Clause]).	% do not instantiate Rule
find_clause(Clause,Rule,[_Rule|Rules]):-
	find_clause(Clause,Rule,Rules).

% transform instantiated, possibly conjunctive, query to list of clauses
transform((A,B),[(A:-true)|Rest]):-!,
    transform(B,Rest).
transform(A,[(A:-true)]).


%%% Two more commands: all_rules/1 and all_answers/2

% collect all stored rules 
all_rules(Answer):-
	findall(R,prolexa:stored_rule(_ID,R),Rules),
	maplist(rule2message,Rules,Messages),
	( Messages=[] -> Answer = "I know nothing"
	; otherwise -> atomic_list_concat(Messages,". ",Answer)
	).

% convert rule to sentence (string)
rule2message(Rule,Message):-
	phrase(sentence1(Rule),Sentence),
	atomics_to_string(Sentence," ",Message).

% collect everything that can be proved about a particular Proper Noun
all_answers(PN, Answer):-
    findall(Q, (pred(P,1,_), (Q=..[P, PN])), Queries), % Include negated queries
    maplist(prove_question, Queries, Msg),
    delete(Msg, "", Messages),
    (Messages=[] -> atomic_list_concat(['I know nothing about', PN], ' ', Answer);
     otherwise -> atomic_list_concat(Messages, ". ", Answer)
    ).

