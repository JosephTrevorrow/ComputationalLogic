# Extended Prolexa - Computational Logic Coursework 2024

*Abstract* \
Prolexa, and its sibling Prolexa Plus, use Python and Prolog to understand natural language. Utilising Simply Logical by Flach and Sokol as well as Prolexa's  Definite Clause Grammar and  Meta-Interpreter provided, I extend Prolexa Plus to understand concepts of Negation and Existential Quanitification.

Accompanied by this report is a `assignment_notebook.ipynb`, that contains runnable test cases, and an interactive shell for this project.

## Background

### The Definite Clause Grammar

Definite Clause Grammars are used in Prolog to represent natural language. Prolexa utilises these in full, and `prolexa_grammar.pl` defines these. `sentence1` is of key importance,as it defines the structure of the natural language allowed to be parsed, alongside its appropriate logical representation of each rule

### The `prove_rb` Meta-Interpreter

`prove_rb` is the primary method that Prolexa uses to reason about its knowledge store, described above. The interpreter has multiple use cases, for a single atom `A`, `prove_rb` finds a clause `A:-B` in the rulebase and recursively calls itself with `B` as the new query. If `B` is true in the knowledge store, then the program moves to the final case (returning the accumulator) . For a conjunction `A,B`, the program finds a clause `A:-C` in the rulebase, appending `C` to `B`. Following this, the rule recursively calls itself with a new conjunction `C,B`.

## New Functionality

### Negation

`Every teacher is happy. Donald is not happy. Therefore, Donald is not a teacher.`

*The implementation is as follows*

+ Decide how to represent negation (Either as L:-False or not L:-True)
+ Allow Prolexa to understand that it should parse in something can be _not_ something
  - Change the DCG so that sentence1 allows for when not `L:-True`, by adding a negated phrases to all needed rules. For example, both instances of `sentence1` for single properties, and `H::-B` rules.
+ Modify the meta-interpreter so that it can perform reasoning on `not L:-True` rules.

There are two key different modifications to be completed to allow for negation. Firstly the DCG must be modified to parse in false statements to the rule engine. I add 'not' as a DCG property that can exist on a noun, verbs and all other definitions that make up a sentence, and add a secondary definition of what a sentence is to allow that sentence to exist where the semantic representation `L` rules are preceded with `not`. As a baseline, here is the barebones implementation that allows parsing. I select `not` rather than defining something as false as Prolexa works under the closed world assumption. When using `not`, something will only be false if Prolog fails to prove the opposite `Is Joseph not happy` will attempt to prove `Is Joseph happy`. By using this, Prolog can express everything through positive rules with `not` included, which makes reasoning easier. This is because by reasoning when asking about every negated phrase, `not` allows for dynamic changes in the rules to explore alternative paths in the proof tree, where things may not always be false.

The use of `false` would be technically faster for individual queries, but as Prolog uses the closed world assumption it cannot be applicable to Prolexa: The universe is always expanding as the user inputs new data. By stating something as explicitly false, each time a new interaction with a false fact is parsed, an unnecessary amount of computation would need to be used to add every new negative fact. For this reason, negation as failure must be used in the form of `not` which is a predicate that will only say something is false if it fails to prove it. This implicit representation of falsity therefore makes more sense for this particular use case.

The DCG is modified such that it can understand negated sentences:

```prolog
sentence1([(not L:-true)]) --> proper_noun(N,X),verb_phrase(N, not X=>L).
sentence1([(not H:-B)]) --> determiner(N,M1,M2,[(not H: B)]),noun(N,M1),verb_phrase(N,not M2).
```

This is not enough, and each predicate that makes up a sentence must also be modified. For example:

```prolog
question1(Q) --> [who],verb_phrase(s,_X=>Q).
question1(Q) --> [who],verb_phrase(s,[(not _X=>Q)]).
question1(Q) --> [is], proper_noun(N,X),property(N,X=>Q).
question1(Q) --> [is], proper_noun(N,X),property(N,not X=>Q).

%% Negated verb phrases
verb_phrase(s,not M) --> [is], [not], property(s,M).
verb_phrase(p,not M) --> [are], [not], property(p,M).
verb_phrase(N,not M) --> [does], [not], iverb(N,M).
verb_phrase(s,not M) --> [can], [not], iverb(s, M).
```

For Prolexa to reason about negation we need to add an extra case to `prove_rb` that uses the `not` operator to automatically give negation to a predicate.

```prolog
% Same as the above cases, but with the not function added which is the case for when query is single atom e.g. 'bird(tweety)'
prove_rb(not B, Rulebase, P0, P):- 
    find_clause((A:-B), Rule, Rulebase),
    prove_rb(not A, Rulebase, [p(not B, Rule)|P0], P).
```

Added negation is also needed in reasoning to prove questions, so that the engine can reason about negative queries in questions. The same is done to `explain_question`:

```prolog
% two-argument version that can be used in maplist/3 (see all_answers/2)
prove_question(Query,Answer):-
	findall(R,prolexa:stored_rule(_SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; prove_rb(not Query,Rulebase) ->
		transform(not Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = ""
	).	
```

### Negation Testing

*Test 1: basic understanding*

```txt
USR: Joseph is not happy
PLX: I will remember that joseph is not happy
USR: Spill the beans
PLX: Every human is mortal. peter is human. joseph is human. joseph is not happy
USR: explain why joseph is not happy
PLX: joseph is not happy; therefore joseph is not happy
```

*Test 2: deeper reasoning*

```txt
USR: Every teacher is happy
PLX: I will remember every teacher is happy
USR: Joseph is not happy
PLX: I will remember that joseph is not happy
USR: explain why joseph is not a teacher
PLX: joseph is not happy' every teacher is happy; therefore joseph is not a teacher
USR: spill the beans
PLX: every human is mortal. peter is human. joseph is human. every teacher is happy.
```

## Existential Quantification:
`Some humans are geniuses. Geniuses win prizes. Therefore, some humans win prizes.`

*The implementation is as follows*

+ Prolexa uses determiners to understand "every" and "all" clauses, such as "all x are y" or "every a is b". Therefore, add a new determiner to parse "some" clauses logically
+ Modify the engine so that it can be stored as a rule in a similar format as "every" and "all" logically
+ Modify `prove_rb` to allow for reasoning about rules that follow the "some" representation

Included with the Prolexa Grammar in the original repository is the below Skolemisation function. It represents a rule `p`, and that if two individual rules are true, then that response is correctly matched. Skolemisation is a function of predicate logic where two new predicates are created to eliminate logical $\exists$ from (See Flach and Sokol, sections 11.1, 2.5). The process converts all variables to constants, and all new constants to new functors. Because the rule will only parse variables (as that is all Prolexa can identify), the function simply converts the two to be a list of two true facts within the knowledge store.

```prolog
determiner(p, sk=>H1, sk=>H2, [(H1:-true),(H2 :- true)]) -->[some].
```

Limitations of this method is that it cannot parse "Some geniuses win prizes", only some geniuses win. To allow this, `property` and `verb_phrase` can be modified, so that the stored rule results in the property, rather than the verb being treated as the truth ("geniuses win prizes" translates to `[(genius:-true),(prizes:-true)]`). However, this would result in the loss of the verb, so, it could be possible to treat "win prizes" as a predicate in itself, however this would be a lazy fix and would result in prizes not being a property in itself. A better fix would be to use transitive verbs, rather than just generic and independent verbs. This would require storing the verb as well within the rule, which would be possible but reqiure an overhaul of how Prolexa understands rules.

I also made a basic change to sentence parsing, so that the new determiner rule is accounted for. The rule here is not the same as the deteminer, as is with other rules, because the query is with the individual query (H1,H2) being true. If `sentence1` was formatted as `sentence1([(H1:-true),(H2:-true)])` this would not be understood, as there is no query being proven, there is a list of two seperate queries.

```prolog
sentence1([(H1,H2):-true]) --> determiner(N,M1,M2,[(H1:-true),(H2:-true)]),noun(N,M1),verb_phrase(N,M2).
```

However, some issues arose,  which led to existential quantification (EQ) rules being repeated 4 times within the rule base.

```txt
USR: some huamns are geniuses
* utterance(some humans are geniuses)
* rule([([human(sk),genuis(sk)]:-true)])
* answer(I will remember that some humans are geniuses)
* rule([([human(sk),genuis(sk)]:-true)])
* answer(I will remember that some humans are geniuses)
* rule([(human(sk):-true),(genuis(sk):-true)])
* answer(I will remember that some humans are geniuses)
* rule([(human(sk):-true),(genuis(sk):-true)])
* answer(I will remember that some humans are geniuses)
* answer(I heard you say,  some humans are geniuses , could you rephrase that please?)
PLX: I will remember some humans are geniuses
USR: spill the beans
PLX: every human is mortal. peter is human. joseph is human. some humans are geniuses. some humans are geniuses. some humans are geniuses. some humans are geniuses.
```

On closer inspection, each rule is different, the first and second accounts for the list, and the new determiner. The third and fourth account for when both are true for the other definition of the rule. However, by storing these rules no reasoning is effected and therefore no changes will be made.

For Reasoning, `question1` must be modified to accept "do some" and "are some", rather than just using a singular Q. The process of skolemisation is such that to explain a question, we must prove that both can be true at the same time ("some humans are geniuses" translates to `[(humans:-true),(genius:-true)]` as one rule being a list of 2 rules) to prove this, the question should parse in both 

```prolog
question1((Q1,Q2)) --> [are],[some],noun(p,sk=>Q1), property(p,sk=>Q2). % from original repo 
question1((Q1,Q2)) --> [do],[some],noun(p,sk=>Q1), verb_phrase(p,sk=>Q2).

command(g(explain_question([Q1,Q2],_,Answer),Answer)) -->[explain,why],sentence1([(Q1:-true),(Q2:-true)]).
```

EQ reasoning is described in Simply Logical (section 7.3). The meta-interpreter is modified such that `prove_rb` splits the list of two clauses into two singulars, and runs `prove_rb` again on each. Importantly to allow for explanations, Prolexa's `prove_rb` function uses `P0` and `P` to add rules to an accumulator to be used with `explain_question`. Therefore, the EQ case must account for the fact that the final accumulator for clause `A` must be appended to the start of the accumulator for clause `B`.

```prolog
% Added for existential quantification from end of 7.3
prove_rb((A,B), Rulebase, P0, P):-!,
    prove_rb(A,Rulebase, P0, P1),
    prove_rb(B,Rulebase, P1, P).
```

To complete the reasoning, the two rules must then be able to be picked from the knowledge store. To pick rules from the knowledge store, both Prolexa and the meta-interpreter described in Simply Logical (7.3) use the exact `find_clause` predicate. I take from Simply Logical and expand `find_clause` to include the `copy_element` case, which instead of finding a term in the knowledge store, finds an element of a term (which is useful in EQ, as terms are represented as a list of two elements).

```prolog
% See 7.3 - Copying elements of a rule, so that sk's are not only 1 undecipherable list
find_clause(Clause,Rule, [Rule|_Rules]):-
	copy_element(Clause, Rule).
```

Unlike negation, explanation and proving functions that call `prove_rb` are not modified. this is because even though the query is abnormal, all reasoning about the list is done within `prove_rb` as described above.

### EQ Testing

*Test 1: basic understanding with stored rules*

```txt
% Note: in this case prolexa.pl: stored_rule(1,[(human(sk):-true),(happy(sk):-true)]).
USR: are some humans happy
PLX: some humans are happy
USR: explain why some humans are happy
PLX: some humans are happy; therefore some humans are happy
```

*Test 2: basic understanding with input rules*

```txt
USR: are some humans happy
PLX: sorry, I don't think this is the case 
USR: some humans are happy
PLX: I will remember that some humans are happy
USR: spill the beans
PLX: every human is mortal. peter is human. joseph is human. some humans are happy. some humans are happy. some humans are happy.
USR: are some humans happy
PLX: some humans are happy
USR: explain why some humans are happy
PLX: some humans are happy; therefore some humans are happy
```

**Note: Unformatted output is shown, as repeated rules are needed for full understanding of limitations**

*Test 3: deeper reasoning with stored rules*

```txt
Hello! I'm ProlexaPlus! Tell me anything, ask me anything.
> spill the beans
* utterance(spill the beans)
* goal(all_rules(_52810))
* answer(every human is mortal. peter is human. some humans are happy. some humans are birds. every bird can flies)
every human is mortal. peter is human. some humans are happy. some humans are birds. every bird can flies
> do some humans fly
* utterance(do some humans fly)
* query((human(sk),fly(sk)))
* answer(some humans fly)
b'some humans fly'
> explain why some humans fly
* utterance(explain why some humans fly)
* goal(explain_question([human(sk),fly(sk)],_11928,_11700))
* answer(some humans are birds; every bird can flies; some humans are happy; therefore some humans fly)
some humans are birds; every bird can flies; some humans are happy; therefore some humans fly
```

*Test 4: deeper reasoning with input rules* 

```txt
Hello! I'm ProlexaPlus! Tell me anything, ask me anything.
> every genius wins
* utterance(every genius wins)
* rule([(win(_54408):-genuis(_54408))])
* answer(I will remember that every genius wins)
* rule([(win(_56216):-genuis(_56216))])
* answer(I already knew that every genius wins)
* answer(I heard you say,  every genius wins , could you rephrase that please?)
I will remember that every genius wins
> some humans are geniuses
* utterance(some humans are geniuses)
* rule([([human(sk),genuis(sk)]:-true)])
* answer(I will remember that some humans are geniuses)
* rule([([human(sk),genuis(sk)]:-true)])
* answer(I will remember that some humans are geniuses)
* rule([(human(sk):-true),(genuis(sk):-true)])
* answer(I will remember that some humans are geniuses)
* rule([(human(sk):-true),(genuis(sk):-true)])
* answer(I will remember that some humans are geniuses)
* answer(I heard you say,  some humans are geniuses , could you rephrase that please?)
I will remember that some humans are geniuses
> do some humans win
* utterance(do some humans win)
* query((human(sk),win(sk)))
* answer(some humans win)
b'some humans win'
> explain why some humans win
* utterance(explain why some humans win)
* goal(explain_question([human(sk),win(sk)],_20740,_20512))
* answer(some humans are geniuses; every genius can wins; some humans are happy; therefore some humans win)
some humans are geniuses; every genius can wins; some humans are happy; therefore some humans win
> spill the beans
* utterance(spill the beans)
* goal(all_rules(_29318))
* answer(every human is mortal. peter is human. some humans are happy. some humans are birds. every bird can flies. every genius can wins. some humans are geniuses. some humans are geniuses. some humans are geniuses. some humans are geniuses)
every human is mortal. peter is human. some humans are happy. some humans are birds. every bird can flies. every genius can wins. some humans are geniuses. some humans are geniuses. some humans are geniuses. some humans are geniuses
```