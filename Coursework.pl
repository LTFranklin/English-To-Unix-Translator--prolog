main2(In,X):-
    m(In,Temp),
    sentence(Temp,[],X),
    break(X).

break([[X,Y]|Z]):-
    rule(X,[Y],Z,Out),
    write(Out).

rule([det(a),adj(very),adj(short),noun(command)],[verb(listing)],[det(the),adj(current),noun(directory)],'ls').
rule([det(the),adj(current),noun(directory)],[verb(viewed)],[det(in),adj(more),adj(fine),noun(detail)],'ls-la').
rule([det(a),noun(command)],[verb(moving)],[det(to),det(a),adj(higher),noun(directory)],'cd').
rule([det(a),noun(command)],[verb(moves)],[det(to),det(a),adj(parent),noun(directory)],'cd').
rule([det(the),noun(command)],[verb(prints)],[det(the),adj(current),noun(directory)],'pwd').
rule([det(the),noun(command)],[verb(types)],[det(the),adj(file),noun('08226.txt')],'cat 08226.txt').

/*rule(NP,V,NP2,'cd'):-
    member(noun(command),NP),
    member(verb(moving),V),
    member(det(to),NP2),
    member(adj(higher),NP2),
    member(noun(directory),NP2).
rule(NP,V,NP2,'cd'):-
    member(noun(command),NP),
    member(verb(moves),V),
    member(det(to),NP2),
    member(adj(parent),NP2),
    member(noun(directory),NP2).
rule(NP,V,NP2,'pwd'):-
    member(noun(command),NP),
    member(verb(prints),V),
    member(adj(current),NP2),
    member(noun(directory),NP2).
rule(NP,V,NP2,'cat 08226.txt'):-
    member(noun(command),NP),
    member(verb(types),V),
    member(adj(file),NP2),
    member(noun('08226.txt'),NP2).*/


sentence([],Out,Temp):-
    myRev(Out,[],Temp).

sentence(In,Out,Temp):-
    nsentence(In,Out,Temp);
    vsentence(In,Out,Temp).

nsentence([],Out,Temp):-
    sentence([],Out,Temp).
nsentence(In,Out,Temp):-
    np(In,Out,Temp).
nsentence(Rest,Out,Temp):-
    myRev(Out,[],Nout),
    vsentence(Rest,[Nout],Temp).

vsentence([],Out,Temp):-
    sentence([],Out,Temp).
vsentence(In,Out,Temp):-
    vp(In,Out,Temp).
vsentence(Rest,Out,Temp):-
    myRev(Out,[],Nout),
    sentence(Rest,[Nout],Temp).


%NP->Det->NP2
np([Word|Rest],Out,Temp):-
    isDet(Word,Output),
    np2(Rest,[Output|Out],Temp).
np([Word|Rest],Out,Temp):-
    isDet(Word,Output),
    np(Rest,[Output|Out],Temp).
%NP->NP2
np([Words],Output,Temp):-
    np2(Words,Output,Temp).
%NP->NP->PP
np([Words],Out,Temp):-
    np(Words,Out,Temp),
    pp(Words,Out,Temp).
np([Words],Out,Temp):-
    nsentence(Words,Out,Temp).

%NP2->Noun
np2([Word|Rest],Out,Temp):-
    isNoun(Word,Output),
    nsentence(Rest,[Output|Out],Temp).
%NP2->Adj->NP2
np2([Word|Rest],Out,Temp):-
    isAdj(Word,Output),
    np2(Rest,[Output|Out],Temp).
np2([Words],Out,Temp):-
    nsentence(Words,Out,Temp).

%PP->Prep->NP
pp([Word|Rest],Out,Temp):-
    isPrep(Word,Output),
    np(Rest,[Output|Out],Temp).

%VP->Verb
vp([Word|Rest],Out,Temp):-
    isVerb(Word,Output),
    vsentence(Rest,[Output|Out],Temp).
%VP->(Ad)Verb->NP
vp([Word1,Word2|Rest],Out,Temp):-
    verbAd([Word1,Word2],[],Output),
    np(Rest,[Output|Out],Temp).
%VP->(Ad)Verb
vp([Word1,Word2|Rest],Out,Temp):-
    verbAd([Word1,Word2],[],Output),
    vsentence(Rest,[Output|Out],Temp).
%VP->Verb->PP
vp([Word|Rest],Out,Temp):-
    isVerb(Word,Output),
    pp(Rest,[Output|Out],Temp).
%VP->Verb->NP
vp([Word|Rest],Out,Temp):-
    isVerb(Word,Output),
    np(Rest,[Output|Out],Temp).
vp([Words],Out,Temp):-
    vsentence(Words,Out,Temp).


verbAd([],Out,Out).
verbAd([Word|Rest],[Output|Out],Temp):-
    isVerb(Word,Output),
    verbAd(Rest,Out,Temp).
verbAd([Word|Rest],[Output|Out],Temp):-
    isAdverb(Word,Output),
    verbAd(Rest,Out,Temp).


isNoun(Word,noun(Word)):-
    noun(Word).

isPrep(Word,prep(Word)):-
    prep(Word).

isDet(Word,det(Word)):-
      det(Word).

isAdj(Word,adj(Word)):-
    adj(Word).

isVerb(Word,verb(Word)):-
    verb(Word).

isAdverb(Word,adverb(Word)):-
    adverb(Word).



noun(command).
noun(directory).
noun(detail).
noun('08226.txt').
prep(and).
det(a).
det(to).
det(in).
det(the).
verb(listing).
verb(prints).
verb(types).
verb(moves).
verb(moving).
verb(viewed).
adj(file).
adj(higher).
adj(parent).
adj(very).
adj(more).
adj(fine).
adj(short).
adj(current).
adverb(x).

myRev([],Temp,Temp).
myRev([H|Tail],Temp,OutList):-
	myRev(Tail,[H|Temp],OutList).

m(In,Out):-
    switch(In,[],Out).

switch([],Temp,Out):-
    myRev(Temp,[],Out).
switch([H|List],Temp,Out):-
    swap(H,X),
    switch(List,[X|Temp],Out).
switch([H|List],Temp,Out):-
    switch(List,[H|Temp],Out).

swap(In,H):-
    synonym([H|X]),
    member(In,X).

synonym([directory,folder]).
synonym([prints,cars,views]).


foo([[[det(a), adj(short), noun(command)], verb(listing)], det(the), adj(current), noun(directory)]).
      %[[A,B]|C]
