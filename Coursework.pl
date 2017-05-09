/*main([],Temp):-
    myRev(Temp,[],Out),
    write(Out).
main([Word|Input],Temp):-
    isNoun(Word,Output),
    main(Input,[Output|Temp]).
main([Word|Input],Temp):-
    isDet(Word,Output),
    main(Input,[Output|Temp]).
main([Word|Input],Temp):-
    isAdj(Word,Output),
    main(Input,[Output|Temp]).
main([Word|Input],Temp):-
    isVerb(Word,Output),
    main(Input,[Output|Temp]).
main([_|Input],Output):-
    main(Input,[error|Output]).*/



main2(In,X):-
    m(In,Temp),
    sentence(Temp,[],X).


sentence([],Out,Temp):-
    myRev(Out,[],Temp),
    write(Temp).
sentence(In,Out,_):-
    np(In,Out).
sentence(In,Out,_):-
    vp(In,Out).



np([Word|Rest],Out):-
    isDet(Word,Output),
    np2(Rest,[Output|Out]).
np([Word|Rest],Out):-
    isDet(Word,Output),
    np(Rest,[Output|Out]).
np([Words],Output):-
    np2(Words,Output).
np([Words],Out):-
    np(Words,Out),
    pp(Words,Out).
np([Words],Out):-
    sentence(Words,Out,_).

np2([Word|Rest],Out):-
    isNoun(Word,Output),
    sentence(Rest,[Output|Out],_).
np2([Word|Rest],Out):-
    isAdj(Word,Output),
    np2(Rest,[Output|Out]).
np2([Words],Out):-
    sentence(Words,Out,_).

pp([Word|Rest],Out):-
    isPrep(Word,Output),
    np(Rest,[Output|Out]).

vp([Word|Rest],Out):-
    isVerb(Word,Output),
    sentence(Rest,[Output|Out],_).
vp([Word1,Word2|Rest],Out):-
    verbAd([Word1,Word2],[],Output),
    np(Rest,[Output|Out]).
vp([Word1,Word2|Rest],Out):-
    verbAd([Word1,Word2],[],Output),
    sentence(Rest,[Output|Out],_).
vp([Word|Rest],Out):-
    isVerb(Word,Output),
    pp(Rest,[Output|Out]).
vp([Word|Rest],Out):-
    isVerb(Word,Output),
    np(Rest,[Output|Out]).
vp([Words],Out):-
    sentence(Words,Out,_).


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
noun(file).
noun(detail).
noun('08226txt').
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

/*myLength([],Out,Out).
myLength([_|List],Length,Out):-
	Temp is Length+1,
	myLength(List,Temp,Out).

trim(Num,List,Out):-
    traverse(Num,0,List,Out).
traverse(Num,Num,List,Out):-
    append(List,[],Out).
traverse(Num,Count,[_|List],Out):-
    Count2 is Count + 1,
    traverse(Num,Count2,List,Out). */




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
