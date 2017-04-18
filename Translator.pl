main([],Temp):-
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
    main(Input,[error|Output]).

main2(In,Out):-
    np(In,Out).

np([],Out).
np([Word|Rest],Out):-
    isDet(Word,Output),
    np2(Rest,[Output|Out]).
np([Words],Output):-
    np2(Words,Output).

np([Words],Out):-
    np(Words,Out),
    pp(Words,Out).

np2([],Out).
np2([Word],Out):-
    isNoun(Word,Output),
    np2([],[Output|Out]).
np2([Word|Rest],Out):-
    isAdj(Word,Output),
    np2(Rest,[Output|Out]).

pp([Word|Rest],Out):-
    isPrep(Word,Output),
    np(Rest,[Output|Out]).

vp([Word],Output):-
    isVerb(Word,Output).
vp([Word1,Word2|Rest],Out):-
    verbAd([Word1,Word2],Output),
    np(Rest,[Output|Out]).
vp([Words],Output):-
    verbAd(Words,Output).

verbAd([],Out).
verbAd([Word|Rest],[Output|Out]):-
    isVerb(Word,Output),
    verbAd(Rest,Out).
verbAd([Word|Rest],[Output|Out]):-
    isAdverb(Word,Output),
    verbAd(Rest,Out).


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
%noun(file).
%noun(detail).
%noun(08226txt).
prep(and).
det(a).
%det(to).
%det(in).
det(the).
verb(listing).
%verb(prints).
%verb(types).
%verb(moves).
%verb(moving).
%verb(viewed).
%adj(higher).
%adj(parent).
adj(very).
%adj(more).
%adj(fine).
adj(short).
adj(current).
adverb(x).

myRev([],Temp,Temp).
myRev([H|Tail],Temp,OutList):-
	myRev(Tail,[H|Temp],OutList).



