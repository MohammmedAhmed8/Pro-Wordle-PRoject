main:- 
	writeln("Welcome to Pro-Wordle!"),
	writeln("----------------------"),nl,
	build_kb,
	play.
	
build_kb:-
	writeln("Please enter a word and its category on separate lines:"),
	input(W), W \== done, input(C), C \== done,
	assert(word(W,C)),
	build_kb.
build_kb:-
	( current_predicate(word/2), nl,nl,writeln("Done building the words database..."),nl,nl );
	( \+ current_predicate(word/2), build_kb ).
	
play:-
	categories(Categories),
	write("The available categories are: "), writeln(Categories), 
	select_category(Category), 
	select_length(Length,Category), 
	Guesses is Length + 1,
	write("Game started. You have "),write(Guesses), writeln(" guesses."),nl,
	get_target(Length,Category,Target),
	enter_new(Word,Length),
	game_sequence(Word,Target,Guesses,Length).

categories(L):-
	setof(Category, is_category(Category), L).
	
is_category(C):-
	word(_,C).

select_category(Cat):-
	writeln('Choose a category:'),
	input(X),
	(is_category(X) , Cat = X , nl ; 
	writeln('This category does not exist.'), nl, select_category(Cat)).
	
select_length(Length,Category):-
	writeln("Choose a length:"),
	input(X),
	( ( ( ( ( \+ integer(X); X =< 0), writeln("Enter a positive integer number."), nl );
	( \+ pick_word(_, X, Category), writeln("There are no words of this length.") ) ),
	select_length(Length, Category) );
	( pick_word(_, X, Category), Length = X) ).

pick_word(W, L, C):-
	available_length(L),
	word(W, C),
	string_length(W, L).
	 
get_target(Length, Category, X):-
	setof(Word, pick_word(Word, Length, Category), Set),
	random_member(X, Set).

enter_new(Word, Length):-
	write("Enter a word composed of "), write(Length), writeln(" letters:"),
	input(Word).

game_sequence(_,_,0,_):- writeln("You lost!"), nl.
game_sequence(Word, Target, Guesses, Length):-
	pick_word(Word, Length,_),
	( ( Word == Target, writeln("You Won!"), nl );
	( Word \== Target, Remaining is Guesses - 1,
	( ( Guesses > 1, game_guidance(Word, Target, Remaining), enter_new(X,Length) ); Guesses is 1 ),
	game_sequence(X, Target, Remaining, Length), ! ) ).
game_sequence(X, Target, Guesses, Length):-
	( ( \+ string_length(X,Length), write("Word is not composed of "), write(Length), writeln(" letters. Try again.") );
	( string_length(X,Length), \+ available_word(X), writeln("Enter a valid word from the Knowldge Base. Try again.") ) ),
	write("Remaining Guesses are "), writeln(Guesses),nl, 
	enter_new(Word,Length), game_sequence(Word,Target,Guesses,Length),!.

game_guidance(W1, W2, Rem):-
	string_chars(W1, Chosen), string_chars(W2, Original),
	write("Correct letters are: "), 
	correct_letters(Chosen, Original, CL), 
	writeln(CL), write("Correct letters in correct positions are: "),
	correct_positions(Chosen, Original, CP),
	writeln(CP), write("Remaining Guesses are "), writeln(Rem), nl.
		 	
available_word(W):- word(W,_).	

available_length(L):-
	word(W,_), string_length(W,L).
		
correct_letters(_,[],[]).
correct_letters(L1, [H|T], [H|R]):-
	member(H, L1),
	delete_char(L1, H, L),
	correct_letters(L, T, R).
correct_letters(L1, [H|T], R):-
	\+ member(H, L1),
	correct_letters(L1, T, R).

delete_char([],_,[]).
delete_char([X|T], X, T).
delete_char([H|T], X, [H|R]):-
	delete_char(T, X, R).
	
correct_positions([],[],[]).
correct_positions([H|T1], [H|T2], [H|R]):-
	correct_positions(T1, T2, R).
correct_positions([H1|T1], [H2|T2], R):-
	H1 \== H2,
	correct_positions(T1, T2, R).
	
input(I):- 
	catch(read(X),_, retry(X)), I = X.
retry(I):- 
	writeln("Nothing was entered. Try again :)"), input(I).