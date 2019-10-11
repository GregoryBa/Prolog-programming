% ---------- Exercise 1: ----------

encode_list([], Y, []).
encode_list([X1|X2], Y, [Z1|Z2]) :-
	Z1 is (mod((X1 + Y), 128)),
	encode_list(X2, Y, Z2).

% ---------- Exercise 2: ----------

encode_string(X, Y, A) :-
	string_codes(X, StrList1),
	process(StrList1, Y, Z),
	atom_codes(A, Z).

process([], Y, []).
process([X1|X2], Y, [Z1|Z2]) :-
	Z1 is (mod((X1 + Y), 128)),
	char_code(Z, Z1),
	process(X2, Y, Z2).

% ---------- Exercise 3: ----------

decode_list([], Y, []).
decode_list([X1|X2], Y, [Z1|Z2]) :-
	Z1 is (mod((X1 - Y), 128)),
	decode_list(X2, Y, Z2).

decode_string(X, Y, Z) :-
	string_codes(X, StrList1),
	process_decode(StrList1, Y, A),
	atom_codes(Z, A).

process_decode([], Y, []).
process_decode([X1|X2], Y, [Z1|Z2]) :-
	Z1 is (mod((X1 - Y), 128)),
	char_code(Z, Z1),
	process_decode(X2, Y, Z2).

% ---------- Exercise 4: ----------

shift_distance(Str1, Str2, Shiftdistance) :-
	string_codes(Str1, StrList1),
	string_codes(Str2, StrList2),
	process_shift(StrList1, StrList2, Shiftdistance).

process_shift([], [], Shiftdistance).
process_shift([H1|T1], [H2|T2], Shiftdistance) :- 
	Shiftdistance is H1 - mod(H2, 128),		% TEST MORE WITH ABS!
	process_shift(T1, T2, Shiftdistance).

% ---------- Exercise 5: ----------

caesar_candidate([], StrCipher, Shiftdistance, StrPlain).
caesar_candidate([H1|T1], StrCipher, Shiftdistance, StrPlain) :-
	string_codes(H1, StrList1),
	string_codes(StrCipher, StrList2),
	process_caesar_shift(StrList1, StrList2, ShiftTemp),
	decode_string(StrCipher, ShiftTemp, DecodedStr),
	check_equal(DecodedStr, H1, ShiftTemp, Shiftdistance, StrPlain),
	caesar_candidate(T1, StrCipher, Shiftdistance, StrPlain).
	
process_caesar_shift([H1|T1], [H2|T2], ShiftTemp) :-
	ShiftTemp is H2 - mod(H1, 128).

check_equal(Str1, Str2, ShiftCandidate, Shiftdistance, StrPlain) :-
	atom_codes(Str3, Str2),
	Str1 = Str3,
	StrPlain = Str1,
	Shiftdistance = ShiftCandidate.

check_equal(Str1, Str2, ShiftCandidate, Shiftdistance, StrPlain) :-
	Str1 \= Str2.
