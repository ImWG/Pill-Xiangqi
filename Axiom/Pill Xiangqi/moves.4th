: Enemy?
	empty? Friend? OR NOT
;

: Add
	here off-board <> IF
		player $LosingPlayer !
		from here move CaptureKing add-move
	ENDIF
;

: Shift!
	Friend? NOT IF
		Add
	ENDIF
	back
;

: ShiftMySide!
	InMySide? IF
		Friend? NOT IF
			Add
		ENDIF
	ENDIF
	back
;

: ShiftNotMySide!
	InMySide? NOT IF
		Friend? NOT IF
			Add
		ENDIF
	ENDIF
	back
;

: ShiftFortress!
	InFortress? IF
		Friend? NOT IF
			Add
		ENDIF
	ENDIF
	back
;


: Slide!
	BEGIN
		DUP EXECUTE
		IF
			TRUE empty?
		ELSE
			FALSE FALSE
		ENDIF
	WHILE
		Add
		DROP
	REPEAT
	IF
		Enemy?
		IF
			Add
		ENDIF
	ENDIF
	DROP
	back
;

: SlideBombard!
	BEGIN
		DUP EXECUTE
		IF
			TRUE empty?
		ELSE
			FALSE FALSE
		ENDIF
	WHILE
		Add
		DROP
	REPEAT
	
	SWAP
	
	BEGIN
		DUP EXECUTE
		IF
			TRUE empty?
		ELSE
			FALSE FALSE
		ENDIF
	WHILE
		DROP
	REPEAT
	
	DROP
	
	Enemy? IF
		Add
	ENDIF
	
	back
;


: General-move
	North ShiftFortress!
	South ShiftFortress!
	West ShiftFortress!
	East ShiftFortress!
;
{moves General-moves
	{move} General-move
moves}

: Mandarin-move
	North East ShiftFortress!
	South East ShiftFortress!
	North West ShiftFortress!
	South West ShiftFortress!
;
{moves Mandarin-moves
	{move} Mandarin-move
moves}

: Elephant-move
	North East
	empty? IF
		North East ShiftMySide!
	ENDIF
	back
	South East
	empty? IF
		South East ShiftMySide!
	ENDIF
	back
	North West
	empty? IF
		North West ShiftMySide!
	ENDIF
	back
	South West
	empty? IF
		South West ShiftMySide!
	ENDIF
;
{moves Elephant-moves
	{move} Elephant-move
moves}

: Chariot-move
	['] North Slide!
	['] South Slide!
	['] East Slide!
	['] West Slide!
;
{moves Chariot-moves
	{move} Chariot-move
moves}

: Horse-move
	North
	empty? IF
		North East Shift!
	ENDIF
	back
	North
	empty? IF
		North West Shift!
	ENDIF
	back
	
	South
	empty? IF
		South East Shift!
	ENDIF
	back
	South
	empty? IF
		South West Shift!
	ENDIF
	back
	
	East
	empty? IF
		East North Shift!
	ENDIF
	back
	East
	empty? IF
		East South Shift!
	ENDIF
	back
	
	West
	empty? IF
		West North Shift!
	ENDIF
	back
	West
	empty? IF
		West South Shift!
	ENDIF

;
{moves Horse-moves
	{move} Horse-move
moves}

: Cannon-move
	['] North SlideBombard!
	['] South SlideBombard!
	['] East SlideBombard!
	['] West SlideBombard!
;
{moves Cannon-moves
	{move} Cannon-move
moves}

: Soldier-move
	North Shift!
	West ShiftNotMySide!
	East ShiftNotMySide!
;
{moves Soldier-moves
	{move} Soldier-move
moves}

: SoldierX-move
	North Shift!
	East Shift!
	West ShiftNotMySide!
	South ShiftNotMySide!
;
{moves SoldierX-moves
	{move} SoldierX-move
moves}