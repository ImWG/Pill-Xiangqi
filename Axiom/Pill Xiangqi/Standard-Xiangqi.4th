{players
	{player} Red
	{player} Blue
players}

{turn-order
	{turn} Red
	{turn} Blue
turn-order}


\ ==== Board ====

{board
	10 9 {grid}
	{position} off-board
	{variable} RedLost
	{variable} BlueLost
board}

{directions
	-1  0 {direction} North
	 1  0 {direction} South
	 0  1 {direction} East
	 0 -1 {direction} West
	 
	{link} North a10 off-board
	{link} North b10 off-board
	{link} North c10 off-board
	{link} North d10 off-board
	{link} North e10 off-board
	{link} North f10 off-board
	{link} North g10 off-board
	{link} North h10 off-board
	{link} North i10 off-board
	
	{link} South a1 off-board
	{link} South b1 off-board
	{link} South c1 off-board
	{link} South d1 off-board
	{link} South e1 off-board
	{link} South f1 off-board
	{link} South g1 off-board
	{link} South h1 off-board
	{link} South i1 off-board
	
	{link} East i1 off-board
	{link} East i2 off-board
	{link} East i3 off-board
	{link} East i4 off-board
	{link} East i5 off-board
	{link} East i6 off-board
	{link} East i7 off-board
	{link} East i8 off-board
	{link} East i9 off-board
	{link} East i10 off-board
	
	{link} West a1 off-board
	{link} West a2 off-board
	{link} West a3 off-board
	{link} West a4 off-board
	{link} West a5 off-board
	{link} West a6 off-board
	{link} West a7 off-board
	{link} West a8 off-board
	{link} West a9 off-board
	{link} West a10 off-board
directions}

{symmetries 
	Blue {symmetry} North South
symmetries}

: InMySide?
	current-player Red = IF
		here a5 >=
		here i1 <= AND
	ELSE
		here a10 >=
		here i6 <= AND
	ENDIF
;

\ Public Fortress!
: InFortress?
	here d1 =    here e1 = OR here f1 = OR
	here d2 = OR here e2 = OR here f2 = OR
	here d3 = OR here e3 = OR here f3 = OR
	here d8 = OR here e8 = OR here f8 = OR
	here d9 = OR here e9 = OR here f9 = OR
	here d10 = OR here e10 = OR here f10 = OR
;

: RedLost!  RedLost  ON ;
: BlueLost! BlueLost ON ;

: CaptureKing
	1 piece-type = IF
		player Red = IF
			COMPILE RedLost!
		ELSE
			COMPILE BlueLost!
		ENDIF
	ENDIF
;

\ general Moves
LOAD moves2.4th


{pieces
	{piece} General  {moves} General-moves
	{piece} Mandarin {moves} Mandarin-moves
	{piece} Elephant {moves} Elephant-moves
	{piece} Chariot  {moves} Chariot-moves
	{piece} Horse    {moves} Horse-moves
	{piece} Cannon   {moves} Cannon-moves
	{piece} Soldier  {moves} Soldier-moves
pieces}

: OnNewGame
	RedLost OFF
	BlueLost OFF
;

: OnIsGameOver
	#UnknownScore
	RedLost @
	IF
		DROP
		current-player Red = IF
			^" You lose " #LossScore
		ELSE
			^" You win " #WinScore
		ENDIF
	ELSE
		BlueLost @
		IF
			DROP
			current-player Blue = IF
				^" You win "  #WinScore
			ELSE
				^" You lose " #LossScore
			ENDIF
		ENDIF
	ENDIF
;
