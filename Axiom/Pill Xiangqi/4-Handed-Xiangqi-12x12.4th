{players
	{player} Red
	{player} Blue
	{player} Grey
	{player} Yellow
players}

{turn-order
	{turn} Red
	{turn} Blue
	{turn} Grey
	{turn} Yellow
turn-order}

$passTurnForced ON


\ ==== Board ====

{board
	12 12 {grid}
	{position} off-board
	{variable} RedLost
	{variable} BlueLost
	{variable} GreyLost
	{variable} YellowLost
board}

{directions
	-1  0 {direction} North
	 1  0 {direction} South
	 0  1 {direction} East
	 0 -1 {direction} West
	 
	{link} North a12 off-board
	{link} North b12 off-board
	{link} North c12 off-board
	{link} North d12 off-board
	{link} North e12 off-board
	{link} North f12 off-board
	{link} North g12 off-board
	{link} North h12 off-board
	{link} North i12 off-board
	{link} North j12 off-board
	{link} North k12 off-board
	{link} North l12 off-board
	
	{link} South a1 off-board
	{link} South b1 off-board
	{link} South c1 off-board
	{link} South d1 off-board
	{link} South e1 off-board
	{link} South f1 off-board
	{link} South g1 off-board
	{link} South h1 off-board
	{link} South i1 off-board
	{link} South j1 off-board
	{link} South k1 off-board
	{link} South l1 off-board
	
	{link} East l1 off-board
	{link} East l2 off-board
	{link} East l3 off-board
	{link} East l4 off-board
	{link} East l5 off-board
	{link} East l6 off-board
	{link} East l7 off-board
	{link} East l8 off-board
	{link} East l9 off-board
	{link} East l10 off-board
	{link} East l11 off-board
	{link} East l12 off-board
	
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
	{link} West a11 off-board
	{link} West a12 off-board
directions}

{symmetries 
	Blue {symmetry} North South
symmetries}

: InMySide?
	FALSE \ In this game no Elephant
;

\ Public Fortress!
: InFortress?
	here a12 >= here c12 <= AND
	here j12 >= here c11 <= AND OR
	here j11 >= here c10 <= AND OR
	here j10 >= here l10 <= AND OR
	here a3 >= here c3 <= AND OR
	here j3 >= here c2 <= AND OR
	here j2 >= here c1 <= AND OR
	here j1 >= here l1 <= AND OR
;

VARIABLE $LosingPlayer
VARIABLE $CurrentPosition

: RedLost!  RedLost  ON ;
: BlueLost! BlueLost ON ;
: GreyLost! GreyLost ON ;
: YellowLost! YellowLost ON ;

: ConvertAll
	a12 $CurrentPosition ! \ Pos
	BEGIN
		$CurrentPosition @
		DUP player-at $LosingPlayer @ = IF \ Pos
			DUP current-player SWAP change-owner-at \ Pos
		ENDIF
		$CurrentPosition ++
		l1 = 
	UNTIL
;

: CaptureKing
	1 piece-type = IF
		ConvertAll
		$LosingPlayer @ Red = IF
			COMPILE RedLost!
		ELSE
			$LosingPlayer @ Blue = IF
				COMPILE BlueLost!
			ELSE
				$LosingPlayer @ Grey = IF
					COMPILE GreyLost!
				ELSE
					COMPILE YellowLost!
				ENDIF
			ENDIF
		ENDIF
	ENDIF
;

: Friend?
	current-player Red = IF
		player DUP Yellow = SWAP Red = OR
	ELSE
		current-player Blue = IF
			player DUP Grey = SWAP Blue = OR
		ELSE
			current-player Grey = IF
				player DUP Grey = SWAP Blue = OR
			ELSE
				player DUP Yellow = SWAP Red = OR
			ENDIF
		ENDIF
	ENDIF
;

\ general Moves
LOAD moves.4th


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
	GreyLost OFF
	YellowLost OFF
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
				^" You win "  #UnknownScore
			ELSE
				^" You lose " #LossScore
			ENDIF
		ENDIF
	ENDIF 
;
