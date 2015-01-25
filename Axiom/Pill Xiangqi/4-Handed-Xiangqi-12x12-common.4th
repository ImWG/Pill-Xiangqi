\ Common codes for 4-handed 12x12

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
	Blue {symmetry} East West
	Grey {symmetry} East West
	Grey {symmetry} North South
	Yellow {symmetry} North South
symmetries}


{turn-order
	{turn} Red
	{turn} Blue
	{turn} Grey
	{turn} Yellow
turn-order}

$passTurnForced ON

: InMySide?
	Red current-player = IF
		here DUP a6 >= SWAP f6 <= AND
		here DUP a5 >= SWAP f5 <= AND OR
		here DUP a4 >= SWAP f4 <= AND OR
		here DUP a3 >= SWAP f3 <= AND OR
		here DUP a2 >= SWAP f2 <= AND OR
		here DUP a1 >= SWAP f1 <= AND OR
	ELSE
		Blue current-player = IF
			here DUP g6 >= SWAP l6 <= AND
			here DUP g5 >= SWAP l5 <= AND OR
			here DUP g4 >= SWAP l4 <= AND OR
			here DUP g3 >= SWAP l3 <= AND OR
			here DUP g2 >= SWAP l2 <= AND OR
			here DUP g1 >= SWAP l1 <= AND OR
		ELSE
			Yellow current-player = IF
				here DUP a12 >= SWAP f12 <= AND
				here DUP a11 >= SWAP f11 <= AND OR
				here DUP a10 >= SWAP f10 <= AND OR
				here DUP a9 >= SWAP f9 <= AND OR
				here DUP a8 >= SWAP f8 <= AND OR
				here DUP a7 >= SWAP f7 <= AND OR
			ELSE
				here DUP g12 >= SWAP l12 <= AND
				here DUP g11 >= SWAP l11 <= AND OR
				here DUP g10 >= SWAP l10 <= AND OR
				here DUP g9 >= SWAP l9 <= AND OR
				here DUP g8 >= SWAP l8 <= AND OR
				here DUP g7 >= SWAP l7 <= AND OR
			ENDIF
		ENDIF
	ENDIF
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
			DUP here <> IF
				DUP
				#Surrender IF
					current-player SWAP change-owner-at \ Pos
				ELSE
					capture-at
				ENDIF
			ENDIF
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
	-1 #Allied1-1 = IF \ For Cutthroat only
		friend?
	ELSE
		current-player #Allied1-1 = IF
			player DUP #Allied1-2 = SWAP #Allied1-1 = OR
		ELSE
			current-player #Allied2-1 = IF
				player DUP #Allied2-1 = SWAP #Allied2-2 = OR
			ELSE
				current-player #Allied2-2 = IF
					player DUP #Allied2-1 = SWAP #Allied2-2 = OR
				ELSE
					player DUP #Allied1-1 = SWAP #Allied1-2 = OR
				ENDIF
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
	{piece} Soldier  {moves} SoldierX-moves
pieces}

: OnNewGame
	RedLost OFF
	BlueLost OFF
	GreyLost OFF
	YellowLost OFF
;

: OnIsGameOverVertical
	#UnknownScore
	RedLost @ YellowLost @ AND IF
		DROP 
		current-player DUP Red = SWAP Yellow = IF
			DROP ^" Blue and Grey win. " #LossScore
		ELSE
			DROP ^" Red and Yellow lose. " #WinScore
		ENDIF
	ELSE
		BlueLost @ GreyLost @ AND IF
			DROP 
			current-player DUP Red = SWAP Yellow = IF
				^" Blue and Grey lose. " #WinScore
			ELSE
				^" Red and Yellow win. " #LossScore
			ENDIF
		ENDIF
	ENDIF
;

: OnIsGameOverHorizontal
	#UnknownScore
	RedLost @ BlueLost @ AND IF
		DROP 
		current-player DUP Red = SWAP Blue = IF
			DROP ^" Grey and Yellow win. " #LossScore
		ELSE
			DROP ^" Red and Blue lose. " #WinScore
		ENDIF
	ELSE
		YellowLost @ GreyLost @ AND IF
			DROP 
			current-player DUP Red = SWAP Blue = IF
				^" Yellow and Grey lose. " #WinScore
			ELSE
				^" Red and Blue win. " #LossScore
			ENDIF
		ENDIF
	ENDIF
;

: OnIsGameOverDiagonal
	#UnknownScore
	RedLost @ GreyLost @ AND IF
		DROP 
		current-player DUP Red = SWAP Grey = IF
			DROP ^" Blue and Yellow win. " #LossScore
		ELSE
			DROP ^" Red and Grey lose. " #WinScore
		ENDIF
	ELSE
		YellowLost @ BlueLost @ AND IF
			DROP 
			current-player DUP Red = SWAP Grey = IF
				^" Yellow and Blue lose. " #WinScore
			ELSE
				^" Red and Grey win. " #LossScore
			ENDIF
		ENDIF
	ENDIF
;

: OnIsGameOverCutthroat
	#UnknownScore
	RedLost @ IF
		GreyLost @ IF
			BlueLost @ IF
				DROP
				current-player Yellow = IF
					^" Yellow wins. " #WinScore
				ELSE
					^" Yellow wins. " #LossScore
				ENDIF
			ELSE
				YellowLost @ IF
					DROP
					current-player Blue = IF
						^" Blue wins. " #WinScore
					ELSE
						^" Blue wins. " #LossScore
					ENDIF
				ENDIF
			ENDIF
		ELSE
			BlueLost @ YellowLost @ AND IF
				DROP
				current-player Grey = IF
					^" Grey wins. " #WinScore
				ELSE
					^" Grey wins. " #LossScore
				ENDIF
			ENDIF
		ENDIF
	ELSE
		GreyLost @ BlueLost @ AND YellowLost @ AND IF
			DROP
			current-player Red = IF
				^" Red wins. " #WinScore
			ELSE
				^" Red wins. " #LossScore
			ENDIF
		ENDIF
	ENDIF
;