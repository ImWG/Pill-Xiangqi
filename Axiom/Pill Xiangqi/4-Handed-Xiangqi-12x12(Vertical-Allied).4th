{players
	{player} Red
	{player} Blue
	{player} Grey
	{player} Yellow
players}


\ Surrender: True is convert all, false is destroy all.
FALSE CONSTANT #Surrender

\ You can make players allied
Red    CONSTANT #Allied1-1
Yellow CONSTANT #Allied1-2
Blue   CONSTANT #Allied2-1
Grey   CONSTANT #Allied2-2

\ Load 
LOAD 4-Handed-Xiangqi-12x12-common.4th


: OnIsGameOver
	OnIsGameOverVertical
;
