# SixelFlip

## Description
### Project
My goal was to make a card pairing game using BBC Basic. For my code to be compatible with my hardware it must use less than 16kb of ram so Mode 7 was choosen. This was my first interaction with teletext and so I used "Sixel"s to create higher resolution graphics cheaply. This code could be easily made into asembly but basic was my aim. There is room for performance improvments but readability was a priority since I will be using this for educational purposes.
### Usage
SixelFlip.bas can be ran in SDLIDE (1.35a was used for devlopment).
SixelFlip.zip holds a windows executable which will play the game instead (Generated by SDLIDE).

WASD or Arrow keys, move the cursor*. 
Space, reveals a hidden card.
R, Restarts the game once all pairs have been found.
The highlighted name in the bottom shows the current player.
*Cursor is the checkered line above and below a card. It's colour is random and irrelevant
## Images
Start
![All Cards are covered, Cursor starts in top right](https://i.imgur.com/25J6rgl.png)

Two different Selected Cards
![Two different random cards are different](https://i.imgur.com/DBwsEev.png)

All Cards are hidden again. Player 2 is now green.
![Please Email me if anyone actually uses the alt for good](https://i.imgur.com/DD1hTRa.png)

Mid-game Image
![Most cards are shown](https://i.imgur.com/DE7HEUu.png)

Finished Game with all cards revealed and on Frame 1*
![All cards are visible and restart instruction is onscreen](https://i.imgur.com/JQWTlKT.png)

Finished Game with all cards revealed and on Frame 2*
![All cards are visible with different images and restart instruction is onscreen](https://i.imgur.com/A7vvABO.png)

The frames alternate every second during the whole game.
There are also two sound effects. One for a correct pair and one for an incorrect match.
## Improvements (TODO)
 - Background Music
 - Menu Screen
 - Card Editor
 - SinglePlayer Mode
 - SP Leaderboard
 - Enterable Names
## License
Copyright (C) 2023
Andrew McCall
AndrewDavidMcCall@hotmail.com
This can not be copied and/or distributed
without the express permission of Andrew McCall
