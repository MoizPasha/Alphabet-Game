The game will start when you run your program on DOSBox.
 Clear your screen and place a box on the middle of the last row on the screen. The ASCII value for box is
0xDC. This box can be moved Left when left arrow key is pressed and Right when right arrow key on the
keyboard is pressed. Hint: Hook keyboard interrupt (9h) and move the box.
Characters A-Z will randomly fall for top to bottom of the screen. The subroutine to generate random
number is given in the rand.asm file. The characters start falling from top of screen towards bottom of
screen until it disappears. Please note that the character appears randomly in the first row and then starts
falling down. At one time, there should be at least 5 characters falling down with different speeds. Hint:
Hook timer interrupt (8h) and make the movement of the characters in the timer interrupt service routine.
If any alphabet touches the box, it will disappear and one point will be added to the score. Display score on
the right upper corner of the screen. 
The game is over when the box misses 10 falling alphabets i.e., your program will terminate and DOSBOX
and command prompt will run normally.
