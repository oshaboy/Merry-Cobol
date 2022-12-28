       IDENTIFICATION DIVISION.
       PROGRAM-ID. MERRY-COBOL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.


       WORKING-STORAGE SECTION.
       78 SCREEN_HEIGHT VALUE 17.
       78 OFFSET_WS VALUE 20.
       01 SNOWFLAKE_BUFFER.
       05 SNOWFLAKE_BUFFER_ARR PIC X(OFFSET_WS) OCCURS 17 TIMES.
       77 X PIC 9(2).
       77 Y PIC 9(2).
       77 I PIC 9(3).
      * The Screen Section acts like a list of instructions for 
      * how to draw the screen line by line
       SCREEN SECTION.
       78 OFFSET VALUE 20.

       01 TREE BACKGROUND-COLOR 0 HIGHLIGHT .
       02 LINE 1 COL OFFSET FOREGROUND-COLOR 6 HIGHLIGHT.
       05 VALUE '           _/^\_' .
       02 LINE 2 COL OFFSET FOREGROUND-COLOR 6.
       05 VALUE '         <     >'.
       02 LINE 3 COL OFFSET FOREGROUND-COLOR 6.
       05 VALUE '          /.-.\'.
       02 LINE 4 COL OFFSET FOREGROUND-COLOR 2.
       05 VALUE '           / \'.
       02 LINE 5 COL OFFSET FOREGROUND-COLOR 2.
       05 VALUE '          / '.
       05 VALUE '@' FOREGROUND-COLOR 4.
       05 VALUE ' \'.
       02 LINE 6 COL OFFSET FOREGROUND-COLOR 2.
       05 VALUE '         /  v  \' .
       02 LINE 7 COL OFFSET FOREGROUND-COLOR 2.
       05 VALUE '        / /   \ \' .
       02 LINE 8 COL OFFSET FOREGROUND-COLOR 2.
       05 VALUE '       /  '.
       05 VALUE 'O   o' FOREGROUND-COLOR 4.
       05 VALUE '  \'.
       02 LINE 9 COL OFFSET FOREGROUND-COLOR 2.
       05 VALUE '      / /   ^   \ \'.
       02 LINE 10 COL OFFSET FOREGROUND-COLOR 2.
       05 VALUE '     /   /     \   \' .
       02 LINE 11 COL OFFSET FOREGROUND-COLOR 2 .
       05 VALUE '    /   '.
       05 VALUE 'O' FOREGROUND-COLOR 3.
       05 VALUE '   /   '.
       05 VALUE '*' FOREGROUND-COLOR 6.
       05 VALUE'   \' .
       02 LINE 12 COL OFFSET FOREGROUND-COLOR 2.
       05 VALUE '   /  \   \     \    \'.
       02 LINE 13 COL OFFSET FOREGROUND-COLOR 2.
       05 VALUE '  /   /    v    \\ '.
       05 VALUE '%' FOREGROUND-COLOR 3.
       05 VALUE '  \'.
       02 LINE 14 COL OFFSET FOREGROUND-COLOR 2.
       05 VALUE ' /_____________________\'.
       02 LINE 15 COL OFFSET FOREGROUND-COLOR 2.
       05 VALUE '/_______________________\'.
       02 LINE 16 COL OFFSET FOREGROUND-COLOR 6.
       05 VALUE '        |      |' .
       02 LINE 17 COL OFFSET FOREGROUND-COLOR 6.
       05 VALUE '        |      |'.
       02 LINE 18 COL OFFSET FOREGROUND-COLOR 6.
       05 VALUE ' '.
       05 VALUE '[]' FOREGROUND-COLOR 4.
       05 VALUE '  '.
       05 VALUE '[] ' FOREGROUND-COLOR 3.
       05 VALUE '|      |  '.
       05 VALUE '[]' FOREGROUND-COLOR 4.

      * The snowflake buffers just exist so COBOL will know where and how 
      * to draw the snowflakes. 
      
       01 SNOWFLAKES_LEFT BACKGROUND-COLOR 0
           HIGHLIGHT FOREGROUND-COLOR 7.
       05 LINE  1 PIC X(OFFSET).
       05 LINE  2 PIC X(OFFSET).
       05 LINE  3 PIC X(OFFSET).
       05 LINE  4 PIC X(OFFSET).
       05 LINE  5 PIC X(OFFSET).
       05 LINE  6 PIC X(OFFSET).
       05 LINE  7 PIC X(OFFSET).
       05 LINE  8 PIC X(OFFSET).
       05 LINE  9 PIC X(OFFSET).
       05 LINE 10 PIC X(OFFSET).
       05 LINE 11 PIC X(OFFSET).
       05 LINE 12 PIC X(OFFSET).
       05 LINE 13 PIC X(OFFSET).
       05 LINE 14 PIC X(OFFSET).
       05 LINE 15 PIC X(OFFSET).
       05 LINE 16 PIC X(OFFSET).
       05 LINE 17 PIC X(OFFSET).

       01 SNOWFLAKES_RIGHT BACKGROUND-COLOR 0
           HIGHLIGHT FOREGROUND-COLOR 7.
       05 LINE  1 COL 45 PIC X(OFFSET).
       05 LINE  2 COL 45 PIC X(OFFSET).
       05 LINE  3 COL 45 PIC X(OFFSET).
       05 LINE  4 COL 45 PIC X(OFFSET).
       05 LINE  5 COL 45 PIC X(OFFSET).
       05 LINE  6 COL 45 PIC X(OFFSET).
       05 LINE  7 COL 45 PIC X(OFFSET).
       05 LINE  8 COL 45 PIC X(OFFSET).
       05 LINE  9 COL 45 PIC X(OFFSET).
       05 LINE 10 COL 45 PIC X(OFFSET).
       05 LINE 11 COL 45 PIC X(OFFSET).
       05 LINE 12 COL 45 PIC X(OFFSET).
       05 LINE 13 COL 45 PIC X(OFFSET).
       05 LINE 14 COL 45 PIC X(OFFSET).
       05 LINE 15 COL 45 PIC X(OFFSET).
       05 LINE 16 COL 45 PIC X(OFFSET).
       05 LINE 17 COL 45 PIC X(OFFSET).


       PROCEDURE DIVISION.
       MAIN.
           DISPLAY TREE.
           PERFORM SNOW.
      * Unreachable 


      * Paragraph in charge of Snow.
      * Randomly sets the SNOWFLAKE and SNOWFLAKE2 screen sections to '*'.
       SNOW.
           PERFORM UNTIL EXIT
      * Clear the buffer from last loop iteration.
             MOVE 1 TO I
             PERFORM OFFSET_WS TIMES
                 MOVE SPACES TO SNOWFLAKE_BUFFER_ARR(I)
                 ADD 1 TO I
             END-PERFORM
             
             PERFORM ADD-SNOWFLAKES-TO-BUFFER

      * Copy the buffer to the left side. The Screen section is already
      * preconfigured to be drawn on the left in WHITE. 
             MOVE SNOWFLAKE_BUFFER TO SNOWFLAKES_LEFT



             PERFORM ADD-SNOWFLAKES-TO-BUFFER
      * Copy the buffer to the right side.
             MOVE SNOWFLAKE_BUFFER TO SNOWFLAKES_RIGHT
             
      * Display the buffers. 
             DISPLAY SNOWFLAKES_LEFT
             DISPLAY SNOWFLAKES_RIGHT
             CALL "C$SLEEP" USING 1
           END-PERFORM.


      * Put up to 8 Asterisks randomly in SNOWFLAKE_BUFFER
       ADD-SNOWFLAKES-TO-BUFFER.
           PERFORM 8 TIMES
               COMPUTE X=OFFSET_WS*FUNCTION RANDOM
      * Y's range is larger than the screen height so some asterisks will be put
      * off screen and are subsequently ignored. Becuase it's doubled there's
      * a 50% chance of the snowflakes not being drawn.
      * This is to make the amount of snowflakes per side random.
               COMPUTE Y=2*SCREEN_HEIGHT*FUNCTION RANDOM
               MOVE '*' TO SNOWFLAKE_BUFFER_ARR(Y)(X:X)
           END-PERFORM.
       END PROGRAM MERRY-COBOL.
