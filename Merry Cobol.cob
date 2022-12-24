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
      *   BACKGROUND-COLOR 6 REVERSE.
       02 LINE 17 COL OFFSET FOREGROUND-COLOR 6.
       05 VALUE '        |      |'.
      *   BACKGROUND-COLOR 6 REVERSE.
       02 LINE 18 COL OFFSET FOREGROUND-COLOR 6.
       05 VALUE ' '.
       05 VALUE '[]' FOREGROUND-COLOR 4.
       05 VALUE '  '.
       05 VALUE '[] ' FOREGROUND-COLOR 3.
       05 VALUE '|      |  '.
       05 VALUE '[]' FOREGROUND-COLOR 4.
       01 SNOWFLAKES BACKGROUND-COLOR 0 HIGHLIGHT FOREGROUND-COLOR 7. 
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
       
       01 SNOWFLAKES2 BACKGROUND-COLOR 0 HIGHLIGHT FOREGROUND-COLOR 7. 
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
           SNOW.


       SNOW.
           PERFORM UNTIL EXIT
             MOVE 1 TO I
             PERFORM 20 TIMES
                 MOVE SPACES TO SNOWFLAKE_BUFFER_ARR(I)
                 ADD 1 TO I
             END-PERFORM
             PERFORM 8 TIMES 
               COMPUTE X=FUNCTION RANDOM*OFFSET_WS
               COMPUTE Y=FUNCTION RANDOM*SCREEN_HEIGHT*2
               MOVE '*' TO SNOWFLAKE_BUFFER_ARR(Y)(X:X)
             END-PERFORM
             
             MOVE SNOWFLAKE_BUFFER TO SNOWFLAKES
             PERFORM 8 TIMES 
               COMPUTE X=FUNCTION RANDOM*OFFSET_WS
               COMPUTE Y=FUNCTION RANDOM*SCREEN_HEIGHT*2
               MOVE '*' TO SNOWFLAKE_BUFFER_ARR(Y)(X:X)
             END-PERFORM
             
             MOVE SNOWFLAKE_BUFFER TO SNOWFLAKES2
             DISPLAY SNOWFLAKES
             DISPLAY SNOWFLAKES2 
             CALL "C$SLEEP" USING 1
           END-PERFORM.
       END PROGRAM MERRY-COBOL.