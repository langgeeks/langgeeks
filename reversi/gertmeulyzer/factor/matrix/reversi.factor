USING: arrays io.encodings.utf8 io.files kernel math.matrices namespaces sequences ;

IN: reversi

: read-board ( filename -- board )
    utf8 file-lines ;

: create-globals ( board -- )
    dup last "player" set-global
    8 head "board" set-global ;

: do-it ( -- )
    "/home/gert/src/langgeeks/reversi/gertmeulyzer/factor/matrix/board1.txt"
    read-board create-globals ;


