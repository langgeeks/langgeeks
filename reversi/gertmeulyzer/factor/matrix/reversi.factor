USING: arrays io.encodings.utf8 io.files math.matrices sequences ;

IN: reversi

: read-board ( filename -- board )
    utf8 file-lines [ >array ] map ;
