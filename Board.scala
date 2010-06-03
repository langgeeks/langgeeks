import scala.collection.mutable.ArrayBuffer

class Board {
    def startGame():Array[Array[Square.Value]] = {
        Array(createRow("........"),
              createRow("........"),
              createRow("........"),
              createRow("...BW..."),
              createRow("...WB..."),
              createRow("........"),
              createRow("........"),
              createRow("........"))
    }

    def createRow(row:String):Array[Square.Value] = {
        val reply = new ArrayBuffer[Square.Value]()
        for (cell <- row) {
            if (cell == '.')
                reply += Square.Empty
            else if (cell == 'W')
                reply += Square.White
            else if (cell == 'B')
                reply += Square.Black
            else if (cell == 'O')
                reply += Square.Possible
        }
        reply.toArray
    }

}
