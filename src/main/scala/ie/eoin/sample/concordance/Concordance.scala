package ie.eoin.sample.concordance

import scala.collection.SortedMap
import scala.io.Source

object Concordance {

  def addLineToMap (map:SortedMap[String,(Int,String)], line:List[String], lineNumber:Int) = {
    line.foldLeft(map) {
      (acc, x) => 
        val newEntry = acc.get(x) match {
          case Some(v) =>
            v match {
              case (count:Int, occ:String) =>(count+1, occ+":"+lineNumber) 
            }
          case None => (1, lineNumber.toString) 
        }
        acc ++ SortedMap((x,newEntry))
    } 
  }

  def processLines(lines:List[String]) = {
    lines.zipWithIndex.foldLeft(SortedMap[String, (Int,String)]()){
      (acc:SortedMap[String,(Int,String)], lineInfo) => 
        lineInfo match { case(line:String, index:Int) => addLineToMap(acc, line.split(" ").toList, index+1)}
    }
  }

  def main(args: Array[String]) = {
    val lines =  Source.fromFile("src/main/resources/input.txt").getLines().toList
    val result = processLines(lines)
    println(result)

  }
}
