import org.specs2.mutable.Specification     


class ConcordanceSpec extends Specification{
  "processLines" should {
    "process single line " in {
      val result = Concordance.processLines(List("four four four four"))
      result.get("four").get._1 must_== 4
    }
    "process multiple lines" in {
      val result = Concordance.processLines(List("some lines are great", "other lines are better", " a few lines some are best"))
      result.get("lines").get._1 must_== 3
      result.get("lines").get._2 must be equalTo "1:2:3" 
    }
  }
}
