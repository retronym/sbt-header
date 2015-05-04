package de.heikoseeberger.sbtheader

import org.scalatest.{ Matchers, WordSpec }

class YearParserSpec extends WordSpec with Matchers {

  import YearParser._


  "Parse Preamble Years Postamble" in {
    def test(source: String, expected: String) = {
      source match {
        case PreYearsPostRegex(pre, years, post) =>
          (pre + years + post) shouldBe source
          years shouldBe expected
        case _ =>
          fail("did not match")
      }
    }
    test("""|/* NSC -- new Scala compiler
            | * Copyright 2005-2013 LAMP/EPFL
            | * @author Martin Odersky
            | */
                 """.stripMargin, "2005-2013")
    test("copyright   (c) 2005  foo", "2005")
    test("COPYRIGHT   (C) 2005   foo", "2005")
    test("Copyright 2005 - 2013 LAMP/EPFL", "2005 - 2013")
    test("Copyright 2005 LAMP/EPFL", "2005")
    test("Copyright 2005, 2006-2012, 2015 LAMP/EPFL", "2005, 2006-2012, 2015")
  }
  
  "ParseYearRange" in {
    val ParseYearRange(Single(2015)) = "2015"
    val ParseYearRange(InclusiveRange(2015, 2016)) = "2015-2016"
    val ParseYearRange(Single(2012), InclusiveRange(2015, 2016)) = "2012,  2015-2016"
    val ParseYearRange(Single(2012), Single(2013), InclusiveRange(2015, 2016)) = "2012,2013,  2015-2016"
  }

  "merge" in {
    merge(Seq(Single(2012), Single(2014)), Single(2014)) shouldBe Seq(Single(2012), Single(2014))
    merge(Seq(Single(2012), Single(2014)), Single(2015)) shouldBe Seq(Single(2012), InclusiveRange(2014, 2015))
    merge(Seq(Single(2012)), Single(2015)) shouldBe Seq(Single(2012), Single(2015))
    merge(Seq(InclusiveRange(2012,2012)), Single(2015)) shouldBe Seq(InclusiveRange(2012,2012), Single(2015))
  }

  "createHeader" in {
    val existing =
      """
        |/* NSC -- new Scala compiler
        | * Copyright 2005-2012, 2014 LAMP/EPFL
        | * @author Martin Odersky
        | */
      """.stripMargin
    val updated =
      """
        |/* NSC -- new new Scala compiler
        | * Copyright 2015 LAMP/EPFL
        | * @author Martin Odersky!!
        | */
      """.stripMargin
    val result = createHeader(existing, updated)
    val expected =
      """
        |/* NSC -- new new Scala compiler
        | * Copyright 2005-2012, 2014-2015 LAMP/EPFL
        | * @author Martin Odersky!!
        | */
      """.stripMargin
    result shouldBe expected

  }
}

object YearParser {
  case class Header(preamble: String, years: Seq[YearRange], postamble: String)

  val PreYearsPostRegex = """(?ims)(.*?(?:copyright(?:\s*(?:\(c\)|©)?)\s*))([\d\s\-,]+?)(\s+[^\d\-].*)""".r

  sealed abstract class YearRange {
    def contains(year: Int): Boolean
  }

  case class Single(year: Int) extends YearRange {
    def contains(year: Int): Boolean = year == this.year

    override def toString: String = year.toString
  }
  case class InclusiveRange(start: Int, end: Int) extends YearRange {
    def contains(year: Int) = (start until end) contains year
    override def toString: String = s"$start-$end"
  }
  object ParseYearRange {
    def unapplySeq(s: String): Option[Seq[YearRange]] = util.Try {
      def parseSingleOrRange(s: String): YearRange = s.split('-').toList match {
        case s :: Nil => Single(s.trim.toInt)
        case start :: end :: Nil => InclusiveRange(start.trim.toInt, end.trim.toInt)
        case _ => sys.error("cannot parse")
      }
      s.split(',').toList.map(_.trim).map(parseSingleOrRange)
    }.toOption
  }
  
  def merge(existing: scala.Seq[YearRange], updated: Single): scala.Seq[YearRange] = {
    if (existing.exists(range => range.contains(updated.year))) existing
    else existing match {
      case as :+ InclusiveRange(start, end) if end == updated.year - 1 =>
        as :+ InclusiveRange(start, updated.year)
      case as :+ Single(year) if year == updated.year - 1 =>
        as :+ InclusiveRange(year, updated.year)
      case _ =>
        existing :+ updated
    }
  }

  def createHeader(existing: String, updated: String): String = {
    (existing, updated) match {
      case (PreYearsPostRegex(_, ParseYearRange(ranges @ _*), _), PreYearsPostRegex(pre, ParseYearRange(s @ Single(updatedYear)), post)) =>
        pre + merge(ranges, s).mkString(", ") + post
      case _ =>
        updated
    }
  }
}
