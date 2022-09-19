import scala.io.Source

trait Part:
  val title: String
  val content: String
  def isEmpty = title.isEmpty && content.isEmpty
object Empty extends Part:
  val title: String = ""
  val content: String = ""
case class Comment(content: String) extends Part:
  val title: String = "comment"
object Comment:
  def apply(lines: List[String]): Part = if lines.isEmpty then Empty else Comment(lines.mkString("\n"))

case class Fill(content: String) extends Part:
  val title: String = "fill"

trait Section extends Part
case class Inline(title: String, content: String) extends Section
case class Block(title: String, content: String) extends Section

object Section:
  def apply(): Part = Empty
  def apply(section: List[String]) : Part =
    if section.isEmpty then Section()
    else if section.head.contains("]") && section.head.indexOf(']') < section.head.length() - 1 then Inline(
       section.head.split("]")(0).replace("[", "").trim,
       section.mkString("\n").split("]")(1)
    )
    else if section.head.contains("]") then Block(
       section.head.split("]")(0).replace("[", "").trim,
       section.mkString("\n").split("]")(1)
    )
    else if section.head.contains("---") then Block(
      section.head.dropWhile(c => c == '-').reverse.dropWhile(c => c == '-').reverse.trim,
      section.tail.mkString("\n")
    )
    else Block("", section.mkString("\n"))
end Section

case class Header(title: String, artist: String) extends Part:
  val content = this.artist
object Header:
  def apply(): Header = Header("","")
  /**
    * @param artistAndTitle "Artist - Title"
    * @return Header with splited artist and title
    */
  def apply(artistAndTitle: String): Header =
    val header = artistAndTitle.split(" - ")
    Header(header(1).trim, header(0).trim)
end Header

def header(lines: List[String]) : Tuple2[Header, List[String]] =
  if lines.isEmpty then Tuple2(Header(), lines)
  else if !lines.head.isEmpty() then Tuple2(Header(lines.head), lines.tail)
  else header(lines.tail)

def part(lines: List[String]) : Tuple2[Part, List[String]] =
  def isPartEnd(content: List[String], lines: List[String]): Boolean =
    val cHead = content.head
    val lHead = if lines.isEmpty then null else lines.head
    lHead == null || // no more lines
    lHead.trim.startsWith("#") || // started comments
    (lHead.trim.startsWith("(") && lHead.trim.endsWith(")"))|| // next is a fill line
    (cHead.contains("]") && cHead.indexOf("]") < cHead.length() - 1) || // single line section 
    (lHead.contains("[") || lHead.contains("---")) // next line is next section

  def sectionContent(content: List[String], lines: List[String]): Tuple2[List[String], List[String]] =
    if isPartEnd(content, lines) then Tuple2(content, lines) 
    else sectionContent(content.appended(lines.head), lines.tail)

  def newSection(content: Tuple2[List[String], List[String]]) = Tuple2(Section(content._1), content._2)

  val linesKeep = lines.dropWhile(l => l.trim.isEmpty)
  val linesComments = linesKeep.takeWhile(l => l.trim.startsWith("#"))
  val linesNonEmpty = linesKeep.drop(linesComments.length)
  val head = linesNonEmpty.head
  if linesNonEmpty.isEmpty then Tuple2(Section(), linesNonEmpty)
  if !linesComments.isEmpty then Tuple2(Comment(linesComments), linesNonEmpty)
  if head.trim.startsWith("(") && head.trim.endsWith(")") then Tuple2(Fill(head), linesNonEmpty.tail)
  else newSection(sectionContent(List(head), linesNonEmpty.tail))

def parts(lines : List[String]) : List[Part]=
  def _parts(lines: List[String], secs: List[Part]= List()) : List[Part]=
    val (sec, remainder) = part(lines)
    val nSecs = if sec.isEmpty then secs else secs.appended(sec)
    if remainder.isEmpty then nSecs
    else _parts(remainder, nSecs)

  val linesKeep = lines.dropWhile(l => l.isEmpty())
  val linesComment = linesKeep.takeWhile(l => l.trim.startsWith("#"))
  val linesToProcess = linesKeep.drop(linesComment.length).dropWhile(l => l.isEmpty())
  val comment = Comment(linesComment)
  val (h, tail) = header(linesToProcess)
  (comment :: h :: _parts(tail)).filterNot(p => p.isEmpty)


@main def main: Unit =
  val strList = Source.fromFile("Whindersson Nunes  - Girassol (part. Priscilla Alcantara).txt")
  .mkString.split("\n").toList

  parts(strList).foreach(println)