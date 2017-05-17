package parser

import java.io.InputStream

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PDFFileParserTest  extends FlatSpec with Matchers {

  "A PDFFileParser" should "extract positional info" in {
    val contentStream: InputStream = Thread.currentThread.getContextClassLoader.getResourceAsStream("parser_example_invoices/pdf-sample.pdf")
    val parsed: ParseResult = PDFFileParser.parse(contentStream)

    assert(parsed.phrases.nonEmpty)
    val firstPhrase: Phrase = parsed.phrases.head

    assert(214 == firstPhrase.x)
    assert(85 == firstPhrase.y)
    assert(1 == firstPhrase.pageNumber)
    assert("Adobe Acrobat PDF Files" == firstPhrase.text)
  }

}

