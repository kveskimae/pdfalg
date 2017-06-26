package parser

import java.io.InputStream

import exception.AppBadInputException
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

  "A PDFFileParser" should "extract text" in {
    val contentStream: InputStream = Thread.currentThread.getContextClassLoader.getResourceAsStream("parser_example_invoices/pdf-sample.pdf")
    val parsed: ParseResult = PDFFileParser.parse(contentStream)
    parsed.text should not be (null)
    assert(parsed.text.startsWith("Adobe Acrobat PDF Files"))
  }

  "A PDFFileParser" should "throw with corrupt PDF file" in {
    val contentStream: InputStream = Thread.currentThread.getContextClassLoader.getResourceAsStream("parser_example_invoices/pdf-corrupt.pdf")

    an [AppBadInputException] should be thrownBy {
      PDFFileParser.parse(contentStream)
    }

  }

}

