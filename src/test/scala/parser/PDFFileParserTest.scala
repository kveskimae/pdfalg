package parser

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import parser.PDFFileParser
import parser.ParseResult
import java.io.{File, InputStream}
import java.net.URL
import exception.AppBadInputException
import org.junit.Test
import parser.PDFFileParser
import java.io.InputStream
import org.junit.Test
import parser.PDFFileParser
import parser.ParseResult
import parser.Phrase
import java.io.InputStream

@RunWith(classOf[JUnitRunner])
class PDFFileParserTest  extends FlatSpec with Matchers {
/*
  "A PDFFileParser" should "read PDF file" in {
    val contentStream: InputStream = Thread.currentThread.getContextClassLoader.getResourceAsStream("parser_example_invoices/pdf-sample.pdf")
    val parsed: ParseResult = PDFFileParser.parse(contentStream)
    assert(parsed.text.startsWith("Adobe Acrobat PDF Files"))
  }

  "A PDFFileParser" should "throw exception with a corrupt PDF file" in {
    val contentStream: InputStream = Thread.currentThread.getContextClassLoader.getResourceAsStream("parser_example_invoices/pdf-corrupt.pdf")

    assertThrows[AppBadInputException] {
      PDFFileParser.parse(contentStream)
    }
  }*/

  "A PDFFileParser" should "extract positional info" in {
    val contentStream: InputStream = Thread.currentThread.getContextClassLoader.getResourceAsStream("parser_example_invoices/pdf-sample.pdf")
    val parsed: ParseResult = PDFFileParser.parse(contentStream)

    assert(parsed.phrases.nonEmpty)
    val firstPhrase: Phrase = parsed.phrases.head
    println(firstPhrase)
    assert(214 == firstPhrase.x)
    assert(85 == firstPhrase.y)
    assert(1 == firstPhrase.pageNumber)
    assert("Adobe Acrobat PDF Files" == firstPhrase.text)
  }

}

