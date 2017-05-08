package parser

import java.io.{IOException, InputStream}

import exception.{AppBadInputException, AppServerFaultException}
import org.apache.pdfbox.pdmodel.PDDocument

import scala.collection.LinearSeq

object PDFFileParser {

  def parse(pdfContentStream: InputStream): ParseResult = {
    var document: PDDocument = null
    try
      document = PDDocument.load(pdfContentStream)
    catch {
      case e: IOException =>
        throw new AppBadInputException("PDF file is corrupt or not supported")
    }
    var text: String = null
    var phrases: LinearSeq[Phrase] = null
    try {
      val processor: PageParser = new PageParser()
      text = processor.getText(document)
      phrases = processor.getPhrases
    } catch {
      case e: IOException =>
        throw new AppServerFaultException("Parsing PDF file failed")
    }
    try
      document.close
    catch {
      case ignored: IOException =>
    }
    new ParseResult(text, phrases)
  }

}
