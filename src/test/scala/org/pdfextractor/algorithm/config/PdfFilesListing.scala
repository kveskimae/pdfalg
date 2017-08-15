package org.pdfextractor.algorithm.config

import java.util.Locale

import org.pdfextractor.algorithm.io._

object PdfFilesListing {

  def getPdfInvoicesListing(lang: Locale): Seq[String] = {
    getFolderAsFile(lang.getLanguage)
      .listFiles
      .filter(_.isFile)
      .filter(_.getName.endsWith(".pdf"))
      .map(f => lang.getLanguage + "/" + f.getName)
  }

}
