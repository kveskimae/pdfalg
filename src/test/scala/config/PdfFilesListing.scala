package config

import java.io.File
import java.util.Locale

import io.IOHelper

object PdfFilesListing {

  def getPdfInvoicesListing(lang: Locale): Seq[String] = {
    val filesInFolder: Array[File] = IOHelper.getFolderAsFile(lang.getLanguage).listFiles
    val ret: Seq[String] = filesInFolder.filter(_.isFile).filter(_.getName.endsWith(".pdf")).map(f => lang.getLanguage + "/" + f.getName)
    ret
  }

}
