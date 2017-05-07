package parser

import org.apache.commons.lang3.builder.{ReflectionToStringBuilder, ToStringStyle}

class Phrase(val x: Int,
             val y: Int,  
             val pageNumber: Int,  
             val height: Int,  
             val width: Int, 
             val text: String,
             val bold: Boolean) {

  override def toString: String = ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE)

}
