package core

import scala.collection.jcl.Conversions._
import java.io._
import java.util.{List,Formatter,LinkedList}
import java.math.BigDecimal

object DefaultLasWriter extends LasWriter {

  def writeLasFile(lf: LasFile, file:File) { 
    val writer = new BufferedWriter(new FileWriter(file))
    try {
      writeHeaders(lf, writer)
      writeCurves(lf, writer)
    } finally {
      writer.close()
    }
  }

  def writeLasFile(lf: LasFile, path:String) { 
    writeLasFile(lf, new File(path))
  }

  private def writeHeaders(lf: LasFile, writer:BufferedWriter) {
    val headers = lf.getHeaders
    for(h <- headers) {
      writeHeader(h, writer)
    }
  }

  private def writeHeader(h:Header, writer:BufferedWriter) {
    writer.write(h.getPrefix); writer.newLine
    writeDescriptors(h.getDescriptors,
		     writer)
  }

  private def writeDescriptors(descriptors:List[Descriptor], writer: BufferedWriter){
    for(d <- descriptors){
      writeDescriptor(d, writer)
    }
  }

  private def writeDescriptor(descriptor:Descriptor, writer: BufferedWriter) {
    val write = (s:String) => writer.write(s)
    write(descriptor.getMnemonic)
    write(" .")
    write(descriptor.getUnit.toString)
    write(" ")
    write(descriptor.getData.toString)
    write(" : ")
    writer.newLine
  }

  private def writeCurves(lf: LasFile, writer: BufferedWriter) {
    writer.write("~A")
    writer.newLine
    val curves = new LinkedList[Curve](lf.getCurves)
    curves.add(lf.getIndex)
    val columns = curves.size
    val rows = curves.first.getLasData.size
    def row_data(r:Int) = curves.map(c => {
      val data = c.getLasData
      val point = data.get(r).asInstanceOf[BigDecimal]
      val precision = Math.max(point.precision,10)
      String.format("%1." + precision + "f",Array(point))
    })

    for(r <- 0 until rows){
      writer.write(row_data(r).mkString(" "))
      writer.newLine
    }
  }			     
    
}
