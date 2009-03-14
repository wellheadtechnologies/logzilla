package core

import scala.collection.jcl.Conversions._
import java.util.{List,LinkedList,StringTokenizer}
import java.io.{File, FileReader, BufferedReader}
import scala.collection.mutable.{Queue,ListBuffer}

class DefaultLasParser extends LasParser {

  private val header_prefixes = Map("~V" -> "VersionHeader",
				    "~W" -> "WellHeader",
				    "~C" -> "CurveHeader",
				    "~P" -> "ParameterHeader")

  private val white_space = "\n\r\t "

  override def parseLasFile(path:String) = {
    parseLasFile(new File(path))
  }

  override def parseLasFile(file:File) = {
    var reader = new BufferedReader(new FileReader(file))
    val headers = parseHeaders(reader)
    val curveHeader = headers.find(_.getType == "CurveHeader").get
    val (index, curves) = parseCurves(curveHeader, reader)
    new DefaultLasFile(headers, curves)
  }

  private def parseCurves(curveHeader:Header,reader:BufferedReader) = {
    val descriptors = curveHeader.getDescriptors().asInstanceOf[List[Descriptor]]
    val n = descriptors.size()
    val data:Queue[Number] = parseData(reader)
    val cdatas = for(_ <- 0 to n) yield new LinkedList[Number]
    while(!data.isEmpty){
      (0 to n).foreach(i => cdatas(i).add(data.dequeue))
    }	
    val index = new DefaultCurve(descriptors(0), null, cdatas(0))
    val final_curves = new LinkedList[Curve]()
    for(i <- 1 to n){
      final_curves.add(new DefaultCurve(descriptors(i), index, cdatas(i)))
    }
    (index, final_curves)
  }

  private def parseHeaders(reader:BufferedReader):List[Header] = {
    skip_white_space(reader)
    val line = reader.readLine()
    val headers = new LinkedList[Header]()
    for(i <- 0 to 4){
      val prefix = line.take(2)
      val descriptors = parseDescriptors(reader)
      val htype = header_prefixes(prefix)
      headers.add(new DefaultHeader(htype, prefix, descriptors))
    }
    return headers
  }

  private def parseData(reader:BufferedReader):Queue[Number] = {
    skip_white_space(reader)
    if(!reader.readLine().contains("~A")){
      throw new RuntimeException("No ~A")
    }
    val data = new Queue[Number]()
    var continue = true
    while(reader.ready()) {
      val row = reader.readLine()
      val tokenizer = new StringTokenizer(row, " ")
      while(tokenizer.hasMoreTokens){
	data += BigDecimal(tokenizer.nextToken())
      }
    }
    return data
  }    
    
    
  
  private def parseDescriptors(reader:BufferedReader):List[Descriptor] = null

  private def skip_white_space(reader:BufferedReader) {
    var continue = true
    while(reader.ready() && continue) {
      val c = reader.read()
      if(!white_space.contains(c)){
	if(c == '#')
	  reader.readLine()
	else 
	  continue = false
      }
    }      
  }
}
  
