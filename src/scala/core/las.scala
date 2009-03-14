package core

import core._
import scala.collection.jcl.Conversions._
import org.apache.commons.collections.list.UnmodifiableList
import java.util.{List, LinkedList}

object Compat {
  def unmodifiable[A](l: List[A]):List[A] = {
    UnmodifiableList.decorate(l).asInstanceOf[List[A]]
  }
  def toList[A](args: A*):List[A] = {
    val list = new LinkedList[A]()
    for(a <- args){
      list.add(a)
    }
    return Compat.unmodifiable(list)
  }
}

class DefaultLasFile(mheaders: List[Header], mcurves: List[Curve])
extends LasFile {
  private val headers = Compat.unmodifiable(mheaders)
  private val curves = Compat.unmodifiable(mcurves)

  override def equals(_that:Any):Boolean = {
    if(!_that.isInstanceOf[LasFile]) return false
    val that = _that.asInstanceOf[LasFile]
    if(this.headers != that.getHeaders) return false
    if(this.curves != that.getCurves) return false
    return true
  }

  override def getCurves = curves
  override def getHeaders = headers
  override def getVersionHeader = {
    headers.find(_.getType == "VersionHeader").get
  }
  override def getWellHeader = {
    headers.find(_.getType == "WellHeader").get
  }
  override def getCurveHeader = {
    headers.find(_.getType == "CurveHeader").get
  }
  override def getParameterHeader = {
    headers.find(_.getType == "ParameterHeader").get
  }

  override def toString = {
    val buf = new StringBuffer
    buf.append("LasFile")
    for(h <- headers){
      buf.append(h)
      buf.append("\n")
    }
    for(c <- curves){
      buf.append(c)
      buf.append("\n")
    }
    buf.toString
  }
}

class DefaultCurve(descriptor:Descriptor, index:Curve, data:List[Number]) 
extends Curve {
  override def getDescriptor = descriptor
  override def getData = data
  override def getIndex = index
  override def toString = descriptor.getMnemonic + data.size()
}

class DefaultHeader(htype:String, prefix:String, mdescriptors:List[Descriptor]) 
extends Header {
  val descriptors = Compat.unmodifiable(mdescriptors)
  override def getType = htype
  override def getPrefix = prefix
  override def getDescriptors = descriptors
  override def toString = {
    val buf = new StringBuffer 
    def a(s:String) { buf.append(s) }
    a(htype)
    a("\n")
    for(d <- descriptors){
      a(d.toString)
      a("\n")
    }
    buf.toString
  }
}

class DefaultDescriptor(mnemonic:String, unit:Object, data:Object, description:String)
extends Descriptor {
  override def getMnemonic = mnemonic
  override def getUnit = unit
  override def getData = data
  override def getDescription = description
  override def toString = mnemonic + " " + unit + " " + data + " " + description
}

object Headers {
  def VersionHeader(version:String, wrap:String) = {
    new DefaultHeader("VersionHeader", "~V", 
		      Compat.toList(
			new DefaultDescriptor("VERS", null, version, null),
			new DefaultDescriptor("WRAP", null, wrap, null)))
  }

  def CurveHeader(descriptors:List[Descriptor]) = {
    new DefaultHeader("CurveHeader", "~C", descriptors)
  }

  def WellHeader(descriptors:List[Descriptor]) = {
    new DefaultHeader("WellHeader", "~W", descriptors)
  }

  def ParameterHeader(descriptors:List[Descriptor]) = {
    new DefaultHeader("ParameterHeader", "~P", descriptors)
  }
}
