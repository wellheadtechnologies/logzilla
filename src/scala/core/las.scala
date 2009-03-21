package core

import java.math.BigDecimal
import core._
import scala.collection.jcl.Conversions._
import org.apache.commons.collections.list.UnmodifiableList
import java.util.{List, LinkedList}
import java.util.concurrent.locks.{ReadWriteLock,ReentrantReadWriteLock}

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
  implicit def fun2Run[T](x: => T) : Runnable = new Runnable() { def run = x }

  def withReadLock[A](lock:ReadWriteLock)(fn: => A):A = {
    val readLock = lock.readLock
    readLock.lock()
    try {
      fn
    } finally {
      readLock.unlock()
    }
  }

  def withWriteLock[A](lock:ReadWriteLock)(fn: => A):A = {
    val writeLock = lock.writeLock
    writeLock.lock()
    try {
      fn
    } finally {
      writeLock.unlock()
    }
  }
    
}

class DefaultLasFile(name:String, mheaders: List[Header], index: Curve, mcurves: List[Curve])
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

  override def getName = name
  override def getCurves = curves
  override def getIndex = index
  override def getCurve(mnemonic:String) = curves.find(_.getMnemonic == mnemonic).get
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

class DefaultCurve(descriptor:Descriptor, index:Curve, data:List[BigDecimal]) 
extends Curve {
  override def getDescriptor = descriptor
  override def getLasData = data
  override def getIndex = index
  override def toString = descriptor.getMnemonic + data.size()
  override def getMnemonic = descriptor.getMnemonic
  override def getUnit = descriptor.getUnit
  override def getData = descriptor.getData
  override def getDescription = descriptor.getDescription
}

class DefaultHeader(htype:String, prefix:String, mdescriptors:List[Descriptor]) 
extends Header {
  val descriptors = Compat.unmodifiable(mdescriptors)
  override def getType = htype
  override def getPrefix = prefix
  override def getDescriptors = descriptors
  override def getDescriptor(name:String) =
    descriptors.find(_.getMnemonic == name).get

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
  override def equals(_that:Any):Boolean = {
    if(!_that.isInstanceOf[Descriptor]) return false
    val that = _that.asInstanceOf[Descriptor]
    if(that.getMnemonic != this.getMnemonic) return false
    if(that.getUnit != this.getUnit) return false
    if(that.getData != this.getData) return false
    if(that.getDescription != this.getDescription) return false
    return true
  }
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
