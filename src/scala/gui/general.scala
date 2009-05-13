package gui

import java.lang.Math
import java.util.Collections
import java.awt._
import java.awt.dnd._
import java.awt.event._
import java.awt.datatransfer._
import java.awt.image.{BufferedImage}
import java.awt.geom.{Rectangle2D,Point2D}
import javax.swing._
import javax.swing.tree._
import javax.swing.border._
import javax.imageio._
import java.io._
import java.util.{List,LinkedList,ArrayList}
import scala.collection.jcl.Conversions._
import org.jfree.chart._
import org.jfree.chart.plot.{PlotOrientation, XYPlot}
import org.jfree.chart.entity.{ChartEntity,XYItemEntity}
import org.jfree.chart.{ChartMouseEvent, ChartMouseListener}
import org.jfree.data.xy.{AbstractXYDataset, XYDataset, 
			  XYSeries, XYSeriesCollection}
import org.jfree.ui.{RectangleEdge,TextAnchor}

import org.jdesktop.swingx.graphics.ShadowRenderer
import java.util.concurrent.locks.{ReadWriteLock,ReentrantReadWriteLock}
import org.jfree.chart.axis._
import org.jfree.chart.renderer.xy._
import org.jfree.chart.labels._
import org.jfree.chart.urls._

object ChartUtil {
  val currentTheme = new StandardChartTheme("JFree")
  def createXYLineChart(title:String,xAxisLabel:String,yAxisLabel:String, dataset:XYDataset,orientation:PlotOrientation,legend:Boolean) = {
    if (orientation == null) throw new IllegalArgumentException("Null 'orientation' argument.")
    val xAxis = new NumberAxis(xAxisLabel)
    xAxis.setAutoRangeIncludesZero(false)
    xAxis.setInverted(true)
    val yAxis = new NumberAxis(yAxisLabel)
    val renderer = new XYLineAndShapeRenderer(true, false)
    val plot = new XYPlot(dataset, xAxis, yAxis, renderer)
    plot.setOrientation(orientation);
    val chart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, legend)
    currentTheme.apply(chart)
    chart
  }

  def mergeData(left:java.lang.Iterable[Double], right:java.lang.Iterable[Double]):List[Double] = {
    val result = new ArrayList[Double]()
    val lefti = left.iterator
    val righti = right.iterator
    val nan = Double.box(java.lang.Double.NaN)
    def isNaN(x:Double) = Double.box(x) == nan
    var count = 0

    while(lefti.hasNext && righti.hasNext){
      val a = lefti.next
      val b = righti.next
      if(isNaN(a) && isNaN(b))
	result.add(java.lang.Double.NaN)
      else if(isNaN(a))
	result.add(b)
      else if(isNaN(b))
	result.add(a)
      else 
	result.add((a + b)/2)
    }
    result
  }

  def addToSeries(series:XYSeries, idata:java.lang.Iterable[Double], cdata:java.lang.Iterable[Double]) { 
    val idataIt = idata.iterator
    val cdataIt = cdata.iterator
    while(idataIt.hasNext && cdataIt.hasNext){
      val x = idataIt.next
      val y = cdataIt.next
      series.add(x,y)
    }
  }
}

abstract class Dragger(comp:JComponent)
extends DragGestureListener with DragSourceListener with DragSourceMotionListener {
  private var dragSource:DragSource = DragSource.getDefaultDragSource
  dragSource.createDefaultDragGestureRecognizer(comp, DnDConstants.ACTION_COPY, this)
  dragSource.addDragSourceMotionListener(this)

  override def dragGestureRecognized(dge:DragGestureEvent){
    dragSource.startDrag(dge, DragSource.DefaultCopyNoDrop,
			 createIcon(comp), new Point(0,0), createTransferable(comp), this)
  }
  def createIcon(comp:JComponent):Image
  def createTransferable(comp:JComponent):Transferable
  override def dragDropEnd(dsde:DragSourceDropEvent) {
    println("drop ended")
  }
  override def dragEnter(dsde:DragSourceDragEvent) {}
  override def dragExit(dse:DragSourceEvent) {}
  override def dragOver(dsde:DragSourceDragEvent) {}
  override def dropActionChanged(dsde:DragSourceDragEvent) {}
  override def dragMouseMoved(dsde:DragSourceDragEvent) {}
}

class CurveIcon(curve: Object, icon:ImageIcon) extends JComponent {
  def getCurve:Object = curve
  def getIcon = icon
  override def paint(graphics:Graphics) {
    icon.paintIcon(this, graphics, 0, 0)
    super.paint(graphics)
  }

  override def getPreferredSize = new Dimension(icon.getIconWidth, icon.getIconHeight)
  override def getMinimumSize = getPreferredSize
  override def getMaximumSize = getPreferredSize
}

trait NodePayload {
  def getFile:Object
}

class IconListCellRenderer extends JLabel with ListCellRenderer {

  override def getListCellRendererComponent(list:JList, _value:Object, index:int, isSelected:boolean, hasFocus:boolean) = {
    val value = _value.asInstanceOf[CurveIcon]
    setIcon(value.getIcon)
    setOpaque(true)
    if(isSelected){
      setBackground(list.getSelectionBackground)
      setForeground(list.getSelectionForeground)
    }
    else{
      setBackground(list.getBackground)
      setForeground(list.getForeground)
    }
    if(hasFocus){
      setBorder(UIManager.getBorder("List.focusCellHighlightBorder"))
    }
    else {
      setBorder(new EmptyBorder(1,1,1,1))
    }
    this
  }

  override protected def paintComponent(g: Graphics){
    import RenderingHints._
    val g2 = g.asInstanceOf[Graphics2D]
    val hint = g2.setRenderingHint _
    hint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)
    hint(KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_ON)
    hint(KEY_RENDERING, VALUE_RENDER_QUALITY)
    super.paintComponent(g2)
  }

} 

object ImageUtil {
  def fastScale(img:BufferedImage, targetWidth: Int, targetHeight: Int) = {
    var itype:Int = BufferedImage.TYPE_INT_ARGB

    var ret = img
    var scratchImage:BufferedImage = null
    var g2:Graphics2D = null
    var (w,h) = (0,0)
      var prevW = ret.getWidth
    var prevH = ret.getHeight
    w = img.getWidth
    h = img.getHeight
    
    do {
      if(w > targetWidth){
	w /= 2
	if(w < targetWidth){
	  w = targetWidth
	}
      }
      
      if(h > targetHeight){
	h /= 2
	if(h < targetHeight){
	  h = targetHeight
	}
      }

      if(scratchImage == null){
	scratchImage = new BufferedImage(w,h,itype)
	g2 = scratchImage.createGraphics
      }

      g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
			  RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g2.drawImage(ret, 0, 0, w, h, 0, 0, prevW, prevH, null)
      prevW = w
      prevH = h
      
      ret = scratchImage
    } while(w != targetWidth || h != targetHeight)

    if(g2 != null){
      g2.dispose()
    }

    if(targetWidth != ret.getWidth || targetHeight != ret.getHeight) {
      scratchImage = new BufferedImage(targetWidth,
				       targetHeight, itype)
      g2 = scratchImage.createGraphics
      g2.drawImage(ret, 0, 0, null)
      g2.dispose()
      ret = scratchImage
    }
    
    ret
  }

}
