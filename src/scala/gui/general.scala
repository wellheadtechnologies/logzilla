package gui

import java.awt._
import java.awt.event.{MouseEvent}
import java.awt.image.{BufferedImage}
import java.awt.geom.{Rectangle2D,Point2D}
import javax.swing._
import javax.swing.border._
import java.util.{List,LinkedList}
import scala.collection.jcl.Conversions._
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart, ChartRenderingInfo}
import org.jfree.chart.plot.{PlotOrientation, XYPlot}
import org.jfree.chart.entity.{ChartEntity,XYItemEntity}
import org.jfree.chart.{ChartMouseEvent, ChartMouseListener}
import org.jfree.data.xy.{AbstractXYDataset, XYDataset, 
			  XYSeries, XYSeriesCollection}
import org.jfree.ui.RectangleEdge

import org.jdesktop.swingx.graphics.ShadowRenderer
import java.util.concurrent.locks.{ReadWriteLock,ReentrantReadWriteLock}

class CustomChartPanel(chart:JFreeChart) 
extends ChartPanel(chart, false, false, false, false, false) {
  val chartMouseListeners = new LinkedList[ChartMouseListener]

  override def mouseClicked(event:MouseEvent) {
    val insets = getInsets()
    var x = ((event.getX() - insets.left) / getScaleX).asInstanceOf[Int]
    var y = ((event.getY() - insets.top) / getScaleY).asInstanceOf[Int]
    
    setAnchor(new Point2D.Double(x, y))
    if (getChart == null) return
    getChart.setNotify(true)  // force a redraw
    // new entity code...
    if(chartMouseListeners.length == 0) return
    
    var entity:ChartEntity = null
    if (getChartRenderingInfo != null) {
      val entities = getChartRenderingInfo.getEntityCollection()
      if (entities != null) {
	//bounds are reversed because graph is vertical
	val picks = new LinkedList[ChartEntity]
	for(entity <- entities.getEntities.toArray){
	  val bounds = entity.asInstanceOf[ChartEntity].getArea.getBounds
	  if(Math.abs(x - bounds.y) < 10 && Math.abs(y - bounds.x) < 10){
	    picks.add(entity.asInstanceOf[ChartEntity])
	  }
	}
	if(picks.length > 0){
	  var chosen = picks.first
	  def delta(p:ChartEntity) = {
	    val bounds = p.getArea.getBounds
	    Math.sqrt(Math.pow(Math.abs(x - bounds.y),2) + 
		      Math.pow(Math.abs(y - bounds.x),2))
	  }
	  for(pick <- picks){
	    if(delta(pick) < delta(chosen)){
	      chosen = pick
	    }
	  }
	  entity = chosen
	}
      }
    }
    val chartEvent = new ChartMouseEvent(getChart(), event, entity)
    for(listener <- chartMouseListeners.reverse){
      listener.asInstanceOf[ChartMouseListener].chartMouseClicked(chartEvent)
    }
  }
  
  override def addChartMouseListener(listener:ChartMouseListener){
    chartMouseListeners += listener
    super.addChartMouseListener(listener)
  }

  override def removeChartMouseListener(listener:ChartMouseListener){
    chartMouseListeners -= listener
    super.addChartMouseListener(listener)
  }

}

class IconListCellRenderer extends JLabel with ListCellRenderer {

  override def getListCellRendererComponent(list:JList, value:Object, index:int, isSelected:boolean, hasFocus:boolean) = {
    val jlabel = value.asInstanceOf[JLabel]
    setOpaque(true)
    setText(jlabel.getText)
    setIcon(jlabel.getIcon)
    setComponentOrientation(list.getComponentOrientation)
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
