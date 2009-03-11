package gui

import java.awt._
import javax.swing._

class IconListCellRenderer extends DefaultListCellRenderer {
  override def getListCellRendererComponent(list:JList, value:Object, index:int, isSelected:boolean, cellHasFocus:boolean) = {
    if(value.isInstanceOf[JLabel]){
      val jl = value.asInstanceOf[JLabel]
      setText(jl.getText)
      setIcon(jl.getIcon)
    } else if (value.isInstanceOf[String]) {
      setText(value.asInstanceOf[String])
    } else {
      setText(value.toString)
    }

    if(isSelected) {
      setBackground(list.getSelectionBackground)
    } else {
      setBackground(Color.white)
    }
    
    this
  }

  override protected def paintComponent(g: Graphics){
    import RenderingHints._
    val g2d = g.asInstanceOf[Graphics2D]
    val hint = g2d.setRenderingHint _
    hint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)
    hint(KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_ON)
    hint(KEY_RENDERING, VALUE_RENDER_QUALITY)
    super.paintComponent(g2d)
  }
}
