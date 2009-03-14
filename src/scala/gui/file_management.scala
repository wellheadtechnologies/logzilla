package gui

import java.awt._
import javax.swing._
import core._  

class LasFileList extends JList {
  val model = new DefaultListModel
  setModel(model)
}
