package gui;
import java.awt.*;
import java.awt.event.*;
import java.awt.datatransfer.*;
import javax.swing.*;
import java.awt.dnd.*;

public class CustomTransferHandler extends TransferHandler {
    private SwingDragGestureRecognizer recognizer = null;

    @Override
    public void exportAsDrag(JComponent comp, InputEvent e, int action) {
        int srcActions = getSourceActions(comp);

        // only mouse events supported for drag operations
        if (!(e instanceof MouseEvent)
                // only support known actions
	    || !(action == COPY || action == MOVE)
                // only support valid source actions
	    || (srcActions & action) == 0) {

            action = NONE;
        }

        if (action != NONE && !GraphicsEnvironment.isHeadless()) {
            if (recognizer == null) {
                recognizer = new SwingDragGestureRecognizer(new CustomDragHandler());
            }
            recognizer.gestured(comp, (MouseEvent)e, srcActions, action);
        } else {
            exportDone(comp, null, NONE);
        }
    }

    private class CustomDragHandler implements DragGestureListener, DragSourceListener {

        private boolean scrolls;

        // --- DragGestureListener methods -----------------------------------

        /**
         * a Drag gesture has been recognized
         */
        public void dragGestureRecognized(DragGestureEvent dge) {
            JComponent c = (JComponent) dge.getComponent();
	    if(c.getTransferHandler() != CustomTransferHandler.this) {
		throw new RuntimeException("c.getTransferHandler() must equal this!");
	    }
            Transferable t = createTransferable(c);
            if (t != null) {
                scrolls = c.getAutoscrolls();
                c.setAutoscrolls(false);
                try {
		    System.out.println("Starting drag with hand cursor");
                    dge.startDrag(new Cursor(Cursor.HAND_CURSOR), t, this);
                    return;
                } catch (RuntimeException re) {
                    c.setAutoscrolls(scrolls);
                }
            }

            exportDone(c, t, NONE);
        }

        // --- DragSourceListener methods -----------------------------------

        /**
         * as the hotspot enters a platform dependent drop site
         */
        public void dragEnter(DragSourceDragEvent dsde) {
        }

        /**
         * as the hotspot moves over a platform dependent drop site
         */
        public void dragOver(DragSourceDragEvent dsde) {
        }

        /**
         * as the hotspot exits a platform dependent drop site
         */
        public void dragExit(DragSourceEvent dsde) {
        }

        /**
         * as the operation completes
         */
        public void dragDropEnd(DragSourceDropEvent dsde) {
            DragSourceContext dsc = dsde.getDragSourceContext();
            JComponent c = (JComponent)dsc.getComponent();
            if (dsde.getDropSuccess()) {
                exportDone(c, dsc.getTransferable(), dsde.getDropAction());
            } else {
                exportDone(c, dsc.getTransferable(), NONE);
            }
            c.setAutoscrolls(scrolls);
        }

        public void dropActionChanged(DragSourceDragEvent dsde) {
        }
    }

    private class SwingDragGestureRecognizer extends DragGestureRecognizer {

        SwingDragGestureRecognizer(DragGestureListener dgl) {
            super(DragSource.getDefaultDragSource(), null, NONE, dgl);
        }

        void gestured(JComponent c, MouseEvent e, int srcActions, int action) {
            setComponent(c);
            setSourceActions(srcActions);
            appendEvent(e);
            fireDragGestureRecognized(action, e.getPoint());
        }

        /**
         * register this DragGestureRecognizer's Listeners with the Component
         */
        protected void registerListeners() {
        }

        /**
         * unregister this DragGestureRecognizer's Listeners with the Component
         *
         * subclasses must override this method
         */
        protected void unregisterListeners() {
        }

    }
}