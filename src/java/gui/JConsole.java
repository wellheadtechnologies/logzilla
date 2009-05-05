/*****************************************************************************
 *                                                                           *
 *  This file is part of the BeanShell Java Scripting distribution.          *
 *  Documentation and updates may be found at http://www.beanshell.org/      *
 *                                                                           *
 *  Sun Public License Notice:                                               *
 *                                                                           *
 *  The contents of this file are subject to the Sun Public License Version  *
 *  1.0 (the "License"); you may not use this file except in compliance with *
 *  the License. A copy of the License is available at http://www.sun.com    * 
 *                                                                           *
 *  The Original Code is BeanShell. The Initial Developer of the Original    *
 *  Code is Pat Niemeyer. Portions created by Pat Niemeyer are Copyright     *
 *  (C) 2000.  All Rights Reserved.                                          *
 *                                                                           *
 *  GNU Public License Notice:                                               *
 *                                                                           *
 *  Alternatively, the contents of this file may be used under the terms of  *
 *  the GNU Lesser General Public License (the "LGPL"), in which case the    *
 *  provisions of LGPL are applicable instead of those above. If you wish to *
 *  allow use of your version of this file only under the  terms of the LGPL *
 *  and not to allow others to use your version of this file under the SPL,  *
 *  indicate your decision by deleting the provisions above and replace      *
 *  them with the notice and other provisions required by the LGPL.  If you  *
 *  do not delete the provisions above, a recipient may use your version of  *
 *  this file under either the SPL or the LGPL.                              *
 *                                                                           *
 *  Patrick Niemeyer (pat@pat.net)                                           *
 *  Author of Learning Java, O'Reilly & Associates                           *
 *  http://www.pat.net/~pat/                                                 *
 *                                                                           *
 *****************************************************************************/

package	gui;

import java.awt.Component;
import java.awt.Font;
import java.awt.Color;
import java.awt.Insets;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.*;
import java.util.*;
import java.awt.Cursor;
import javax.swing.text.*;
import javax.swing.*;
import clojure.lang.Compiler;
import clojure.lang.LispReader;

/**
   A JFC/Swing based console for the BeanShell desktop.
   This is a descendant of the old AWTConsole.

   Improvements by: Mark Donszelmann <Mark.Donszelmann@cern.ch>
   including Cut & Paste

   Improvements by: Daniel Leuck
   including Color and Image support, key press bug workaround
*/
public class JConsole extends JScrollPane implements KeyListener {
    private Boolean ignore = false;

    private int lineStart = 0;

    private List history = new ArrayList();
    private String startedLine;
    private int histLine = 0;

    private JTextPane text;

    public JConsole()
    {
	super();

	// Special TextPane which catches for cut and paste, both L&F keys and
	// programmatic	behaviour
	text = new JTextPane();

	Font font = new	Font("Monospaced",Font.PLAIN,14);
	text.setText("");
	text.setFont( font );
	text.setMargin(	new Insets(7,5,7,5) );
	text.addKeyListener(this);
	setViewportView(text);

	try {
	    evalClojure("(require ['sources.controller :as 'sources])");
	    evalClojure("(require ['editor.controller :as 'editor])");
	    evalClojure("(use 'util 'gutil 'global)");
	    evalClojure("(enable-interaction)");
	} catch (Exception e){ 
	    throw new RuntimeException(e);
	}

	requestFocus();
	print(">>");
    }

    public void requestFocus() 
    {
	super.requestFocus();
	text.requestFocus();
    }

    public void keyPressed(KeyEvent e) {
	press(e);
    }

    public void keyTyped(KeyEvent e) {
	type(e);
    }

    public void	keyReleased(KeyEvent e)	{
	e.consume();
    }

    private void press(KeyEvent e){
	int keyCode = e.getKeyCode();
	if(keyCode == KeyEvent.VK_UP){
	    historyUp();
	    e.consume();
	}
	else if(keyCode == KeyEvent.VK_A && (e.getModifiers() & InputEvent.CTRL_MASK) > 0){
	    ignore = true;
	    forceCaretMoveToStart();
	    e.consume();
	}
	else if(keyCode == KeyEvent.VK_K && (e.getModifiers() & InputEvent.CTRL_MASK) > 0){
	    ignore = true;
	    replaceRange("", text.getCaretPosition(), textLength());
	    e.consume();
	}
	else if(keyCode == KeyEvent.VK_DOWN){
	    historyDown();
	    e.consume();
	}
	else if(keyCode == KeyEvent.VK_LEFT ||
		keyCode == KeyEvent.VK_BACK_SPACE){
	    ignore = true;
	    if(text.getCaretPosition() <= lineStart){
		e.consume();
	    }
	}
	else if(keyCode == KeyEvent.VK_DELETE){
	    ignore = true;
	}
	else if(keyCode == KeyEvent.VK_RIGHT){
	    ignore = true;
	}
	else if(keyCode == KeyEvent.VK_ENTER){
	    ignore = true;
	    enter();
	    resetCommandStart();
	    text.setCaretPosition(lineStart);
	    e.consume();
	    text.repaint();
	}
    }
    
    private void type( KeyEvent e ) {	    
	char keyChar = e.getKeyChar();
	if(ignore){
	    ignore = false;
	    e.consume();
	    return;
	}
	if ((e.getModifiers() & (InputEvent.CTRL_MASK | InputEvent.ALT_MASK | InputEvent.META_MASK)) == 0 ){		
	    append(String.valueOf(keyChar));
	}
	e.consume();
    }

    private void resetCommandStart() {
	lineStart = textLength();
    }

    private void append(String string) {
	int slen = text.getCaretPosition();
	text.select(slen, slen);
	text.replaceSelection(string);
    }

    private String replaceRange(Object s, int start, int end) {
	String st = s.toString();
	text.select(start, end);
	text.replaceSelection(st);
	//text.repaint();
	return st;
    }

    private void forceCaretMoveToEnd() {
	text.setCaretPosition(textLength());
	text.repaint();
   }

    private  void forceCaretMoveToStart() {
	text.setCaretPosition(lineStart);
	text.repaint();
    }

    private	void enter() {
	String s = getCmd();

	if ( s.length()	== 0 )	// special hack	for empty return!
	    s = ";\n";
	else {
	    history.add( s );
	    s = s +"\n";
	}

	append("\n");
	histLine = 0;
	acceptLine( s );
	text.repaint();
    }

    private String getCmd() {
	String s = "";
	try {
	    s =	text.getText(lineStart, textLength() - lineStart);
	} catch	(BadLocationException e) {
	    // should not happen
	    System.out.println("Internal JConsole Error: "+e);
	}
	return s;
    }

    private void historyUp() {
	if ( history.size() == 0 )
	    return;
	if ( histLine == 0 )  // save current line
	    startedLine = getCmd();
	if ( histLine <	history.size() ) {
	    histLine++;
	    showHistoryLine();
	}
    }
	
    private void historyDown() {
	if ( histLine == 0 )
	    return;

	histLine--;
	showHistoryLine();
    }

    private void showHistoryLine() {
	String showline;
	if ( histLine == 0 )
	    showline = startedLine;
	else
	    showline = (String)history.get( history.size() - histLine	);

	replaceRange( showline,	lineStart, textLength() );
	text.setCaretPosition(textLength());
	text.repaint();
    }

    String ZEROS = "000";

    private void acceptLine(String line) {
	try{
	    Object result = evalClojure(line);
	    if(result != null){
		println(result.toString());	    
	    }
	    print(">>");
	} catch (Exception e){
	    println(e.getMessage());
	    print(">>");
	}
	//text.repaint();
    }

    public void println(Object o) {
	print( String.valueOf(o) + "\n" );
	text.repaint();
    }

    public void print(final Object o) {
	invokeAndWait(new Runnable() {
		public void run() {
		    append(String.valueOf(o));
		    resetCommandStart();
		    text.setCaretPosition(lineStart);
		}
	    });
    }

    /**
     * Prints "\\n" (i.e. newline)
     */
    public void println() {
	print("\n");
	text.repaint();
    }

    public void error( Object o ) {
	print( o, Color.red );
    }

    public void print(Object s, Font font) {
	print(s, font, null);
    }

    public void print(Object s, Color color) {
	print(s, null, color);
    }

    public void print(final Object o, final Font font, final Color color) {
	invokeAndWait(new Runnable() {
		public void run() {
		    AttributeSet old = getStyle();
		    setStyle(font, color);
		    append(String.valueOf(o));
		    resetCommandStart();
		    text.setCaretPosition(lineStart);
		    setStyle(old, true);
		}
	    });	
    }

    public void print(
		      Object s,
		      String fontFamilyName,
		      int	size,
		      Color color
		      ) {
			
	print(s,fontFamilyName,size,color,false,false,false);
    }

    public void print(
		      final Object o,
		      final String fontFamilyName,
		      final int	size,
		      final Color color,
		      final boolean bold,
		      final  boolean italic,
		      final boolean underline
		      ) 
    {
	invokeAndWait(new Runnable() {
		public void run() {
		    AttributeSet old = getStyle();
		    setStyle(fontFamilyName, size, color, bold,	italic,	underline);
		    append(String.valueOf(o));
		    resetCommandStart();
		    text.setCaretPosition(lineStart);
		    setStyle(old, true);
		}
	    });			
    }

    private AttributeSet setStyle(Font font) {
	return setStyle(font, null);
    }

    private AttributeSet setStyle(Color color) {
	return setStyle(null, color);
    }

    private AttributeSet setStyle( Font font, Color color) 
    {
	if (font!=null)
	    return setStyle( font.getFamily(), font.getSize(), color, 
			     font.isBold(), font.isItalic(), 
			     StyleConstants.isUnderline(getStyle()) );
	else
	    return setStyle(null,-1,color);
    }

    private AttributeSet setStyle (
				   String fontFamilyName, int	size, Color color) 
    {
	MutableAttributeSet attr = new SimpleAttributeSet();
	if (color!=null)
	    StyleConstants.setForeground(attr, color);
	if (fontFamilyName!=null)
	    StyleConstants.setFontFamily(attr, fontFamilyName);
	if (size!=-1)
	    StyleConstants.setFontSize(attr, size);

	setStyle(attr);

	return getStyle();
    }

    private AttributeSet setStyle(
				  String fontFamilyName,
				  int	size,
				  Color color,
				  boolean bold,
				  boolean italic,
				  boolean underline
				  ) 
    {
	MutableAttributeSet attr = new SimpleAttributeSet();
	if (color!=null)
	    StyleConstants.setForeground(attr, color);
	if (fontFamilyName!=null)
	    StyleConstants.setFontFamily(attr, fontFamilyName);
	if (size!=-1)
	    StyleConstants.setFontSize(attr, size);
	StyleConstants.setBold(attr, bold);
	StyleConstants.setItalic(attr, italic);
	StyleConstants.setUnderline(attr, underline);

	setStyle(attr);

	return getStyle();
    }

    private void setStyle(AttributeSet attributes) {
	setStyle(attributes, false);
    }

    private void setStyle(AttributeSet attributes, boolean overWrite) {
	text.setCharacterAttributes(attributes,	overWrite);
    }

    private AttributeSet getStyle() {
	return text.getCharacterAttributes();
    }

    public void setFont( Font font ) {
	super.setFont( font );

	if ( text != null )
	    text.setFont( font );
    }


    /**
     * If not in the event thread run via SwingUtilities.invokeAndWait()
     */
    private void invokeAndWait(Runnable run) {
	if(!SwingUtilities.isEventDispatchThread()) {
	    try {
		SwingUtilities.invokeAndWait(run);
	    } catch(Exception e) {
		// shouldn't happen
		e.printStackTrace();
	    }
	} else {
	    run.run();
	}
    }

    public void setWaitFeedback( boolean on ) {
	if ( on )
	    setCursor( Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR) );
	else
	    setCursor( Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR) );
    }

    private int textLength() { return text.getDocument().getLength(); }

    private Object evalClojure(String str) throws Exception {
	PushbackReader reader = new PushbackReader(new StringReader(str));	    
	Object data = LispReader.read(reader, false, "", false);
	Object result = Compiler.eval(data);
	return result;
    }
}


