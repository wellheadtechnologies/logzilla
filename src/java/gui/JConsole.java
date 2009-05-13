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
/*
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

public class JConsole implements KeyListener {
    public Boolean ignore = false;
    public int lineStart = 0;
    public String startedLine;
    public History history;
    public ConsoleTextPane text;

    public JConsole()
    {
	history = new History();
	text = new ConsoleTextPane();
	text.addKeyListener(this);
	initUserEnvironment();
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
	    showHistoryLine(history.historyUp());
	    e.consume();
	}
	else if(keyCode == KeyEvent.VK_A && (e.getModifiers() & InputEvent.CTRL_MASK) > 0){
	    ignore = true;
	    forceCaretMoveToStart();
	    e.consume();
	}
	else if(keyCode == KeyEvent.VK_K && (e.getModifiers() & InputEvent.CTRL_MASK) > 0){
	    ignore = true;
	    replaceRange("", text.getCaretPosition(), textLength(text));
	    e.consume();
	}
	else if(keyCode == KeyEvent.VK_DOWN){
	    showHistoryLine(history.historyDown());
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

    private void forceCaretMoveToEnd() {
	text.setCaretPosition(textLength(text));
	text.repaint();
   }

    private void forceCaretMoveToStart() {
	text.setCaretPosition(lineStart);
	text.repaint();
    }

    private void enter() {
	String s = getCmd(text, lineStart);
	if(!s.trim().equals("")){
	    history.add(s);
	    s = s +"\n";
	    append("\n");
	    acceptLine( s );
	    text.repaint();
	}
	else {
	    append("\n");
	    print(this, ">>");
	    text.repaint();
	}
    }

    private void showHistoryLine(String line) {
	if(line != null){
	    replaceRange(line, lineStart, textLength(text));
	    text.setCaretPosition(textLength(text));
	    text.repaint();
	}
    }

    private void acceptLine(String line) {
	try{
	    Object result = evalClojureToStr(line);
	    if(result != null){
		println(this, result.toString());
	    }
	    print(this, ">>");
	} catch (Exception e){
	    println(this, e.getMessage());
	    print(this, ">>");
	}
    }


    public static void invokeAndWait(Runnable run) {
	if(!SwingUtilities.isEventDispatchThread()) {
	    try {
		SwingUtilities.invokeAndWait(run);
	    } catch(Exception e) {
		e.printStackTrace();
		throw new RuntimeException(e);
	    }
	} else {
	    run.run();
	}
    }

    public static Object evalClojure(String str) throws Exception {
	PushbackReader reader = new PushbackReader(new StringReader(str));	    
	Object data = LispReader.read(reader, false, "", false);
	Object result = Compiler.eval(data);
	return result;
    }

    public static Object evalClojureToStr(String str) throws Exception {
	PushbackReader reader = new PushbackReader(new StringReader(str));
	Object data = LispReader.read(reader, false, "", false);
	Object result = Compiler.eval(data);
	return result;
    }
}


*/