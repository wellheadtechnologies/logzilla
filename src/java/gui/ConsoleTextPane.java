package gui;
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


public class ConsoleTextPane extends JTextPane {
    public ConsoleTextPane() {
	Font font = new	Font("Monospaced",Font.PLAIN,14);
	setText("");
	setFont(font);
	setMargin(new Insets(7,5,7,5));
    }

    public void println(Object o) {
	print(console, String.valueOf(o) + "\n" );
	repaint();
    }

    public void println() {
	print(console,"\n");
	repaint();
    }

    public void print(final Object o) {
	invokeAndWait(new Runnable() {
		public void run() {
		    append(String.valueOf(o));
		    resetCommandStart();
		    setCaretPosition(textLength());
		}
	    });	
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

    public int textLength() { 
	return getDocument().getLength();
    }

    public void append(String string) {
	int slen = getCaretPosition();
	select(slen, slen);
	replaceSelection(string);
    }

    public String replaceRange(Object s, int start, int end) {
	String st = s.toString();
	select(start, end);
	replaceSelection(st);
	return st;
    }


    }*/
