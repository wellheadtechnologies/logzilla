package gui;
import java.util.*;

public class History {
    private Stack<String> previous = new Stack<String>();
    private Stack<String> next = new Stack<String>();

    public String historyUp() {
	if(!previous.isEmpty()){
	    String line = previous.pop();
	    next.push(line);
	    return line;
	}
	return null;
    }
	
    public String historyDown() {
	if(!next.isEmpty()){
	    String line = next.pop();
	    previous.push(line);
	    return line;
	}
	return null;
    }

    public void add(String line){
	previous.add(line);
	next.clear();
    }
}

    
