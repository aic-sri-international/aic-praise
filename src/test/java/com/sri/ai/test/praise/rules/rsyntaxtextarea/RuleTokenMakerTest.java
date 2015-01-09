package com.sri.ai.test.praise.rules.rsyntaxtextarea;

import java.awt.BorderLayout;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.fife.ui.rsyntaxtextarea.AbstractTokenMakerFactory;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.TokenMakerFactory;
import org.fife.ui.rtextarea.RTextScrollPane;

/**
 * A simple example showing how to use RSyntaxTextArea to add Java syntax
 * highlighting to a Swing application.<p>
 * 
 * This example uses RSyntaxTextArea 2.0.1.<p>
 * 
 * Project Home: http://fifesoft.com/rsyntaxtextarea<br>
 * Downloads: https://sourceforge.net/projects/rsyntaxtextarea
 */
public class RuleTokenMakerTest extends JFrame {

	   private static final long serialVersionUID = 1L;
	   
	   private static final String SYNTAX_STYLE_RULES = "Rule Syntax Style";

	   public RuleTokenMakerTest() {

	      JPanel cp = new JPanel(new BorderLayout());

	      RSyntaxTextArea textArea = new RSyntaxTextArea(20, 60);

	      // The next four lines of code are the important ones to copy when using RSyntaxTextArea
	      // for the rules syntax highlighting.
	      AbstractTokenMakerFactory atmf = (AbstractTokenMakerFactory) TokenMakerFactory.getDefaultInstance();
	      atmf.putMapping(SYNTAX_STYLE_RULES, "com.sri.ai.praise.rules.rsyntaxtextarea.RuleTokenMaker");
	      TokenMakerFactory.setDefaultInstance(atmf);
	      textArea.setSyntaxEditingStyle(SYNTAX_STYLE_RULES);

	      textArea.setCodeFoldingEnabled(true);
	      textArea.setAntiAliasingEnabled(true);
	      RTextScrollPane sp = new RTextScrollPane(textArea);
	      sp.setFoldIndicatorEnabled(true);
	      cp.add(sp);

	      setContentPane(cp);
	      setTitle("Text Editor Demo");
	      setDefaultCloseOperation(EXIT_ON_CLOSE);
	      pack();
	      setLocationRelativeTo(null);

	      textArea.setText("/**\n"+
				" * Example 1: Epidemic and Sick with Symtoms.\n"+
				" * An example of the interplay between symtoms.\n" +
				" * Using Atomic and Conditional Rule Syntax.\n" +
				" */\n"+
				"//\n"+
				"// SORT DECLARATIONS:\n"+
				"sort People: 10, bob, dave, rodrigo, ciaran;\n"+
				"\n"+
				"//\n"+
				"// RANDOM VARIABLE DECLARATIONS:\n"+
				"random epidemic: Boolean;\n"+
				"random sick: People -> Boolean;\n"+
				"random fever: People -> Boolean;\n"+
				"random rash: People -> Boolean;\n"+
				"random notAtWork: People -> Boolean;\n" +
				"\n"+
				"//\n"+
				"// RULES\n" +
				"if epidemic then sick(X) 0.6 else sick(X) 0.05;\n" +
				"if sick(X) then fever(X) 0.7 else fever(X) 0.01;\n"+
				"if sick(X) then rash(X) 0.6 else rash(X) 0.07;\n"+
				"if sick(X) then notAtWork(X) 0.8 else notAtWork(X) 0.05;\n"+
				"\n"+
				"// By default, how likely is an epidemic?\n" +
				"epidemic 0.001;\n" +
				"\n"+
				"//\n"+
				"// By default, how likely are the following conditions?\n" +
				"sick(X) 0.009;\n"+
				"rash(X) 0.005;\n"+
				"fever(X) 0.001;");
	   }

	   public static void main(String[] args) {
	      // Start all Swing applications on the EDT.
	      SwingUtilities.invokeLater(new Runnable() {
	         @Override
			public void run() {
	            new RuleTokenMakerTest().setVisible(true);
	         }
	      });
	   }
}
