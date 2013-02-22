/*
 * Copyright (c) 2013, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-expresso nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.praise.rules.antlr;

import java.util.List;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;


import com.google.common.annotations.Beta;
import com.sri.ai.brewer.api.Parser;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.rules.antlr.RuleAssociativeNodeWalker;
import com.sri.ai.praise.rules.antlr.RuleLexer;
import com.sri.ai.praise.rules.antlr.RuleOutputWalker;
import com.sri.ai.praise.rules.antlr.RuleParser;

/**
 * An implementation of the {@link Parser} interface that uses Antlr as the underlying implementation for rule based parsing.
 * 
 * @author tsai
 *
 */
@Beta
public class RuleParserWrapper implements Parser {

	public List<Expression> parseAll(String string) {
    	try {
//    		System.out.println("\nAttempting to parse: " + string);
    		CharStream cs = new ANTLRStringStream(string);
    		RuleLexer lexer = new RuleLexer(cs);
    		CommonTokenStream tokens = new CommonTokenStream(lexer);
    		RuleParser parser = new RuleParser(tokens);
    		CommonTree t = (CommonTree)parser.start().getTree();
//    		System.out.println("Original Tree: " + t.toStringTree());

    		CommonTreeNodeStream nodes = new CommonTreeNodeStream(t);//parser.start().getTree());

//    		System.out.println("Attempting to flatten associative operations...");
    		RuleAssociativeNodeWalker assoc = new RuleAssociativeNodeWalker(nodes);
    		t = (CommonTree)assoc.downup(t);//.start().getTree();
//    		System.out.println("New Tree: " + t.toStringTree());
    		nodes = new CommonTreeNodeStream(t);

    		RuleOutputWalker outputWalker = new RuleOutputWalker(nodes);
    		List<Expression> ret = outputWalker.start();
//    		System.out.println("Sucessful parse: " + ret);

    		return ret;
    	}
    	catch (RecognitionException re) {
    		System.out.println("**** Failed to parse: " + string);
    		re.printStackTrace();
    		return null;
    	}
    	catch (RuntimeException re) {
    		System.out.println("**** Failed to parse: " + string);
    		re.printStackTrace();
    		return null;
    	}
	}

	@Override
    public Expression parse(String string) {
		List<Expression> result = parseAll(string);
		if (result == null || result.size() == 0)
			return null;
		return result.get(0);
    }
	
	public Expression parseFormula (String string) {
    	try {
//    		System.out.println("\nAttempting to parse: " + string);
    		CharStream cs = new ANTLRStringStream(string);
    		RuleLexer lexer = new RuleLexer(cs);
    		CommonTokenStream tokens = new CommonTokenStream(lexer);
    		RuleParser parser = new RuleParser(tokens);
    		CommonTree t = (CommonTree)parser.formula().getTree();
//    		System.out.println("Original Tree: " + t.toStringTree());

    		CommonTreeNodeStream nodes = new CommonTreeNodeStream(t);//parser.start().getTree());

//    		System.out.println("Attempting to flatten associative operations...");
    		RuleAssociativeNodeWalker assoc = new RuleAssociativeNodeWalker(nodes);
    		t = (CommonTree)assoc.downup(t);//.start().getTree();
//    		System.out.println("New Tree: " + t.toStringTree());
    		nodes = new CommonTreeNodeStream(t);

    		RuleOutputWalker outputWalker = new RuleOutputWalker(nodes);
    		List<Expression> ret = outputWalker.start();
//    		System.out.println("Sucessful parse: " + ret);

    		return ret.get(0);
    	}
    	catch (RecognitionException re) {
    		System.out.println("**** Failed to parse: " + string);
    		re.printStackTrace();
    		return null;
    	}
    	catch (RuntimeException re) {
    		System.out.println("**** Failed to parse: " + string);
    		re.printStackTrace();
    		return null;
    	}
	}
    
    @Override
    public void close() {
	// TODO Auto-generated method stub
	
    }

}
