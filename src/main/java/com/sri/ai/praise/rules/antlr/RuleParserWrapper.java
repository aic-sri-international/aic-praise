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

import java.util.ArrayList;
import java.util.List;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTree;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;

/**
 * An implementation of the {@link Parser} interface that uses Antlr as the
 * underlying implementation for rule based parsing.
 * 
 * @author tsai
 * 
 */
@Beta
public class RuleParserWrapper implements Parser {

	@Override
	public Expression parse(String string) {
		List<Expression> result = parseAll(string);
		if (result == null || result.size() == 0) {
			return null;
		}
		return result.get(0);
	}

	public List<Expression> parseAll(String string) {
		List<Expression> result = parseAll(string, new ModelParseTreeRetriever());

		return result;
	}

	public Expression parseFormula(String string) {
		List<Expression> result = parseAll(string, new AFormulaParseTreeRetriever());
		if (result == null || result.size() == 0) {
			return null;
		}
		return result.get(0);
	}

	@Override
	public void close() {
	}

	public List<Expression> parseAll(String string, ParseTreeRetriever parseTreeRetriever) {
		List<Expression> result = new ArrayList<Expression>();
		try {
			ErrorListener lexerErrorListener = new ErrorListener("Lexer Error");
			ErrorListener parseErrorListener = new ErrorListener("Parse Error");

			ANTLRInputStream input = new ANTLRInputStream(string);
			RuleLexer lexer = new RuleLexer(input);

			CommonTokenStream tokens = new CommonTokenStream(lexer);
			RuleParser parser = new RuleParser(tokens);

			lexer.removeErrorListeners();
			parser.removeErrorListeners();
			lexer.addErrorListener(lexerErrorListener);
			parser.addErrorListener(parseErrorListener);

			ParseTree tree = parseTreeRetriever.retrieve(parser);

			boolean eof = parser.getInputStream().LA(1) == Recognizer.EOF;

			if (!lexerErrorListener.errorsDetected
					&& !parseErrorListener.errorsDetected) {
				if (!eof) {
					System.err
							.println("Unable to parse the complete input model: "
									+ input);
				} else {
					lexer.removeErrorListeners();
					parser.removeErrorListeners();
					ModelVisitor modelVisitor = new ModelVisitor();
					Expression parseExpression = modelVisitor.visit(tree);
					result = parseTreeRetriever.extractResults(parseExpression);
				}
			}
		} catch (RecognitionException re) {
			re.printStackTrace();
		} catch (RuntimeException re) {
			re.printStackTrace();
		}

		return result;
	}
	

	//
	// PRIVATE
	//
	
	private interface ParseTreeRetriever {
		ParseTree retrieve(RuleParser ruleParser);
		List<Expression> extractResults(Expression parseExpression);
	}
	
	private class ModelParseTreeRetriever implements ParseTreeRetriever {
		@Override
		public ParseTree retrieve(RuleParser ruleParser) {
			return ruleParser.model();
		}
		
		@Override
		public List<Expression> extractResults(Expression parseExpression) {
			List<Expression> result = new ArrayList<Expression>();
			for (Expression modelElement : parseExpression.getArguments()) {
				result.add(modelElement);
			}
			return result;
		}
	}
	
	private class AFormulaParseTreeRetriever implements ParseTreeRetriever {
		@Override
		public ParseTree retrieve(RuleParser ruleParser) {
			return ruleParser.aformula();
		}
		
		@Override
		public List<Expression> extractResults(Expression parseExpression) {
			List<Expression> result = new ArrayList<Expression>();
			result.add(parseExpression); // Should just be the formula
			return result;
		}
	}

	private class ErrorListener extends BaseErrorListener {
		public boolean errorsDetected = false;
		private String name;

		public ErrorListener(String name) {
			this.name = name;
		}

		@Override
		public void syntaxError(Recognizer<?, ?> recognizer,
				Object offendingSymbol, int line, int charPositionInLine,
				String msg, RecognitionException e) {
			System.err.println(name + ": line " + line + ":"
					+ charPositionInLine + " " + msg);
			errorsDetected = true;
		}
	}
}
