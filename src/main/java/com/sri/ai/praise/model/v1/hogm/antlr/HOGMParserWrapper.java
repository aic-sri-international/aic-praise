/*
 * Copyright (c) 2015, SRI International
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
 * Neither the name of the aic-praise nor the names of its
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
package com.sri.ai.praise.model.v1.hogm.antlr;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTree;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.praise.model.v1.HOGModelException;
import com.sri.ai.praise.model.v1.hogm.antlr.HOGMLexer;
import com.sri.ai.praise.model.v1.hogm.antlr.HOGMParser;

@Beta
public class HOGMParserWrapper implements Parser {
	
	public ParsedHOGModel parseModel(String string) {
		ParsedHOGModel result = parseModel(string, new ErrorListener("Lexer Error"), new ErrorListener("Parse Error"));
		return result;
	}
	
	public ParsedHOGModel parseModel(String string, ErrorListener lexerErrorListener, ErrorListener parseErrorListener) {
		Expression     modelTupleExpr = parse(string, lexerErrorListener, parseErrorListener);
		ParsedHOGModel result         = new ParsedHOGModel(string, modelTupleExpr);
		return result;
	}
	
	@Override
	public Expression parse(String string) 
			throws RecognitionException, UnableToParseAllTheInputError, HOGModelException {
		Expression result = parse(string, new ErrorListener("Lexer Error"), new ErrorListener("Parse Error"));

		return result;
	}
	
	public Expression parse(String string, ErrorListener lexerErrorListener, ErrorListener parseErrorListener) 
			throws RecognitionException, UnableToParseAllTheInputError, HOGModelException {
		Expression result = parse(string, lexerErrorListener, parseErrorListener, new ModelParseTreeRetriever());
		return result;
	}

	
	public Expression parseTerm(String string) 
			throws RecognitionException, UnableToParseAllTheInputError, HOGModelException {
		Expression result = parseTerm(string, new ErrorListener("Lexer Error"), new ErrorListener("Parse Error"));
		return result;
	}
	
	public Expression parseTerm(String string, ErrorListener lexerErrorListener, ErrorListener parseErrorListener)
			throws RecognitionException, UnableToParseAllTheInputError, HOGModelException {
		Expression result = parse(string, lexerErrorListener, parseErrorListener, new ATermParseTreeRetriever());
		return result;
	}
	
	@Override
	public void close() {
	}	
	//
	// PRIVATE
	//
	private Expression parse(String string, ErrorListener lexerErrorListener, ErrorListener parseErrorListener, ParseTreeRetriever parseTreeRetriever) 
			throws RecognitionException, UnableToParseAllTheInputError, HOGModelException {
		Expression result = null;

		ANTLRInputStream input = new ANTLRInputStream(string);
		HOGMLexer        lexer = new HOGMLexer(input);
		
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		HOGMParser        parser = new HOGMParser(tokens);
		
		lexer.removeErrorListeners();
		parser.removeErrorListeners();
		lexer.addErrorListener(lexerErrorListener);
		parser.addErrorListener(parseErrorListener);

		ParseTree tree = parseTreeRetriever.retrieve(parser);

		boolean eofReached = parser.getInputStream().LA(1) == Recognizer.EOF;

		if (!lexerErrorListener.isSyntaxErrorsDetected() && !parseErrorListener.isSyntaxErrorsDetected()) {
			if (!eofReached) {
				throw new UnableToParseAllTheInputError();
			} else {
				lexer.removeErrorListeners();
				parser.removeErrorListeners();
				HOGModelVisitor hogmModelVisitor = new HOGModelVisitor();
				result = hogmModelVisitor.visit(tree);
			}
		}
		
		return result;
	}
	
	private interface ParseTreeRetriever {
		ParseTree retrieve(HOGMParser ruleParser);
	}
	
	private class ModelParseTreeRetriever implements ParseTreeRetriever {
		@Override
		public ParseTree retrieve(HOGMParser hogmParser) {
			return hogmParser.model();
		}
	}
	
	private class ATermParseTreeRetriever implements ParseTreeRetriever {
		@Override
		public ParseTree retrieve(HOGMParser hogmParser) {
			return hogmParser.aterm();
		}
	}
}
