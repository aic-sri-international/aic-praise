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
package com.sri.ai.praise.core.representation.classbased.hogm.parsing;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTree;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.core.representation.classbased.hogm.validation.HOGModelException;
import com.sri.ai.praise.model.v1.hogm.antlr.HOGMLexer;
import com.sri.ai.praise.model.v1.hogm.antlr.HOGMParser;

@Beta
public class HOGMParserWrapper implements Parser {
	
	@Override
	public Expression parse(String string, Parser.ErrorListener errorListener) 
			throws RecognitionException, UnableToParseAllTheInputError, HOGModelException {
		Expression result = parse(string, errorListener, new ModelParseTreeRetriever());

		return result;
	}
	
	public HOGModel parseModel(String string) {
		HOGModel result = parseModel(string, newDefaultErrorListener());
		return result;
	}
	
	public HOGModel parseModel(String string, Parser.ErrorListener errorListener) {
		Expression modelTupleExpression = parse(string, errorListener);
		HOGModel result = new HOGModel(string, modelTupleExpression);
		return result;
	}
	
	public Expression parseTerm(String string) 
			throws RecognitionException, UnableToParseAllTheInputError, HOGModelException {
		Expression result = parseTerm(string, newDefaultErrorListener());
		return result;
	}
	
	public Expression parseTerm(String string, Parser.ErrorListener errorListener)
			throws RecognitionException, UnableToParseAllTheInputError, HOGModelException {
		Expression result = parse(string, errorListener, new ATermParseTreeRetriever());
		return result;
	}
	
	@Override
	public void close() {
	}	
	//
	// PRIVATE
	//
	private Expression parse(String string, Parser.ErrorListener errorListener, ParseTreeRetriever parseTreeRetriever) 
			throws RecognitionException, UnableToParseAllTheInputError, HOGModelException {
		Expression result = null;
		
		AntlrErrorListener antlrErrorListener = new AntlrErrorListener(errorListener);

		ANTLRInputStream input = new ANTLRInputStream(string);
		HOGMLexer        lexer = new HOGMLexer(input);
		
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		HOGMParser        parser = new HOGMParser(tokens);
		
		lexer.removeErrorListeners();
		parser.removeErrorListeners();
		lexer.addErrorListener(antlrErrorListener);
		parser.addErrorListener(antlrErrorListener);

		ParseTree tree = parseTreeRetriever.retrieve(parser);

		boolean eofReached = parser.getInputStream().LA(1) == Recognizer.EOF;

		if (!antlrErrorListener.errorsDetected) {
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
	
	private class AntlrErrorListener extends BaseErrorListener {
		public boolean errorsDetected = false;
		
		private Parser.ErrorListener parserEerrorListener;
		
		public AntlrErrorListener(Parser.ErrorListener parserEerrorListener) {
			this.parserEerrorListener = parserEerrorListener;
		}

		@Override
		public void syntaxError(Recognizer<?, ?> recognizer,
				Object offendingSymbol, int line, int charPositionInLine,
				String msg, RecognitionException e) {
			errorsDetected = true;
			parserEerrorListener.parseError(offendingSymbol, line, charPositionInLine, msg, e);
		}
	}
}
