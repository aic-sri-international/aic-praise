/*
 * Copyright (c) 2014, SRI International
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
package com.sri.ai.praise.model.imports.church;

import java.util.Collections;
import java.util.List;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTree;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.imports.church.antlr.ChurchLexer;
import com.sri.ai.praise.imports.church.antlr.ChurchParser;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.Model.ModelError;
import com.sri.ai.praise.model.Model.ModelException;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.util.base.Pair;

/**
 * Utility class for parsing a Church Program and translating it to a HOGMs model.
 * 
 * @author oreilly
 *
 */
@Beta
public class TranslateChurchToModel {
	
	public static void main(String[] args) {
		TranslateChurchToModel translator = new TranslateChurchToModel();
		
		callTranslate(translator, "" 
				+ "(define sunny #t)\n"
				);
	}

	public Pair<Model, List<Expression>> translate(String churchProgram) {
		Pair<Model, List<Expression>> result = null;
		try {
			ErrorListener lexerErrorListener = new ErrorListener("Lexer Error");
			ErrorListener parseErrorListener = new ErrorListener("Parse Error");

			ANTLRInputStream input = new ANTLRInputStream(churchProgram);
			ChurchLexer lexer = new ChurchLexer(input);
			
			CommonTokenStream tokens = new CommonTokenStream(lexer);
			ChurchParser parser = new ChurchParser(tokens);
			
			lexer.removeErrorListeners();
			parser.removeErrorListeners();
			lexer.addErrorListener(lexerErrorListener);
			parser.addErrorListener(parseErrorListener);

			ParseTree tree = parser.parse();
			
			boolean eof = parser.getInputStream().LA(1) == ChurchParser.EOF;
			
			if (!lexerErrorListener.errorsDetected
					&& !parseErrorListener.errorsDetected) {
				if (!eof) {
					System.err
							.println("Unable to parse the complete input model: "
									+ input);
				} else {
					lexer.removeErrorListeners();
					parser.removeErrorListeners();
					ChurchToModelVisitor churchToModelVisitor = new ChurchToModelVisitor();
					Expression modelAndQueriesTuple = churchToModelVisitor.visit(tree);
					
					result = new Pair<Model, List<Expression>>(
								new Model(Tuple.get(modelAndQueriesTuple, 0), Collections.<String>emptySet()),
// TODO - get queries								
								Collections.<Expression>emptyList());
				}
			}
		} catch (RecognitionException re) {
			re.printStackTrace();
		} catch (ModelException me) {
			System.err.println("Model Errors");
			for (ModelError error: me.getErrors()) {
				System.err.println(error);				
			}
			me.printStackTrace();
		} catch (RuntimeException re) {
			re.printStackTrace();
		}
				
		return result;
	}
	
	//
	// PRIVATE
	//
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
	
	private static void callTranslate(TranslateChurchToModel translator, String churchProgram) {
		System.out.println("CHURCH PROGRAM");
		System.out.println("==============");
		System.out.println(churchProgram);
		Pair<Model, List<Expression>> translation = translator.translate(churchProgram);
		System.out.println("--- TRANSLATES TO HOGM --->");
		Model model = translation.first;
		System.out.println("Name:"+model.getName());
		System.out.println("----");
		System.out.println("Description:");
		System.out.println(model.getDescription());
		System.out.println("----");
		for (SortDeclaration sort : model.getSortDeclarations()) {
			System.out.println(sort.getSortDeclaration());
		}
		for (RandomVariableDeclaration rv : model.getRandomVariableDeclarations()) {
			System.out.println(rv.getRandomVariableDeclaration());
		}
		for (Expression parfactor : model.getParfactorsDeclaration().getParfactors()) {
			System.out.println(parfactor);
		}
		System.out.println("--- WITH QUERIES --->");
		for (Expression query : translation.second) {
			System.out.println(query);
		}
	}
}