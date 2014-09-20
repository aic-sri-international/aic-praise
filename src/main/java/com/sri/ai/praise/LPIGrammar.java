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
package com.sri.ai.praise;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.core.DefaultGrammar;
import com.sri.ai.brewer.parsingexpression.core.Disjunction;
import com.sri.ai.brewer.parsingexpression.core.Kleene;
import com.sri.ai.brewer.parsingexpression.core.NonTerminal;
import com.sri.ai.brewer.parsingexpression.core.Optional;
import com.sri.ai.brewer.parsingexpression.core.Sequence;
import com.sri.ai.brewer.parsingexpression.core.ParsingSymbol;
import com.sri.ai.brewer.parsingexpression.core.Terminal;
import com.sri.ai.brewer.parsingexpression.helper.AssociativeSequence;
import com.sri.ai.brewer.parsingexpression.helper.ParenthesizedNonTerminal;
import com.sri.ai.brewer.parsingexpression.helper.ParsingExpressionForFunctionApplications;
import com.sri.ai.grinder.core.DefaultRewritingProcess;

/**
 * A {@link DefaultGrammar} containing parsing expressions for LBP.
 * 
 * @author braz
 */
@Beta
public class LPIGrammar extends DefaultGrammar {
	private static final long serialVersionUID = 1L;

	//  This should be kept consistent with CommonGrammar.
	public LPIGrammar() {
		DefaultRewritingProcess previousProcess = (DefaultRewritingProcess) DefaultRewritingProcess.getGlobalRewritingProcessForKnowledgeBasedExpressions();
		DefaultRewritingProcess.setGlobalRewritingProcessForKnowledgeBasedExpressions(new DefaultRewritingProcess(null));
		// We create a default rewriting process here so that the expressions about to be constructed are
		// associated with it for order normalization and sub-expressions.
		// This is a way of assigning these expressions a given "language" specified by the process's rewriters and modules (in this case, none).
		// In the future, we will probably want to explicitly specify an expression's language at construction time.
		// The previous process is restored at the end of grammar creation.
		
		setInitialNonTerminal("Expression");

		put("Expression", new Disjunction(
				new Sequence(new Terminal("previous"), new Terminal("message"), new Terminal("to"), new NonTerminal("Expression"), new Terminal("from"), new NonTerminal("Expression")),
				new Sequence(new Terminal("message"), new Terminal("to"), new NonTerminal("Expression"), new Terminal("from"), new NonTerminal("Expression")),
				new Sequence(new Terminal("lambda"), new Kleene(new NonTerminal("Expression")), new Terminal(":"), new NonTerminal("Expression")),
				new Sequence(new Terminal("if"), new NonTerminal("Expression"), new Terminal("then"), new NonTerminal("Expression"), new Terminal("else"), new NonTerminal("Expression")),
				new Sequence(new Terminal("there"), new Terminal("exists"), new NonTerminal("Expression"), new Terminal(":"), new NonTerminal("Expression")),
				new Sequence(new Terminal("for"), new Terminal("all"), new NonTerminal("Expression"), new Terminal(":"), new NonTerminal("Expression")),
				new            Sequence(new NonTerminal("Expression"), new Terminal("=>"),  new NonTerminal("Expression")),
				new            Sequence(new NonTerminal("Expression"), new Terminal("<=>"), new NonTerminal("Expression")),
				new AssociativeSequence(new NonTerminal("Expression"), new Terminal("or"), new NonTerminal("Expression")),
				new AssociativeSequence(new NonTerminal("Expression"), new Terminal("and"), new NonTerminal("Expression")),
				new AssociativeSequence(new NonTerminal("Expression"), new Terminal("="), new NonTerminal("Expression")),
				new            Sequence(new NonTerminal("Expression"), new Terminal("!="), new NonTerminal("Expression")),
				new            Sequence(new NonTerminal("Expression"), new Terminal(">"), new NonTerminal("Expression")),
				new            Sequence(new NonTerminal("Expression"), new Terminal(">="), new NonTerminal("Expression")),
				new            Sequence(new NonTerminal("Expression"), new Terminal("<"), new NonTerminal("Expression")),
				new            Sequence(new NonTerminal("Expression"), new Terminal("<="), new NonTerminal("Expression")),
				new            Sequence(new NonTerminal("Expression"), new Terminal("in"), new NonTerminal("Expression")),
				new AssociativeSequence(new NonTerminal("Expression"), new Terminal("union"), new NonTerminal("Expression")),
				new AssociativeSequence(new NonTerminal("Expression"), new Terminal("+"), new NonTerminal("Expression")),
				new            Sequence(new NonTerminal("Expression"), new Terminal("-"), new NonTerminal("Expression")),
				new AssociativeSequence(new NonTerminal("Expression"), new Terminal("*"), new NonTerminal("Expression")),
				new            Sequence(new NonTerminal("Expression"), new Terminal("/"), new NonTerminal("Expression")),
				new            Sequence(new NonTerminal("Expression"), new Terminal("^"), new NonTerminal("Expression")),
				new            Sequence(new Terminal("-"), new NonTerminal("Expression")),
				new            Sequence(new Terminal("not"), new NonTerminal("Expression")),
				new Sequence(new Terminal("neighbors"), new Terminal("of"), new NonTerminal("Expression"), new Terminal("from"), new NonTerminal("Expression")),
				new Sequence(new Terminal("neighbors"), new Terminal("of"), new Terminal("variable"), new NonTerminal("Expression")),
				new Sequence(new Terminal("neighbors"), new Terminal("of"), new Terminal("factor"), new NonTerminal("Expression")),
				new Sequence(new Terminal("value"), new Terminal("of"), new NonTerminal("Expression")),
				new Sequence(new Terminal("["), new NonTerminal("Expression"), new Terminal("]")), // for random variables and factor shorthand. Should be placed as parser extension.
				new Sequence(new Terminal("{"), new Kleene(new NonTerminal("Expression")), new Terminal("}")),
				new Sequence(new Terminal("{"), new Optional(new Sequence(new Terminal("("), new Terminal("on"), new Kleene(new NonTerminal("Expression")), new Terminal(")"))), new NonTerminal("Expression"), new Optional(new Sequence(new Terminal("|"), new NonTerminal("Expression"))), new Terminal("}")),
				new Sequence(new Terminal("{{"), new Kleene(new NonTerminal("Expression")), new Terminal("}}")),
				new Sequence(new Terminal("{{"), new Optional(new Sequence(new Terminal("("), new Terminal("on"), new Kleene(new NonTerminal("Expression")), new Terminal(")"))), new NonTerminal("Expression"), new Optional(new Sequence(new Terminal("|"), new NonTerminal("Expression"))), new Terminal("}}")),
				new Sequence(new Terminal("|"), new NonTerminal("Expression"), new Terminal("|")),
				new Sequence(new Terminal("("), new Kleene(new NonTerminal("Expression"), new Terminal(","), 2), new Terminal(")")),
				new ParsingExpressionForFunctionApplications(new NonTerminal("Expression")),
				new ParenthesizedNonTerminal("Expression"),
				new ParsingSymbol("Expression")
		));

		if (previousProcess != null) {
			DefaultRewritingProcess.setGlobalRewritingProcessForKnowledgeBasedExpressions(previousProcess);
		}
	}
}
