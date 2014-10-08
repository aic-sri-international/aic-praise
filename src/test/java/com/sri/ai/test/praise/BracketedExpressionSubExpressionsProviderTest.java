package com.sri.ai.test.praise;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.praise.BracketedExpressionSubExpressionsProvider;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.example.TrivialPQR;

public class BracketedExpressionSubExpressionsProviderTest extends AbstractLPITest {

	@Test
	public void testGetInjectiveFunctionToken() {
		RewritingProcess process = newRewritingProcess(Expressions.TRUE);
		Model model = new TrivialPQR();
		Expression modelExpression = parse(model.getModelDeclaration());
		process = Model.setRewritingProcessesModel(modelExpression, model.getKnownRandomVariableNameAndArities(), process);
		
		BracketedExpressionSubExpressionsProvider provider = new BracketedExpressionSubExpressionsProvider();
		
		Assert.assertEquals(parse("lambda X1 : [ p(X1) ]"), provider.getInjectiveFunctionToken(parse("[p(X)]"), process));
		Assert.assertEquals(parse("lambda X1 : [ p(X1) ]"), provider.getInjectiveFunctionToken(parse("[p(X1)]"), process));
		
		Assert.assertEquals(parse("lambda X1, X2 : [ q(X1, X2) ]"), provider.getInjectiveFunctionToken(parse("[q(X, Y)]"), process));
		Assert.assertEquals(parse("lambda X1, X2 : [ q(X1, X2) ]"), provider.getInjectiveFunctionToken(parse("[q(X, X)]"), process));
		Assert.assertEquals(parse("lambda X1, X2 : [ q(X1, X2) ]"), provider.getInjectiveFunctionToken(parse("[q(X, X1)]"), process));

		Assert.assertEquals(parse("lambda X1, X2, X3 : [ p(X1) and q(X2, X3) ]"), provider.getInjectiveFunctionToken(parse("[p(X) and q(Y, Z)]"), process));
		Assert.assertEquals(parse("lambda X1, X2, X3 : [ p(X1) and q(X2, X3) ]"), provider.getInjectiveFunctionToken(parse("[p(X) and q(X, Y)]"), process));
	}
}
