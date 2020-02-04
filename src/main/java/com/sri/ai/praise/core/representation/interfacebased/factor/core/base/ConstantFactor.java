package com.sri.ai.praise.core.representation.interfacebased.factor.core.base;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ZeroFactor.ZERO_FACTOR;
import static com.sri.ai.util.Util.list;
import static java.util.Collections.unmodifiableList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.util.explanation.tree.DefaultExplanationTree;
import com.sri.ai.util.explanation.tree.ExplanationTree;

public class ConstantFactor implements Factor {

	Double constant;
	
	public ConstantFactor(Double c) {
		constant = c;
	}
	
	@Override
	public String toString() {
		return constant.toString();
	}

	public Double getConstant() {
		return constant;
	}
	
	@Override
	public boolean contains(Variable variable) {
		return false;
	}

	@Override
	public List<? extends Variable> getVariables() {
		return unmodifiableList(list());
	}

	@Override
	public Factor multiply(Factor another) {
		
		Factor result = null;
		
		if (another instanceof ConstantFactor) {
			ConstantFactor anotherConstant = (ConstantFactor) another;
			result = multiplyTwoConstantFactors(anotherConstant);
		}
		
		else if (another instanceof ExpressionFactor) {
			ExpressionFactor anotherExpression = (ExpressionFactor) another;
			result = evaluateAsFactor(Times.make(makeSymbol(getConstant()), (Expression) another),anotherExpression.getContext());
		}
		
		else if (another instanceof TableFactor) {
			TableFactor anotherTable = (TableFactor) another;
			result = multiplyWithTableFactor(anotherTable);
		}
		
		else {
			throw new Error("Unknown class for another : class was " + another.getClass());
		}
		
		return result;
		
	}
	
	private ConstantFactor multiplyTwoConstantFactors(ConstantFactor another) {
		ConstantFactor result;
		Double prodConstant = getConstant()*another.getConstant();
		boolean isIdentityOrZero = false;
		result = checkIsIdentityAndSetsValueToIdentityIfYes(prodConstant, isIdentityOrZero);
		if(!isIdentityOrZero) {
			result = checkIsZeroAndSetsValueToZeroIfYes(prodConstant, isIdentityOrZero);
		}
		if(!isIdentityOrZero) {
			result = new ConstantFactor(prodConstant);
		}
		return result;
	}
	
	private IdentityFactor checkIsIdentityAndSetsValueToIdentityIfYes(Double constant, boolean resultNotNull) {
		if(Math.abs(constant - 1.) < 0.0000001) {
			resultNotNull = true;
			return IDENTITY_FACTOR;
		}
		return null;
	}
	
	private ZeroFactor checkIsZeroAndSetsValueToZeroIfYes(Double constant, boolean resultNotNull) {
		if(Math.abs(constant - 0.) < 0.0000001) {
			resultNotNull = true;
			return ZERO_FACTOR;
		}
		return null;
	}
	
	private Factor evaluateAsFactor(Expression expression, Context context) {
		Expression resultFactorExpression = evaluate(expression, context);
		Factor result = makeFactor(resultFactorExpression, context);
		return result;
	}

	private Expression evaluate(Expression expression, Context context) {
		Expression result = context.evaluate(expression);
		return result;
	}

	private ExpressionFactor makeFactor(Expression expression, Context context) {
		ExpressionFactor result = new DefaultExpressionFactor(expression, context);
		return result;
	}
	
	private TableFactor multiplyWithTableFactor(TableFactor table) {
		TableFactor result;
		ArrayList<Double> newEntries = new ArrayList<>(table.STAY_getEntries().size());
		for (Double entry : table.STAY_getEntries()) {
			newEntries.add(getConstant()*entry);
		}
		result = new TableFactor(table.getVariables(), newEntries);
		return result;
	}

	// sums up to a multiplicative constant
	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		return this;
	}

	@Override
	public boolean isIdentity() {
		return Math.abs(getConstant() - 1.) < 0.0000001;
	}

	@Override
	public boolean isZero() {
		return Math.abs(getConstant() - 0.) < 0.0000001;
	}

	@Override
	public Double getEntryFor(Map<? extends Variable, ? extends Object> variableInstantiations) {
		return getConstant();
	}

	@Override
	public Factor normalize() {
		throw new Error("A constant factor cannot be normalized.");
	}

	@Override
	public Factor add(Factor another) {
		
		Factor result = null;
		
		if (another instanceof ConstantFactor) {
			ConstantFactor anotherConstant = (ConstantFactor) another;
			result = addTwoConstantFactors(anotherConstant);
		}
		
		else if (another instanceof ExpressionFactor) {
			ExpressionFactor anotherExpression = (ExpressionFactor) another;
			result = evaluateAsFactor(Plus.make(makeSymbol(getConstant()), (Expression) another),anotherExpression.getContext());
		}
		
		else if (another instanceof TableFactor) {
			TableFactor anotherTable = (TableFactor) another;
			result = addATableFactor(anotherTable);
		}
		
		else {
			throw new Error("Unknown class for another : class was " + another.getClass());
		}
		
		return result;
		
	}
	
	private ConstantFactor addTwoConstantFactors(ConstantFactor another) {
		ConstantFactor result;
		Double prodConstant = getConstant() + another.getConstant();
		boolean isIdentityOrZero = false;
		result = checkIsIdentityAndSetsValueToIdentityIfYes(prodConstant, isIdentityOrZero);
		if(!isIdentityOrZero) {
			result = checkIsZeroAndSetsValueToZeroIfYes(prodConstant, isIdentityOrZero);
		}
		if(!isIdentityOrZero) {
			result = new ConstantFactor(prodConstant);
		}
		return result;
	}
	
	private TableFactor addATableFactor(TableFactor table) {
		TableFactor result;
		ArrayList<Double> newEntries = new ArrayList<>(table.STAY_getEntries().size());
		for (Double entry : table.STAY_getEntries()) {
			newEntries.add(getConstant() + entry);
		}
		result = new TableFactor(table.getVariables(), newEntries);
		return result;
	}

	@Override
	public Factor ABS_invert() {
		if(isZero()) {
			throw new Error("Cannot invert the Zero factor.");
		}
		else {
			Factor result;
			result = new ConstantFactor(1/getConstant());
			return result;
		}
	}

	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		return this;
	}
	
	private ExplanationTree explanation = DefaultExplanationTree.PLACEHOLDER;

	@Override
	public ExplanationTree getExplanation() {
		return explanation;
	}
	
	@Override
	public void setExplanation(ExplanationTree explanation) {
		this.explanation = explanation;
	}

	@Override
	public Factor argmax(Collection<? extends Variable> variablesToMaximize) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Factor min(Collection<? extends Variable> variablesToMinimize) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Factor argmin(Collection<? extends Variable> variablesToMinimize) {
		// TODO Auto-generated method stub
		return null;
	}
}
