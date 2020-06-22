package com.sri.ai.praise.core.representation.interfacebased.factor.core.base;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ZeroFactor.ZERO_FACTOR;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.Util.unorderedEquals;

import java.util.ArrayList;
import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsEqualityCheck;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality.DefaultFactorsAreEqual;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality.DefaultFactorsAreOfIncomparableClasses;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality.DefaultFactorsHaveDifferentValues;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.util.explanation.tree.DefaultExplanationTree;
import com.sri.ai.util.explanation.tree.ExplanationTree;

public class ConstantFactor extends AbstractFactorWithVariables {

	Double constant;
	
	public ConstantFactor(Double c) {
		super(arrayList());
		constant = c;
	}
	
	@Override
	public String toString() {
		return "ConstantFactor(" + join(getVariables()) + ") = " + constant.toString();
	}

	public Double getConstant() {
		return constant;
	}

	@Override
	public Factor multiply(Factor another) {
		
		// TODO: given that ConstantFactor is a more fundamental type of Factor,
		// it makes sense the other types know about it, as opposed to it knowing about them as below.
		// Therefore we must invert the responsibility of multiplication between them.
		
		Factor result = null;
		
		if (another instanceof ConstantFactor) {
			ConstantFactor anotherConstant = (ConstantFactor) another;
			result = multiplyTwoConstantFactors(anotherConstant);
		}
		else if (another instanceof ExpressionFactor) {
			ExpressionFactor anotherExpression = (ExpressionFactor) another;
			result = evaluateAsFactor(Times.make(makeSymbol(getConstant()), (Expression) another),anotherExpression.getContext());
		}
		else if (another instanceof ArrayTableFactor) {
			ArrayTableFactor anotherTable = (ArrayTableFactor) another;
			result = multiplyByTableFactor(anotherTable);
		}
		else {
			throw new Error("Unknown class for another : class was " + another.getClass());
		}
		
		return result;
		
	}
	
	private ConstantFactor multiplyTwoConstantFactors(ConstantFactor another) {
		ConstantFactor result;
		Double productConstant = getConstant()*another.getConstant();
		result = identityFactorIfConstantIsOne(productConstant);
		if (result == null) {
			result = zeroFactorIfConstantIsZero(productConstant);
		}
		if (result == null) {
			result = new ConstantFactor(productConstant);
		}
		return result;
	}
	
	private IdentityFactor identityFactorIfConstantIsOne(Double constant) {
		if (Math.abs(constant - 1.) < 0.0000001) {
			return IDENTITY_FACTOR;
		}
		return null;
	}
	
	private ZeroFactor zeroFactorIfConstantIsZero(Double constant) {
		if (Math.abs(constant - 0.) < 0.0000001) {
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
	
	private ArrayTableFactor multiplyByTableFactor(ArrayTableFactor table) {
		ArrayTableFactor result;
		ArrayList<Double> newEntries = new ArrayList<>(table.getEntries().size());
		for (Double entry : table.getEntries()) {
			newEntries.add(getConstant()*entry);
		}
		result = new ArrayTableFactor(table.getVariables(), newEntries);
		return result;
	}

	// sums up to a multiplicative constant
	@Override
	public Factor sumOut(Collection<? extends Variable> variablesToSumOut) {
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
	public Factor normalize() {
		return new UniformFactor(new ArrayList<>(getVariables()));
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
		else if (another instanceof ArrayTableFactor) {
			ArrayTableFactor anotherTable = (ArrayTableFactor) another;
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
		result = identityFactorIfConstantIsOne(prodConstant);
		if (result == null) {
			result = zeroFactorIfConstantIsZero(prodConstant);
		}
		if (result == null) {
			result = new ConstantFactor(prodConstant);
		}
		return result;
	}
	
	private ArrayTableFactor addATableFactor(ArrayTableFactor table) {
		ArrayTableFactor result;
		ArrayList<Double> newEntries = new ArrayList<>(table.getEntries().size());
		for (Double entry : table.getEntries()) {
			newEntries.add(getConstant() + entry);
		}
		result = new ArrayTableFactor(table.getVariables(), newEntries);
		return result;
	}

	@Override
	public Factor invert() {
		if (isZero()) {
			throw new Error("Cannot invert the Zero factor.");
		}
		else {
			Factor result;
			result = new ConstantFactor(1/getConstant());
			return result;
		}
	}

	@Override
	public Factor normalizeBySummingOverThese(Collection<? extends Variable> variablesToNormalize) {
		var remaining = subtract(getVariables(), variablesToNormalize, arrayList());
		return new UniformFactor(remaining);
	}

	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		return this;
	}
	
	@Override
	public double value() {
		return constant;
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
	public int summationCost() {
		return 0; // costs constant time to sum over
	}

	@Override
	public boolean mathematicallyEquals(Factor another) {
		if (another instanceof ConstantFactor) {
			var anotherConstantFactor = (ConstantFactor) another;
			return 
					constant.equals(((ConstantFactor) another).constant) 
					&& 
					unorderedEquals(getVariables(), anotherConstantFactor.getVariables());
		}
		else {
			return false;
		}
	}
	
	@Override
	public FactorsEqualityCheck<Factor> checkEquality(Factor another) {
		if (another instanceof ConstantFactor) {
			var anotherConstantFactor = (ConstantFactor) another;
			var anotherConstant = anotherConstantFactor.constant;
			boolean equals = 					
					constant.equals(anotherConstant) 
					&& 
					unorderedEquals(getVariables(), anotherConstantFactor.getVariables());
			if (equals) {
				return new DefaultFactorsAreEqual<>(this, another);
			}
			else {
				var violatingAssignment = list();
				return new DefaultFactorsHaveDifferentValues<>(this, another, violatingAssignment, constant, anotherConstant);
			}
		}
		else {
			return new DefaultFactorsAreOfIncomparableClasses<>(this, another);
		}
	}

	@Override
	public int memory() {
		return 0;
	}
}
