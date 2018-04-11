package com.sri.ai.praise.inference.gabrielstry.Approximations.core;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import com.sri.ai.praise.inference.gabrielstry.Approximations.api.Approximation;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

public class Polytope implements Approximation{
	
	List<Factor> extremePoints;//Those Factors can't be approximations (TODO: is there a way to restrict that?) 
	
	List<? extends Variable> variables;
	
	public Polytope(Collection<Factor> extremePoints) {
		this(new ArrayList<>(extremePoints));
	}
	
	public Polytope(List<Factor> extremePoints) {
		myAssert(extremePoints != null, ()-> "Should not pass a null list");
		myAssert(!extremePoints.isEmpty(), ()-> "Should not pass an empty list");
		
		this.extremePoints = extremePoints;
		//myAssert(factors) have to have the same variables
		this.variables = extremePoints.get(0).getVariables();
	}
	
	
	@Override
	public boolean contains(Variable variable) {
		if(this.extremePoints == null || this.extremePoints.isEmpty() ) {
			return false;
		}
		return this.variables.contains(variable);
		
	}

	@Override
	public List<? extends Variable> getVariables() {
		return this.variables;
	}

	@Override
	public Factor multiply(Factor another) {
		ArrayList<Factor> newListOfExtremePoints= new ArrayList<>();
		
		for(Factor factor : this.extremePoints) {
			Factor product = another.multiply(factor);
			if(product instanceof Polytope) {
				newListOfExtremePoints.addAll(((Polytope) product).getExtremePoints());
			}
			if(!(product instanceof Approximation)) {
				newListOfExtremePoints.add(product);
			}
		}
		
		Polytope result = new Polytope(newListOfExtremePoints);
		return result;
	}

	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		List<Factor> newListOfExtremePoints = 
				this.extremePoints.stream().map(f -> f.sumOut(variablesToSumOut)).collect(Collectors.toList());
		Polytope result = new Polytope(newListOfExtremePoints);
		return result;
	}

	@Override
	public boolean isIdentity() {
		if(this.extremePoints == null || this.extremePoints.isEmpty()) {
			return false;
		}
		for(Factor f : this.extremePoints) {
			if(!f.isIdentity()) {
				return false;
			}
		}
		this.extremePoints = list(this.extremePoints.get(0));
		return true;
	}

	public List<Factor> getExtremePoints(){
		return this.extremePoints;
	}
/*	@Override
	public Approximation totalIgnoreance(Variable variable, Class<? extends Factor> typeOfFactor) {
		// TODO Auto-generated method stub
		return null;
	}*/
}
