package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample;

import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Arrays;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.util.number.representation.api.ArithmeticNumber;
import com.sri.ai.util.number.representation.core.ArithmeticDouble;

public class DoublePotential extends ArithmeticDouble implements Potential {

	public DoublePotential(double value) {
		this(new ArithmeticDouble(value));
	}

	public DoublePotential(ArithmeticDouble another) {
		super(another);
	}

	private DoublePotential wrapper(ArithmeticDouble another) {
		return new DoublePotential(another);
	}
	
	@Override
	public DoublePotential add(ArithmeticNumber another) {
		return wrapper(super.add(another));
	}

	@Override
	public DoublePotential subtract(ArithmeticNumber another) {
		return wrapper(super.subtract(another));
	}

	@Override
	public DoublePotential multiply(ArithmeticNumber another) {
		return wrapper(super.multiply(another));
	}

	@Override
	public DoublePotential divide(ArithmeticNumber another) {
		return wrapper(super.divide(another));
	}

	@Override
	public DoublePotential pow(ArithmeticNumber another) {
		return wrapper(super.pow(another));
	}
	
	@Override
	public DoublePotential one() {
		return new DoublePotential(super.one());
	}
	
	@Override
	public DoublePotential zero() {
		return new DoublePotential(super.zero());
	}

	public static ArrayList<Potential> arrayListOfDoublePotentials(Double... doubles) {
		return mapIntoArrayList(Arrays.asList(doubles), d -> new DoublePotential(d));
	}

}
