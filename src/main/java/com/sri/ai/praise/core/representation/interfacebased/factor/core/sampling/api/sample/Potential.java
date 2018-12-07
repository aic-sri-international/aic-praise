package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample;

import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.Util;
import com.sri.ai.util.number.representation.api.ArithmeticNumber;

public interface Potential extends ArithmeticNumber {

	@Override
	Potential add(ArithmeticNumber another);
	
	@Override
	Potential subtract(ArithmeticNumber another);
	
	@Override
	Potential multiply(ArithmeticNumber another);
	
	@Override
	Potential divide(ArithmeticNumber another);
	
	@Override
	Potential pow(ArithmeticNumber another);

	public static Potential make(double number) {
		return getThreadPotentialFactory().make(number);
	}
	
	public static Map<Thread, PotentialFactory> threadPotentialFactory = map();
	
	public static void setThreadPotentialFactory(PotentialFactory potentialFactory) {
		threadPotentialFactory.put(Thread.currentThread(), potentialFactory);
	}
	
	public static PotentialFactory getThreadPotentialFactory() {
		return Util.getValuePossiblyCreatingIt(threadPotentialFactory, Thread.currentThread(), (key) -> new DoublePotentialFactory());
	}
}
