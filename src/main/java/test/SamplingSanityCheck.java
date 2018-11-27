package test;

import static com.sri.ai.util.Util.println;

import org.apache.commons.math3.distribution.NormalDistribution;

import com.sri.ai.util.number.representation.core.ArithmeticDoubleFactory;
import com.sri.ai.util.number.statistics.core.MeanAndVariance;

public class SamplingSanityCheck {

	public static void main(String[] args) {
		
		NormalDistribution n10And4 = new NormalDistribution(10, 2);
		
		int N = 500000;
		
		double[] samples = new double[N + 1];
		
		double currentTotal = 0;
		double currentMean = 0;
		double currentVarianceTotal = 0;
		double currentVariance = 0;
		
		MeanAndVariance meanAndVariance = new MeanAndVariance(new ArithmeticDoubleFactory());
		
		for (int i = 0; i != N; i++) {
			
			double z = n10And4.sample();
			
			NormalDistribution nZAnd4 = new NormalDistribution(z, 2);
			double y = nZAnd4.sample();
			
			NormalDistribution nYAnd4 = new NormalDistribution(y, 2);
			double x = nYAnd4.sample();
			
			samples[i] = x;
			
			currentTotal += x;
			currentMean = currentTotal/(i + 1.0);
			currentVarianceTotal += Math.pow(x - currentMean, 2);
			currentVariance = currentVarianceTotal/(i + 1.0);
//			println("Current mean:" + currentMean);
//			println("Current variance total:" + currentVarianceTotal);
//			println("Current variance:" + currentVariance);
			
			meanAndVariance.add(x);
		}
		
		double total = 0;
		for (int i = 0; i != N; i++) {
			total += samples[i];
		}
		double mean = total/N;
		
		double varianceTotal = 0;
		for (int i = 0; i != N; i++) {
			varianceTotal += Math.pow(samples[i] - mean, 2);
		}
		
		double variance = varianceTotal/(N - 1);

		println("Correct variance: " + variance);

		println("Current mean: " + currentMean);
		println("Current variance: " + currentVariance);
		
		println("Mean and variance: " + meanAndVariance.getValue());
		
	}
}
