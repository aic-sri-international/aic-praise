package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.ndarray;

import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.Random;

public class ArraySandbox {
	
	public static void main(String[] args) {
		
		long start, end;
		
		Random random = new Random();
		
		int n = 33 * 1000 * 1000;
		
		println("Allocating array");
		start = System.currentTimeMillis();

		double[] array = new double[n];
//		ArrayList<Double> array = new ArrayList<>(n);
//		for (int i = 0; i != n; i++) {
//			array.add(0.0);
//		}
		
		end = System.currentTimeMillis();
		println("Total allocation time: " + (end - start) + " ms.");
		
		println("Filling in array");
		start = System.currentTimeMillis();
		for (int i = 0; i != n; i++) {
			set(array, i, random.nextDouble());
		}
		end = System.currentTimeMillis();
		println("Total filling in time: " + (end - start) + " ms.");
		
		println("Operating linearly in array");
		start = System.currentTimeMillis();
		for (int i = 0; i < n; i++) {
			set(array, i, get(array, i) * get(array, i));
		}
		end = System.currentTimeMillis();
		println("Total time for operating: " + (end - start) + " ms.");
		
		println("Operating linearly by strides in array");
		start = System.currentTimeMillis();
		int stride = 8;
		for (int j = 0; j < stride; j++) {
			for (int i = 0; i < n; i += stride) {
				set(array, i, get(array, i) * get(array, i));
			}
		}
		end = System.currentTimeMillis();
		println("Total time for operating linearly by strides: " + (end - start) + " ms.");
		
		println("Operating by non-linear access in array");
		start = System.currentTimeMillis();
		for (int i = 0; i != n; i++) {
			int arbitraryIndex = (i * 7) % n;
			set(array, arbitraryIndex, get(array, arbitraryIndex) * get(array, arbitraryIndex));
		}
		end = System.currentTimeMillis();
		println("Total time for operating by non-linear access: " + (end - start) + " ms.");
		
		println("Done");
	}
	
	static double get(double[] array, int i) {
		return array[i];
	}
	
	static void set(double[] array, int i, double value) {
		array[i] = value;
	}
	
	static double get(ArrayList<Double> array, int i) {
		return array.get(i);
	}
	
	static void set(ArrayList<Double> array, int i, double value) {
		array.set(i, value);
	}

}
