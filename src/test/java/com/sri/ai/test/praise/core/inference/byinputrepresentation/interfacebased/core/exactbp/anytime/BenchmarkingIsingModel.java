package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime;

import java.util.LinkedList;

import com.sri.ai.util.Util;

public class BenchmarkingIsingModel {
	public static void main(String[] args) {
		LinkedList<Integer> complexitiesForExactBP = Util.list(2,3,4,5,6,7,8,9,10); // 10 is too much I think
		int complexityForAnytime = 20;
		
		Util.list("gridModelWithRandomFactors","isingModelGridWithWeigthsAndPotetialFixed","isingModelGridWithWeigthsAndPotetialNormalyDistributed");
		
		
		
		
		Util.println("df");
	}
}
