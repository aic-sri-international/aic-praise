package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.arrayTableFactor;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.round;
import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api.AnytimeSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.algorithm.AnytimeExactBP;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.node.api.AnytimeExactBPNodeWithSimplification;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.node.core.AnytimeExactBPNodeWithIdentitySimplification;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.node.core.AnytimeExactBPNodeWithMinimumBasedSimplificationWithForcedSimplification;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.api.ExactBPNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBPRootNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.util.computation.anytime.api.Approximation;


public class AnytimeExactBPWithSimplificationTest {
	
	@BeforeEach
	public void before() {
		ArrayTableFactor.decimalPlaces = 2;
		ArrayTableFactor.maximumNumberOfEntriesToShow = 20;
	}

	@Test
	public void testSmallCycle() {
		
		FactorNetwork factorNetwork;
		Variable query;
		
		TableVariable a = new TableVariable("a", 2);
		TableVariable b = new TableVariable("b", 2);
		TableVariable c = new TableVariable("c", 2);
		TableVariable d = new TableVariable("d", 2);
		
		factorNetwork = new DefaultFactorNetwork(
				arrayTableFactor(
						list(d), 
						(vd) -> 
						vd == 0? 0.5: 0.5),
				arrayTableFactor(
						list(a, d), 
						(va, vd) -> 
						vd == 0? 
								va == 1? 0.8 : 0.2 :  
								va == 1? 0.9 : 0.1),
				arrayTableFactor(
						list(c, d), 
						(vc, vd) -> 
						vd == 0? 
								vc == 1? 0.7 : 0.3 : 
								vc == 1? 0.6 : 0.4),
				arrayTableFactor(
						list(a, b, c), 
						(va, vb, vc) -> vb == va? 1.0 : 0.0)
				);
		
		query = b;
		
		var expectedIdentitySimplificationHistory = list(
				"", 
				"Message   : b   <----   phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]", 
				"Simplified: {(on a, c) phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]}", 
				"to        : {(on a, c) phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]}", 
				"", 
				"Message   :    <----   b", 
				"Simplified: {(on a, c) phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]}", 
				"to        : {(on a, c) phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]}", 
				"", 
				"Message   : a   <----   phi[a, d]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a", 
				"Simplified: {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c", 
				"Updated   : Simplex(c)", 
				"to        : Simplex(c)", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c", 
				"Simplified: Simplex(c)", 
				"to        : Simplex(c)", 
				"", 
				"Message   : b   <----   phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]", 
				"Simplified: {(on c, d) phi[d, b, c]: [0.2, 0.2, 0.8, 0.8, 0.1, 0.1, 0.9, 0.9]}", 
				"to        : {(on c, d) phi[d, b, c]: [0.2, 0.2, 0.8, 0.8, 0.1, 0.1, 0.9, 0.9]}", 
				"", 
				"Message   :    <----   b", 
				"Simplified: {(on c, d) phi[d, b, c]: [0.2, 0.2, 0.8, 0.8, 0.1, 0.1, 0.9, 0.9]}", 
				"to        : {(on c, d) phi[d, b, c]: [0.2, 0.2, 0.8, 0.8, 0.1, 0.1, 0.9, 0.9]}", 
				"", 
				"Message   : c   <----   phi[c, d]: [0.3, 0.4, 0.7, 0.6]", 
				"Simplified: {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"to        : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c", 
				"Simplified: {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"to        : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a", 
				"Updated   : {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : Simplex(d)*{phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a", 
				"Simplified: Simplex(d)*{phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : Simplex(d)*{phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : b   <----   phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]", 
				"Simplified: {(on d) phi[d, b]: [0.2, 0.8, 0.1, 0.9]}", 
				"to        : {(on d) phi[d, b]: [0.2, 0.8, 0.1, 0.9]}", 
				"", 
				"Message   :    <----   b", 
				"Simplified: {(on d) phi[d, b]: [0.2, 0.8, 0.1, 0.9]}", 
				"to        : {(on d) phi[d, b]: [0.2, 0.8, 0.1, 0.9]}", 
				"", 
				"Message   : d   <----   phi[d]: [0.5, 0.5]", 
				"Simplified: {phi[d]: [0.5, 0.5]}", 
				"to        : {phi[d]: [0.5, 0.5]}", 
				"", 
				"Message   : phi[a, d]: [0.2, 0.1, 0.8, 0.9]   <----   d", 
				"Simplified: {phi[d]: [0.5, 0.5]}", 
				"to        : {phi[d]: [0.5, 0.5]}", 
				"", 
				"Message   : a   <----   phi[a, d]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a", 
				"Simplified: {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c", 
				"Updated   : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"to        : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"", 
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c", 
				"Simplified: {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"to        : {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}", 
				"", 
				"Message   : b   <----   phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]", 
				"Simplified: {phi[b]: [0.3, 1.7]}", 
				"to        : {phi[b]: [0.3, 1.7]}", 
				"", 
				"Message   :    <----   b", 
				"Simplified: {phi[b]: [0.3, 1.7]}", 
				"to        : {phi[b]: [0.3, 1.7]}"
				);
		
		var expectedMinimumBasedSimplificationHistory = list(
				"",
				"Message   : b   <----   phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]",
				"Simplified: {(on a, c) phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]}",
				"to        : {(on I0) phi[I0, b]: [1.0, 0.0, 0.0, 1.0]}",
				"",
				"Message   :    <----   b",
				"Simplified: {(on I0) phi[I0, b]: [1.0, 0.0, 0.0, 1.0]}",
				"to        : {(on I0) phi[I0, b]: [1.0, 0.0, 0.0, 1.0]}",
				"",
				"Message   : a   <----   phi[a, d]: [0.2, 0.1, 0.8, 0.9]",
				"Simplified: {(on d) phi[a, d]: [0.2, 0.1, 0.8, 0.9]}",
				"to        : {(on I0) phi[I0, a]: [0.2, 0.8, 0.1, 0.9]}",
				"",
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a",
				"Simplified: {(on I0) phi[I0, a]: [0.2, 0.8, 0.1, 0.9]}",
				"to        : {(on I0) phi[I0, a]: [0.2, 0.8, 0.1, 0.9]}",
				"",
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c",
				"Updated   : Simplex(c)",
				"to        : Simplex(c)",
				"",
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c",
				"Simplified: Simplex(c)",
				"to        : Simplex(c)",
				"",
				"Message   : b   <----   phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]",
				"Simplified: {(on c, I0) phi[I0, b, c]: [0.2, 0.2, 0.8, 0.8, 0.1, 0.1, 0.9, 0.9]}",
				"to        : {(on I0) phi[I0, b]: [0.2, 0.8, 0.1, 0.9]}",
				"",
				"Message   :    <----   b",
				"Simplified: {(on I0) phi[I0, b]: [0.2, 0.8, 0.1, 0.9]}",
				"to        : {(on I0) phi[I0, b]: [0.2, 0.8, 0.1, 0.9]}",
				"",
				"Message   : c   <----   phi[c, d]: [0.3, 0.4, 0.7, 0.6]",
				"Simplified: {phi[c, d]: [0.3, 0.4, 0.7, 0.6]}",
				"to        : {(on I0, I1) phi[I0, I1, c, d]: [0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3]}",
				"",
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c",
				"Simplified: {(on I0, I1) phi[I0, I1, c, d]: [0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3]}",
				"to        : {(on I0, I1) phi[I0, I1, c, d]: [0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3]}",
				"",
				"Message   : a   <----   phi[a, d]: [0.2, 0.1, 0.8, 0.9]",
				"Updated   : {(on I0) phi[I0, a]: [0.2, 0.8, 0.1, 0.9]}",
				"to        : {(on I0) phi[I0, a]: [0.2, 0.8, 0.1, 0.9]}",
				"",
				"Message   : a   <----   phi[a, d]: [0.2, 0.1, 0.8, 0.9]",
				"Simplified: {(on I0) phi[I0, a]: [0.2, 0.8, 0.1, 0.9]}",
				"to        : {(on I0) phi[I0, a]: [0.2, 0.8, 0.1, 0.9]}",
				"",
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a",
				"Simplified: {(on I0) phi[I0, a]: [0.2, 0.8, 0.1, 0.9]}",
				"to        : {(on I0) phi[I0, a]: [0.2, 0.8, 0.1, 0.9]}",
				"",
				"Message   : b   <----   phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]",
				"Simplified: {(on I0, I1) phi[I1, I0, b]: [0.2, 0.8, 0.1, 0.9, 0.2, 0.8, 0.1, 0.9]}",
				"to        : {(on I0) phi[I0, b]: [0.2, 0.8, 0.1, 0.9]}",
				"",
				"Message   :    <----   b",
				"Simplified: {(on I0) phi[I0, b]: [0.2, 0.8, 0.1, 0.9]}",
				"to        : {(on I0) phi[I0, b]: [0.2, 0.8, 0.1, 0.9]}",
				"",
				"Message   : d   <----   phi[d]: [0.5, 0.5]",
				"Simplified: {phi[d]: [0.5, 0.5]}",
				"to        : Identity polytope",
				"",
				"Message   : phi[a, d]: [0.2, 0.1, 0.8, 0.9]   <----   d",
				"Simplified: {1}",
				"to        : Identity polytope",
				"",
				"Message   : a   <----   phi[a, d]: [0.2, 0.1, 0.8, 0.9]",
				"Simplified: {phi[a, d]: [0.2, 0.1, 0.8, 0.9]}",
				"to        : {(on I0, I1) phi[I0, I1, a, d]: [0.1, 0.05, 0.4, 0.45, 0.1, 0.05, 0.4, 0.45, 0.1, 0.05, 0.4, 0.45, 0.1, 0.05, 0.4, 0.45]}",
				"",
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   a",
				"Simplified: {(on I0, I1) phi[I0, I1, a, d]: [0.1, 0.05, 0.4, 0.45, 0.1, 0.05, 0.4, 0.45, 0.1, 0.05, 0.4, 0.45, 0.1, 0.05, 0.4, 0.45]}",
				"to        : {(on I0, I1) phi[I0, I1, a, d]: [0.1, 0.05, 0.4, 0.45, 0.1, 0.05, 0.4, 0.45, 0.1, 0.05, 0.4, 0.45, 0.1, 0.05, 0.4, 0.45]}",
				"",
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c",
				"Updated   : {(on I0, I1) phi[I0, I1, c, d]: [0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3]}",
				"to        : {(on I0, I1) phi[I0, I1, c, d]: [0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3]}",
				"",
				"Message   : phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]   <----   c",
				"Simplified: {(on I0, I1) phi[I0, I1, c, d]: [0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3]}",
				"to        : {(on I0, I1) phi[I0, I1, c, d]: [0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3, 0.15, 0.2, 0.35, 0.3]}",
				"",
				"Message   : b   <----   phi[a, b, c]: [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0]",
				"Simplified: {(on I0, I1) phi[I0, I1, b]: [0.07, 0.42, 0.08, 0.42, 0.07, 0.42, 0.07, 0.42]}",
				"to        : {(on I0) phi[I0, b]: [0.15, 0.85, 0.15, 0.85]}",
				"",
				"Message   :    <----   b",
				"Simplified: {(on I0) phi[I0, b]: [0.15, 0.85, 0.15, 0.85]}",
				"to        : {(on I0) phi[I0, b]: [0.15, 0.85, 0.15, 0.85]}");

		runTest(query, factorNetwork, expectedIdentitySimplificationHistory, expectedMinimumBasedSimplificationHistory);
	}


	@Test
	public void testQueryConnectedToIdentityLikeFactor() {
		
		FactorNetwork factorNetwork;
		Variable query;
		
		TableVariable q = new TableVariable("q", 2);
		TableVariable a = new TableVariable("a", 2);
		TableVariable b = new TableVariable("b", 2);
		
		factorNetwork = new DefaultFactorNetwork(
				arrayTableFactor(
						list(q,a), 
						(vq, va) -> 
						va == 0? 
								vq == 0? 0.5 : 0.5 :  
								vq == 0? 0.5 : 0.5)
				// table factor with equal entries is considered identity factor
				,
				arrayTableFactor(
						list(a,b), 
						(va, vb) -> 
						vb == 0? 
								va == 0? 0.4 : 0.6 :  
								va == 0? 0.3 : 0.7)
				);
		
		query = q;
		
		var expectedIdentitySimplificationHistory = list(
				"", 
				"Message   : q   <----   phi[q, a]: [0.5, 0.5, 0.5, 0.5]", 
				"Simplified: Identity polytope", 
				"to        : Identity polytope", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {1}", 
				"to        : {1}", 
				"", 
				"Message   : a   <----   phi[a, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Simplified: {(on b) phi[a, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"to        : {(on b) phi[a, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"", 
				"Message   : phi[q, a]: [0.5, 0.5, 0.5, 0.5]   <----   a", 
				"Simplified: {(on b) phi[a, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"to        : {(on b) phi[a, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.5, 0.5, 0.5, 0.5]", 
				"Simplified: {(on b) phi[b]: [1.0, 1.0]}", 
				"to        : {(on b) phi[b]: [1.0, 1.0]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {(on b) phi[b]: [1.0, 1.0]}", 
				"to        : {(on b) phi[b]: [1.0, 1.0]}", 
				"", 
				"Message   : phi[a, b]: [0.4, 0.3, 0.6, 0.7]   <----   b", 
				"Simplified: {1}", 
				"to        : {1}", 
				"", 
				"Message   : a   <----   phi[a, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Simplified: {phi[a]: [0.7, 1.3]}", 
				"to        : {phi[a]: [0.7, 1.3]}", 
				"", 
				"Message   : phi[q, a]: [0.5, 0.5, 0.5, 0.5]   <----   a", 
				"Simplified: {phi[a]: [0.7, 1.3]}", 
				"to        : {phi[a]: [0.7, 1.3]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.5, 0.5, 0.5, 0.5]", 
				"Simplified: {phi[]: [2.0]}", 
				"to        : {phi[]: [2.0]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {phi[]: [2.0]}", 
				"to        : {phi[]: [2.0]}"
				);
		
		var expectedMinimumBasedSimplificationHistory = list(
				"",
				"Message   : q   <----   phi[q, a]: [0.5, 0.5, 0.5, 0.5]",
				"Simplified: Identity polytope",
				"to        : Identity polytope",
				"",
				"Message   :    <----   q",
				"Simplified: {1}",
				"to        : Identity polytope",
				"",
				"Message   : a   <----   phi[a, b]: [0.4, 0.3, 0.6, 0.7]",
				"Simplified: {(on b) phi[a, b]: [0.4, 0.3, 0.6, 0.7]}",
				"to        : {(on I0) phi[I0, a]: [0.4, 0.6, 0.3, 0.7]}",
				"",
				"Message   : phi[q, a]: [0.5, 0.5, 0.5, 0.5]   <----   a",
				"Simplified: {(on I0) phi[I0, a]: [0.4, 0.6, 0.3, 0.7]}",
				"to        : {(on I0) phi[I0, a]: [0.4, 0.6, 0.3, 0.7]}",
				"",
				"Message   : q   <----   phi[q, a]: [0.5, 0.5, 0.5, 0.5]",
				"Simplified: {(on I0) phi[I0]: [1.0, 1.0]}",
				"to        : {phi[]: [1.0]}",
				"",
				"Message   :    <----   q",
				"Simplified: {phi[]: [1.0]}",
				"to        : Identity polytope",
				"",
				"Message   : phi[a, b]: [0.4, 0.3, 0.6, 0.7]   <----   b",
				"Simplified: {1}",
				"to        : Identity polytope",
				"",
				"Message   : a   <----   phi[a, b]: [0.4, 0.3, 0.6, 0.7]",
				"Simplified: {phi[a]: [0.7, 1.3]}",
				"to        : {(on I0) phi[I0, a]: [0.35, 0.65, 0.35, 0.65]}",
				"",
				"Message   : phi[q, a]: [0.5, 0.5, 0.5, 0.5]   <----   a",
				"Simplified: {(on I0) phi[I0, a]: [0.35, 0.65, 0.35, 0.65]}",
				"to        : {(on I0) phi[I0, a]: [0.35, 0.65, 0.35, 0.65]}",
				"",
				"Message   : q   <----   phi[q, a]: [0.5, 0.5, 0.5, 0.5]",
				"Simplified: {(on I0) phi[I0]: [1.0, 1.0]}",
				"to        : {phi[]: [1.0]}",
				"",
				"Message   :    <----   q",
				"Simplified: {phi[]: [1.0]}",
				"to        : Identity polytope");
		
		runTest(query, factorNetwork, expectedIdentitySimplificationHistory, expectedMinimumBasedSimplificationHistory);
	}

	@Test
	public void testNoCycle() {
		
		FactorNetwork factorNetwork;
		Variable query;
		
		TableVariable q = new TableVariable("q", 2);
		TableVariable a = new TableVariable("a", 2);
		TableVariable b = new TableVariable("b", 2);
		TableVariable c = new TableVariable("c", 2);
		
		factorNetwork = new DefaultFactorNetwork(
				arrayTableFactor(
						list(q,a), 
						(vq, va) -> 
						va == 0? 
								vq == 0? 0.8 : 0.2 :  
								vq == 0? 0.9 : 0.1)
				,
				arrayTableFactor(
						list(q,b), 
						(vq, vb) -> 
						vb == 0? 
								vq == 0? 0.4 : 0.6 :  
								vq == 0? 0.3 : 0.7)
				,
				arrayTableFactor(
						list(b,c), 
						(vb, vc) -> 
						vc == 0? 
								vb == 0? 0.9 : 0.1 :  
								vb == 0? 0.8 : 0.2)
				);
		
		query = q;
		
		var expectedIdentitySimplificationHistory = list(
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Simplified: {(on a) phi[q, a]: [0.8, 0.9, 0.2, 0.1]}", 
				"to        : {(on a) phi[q, a]: [0.8, 0.9, 0.2, 0.1]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Simplified: {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"to        : {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {(on a) phi[q, a]: [0.8, 0.9, 0.2, 0.1]}*{(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"to        : {(on a) phi[q, a]: [0.8, 0.9, 0.2, 0.1]}*{(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"", 
				"Message   : phi[q, a]: [0.8, 0.9, 0.2, 0.1]   <----   a", 
				"Simplified: {1}", 
				"to        : {1}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Simplified: {phi[q]: [1.7, 0.3]}", 
				"to        : {phi[q]: [1.7, 0.3]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Updated   : {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"to        : {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Simplified: {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"to        : {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {phi[q]: [1.7, 0.3]}*{(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"to        : {phi[q]: [1.7, 0.3]}*{(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}", 
				"", 
				"Message   : b   <----   phi[b, c]: [0.9, 0.8, 0.1, 0.2]", 
				"Simplified: {(on c) phi[b, c]: [0.9, 0.8, 0.1, 0.2]}", 
				"to        : {(on c) phi[b, c]: [0.9, 0.8, 0.1, 0.2]}", 
				"", 
				"Message   : phi[q, b]: [0.4, 0.3, 0.6, 0.7]   <----   b", 
				"Simplified: {(on c) phi[b, c]: [0.9, 0.8, 0.1, 0.2]}", 
				"to        : {(on c) phi[b, c]: [0.9, 0.8, 0.1, 0.2]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Simplified: {(on c) phi[c, q]: [0.39, 0.61, 0.38, 0.62]}", 
				"to        : {(on c) phi[c, q]: [0.39, 0.61, 0.38, 0.62]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Updated   : {phi[q]: [1.7, 0.3]}", 
				"to        : {phi[q]: [1.7, 0.3]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Simplified: {phi[q]: [1.7, 0.3]}", 
				"to        : {phi[q]: [1.7, 0.3]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {phi[q]: [1.7, 0.3]}*{(on c) phi[c, q]: [0.39, 0.61, 0.38, 0.62]}", 
				"to        : {phi[q]: [1.7, 0.3]}*{(on c) phi[c, q]: [0.39, 0.61, 0.38, 0.62]}", 
				"", 
				"Message   : phi[b, c]: [0.9, 0.8, 0.1, 0.2]   <----   c", 
				"Simplified: {1}", 
				"to        : {1}", 
				"", 
				"Message   : b   <----   phi[b, c]: [0.9, 0.8, 0.1, 0.2]", 
				"Simplified: {phi[b]: [1.7, 0.3]}", 
				"to        : {phi[b]: [1.7, 0.3]}", 
				"", 
				"Message   : phi[q, b]: [0.4, 0.3, 0.6, 0.7]   <----   b", 
				"Simplified: {phi[b]: [1.7, 0.3]}", 
				"to        : {phi[b]: [1.7, 0.3]}", 
				"", 
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]", 
				"Simplified: {phi[q]: [0.77, 1.23]}", 
				"to        : {phi[q]: [0.77, 1.23]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Updated   : {phi[q]: [1.7, 0.3]}", 
				"to        : {phi[q]: [1.7, 0.3]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]", 
				"Simplified: {phi[q]: [1.7, 0.3]}", 
				"to        : {phi[q]: [1.7, 0.3]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {phi[q]: [1.31, 0.37]}", 
				"to        : {phi[q]: [1.31, 0.37]}"
				);
		
		var expectedMinimumBasedSimplificationHistory = list(
				"",
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]",
				"Simplified: {(on a) phi[q, a]: [0.8, 0.9, 0.2, 0.1]}",
				"to        : {(on I0) phi[I0, q]: [0.9, 0.1, 0.8, 0.2]}",
				"",
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]",
				"Simplified: {(on b) phi[q, b]: [0.4, 0.3, 0.6, 0.7]}",
				"to        : {(on I0) phi[I0, q]: [0.4, 0.6, 0.3, 0.7]}",
				"",
				"Message   :    <----   q",
				"Simplified: {(on I0) phi[I0, q]: [0.36, 0.06, 0.24, 0.14]}",
				"to        : {(on I0) phi[I0, q]: [0.86, 0.14, 0.63, 0.37]}",
				"",
				"Message   : phi[q, a]: [0.8, 0.9, 0.2, 0.1]   <----   a",
				"Simplified: {1}",
				"to        : Identity polytope",
				"",
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]",
				"Simplified: {phi[q]: [1.7, 0.3]}",
				"to        : {(on I0) phi[I0, q]: [0.85, 0.15, 0.85, 0.15]}",
				"",
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]",
				"Updated   : {(on I0) phi[I0, q]: [0.4, 0.6, 0.3, 0.7]}",
				"to        : {(on I0) phi[I0, q]: [0.4, 0.6, 0.3, 0.7]}",
				"",
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]",
				"Simplified: {(on I0) phi[I0, q]: [0.4, 0.6, 0.3, 0.7]}",
				"to        : {(on I0) phi[I0, q]: [0.4, 0.6, 0.3, 0.7]}",
				"",
				"Message   :    <----   q",
				"Simplified: {(on I0) phi[I0, q]: [0.34, 0.09, 0.26, 0.11]}",
				"to        : {(on I0) phi[I0, q]: [0.79, 0.21, 0.71, 0.29]}",
				"",
				"Message   : b   <----   phi[b, c]: [0.9, 0.8, 0.1, 0.2]",
				"Simplified: {(on c) phi[b, c]: [0.9, 0.8, 0.1, 0.2]}",
				"to        : {(on I0) phi[I0, b]: [0.9, 0.1, 0.8, 0.2]}",
				"",
				"Message   : phi[q, b]: [0.4, 0.3, 0.6, 0.7]   <----   b",
				"Simplified: {(on I0) phi[I0, b]: [0.9, 0.1, 0.8, 0.2]}",
				"to        : {(on I0) phi[I0, b]: [0.9, 0.1, 0.8, 0.2]}",
				"",
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]",
				"Simplified: {(on I0) phi[I0, q]: [0.39, 0.61, 0.38, 0.62]}",
				"to        : {(on I0) phi[I0, q]: [0.39, 0.61, 0.38, 0.62]}",
				"",
				"Message   : phi[q, a]: [0.8, 0.9, 0.2, 0.1]   <----   a",
				"Updated   : Identity polytope",
				"to        : Identity polytope",
				"",
				"Message   : phi[q, a]: [0.8, 0.9, 0.2, 0.1]   <----   a",
				"Simplified: Identity polytope",
				"to        : Identity polytope",
				"",
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]",
				"Simplified: {phi[q]: [1.7, 0.3]}",
				"to        : {(on I0) phi[I0, q]: [0.85, 0.15, 0.85, 0.15]}",
				"",
				"Message   :    <----   q",
				"Simplified: {(on I0) phi[I0, q]: [0.33, 0.09, 0.32, 0.09]}",
				"to        : {(on I0) phi[I0, q]: [0.78, 0.22, 0.78, 0.22]}",
				"",
				"Message   : phi[b, c]: [0.9, 0.8, 0.1, 0.2]   <----   c",
				"Simplified: {1}",
				"to        : Identity polytope",
				"",
				"Message   : b   <----   phi[b, c]: [0.9, 0.8, 0.1, 0.2]",
				"Simplified: {phi[b]: [1.7, 0.3]}",
				"to        : {(on I0) phi[I0, b]: [0.85, 0.15, 0.85, 0.15]}",
				"",
				"Message   : phi[q, b]: [0.4, 0.3, 0.6, 0.7]   <----   b",
				"Simplified: {(on I0) phi[I0, b]: [0.85, 0.15, 0.85, 0.15]}",
				"to        : {(on I0) phi[I0, b]: [0.85, 0.15, 0.85, 0.15]}",
				"",
				"Message   : q   <----   phi[q, b]: [0.4, 0.3, 0.6, 0.7]",
				"Simplified: {(on I0) phi[I0, q]: [0.39, 0.62, 0.39, 0.62]}",
				"to        : {(on I0) phi[I0, q]: [0.39, 0.62, 0.39, 0.62]}",
				"",
				"Message   : phi[q, a]: [0.8, 0.9, 0.2, 0.1]   <----   a",
				"Updated   : Identity polytope",
				"to        : Identity polytope",
				"",
				"Message   : phi[q, a]: [0.8, 0.9, 0.2, 0.1]   <----   a",
				"Simplified: Identity polytope",
				"to        : Identity polytope",
				"",
				"Message   : q   <----   phi[q, a]: [0.8, 0.9, 0.2, 0.1]",
				"Simplified: {phi[q]: [1.7, 0.3]}",
				"to        : {(on I0) phi[I0, q]: [0.85, 0.15, 0.85, 0.15]}",
				"",
				"Message   :    <----   q",
				"Simplified: {(on I0) phi[I0, q]: [0.33, 0.09, 0.33, 0.09]}",
				"to        : {(on I0) phi[I0, q]: [0.78, 0.22, 0.78, 0.22]}");
		
		runTest(query, factorNetwork, expectedIdentitySimplificationHistory, expectedMinimumBasedSimplificationHistory);
	}

	@Test
	public void testCycle() {
		
		FactorNetwork factorNetwork;
		Variable query;
		
		TableVariable q = new TableVariable("q", 2);
		TableVariable a = new TableVariable("a", 2);
		TableVariable b = new TableVariable("b", 2);
		TableVariable c = new TableVariable("c", 2);
		
		factorNetwork = new DefaultFactorNetwork(
				arrayTableFactor(
						list(q,a), 
						(vq, va) -> 
						vq == 0? 
								va == 1? 0.8 : 0.2 :  
								va == 1? 0.9 : 0.1),
				arrayTableFactor(
						list(a, b), 
						(va, vb) -> 
						vb == 0? 
								va == 1? 0.8 : 0.2 :  
								va == 1? 0.9 : 0.1),
				arrayTableFactor(
						list(b, c), 
						(vb, vc) -> 
						vc == 0? 
								vb == 1? 0.8 : 0.2 :  
								vb == 1? 0.9 : 0.1),
				arrayTableFactor(
						list(c, a), 
						(vc, va) -> 
						va == 0? 
								vc == 1? 0.8 : 0.2 :  
								vc == 1? 0.9 : 0.1)
				);
		
		query = q;
		
		var expectedIdentitySimplificationHistory = list(
				"", 
				"Message   : q   <----   phi[q, a]: [0.2, 0.8, 0.1, 0.9]", 
				"Simplified: {(on a) phi[q, a]: [0.2, 0.8, 0.1, 0.9]}", 
				"to        : {(on a) phi[q, a]: [0.2, 0.8, 0.1, 0.9]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {(on a) phi[q, a]: [0.2, 0.8, 0.1, 0.9]}", 
				"to        : {(on a) phi[q, a]: [0.2, 0.8, 0.1, 0.9]}", 
				"", 
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {(on b) phi[a, b]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on b) phi[a, b]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[q, a]: [0.2, 0.8, 0.1, 0.9]   <----   a", 
				"Simplified: {(on b) phi[a, b]: [0.2, 0.1, 0.8, 0.9]}*{(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on b) phi[a, b]: [0.2, 0.1, 0.8, 0.9]}*{(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.2, 0.8, 0.1, 0.9]", 
				"Simplified: {(on b, c) phi[b, c, q]: [0.07, 0.08, 0.61, 0.66, 0.08, 0.08, 0.66, 0.74]}", 
				"to        : {(on b, c) phi[b, c, q]: [0.07, 0.08, 0.61, 0.66, 0.08, 0.08, 0.66, 0.74]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {(on b, c) phi[b, c, q]: [0.07, 0.08, 0.61, 0.66, 0.08, 0.08, 0.66, 0.74]}", 
				"to        : {(on b, c) phi[b, c, q]: [0.07, 0.08, 0.61, 0.66, 0.08, 0.08, 0.66, 0.74]}", 
				"", 
				"Message   : phi[a, b]: [0.2, 0.1, 0.8, 0.9]   <----   b", 
				"Simplified: {1}", 
				"to        : {1}", 
				"", 
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {phi[a]: [0.3, 1.7]}", 
				"to        : {phi[a]: [0.3, 1.7]}", 
				"", 
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]", 
				"Updated   : {(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : phi[q, a]: [0.2, 0.8, 0.1, 0.9]   <----   a", 
				"Simplified: {phi[a]: [0.3, 1.7]}*{(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"to        : {phi[a]: [0.3, 1.7]}*{(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.2, 0.8, 0.1, 0.9]", 
				"Simplified: {(on c) phi[q, c]: [0.15, 1.27, 0.16, 1.4]}", 
				"to        : {(on c) phi[q, c]: [0.15, 1.27, 0.16, 1.4]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {(on c) phi[q, c]: [0.15, 1.27, 0.16, 1.4]}", 
				"to        : {(on c) phi[q, c]: [0.15, 1.27, 0.16, 1.4]}", 
				"", 
				"Message   : phi[c, a]: [0.2, 0.1, 0.8, 0.9]   <----   c", 
				"Simplified: {1}", 
				"to        : {1}", 
				"", 
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {phi[a]: [1.0, 1.0]}", 
				"to        : {phi[a]: [1.0, 1.0]}", 
				"", 
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]", 
				"Updated   : {phi[a]: [0.3, 1.7]}", 
				"to        : {phi[a]: [0.3, 1.7]}", 
				"", 
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]", 
				"Simplified: {phi[a]: [0.3, 1.7]}", 
				"to        : {phi[a]: [0.3, 1.7]}", 
				"", 
				"Message   : phi[q, a]: [0.2, 0.8, 0.1, 0.9]   <----   a", 
				"Simplified: {phi[a]: [0.3, 1.7]}", 
				"to        : {phi[a]: [0.3, 1.7]}", 
				"", 
				"Message   : q   <----   phi[q, a]: [0.2, 0.8, 0.1, 0.9]", 
				"Simplified: {phi[q]: [1.42, 1.56]}", 
				"to        : {phi[q]: [1.42, 1.56]}", 
				"", 
				"Message   :    <----   q", 
				"Simplified: {phi[q]: [1.42, 1.56]}", 
				"to        : {phi[q]: [1.42, 1.56]}"
				);
		
		var expectedMinimumBasedSimplificationHistory = list(
				"",
				"Message   : q   <----   phi[q, a]: [0.2, 0.8, 0.1, 0.9]",
				"Simplified: {(on a) phi[q, a]: [0.2, 0.8, 0.1, 0.9]}",
				"to        : {(on I0) phi[I0, q]: [0.67, 0.33, 0.47, 0.53]}",
				"",
				"Message   :    <----   q",
				"Simplified: {(on I0) phi[I0, q]: [0.67, 0.33, 0.47, 0.53]}",
				"to        : {(on I0) phi[I0, q]: [0.67, 0.33, 0.47, 0.53]}",
				"",
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]",
				"Simplified: {(on b) phi[a, b]: [0.2, 0.1, 0.8, 0.9]}",
				"to        : {(on I0) phi[I0, a]: [0.2, 0.8, 0.1, 0.9]}",
				"",
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]",
				"Simplified: {(on c) phi[c, a]: [0.2, 0.1, 0.8, 0.9]}",
				"to        : {(on I0) phi[I0, a]: [0.67, 0.33, 0.47, 0.53]}",
				"",
				"Message   : phi[q, a]: [0.2, 0.8, 0.1, 0.9]   <----   a",
				"Simplified: {(on I0) phi[I0, a]: [0.13, 0.27, 0.05, 0.48]}",
				"to        : {(on I0) phi[I0, a]: [0.33, 0.67, 0.09, 0.91]}",
				"",
				"Message   : q   <----   phi[q, a]: [0.2, 0.8, 0.1, 0.9]",
				"Simplified: {(on I0) phi[I0, q]: [0.6, 0.63, 0.75, 0.83]}",
				"to        : {(on I0) phi[I0, q]: [0.49, 0.51, 0.47, 0.53]}",
				"",
				"Message   :    <----   q",
				"Simplified: {(on I0) phi[I0, q]: [0.49, 0.51, 0.47, 0.53]}",
				"to        : {(on I0) phi[I0, q]: [0.49, 0.51, 0.47, 0.53]}",
				"",
				"Message   : phi[a, b]: [0.2, 0.1, 0.8, 0.9]   <----   b",
				"Simplified: {1}",
				"to        : Identity polytope",
				"",
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]",
				"Simplified: {phi[a]: [0.3, 1.7]}",
				"to        : {(on I0) phi[I0, a]: [0.15, 0.85, 0.15, 0.85]}",
				"",
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]",
				"Updated   : {(on I0) phi[I0, a]: [0.67, 0.33, 0.47, 0.53]}",
				"to        : {(on I0) phi[I0, a]: [0.67, 0.33, 0.47, 0.53]}",
				"",
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]",
				"Simplified: {(on I0) phi[I0, a]: [0.67, 0.33, 0.47, 0.53]}",
				"to        : {(on I0) phi[I0, a]: [0.67, 0.33, 0.47, 0.53]}",
				"",
				"Message   : phi[q, a]: [0.2, 0.8, 0.1, 0.9]   <----   a",
				"Simplified: {(on I0) phi[I0, a]: [0.1, 0.28, 0.07, 0.45]}",
				"to        : {(on I0) phi[I0, a]: [0.26, 0.74, 0.14, 0.86]}",
				"",
				"Message   : q   <----   phi[q, a]: [0.2, 0.8, 0.1, 0.9]",
				"Simplified: {(on I0) phi[I0, q]: [0.64, 0.69, 0.72, 0.79]}",
				"to        : {(on I0) phi[I0, q]: [0.48, 0.52, 0.48, 0.52]}",
				"",
				"Message   :    <----   q",
				"Simplified: {(on I0) phi[I0, q]: [0.48, 0.52, 0.48, 0.52]}",
				"to        : {(on I0) phi[I0, q]: [0.48, 0.52, 0.48, 0.52]}",
				"",
				"Message   : phi[c, a]: [0.2, 0.1, 0.8, 0.9]   <----   c",
				"Simplified: {1}",
				"to        : Identity polytope",
				"",
				"Message   : a   <----   phi[c, a]: [0.2, 0.1, 0.8, 0.9]",
				"Simplified: {phi[a]: [1.0, 1.0]}",
				"to        : Identity polytope",
				"",
				"Message   : phi[a, b]: [0.2, 0.1, 0.8, 0.9]   <----   b",
				"Updated   : Identity polytope",
				"to        : Identity polytope",
				"",
				"Message   : phi[a, b]: [0.2, 0.1, 0.8, 0.9]   <----   b",
				"Simplified: Identity polytope",
				"to        : Identity polytope",
				"",
				"Message   : a   <----   phi[a, b]: [0.2, 0.1, 0.8, 0.9]",
				"Simplified: {phi[a]: [0.3, 1.7]}",
				"to        : {(on I0) phi[I0, a]: [0.15, 0.85, 0.15, 0.85]}",
				"",
				"Message   : phi[q, a]: [0.2, 0.8, 0.1, 0.9]   <----   a",
				"Simplified: {(on I0) phi[I0, a]: [0.15, 0.85, 0.15, 0.85]}",
				"to        : {(on I0) phi[I0, a]: [0.15, 0.85, 0.15, 0.85]}",
				"",
				"Message   : q   <----   phi[q, a]: [0.2, 0.8, 0.1, 0.9]",
				"Simplified: {(on I0) phi[I0, q]: [0.71, 0.78, 0.71, 0.78]}",
				"to        : {(on I0) phi[I0, q]: [0.48, 0.52, 0.48, 0.52]}",
				"",
				"Message   :    <----   q",
				"Simplified: {(on I0) phi[I0, q]: [0.48, 0.52, 0.48, 0.52]}",
				"to        : {(on I0) phi[I0, q]: [0.48, 0.52, 0.48, 0.52]}");				
		
		runTest(query, factorNetwork, expectedIdentitySimplificationHistory, expectedMinimumBasedSimplificationHistory);
	}
	
	private void runTest(
			Variable query, 
			FactorNetwork factorNetwork, 
			List<String> expectedIdentitySimplificationHistory,
			List<String> expectedMinimumBasedSimplificationHistory
			) {
		
		println("Exact: ", new ExactBP().apply(query, factorNetwork).normalize());
		runTest(new AnytimeExactBPWithIdentitySimplificationAndTracing(), query, factorNetwork, expectedIdentitySimplificationHistory);
		runTest(new AnytimeExactBPWithMinimumBasedSimplificationAndTracing(), query, factorNetwork, expectedMinimumBasedSimplificationHistory);
	}

	private void runTest(
			AnytimeSolverWithTracing solver,
			Variable query,
			FactorNetwork factorNetwork,
			List<String> expectedHistory) {
		
		var approximationsIterator = solver.apply(query, factorNetwork);

		while (approximationsIterator.hasNext()) {
			var polytope = (Polytope) approximationsIterator.next();
			println(polytope + ", " + round(polytope.length(), 3));
		}

		println("Trace:");
		println(join("\n", solver.getTrace()));

		if (expectedHistory != null) {
			if (! expectedHistory.equals(solver.getTrace())) {
				printTraceInJava(solver.getTrace());
			}
			assertEquals(expectedHistory, solver.getTrace());
		}
		else {
			printTraceInJava(solver.getTrace());
		}
	}

	private void printTraceInJava(List<String> trace) {
		println("list(\n				\"" + join("\",\n				\"", trace) + "\");");
	}
	
	///////////////// TRACING INTERFACES
	
	private static interface AnytimeSolverWithTracing extends AnytimeSolver {
		List<String> getTrace();
	}

	private static interface AnytimeExactBPNodeWithSimplificationAndTracing<RootType, SubRootType> 
	extends AnytimeExactBPNodeWithSimplification<RootType, SubRootType> {
		List<String> getTrace();
	}

	///////////////// AnytimeSolverWithTracing implementations
	
	private static class AnytimeExactBPWithIdentitySimplificationAndTracing 
	extends AnytimeExactBP 
	implements AnytimeSolverWithTracing {
		
		public List<String> trace;
	
		public AnytimeExactBPWithIdentitySimplificationAndTracing() {
			this.trace = list();
		}
	
		@Override
		protected AnytimeExactBPNodeWithIdentitySimplification<Variable, Factor> makeRootAnytimeExactBPRootNodeNode(ExactBPRootNode exactBPRootNode) {
			return new AnytimeExactBPNodeWithIdentitySimplificationAndTracing<>(exactBPRootNode, trace);
		}
		
		@Override
		public List<String> getTrace() {
			return trace;
		}
		
	}

	private static class AnytimeExactBPWithMinimumBasedSimplificationAndTracing 
	extends AnytimeExactBPWithIdentitySimplificationAndTracing {
		
		@Override
		protected AnytimeExactBPNodeWithMinimumBasedSimplificationAndTracing<Variable, Factor> makeRootAnytimeExactBPRootNodeNode(ExactBPRootNode exactBPRootNode) {
			return new AnytimeExactBPNodeWithMinimumBasedSimplificationAndTracing<>(exactBPRootNode, trace);
		}
		
	}

	///////////////// AnytimeExactBPNodeWithSimplificationAndTracing implementations
	
	//// Common code for all implementations
	
	private static <RootType, SubRootType> void traceSimplification(
			AnytimeExactBPNodeWithSimplificationAndTracing<RootType, SubRootType> node,
			Approximation<Factor> approximation,
			Approximation<Factor> simplification) {

		var root = node.getBase().getRoot();
		var parentifAny = node.getBase().getParent() != null? node.getBase().getParent() : "";
		node.getTrace().add("");
		node.getTrace().add("Message   : " + parentifAny + "   <----   " + root);
		node.getTrace().add("Simplified: " + approximation);
		node.getTrace().add("to        : " + simplification);
	}

	private static <RootType, SubRootType> void traceComputeUpdatedByItselfApproximationGivenThatExternalContextHasChanged(
			AnytimeExactBPNodeWithSimplificationAndTracing<RootType, SubRootType> node,
			Approximation<Factor> currentApproximation,
			Approximation<Factor> updatedApproximation) {
		var root = node.getBase().getRoot();
		var parentifAny = node.getBase().getParent() != null? node.getBase().getParent() : "";
		node.getTrace().add("");
		node.getTrace().add("Message   : " + parentifAny + "   <----   " + root);
		node.getTrace().add("Updated   : " + currentApproximation);
		node.getTrace().add("to        : " + updatedApproximation);
	}

	//// Implementation for identity simplification
	
	private static class AnytimeExactBPNodeWithIdentitySimplificationAndTracing<RootType, SubRootType> 
	extends AnytimeExactBPNodeWithIdentitySimplification<RootType, SubRootType>
	implements AnytimeExactBPNodeWithSimplificationAndTracing<RootType, SubRootType> {
	
		private List<String> trace;
	
		public AnytimeExactBPNodeWithIdentitySimplificationAndTracing(ExactBPNode<RootType, SubRootType> base, List<String> trace) {
			super(base);
			this.trace = trace;
		}
		
		@Override
		protected
		<RootType2, SubRootType2>
		AnytimeExactBPNodeWithIdentitySimplificationAndTracing<RootType2, SubRootType2> 
		newAnytimeExactBPNode(ExactBPNode<RootType2, SubRootType2> base) {
			
			return new AnytimeExactBPNodeWithIdentitySimplificationAndTracing<RootType2, SubRootType2>(base, trace);
		}
		
		@Override
		public Approximation<Factor> simplify(Approximation<Factor> approximation) {
			var simplification = super.simplify(approximation);
			traceSimplification(this, approximation, simplification);
			return simplification;
		}
	
		@Override
		public 
		Approximation<Factor> 
		computeUpdatedByItselfApproximationGivenThatExternalContextHasChanged(Approximation<Factor> currentApproximation) {
			var updatedApproximation = super.computeUpdatedByItselfApproximationGivenThatExternalContextHasChanged(currentApproximation);
			traceComputeUpdatedByItselfApproximationGivenThatExternalContextHasChanged(
					this,
					currentApproximation,
					updatedApproximation);
			return updatedApproximation;
		}



		@Override
		public List<String> getTrace() {
			return trace;
		}
	}

	//// Implementation for minimum-based simplification
	
	private static class AnytimeExactBPNodeWithMinimumBasedSimplificationAndTracing<RootType, SubRootType> 
	extends AnytimeExactBPNodeWithMinimumBasedSimplificationWithForcedSimplification<RootType, SubRootType> 
	implements AnytimeExactBPNodeWithSimplificationAndTracing<RootType, SubRootType> {
	
		private List<String> trace;
	
		public AnytimeExactBPNodeWithMinimumBasedSimplificationAndTracing(ExactBPNode<RootType, SubRootType> base, List<String> trace) {
			super(base);
			this.trace = trace;
		}

		@Override
		protected
		<RootType2, SubRootType2>
		AnytimeExactBPNodeWithMinimumBasedSimplificationAndTracing<RootType2, SubRootType2> newAnytimeExactBPNode(ExactBPNode<RootType2, SubRootType2> base) {
			return new AnytimeExactBPNodeWithMinimumBasedSimplificationAndTracing<RootType2, SubRootType2>(base, trace);
		}
		
		@Override
		public Approximation<Factor> simplify(Approximation<Factor> approximation) {
			var simplification = super.simplify(approximation);
			traceSimplification(this, approximation, simplification);
			return simplification;
		}

		@Override
		public 
		Approximation<Factor> 
		computeUpdatedByItselfApproximationGivenThatExternalContextHasChanged(Approximation<Factor> currentApproximation) {
			var updatedApproximation = super.computeUpdatedByItselfApproximationGivenThatExternalContextHasChanged(currentApproximation);
			traceComputeUpdatedByItselfApproximationGivenThatExternalContextHasChanged(
					this,
					currentApproximation,
					updatedApproximation);
			return updatedApproximation;
		}

		@Override
		public List<String> getTrace() {
			return trace;
		}
	}
}
