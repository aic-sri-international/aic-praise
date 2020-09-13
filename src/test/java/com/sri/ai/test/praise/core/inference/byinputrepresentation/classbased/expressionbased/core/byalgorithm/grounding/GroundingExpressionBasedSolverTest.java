package com.sri.ai.test.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Timer.timeStringInSeconds;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.util.Util;
import org.junit.jupiter.api.Test;

import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.GroundingExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.solver.HOGMMultiQueryProblemSolver;
import com.sri.ai.util.Timer;

class GroundingExpressionBasedSolverTest {

	@Test
	void test() {
		ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(2);
		
		String modelString;
		String queryString;
		String expectedString;
		
		modelString = ""
				+ "random day: 0..4;"
				+ "random temp: 0..3;"
				+ "if day = 0 "
				+ "    then if temp = 0 then 1 else 0"
				+ "    else 0.25;"
				+ "";
		queryString = "temp";
		expectedString = "if temp = 0 then 0.4 else if temp = 1 then 0.2 else if temp = 2 then 0.2 else 0.2";
		runTest(modelString, queryString, expectedString);
	}

	@Test
	void testNonZeroBasedInteger() {
		ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(2);

		String modelString;
		String queryString;
		String expectedString;

		modelString = ""
				+ "random day: 0..4;"
				+ "random temp: 20..25;"
				+ "if day < 3 "
				+ "    then if temp < 23 then 0.01 else 0.04"
				+ "    else if temp < 23 then 0.02 else 0.03;"
				+ "";
		queryString = "temp";
		expectedString = "if temp = 20 then 0.09 else if temp = 21 then 0.09 else if temp = 22 then 0.09 else if temp = 23 then 0.24 else if temp = 24 then 0.24 else 0.24";
		runTest(modelString, queryString, expectedString);
	}

	@Test
	void testLarger() {
		ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(2);

		String modelString;
		String queryString;
		String expectedString;

		modelString = ""
				+ "random day: 0..999;"
				+ "random temp: 0..499;"
				+ "if day < 50 "
				+ "    then if temp < 20 then 0.01 else 0.04"
				+ "    else if temp < 20 then 0.02 else 0.03;"
				+ "";
		queryString = "temp";
		expectedString = "if temp = 0 then 0 else if temp = 1 then 0 else if temp = 2 then 0 else if temp = 3 then 0 else if temp = 4 then 0 else if temp = 5 then 0 else if temp = 6 then 0 else if temp = 7 then 0 else if temp = 8 then 0 else if temp = 9 then 0 else if temp = 10 then 0 else if temp = 11 then 0 else if temp = 12 then 0 else if temp = 13 then 0 else if temp = 14 then 0 else if temp = 15 then 0 else if temp = 16 then 0 else if temp = 17 then 0 else if temp = 18 then 0 else if temp = 19 then 0 else if temp = 20 then 0 else if temp = 21 then 0 else if temp = 22 then 0 else if temp = 23 then 0 else if temp = 24 then 0 else if temp = 25 then 0 else if temp = 26 then 0 else if temp = 27 then 0 else if temp = 28 then 0 else if temp = 29 then 0 else if temp = 30 then 0 else if temp = 31 then 0 else if temp = 32 then 0 else if temp = 33 then 0 else if temp = 34 then 0 else if temp = 35 then 0 else if temp = 36 then 0 else if temp = 37 then 0 else if temp = 38 then 0 else if temp = 39 then 0 else if temp = 40 then 0 else if temp = 41 then 0 else if temp = 42 then 0 else if temp = 43 then 0 else if temp = 44 then 0 else if temp = 45 then 0 else if temp = 46 then 0 else if temp = 47 then 0 else if temp = 48 then 0 else if temp = 49 then 0 else if temp = 50 then 0 else if temp = 51 then 0 else if temp = 52 then 0 else if temp = 53 then 0 else if temp = 54 then 0 else if temp = 55 then 0 else if temp = 56 then 0 else if temp = 57 then 0 else if temp = 58 then 0 else if temp = 59 then 0 else if temp = 60 then 0 else if temp = 61 then 0 else if temp = 62 then 0 else if temp = 63 then 0 else if temp = 64 then 0 else if temp = 65 then 0 else if temp = 66 then 0 else if temp = 67 then 0 else if temp = 68 then 0 else if temp = 69 then 0 else if temp = 70 then 0 else if temp = 71 then 0 else if temp = 72 then 0 else if temp = 73 then 0 else if temp = 74 then 0 else if temp = 75 then 0 else if temp = 76 then 0 else if temp = 77 then 0 else if temp = 78 then 0 else if temp = 79 then 0 else if temp = 80 then 0 else if temp = 81 then 0 else if temp = 82 then 0 else if temp = 83 then 0 else if temp = 84 then 0 else if temp = 85 then 0 else if temp = 86 then 0 else if temp = 87 then 0 else if temp = 88 then 0 else if temp = 89 then 0 else if temp = 90 then 0 else if temp = 91 then 0 else if temp = 92 then 0 else if temp = 93 then 0 else if temp = 94 then 0 else if temp = 95 then 0 else if temp = 96 then 0 else if temp = 97 then 0 else if temp = 98 then 0 else if temp = 99 then 0 else if temp = 100 then 0 else if temp = 101 then 0 else if temp = 102 then 0 else if temp = 103 then 0 else if temp = 104 then 0 else if temp = 105 then 0 else if temp = 106 then 0 else if temp = 107 then 0 else if temp = 108 then 0 else if temp = 109 then 0 else if temp = 110 then 0 else if temp = 111 then 0 else if temp = 112 then 0 else if temp = 113 then 0 else if temp = 114 then 0 else if temp = 115 then 0 else if temp = 116 then 0 else if temp = 117 then 0 else if temp = 118 then 0 else if temp = 119 then 0 else if temp = 120 then 0 else if temp = 121 then 0 else if temp = 122 then 0 else if temp = 123 then 0 else if temp = 124 then 0 else if temp = 125 then 0 else if temp = 126 then 0 else if temp = 127 then 0 else if temp = 128 then 0 else if temp = 129 then 0 else if temp = 130 then 0 else if temp = 131 then 0 else if temp = 132 then 0 else if temp = 133 then 0 else if temp = 134 then 0 else if temp = 135 then 0 else if temp = 136 then 0 else if temp = 137 then 0 else if temp = 138 then 0 else if temp = 139 then 0 else if temp = 140 then 0 else if temp = 141 then 0 else if temp = 142 then 0 else if temp = 143 then 0 else if temp = 144 then 0 else if temp = 145 then 0 else if temp = 146 then 0 else if temp = 147 then 0 else if temp = 148 then 0 else if temp = 149 then 0 else if temp = 150 then 0 else if temp = 151 then 0 else if temp = 152 then 0 else if temp = 153 then 0 else if temp = 154 then 0 else if temp = 155 then 0 else if temp = 156 then 0 else if temp = 157 then 0 else if temp = 158 then 0 else if temp = 159 then 0 else if temp = 160 then 0 else if temp = 161 then 0 else if temp = 162 then 0 else if temp = 163 then 0 else if temp = 164 then 0 else if temp = 165 then 0 else if temp = 166 then 0 else if temp = 167 then 0 else if temp = 168 then 0 else if temp = 169 then 0 else if temp = 170 then 0 else if temp = 171 then 0 else if temp = 172 then 0 else if temp = 173 then 0 else if temp = 174 then 0 else if temp = 175 then 0 else if temp = 176 then 0 else if temp = 177 then 0 else if temp = 178 then 0 else if temp = 179 then 0 else if temp = 180 then 0 else if temp = 181 then 0 else if temp = 182 then 0 else if temp = 183 then 0 else if temp = 184 then 0 else if temp = 185 then 0 else if temp = 186 then 0 else if temp = 187 then 0 else if temp = 188 then 0 else if temp = 189 then 0 else if temp = 190 then 0 else if temp = 191 then 0 else if temp = 192 then 0 else if temp = 193 then 0 else if temp = 194 then 0 else if temp = 195 then 0 else if temp = 196 then 0 else if temp = 197 then 0 else if temp = 198 then 0 else if temp = 199 then 0 else if temp = 200 then 0 else if temp = 201 then 0 else if temp = 202 then 0 else if temp = 203 then 0 else if temp = 204 then 0 else if temp = 205 then 0 else if temp = 206 then 0 else if temp = 207 then 0 else if temp = 208 then 0 else if temp = 209 then 0 else if temp = 210 then 0 else if temp = 211 then 0 else if temp = 212 then 0 else if temp = 213 then 0 else if temp = 214 then 0 else if temp = 215 then 0 else if temp = 216 then 0 else if temp = 217 then 0 else if temp = 218 then 0 else if temp = 219 then 0 else if temp = 220 then 0 else if temp = 221 then 0 else if temp = 222 then 0 else if temp = 223 then 0 else if temp = 224 then 0 else if temp = 225 then 0 else if temp = 226 then 0 else if temp = 227 then 0 else if temp = 228 then 0 else if temp = 229 then 0 else if temp = 230 then 0 else if temp = 231 then 0 else if temp = 232 then 0 else if temp = 233 then 0 else if temp = 234 then 0 else if temp = 235 then 0 else if temp = 236 then 0 else if temp = 237 then 0 else if temp = 238 then 0 else if temp = 239 then 0 else if temp = 240 then 0 else if temp = 241 then 0 else if temp = 242 then 0 else if temp = 243 then 0 else if temp = 244 then 0 else if temp = 245 then 0 else if temp = 246 then 0 else if temp = 247 then 0 else if temp = 248 then 0 else if temp = 249 then 0 else if temp = 250 then 0 else if temp = 251 then 0 else if temp = 252 then 0 else if temp = 253 then 0 else if temp = 254 then 0 else if temp = 255 then 0 else if temp = 256 then 0 else if temp = 257 then 0 else if temp = 258 then 0 else if temp = 259 then 0 else if temp = 260 then 0 else if temp = 261 then 0 else if temp = 262 then 0 else if temp = 263 then 0 else if temp = 264 then 0 else if temp = 265 then 0 else if temp = 266 then 0 else if temp = 267 then 0 else if temp = 268 then 0 else if temp = 269 then 0 else if temp = 270 then 0 else if temp = 271 then 0 else if temp = 272 then 0 else if temp = 273 then 0 else if temp = 274 then 0 else if temp = 275 then 0 else if temp = 276 then 0 else if temp = 277 then 0 else if temp = 278 then 0 else if temp = 279 then 0 else if temp = 280 then 0 else if temp = 281 then 0 else if temp = 282 then 0 else if temp = 283 then 0 else if temp = 284 then 0 else if temp = 285 then 0 else if temp = 286 then 0 else if temp = 287 then 0 else if temp = 288 then 0 else if temp = 289 then 0 else if temp = 290 then 0 else if temp = 291 then 0 else if temp = 292 then 0 else if temp = 293 then 0 else if temp = 294 then 0 else if temp = 295 then 0 else if temp = 296 then 0 else if temp = 297 then 0 else if temp = 298 then 0 else if temp = 299 then 0 else if temp = 300 then 0 else if temp = 301 then 0 else if temp = 302 then 0 else if temp = 303 then 0 else if temp = 304 then 0 else if temp = 305 then 0 else if temp = 306 then 0 else if temp = 307 then 0 else if temp = 308 then 0 else if temp = 309 then 0 else if temp = 310 then 0 else if temp = 311 then 0 else if temp = 312 then 0 else if temp = 313 then 0 else if temp = 314 then 0 else if temp = 315 then 0 else if temp = 316 then 0 else if temp = 317 then 0 else if temp = 318 then 0 else if temp = 319 then 0 else if temp = 320 then 0 else if temp = 321 then 0 else if temp = 322 then 0 else if temp = 323 then 0 else if temp = 324 then 0 else if temp = 325 then 0 else if temp = 326 then 0 else if temp = 327 then 0 else if temp = 328 then 0 else if temp = 329 then 0 else if temp = 330 then 0 else if temp = 331 then 0 else if temp = 332 then 0 else if temp = 333 then 0 else if temp = 334 then 0 else if temp = 335 then 0 else if temp = 336 then 0 else if temp = 337 then 0 else if temp = 338 then 0 else if temp = 339 then 0 else if temp = 340 then 0 else if temp = 341 then 0 else if temp = 342 then 0 else if temp = 343 then 0 else if temp = 344 then 0 else if temp = 345 then 0 else if temp = 346 then 0 else if temp = 347 then 0 else if temp = 348 then 0 else if temp = 349 then 0 else if temp = 350 then 0 else if temp = 351 then 0 else if temp = 352 then 0 else if temp = 353 then 0 else if temp = 354 then 0 else if temp = 355 then 0 else if temp = 356 then 0 else if temp = 357 then 0 else if temp = 358 then 0 else if temp = 359 then 0 else if temp = 360 then 0 else if temp = 361 then 0 else if temp = 362 then 0 else if temp = 363 then 0 else if temp = 364 then 0 else if temp = 365 then 0 else if temp = 366 then 0 else if temp = 367 then 0 else if temp = 368 then 0 else if temp = 369 then 0 else if temp = 370 then 0 else if temp = 371 then 0 else if temp = 372 then 0 else if temp = 373 then 0 else if temp = 374 then 0 else if temp = 375 then 0 else if temp = 376 then 0 else if temp = 377 then 0 else if temp = 378 then 0 else if temp = 379 then 0 else if temp = 380 then 0 else if temp = 381 then 0 else if temp = 382 then 0 else if temp = 383 then 0 else if temp = 384 then 0 else if temp = 385 then 0 else if temp = 386 then 0 else if temp = 387 then 0 else if temp = 388 then 0 else if temp = 389 then 0 else if temp = 390 then 0 else if temp = 391 then 0 else if temp = 392 then 0 else if temp = 393 then 0 else if temp = 394 then 0 else if temp = 395 then 0 else if temp = 396 then 0 else if temp = 397 then 0 else if temp = 398 then 0 else if temp = 399 then 0 else if temp = 400 then 0 else if temp = 401 then 0 else if temp = 402 then 0 else if temp = 403 then 0 else if temp = 404 then 0 else if temp = 405 then 0 else if temp = 406 then 0 else if temp = 407 then 0 else if temp = 408 then 0 else if temp = 409 then 0 else if temp = 410 then 0 else if temp = 411 then 0 else if temp = 412 then 0 else if temp = 413 then 0 else if temp = 414 then 0 else if temp = 415 then 0 else if temp = 416 then 0 else if temp = 417 then 0 else if temp = 418 then 0 else if temp = 419 then 0 else if temp = 420 then 0 else if temp = 421 then 0 else if temp = 422 then 0 else if temp = 423 then 0 else if temp = 424 then 0 else if temp = 425 then 0 else if temp = 426 then 0 else if temp = 427 then 0 else if temp = 428 then 0 else if temp = 429 then 0 else if temp = 430 then 0 else if temp = 431 then 0 else if temp = 432 then 0 else if temp = 433 then 0 else if temp = 434 then 0 else if temp = 435 then 0 else if temp = 436 then 0 else if temp = 437 then 0 else if temp = 438 then 0 else if temp = 439 then 0 else if temp = 440 then 0 else if temp = 441 then 0 else if temp = 442 then 0 else if temp = 443 then 0 else if temp = 444 then 0 else if temp = 445 then 0 else if temp = 446 then 0 else if temp = 447 then 0 else if temp = 448 then 0 else if temp = 449 then 0 else if temp = 450 then 0 else if temp = 451 then 0 else if temp = 452 then 0 else if temp = 453 then 0 else if temp = 454 then 0 else if temp = 455 then 0 else if temp = 456 then 0 else if temp = 457 then 0 else if temp = 458 then 0 else if temp = 459 then 0 else if temp = 460 then 0 else if temp = 461 then 0 else if temp = 462 then 0 else if temp = 463 then 0 else if temp = 464 then 0 else if temp = 465 then 0 else if temp = 466 then 0 else if temp = 467 then 0 else if temp = 468 then 0 else if temp = 469 then 0 else if temp = 470 then 0 else if temp = 471 then 0 else if temp = 472 then 0 else if temp = 473 then 0 else if temp = 474 then 0 else if temp = 475 then 0 else if temp = 476 then 0 else if temp = 477 then 0 else if temp = 478 then 0 else if temp = 479 then 0 else if temp = 480 then 0 else if temp = 481 then 0 else if temp = 482 then 0 else if temp = 483 then 0 else if temp = 484 then 0 else if temp = 485 then 0 else if temp = 486 then 0 else if temp = 487 then 0 else if temp = 488 then 0 else if temp = 489 then 0 else if temp = 490 then 0 else if temp = 491 then 0 else if temp = 492 then 0 else if temp = 493 then 0 else if temp = 494 then 0 else if temp = 495 then 0 else if temp = 496 then 0 else if temp = 497 then 0 else if temp = 498 then 0 else 0";
		runTest(modelString, queryString, expectedString);
	}

	@Test
	void testCategorical() {
		ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(2);

		String modelString;
		String queryString;
		String expectedString;

		modelString = ""
				+ "sort Strength: 3, weak, medium, strong;"
				+ "random earthquake: Strength;"
				+ "random burglary: Boolean;"
				+ "random temp: 0..15;"
				+ "random alarm: Boolean;"
				+ "earthquake = strong or earthquake = medium;"
				+ "if burglary then alarm 0.9 else if earthquake = strong then alarm 0.7 else if temp > 10 then alarm 0.2 else alarm 0.1;"
				+ "";
		queryString = "alarm";
		expectedString = "if not alarm then 0.34 else 0.66";
		runTest(modelString, queryString, expectedString);
	}

	@Test
	void testCategoricalAndNonZeroBasedInteger() {
		ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(2);

		String modelString;
		String queryString;
		String expectedString;

		modelString = ""
				+ "sort Strength: 3, weak, medium, strong;"
				+ "random earthquake: Strength;"
				+ "random burglary: Boolean;"
				+ "random temp: 20..35;"
				+ "random alarm: Boolean;"
				+ "earthquake = strong or earthquake = medium;"
				+ "if burglary then alarm 0.9 else alarm 0.1;"
				+ "if burglary then alarm 0.9 else if earthquake = strong then alarm 0.7 else if temp > 30 then alarm 0.2 else alarm 0.1;"
				+ "";

		queryString = "alarm";
		expectedString = "if not alarm then 0.39 else 0.61";
		runTest(modelString, queryString, expectedString);

		queryString = "earthquake";
		expectedString = "if earthquake = weak then 0 else if earthquake = medium then 0.58 else 0.42";
		runTest(modelString, queryString, expectedString);

		queryString = "temp";
		expectedString = "if temp = 20 then 0.06 else if temp = 21 then 0.06 else if temp = 22 then 0.06 else if temp = 23 then 0.06 else if temp = 24 then 0.06 else if temp = 25 then 0.06 else if temp = 26 then 0.06 else if temp = 27 then 0.06 else if temp = 28 then 0.06 else if temp = 29 then 0.06 else if temp = 30 then 0.06 else if temp = 31 then 0.06 else if temp = 32 then 0.06 else if temp = 33 then 0.06 else if temp = 34 then 0.06 else 0.06";
		runTest(modelString, queryString, expectedString);

		queryString = "temp >= 30";
		expectedString = "if not (temp >= 30) then 0.63 else 0.37";
		runTest(modelString, queryString, expectedString);

		queryString = "temp < 30 and temp >= 30";
		expectedString = "if not ((temp < 30) and (temp >= 30)) then 1 else 0";
		runTest(modelString, queryString, expectedString);

		queryString = "temp < 30 or temp >= 30";
		expectedString = "if not ((temp < 30) or (temp >= 30)) then 0 else 1";
		runTest(modelString, queryString, expectedString);
	}

	private void runTest(String modelString, String queryString, String expectedString) {
		println();
		var resultsAndTime = 
				Timer.timed(
						() -> 
						new HOGMMultiQueryProblemSolver(modelString, queryString, new GroundingExpressionBasedSolver())
						.getResults());
		
		println("Time: " + timeStringInSeconds(resultsAndTime, 4));
		assertEquals(resultsAndTime.first.size(), 1);
		Expression actual = Util.getFirst(resultsAndTime.first).getResult();
		println("Actual  : " + actual);
		println("Expected: " + expectedString);
		assertEquals(expectedString, actual.toString());
	}

}
