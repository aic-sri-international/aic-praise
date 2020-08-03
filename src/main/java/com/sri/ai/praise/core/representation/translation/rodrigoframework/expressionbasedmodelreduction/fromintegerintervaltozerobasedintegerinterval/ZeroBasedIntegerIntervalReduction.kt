package com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval

import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api.ExpressionBasedModelReduction

class ZeroBasedIntegerIntervalReduction(override val expressionBasedModel: ExpressionBasedModel)
    : ExpressionBasedModelReduction {
    override val down = FromIntegerIntervalToZeroBasedIntegerInterval(expressionBasedModel)
    override val up = FromZeroBasedIntegerIntervalToIntegerInterval(expressionBasedModel, down.translation)
    override val translation = down.translation
}