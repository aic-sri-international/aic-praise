#-------------------------------------------------------------------------------
# Copyright (c) 2013, SRI International
# All rights reserved.
# Licensed under the The BSD 3-Clause License;
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at:
# 
# http://opensource.org/licenses/BSD-3-Clause
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 
# Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
# 
# Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
# 
# Neither the name of the aic-expresso nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGE.
#-------------------------------------------------------------------------------
The ANTLR LPI parser has four stages, each of which is defined in its own
file: lexer, parser, associative node walker, and output walker.  

The lexer tokenizes the input.  

The parser inspects the token stream and matches it against recognized 
grammar rules and creates an AST.  

The associative node walker identifies chains of identical associative 
nodes and consolidates them into a single node with all of the children.  

And the output walker takes the AST in ANTLR's native CommonTree type and 
converts it into the expected CompoundSyntaxTree and Symbol 
node AST.


Adding new grammar rules
========================
Below is an overview of how to add new grammar rules to the parser.  
Detailed instructions for how to add new grammar rules to the parser
can be found in the appropriate files.

In the lexer:  
Add any new terminal symbols as constants.

In the parser: 
Add the new grammar rules and define the AST node structure for the rule.
Add function and symbol forms of the new rules if the rule uses only
one terminal symbol.

In the associative node walker:
If the new rule is associative, add to the bottom-up rule to consolidate
consecutive associative operation nodes.

In the output walker:
Add to the expr rule to convert from ANTLR's native AST node type to
the appropriate output type for the new grammar rules.
