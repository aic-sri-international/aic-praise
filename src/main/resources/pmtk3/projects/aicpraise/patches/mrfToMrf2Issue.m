function mrfToMrf2Issue()
    disp('MRF to MRF2 Issue.');
       
    testMisconceptionMRF();
    % TODO - not working currently.
    testMisconceptionMRFtoMRF2();
end

% Issue: appears to be an issue in factorGraphToMrf2.m, line 34:
% 
% edgesWithPots = cell2mat(cellfuncell(@(f)f.domain, epots));
% 
% whereby a row vector is returned, which is inconsistent with edgeEnds
% when compared at line 35.
%
% Proposed Fix:
% After line 32:
%
% epots    = fg.factors(edgeFacNdx);
%
% add the following:
%{
if isrowvec(epots)
    epots = epots';
end
%}
function testMisconceptionMRFtoMRF2()
    disp('Test Misconception MRF2');
    disp('-----------------------');
    [mrfModel a b c d] = makeMisconceptionMRF('enum');
    method = 'Exact';
    mrf2Model = mrfToMrf2(mrfModel, 'method', method);
                   
    clamped = [0 0 0 0]; % i.e. all zeros, no evidence
    [nodeBel, edgeBel, logZ] =  mrf2InferNodesAndEdges(mrf2Model, clamped);
    A = reshape(nodeBel(1, :), 2, 1)
    B = reshape(nodeBel(2, :), 2, 1)
    C = reshape(nodeBel(3, :), 2, 1)
    D = reshape(nodeBel(4, :), 2, 1)

    logZ
    
    disp('---------');
    disp('P(B | c0)');
    disp('---------');
    clamped = [0 0 1 0]; % i.e. c0
    nodeBel = mrf2InferNodesAndEdges(mrf2Model, clamped);
    reshape(nodeBel(2, :), 2, 1)
end

function testMisconceptionMRF()
    disp('Test Misconception MRF');
    disp('----------------------');

    [mrfModel a b c d] = makeMisconceptionMRF('enum');
    
    [nodeBels, logZ, edgeBels] =  mrfInferNodes(mrfModel);
    A = nodeBels{a}.T
    B = nodeBels{b}.T
    C = nodeBels{c}.T
    D = nodeBels{d}.T

    logZ
    
    disp('---------');
    disp('P(B | c0)');
    disp('---------');
    E = sparsevec([c], [1], 4);
    bels = mrfInferQuery(mrfModel, [b], 'clamped', E);
    bels.T
end

%
% Based on the 'Misconception Example' from: 
% Probabilistic Graphical Models - Principles and Techniques
% (Daphne Koller and Nir Fieldman)
% pg. 104.
function [mrfModel a b c d] = makeMisconceptionMRF(infEngine)
    a = 1;
    b = 2;
    c = 3;
    d = 4;
    nNodes = 4;
    G = zeros(nNodes);
    
    % Note to calculate (see mrfCreate.m):
    % list them according to the linear indexing of G,
    % i.e. so that the kth edge is the one
    % between nodes i and j in this code:
    % edges = find(tril(G));
    % [i, j] = ind2sub(size(G), edges(k))
    eAtoB = 1;
    eBtoC = 3;
    eCtoD = 4;
    eDtoA = 2;
    
    % A - B
    G(a, b) = 1;
    G(b, a) = 1;
    % B - C
    G(b, c) = 1;
    G(c, b) = 1;
    % C - D
    G(c, d) = 1;
    G(d, c) = 1;
    % D - A
    G(d, a) = 1;
    G(a, d) = 1;
    
    nodePots = { tabularFactorCreate(reshape([1 1], 2, 1), [a]); ... 
                 tabularFactorCreate(reshape([1 1], 2, 1), [b]); ...
                 tabularFactorCreate(reshape([1 1], 2, 1), [c]); ...
                 tabularFactorCreate(reshape([1 1], 2, 1), [d]) };
             
    edgePots = cell(4, 1);
    % A-B 
    edgePots{eAtoB} = tabularFactorCreate(reshape([30 1 5 10], 2, 2), [a b]);
    % B-C
    edgePots{eBtoC} = tabularFactorCreate(reshape([100 1 1 100], 2, 2), [b c]);
    % C-D
    edgePots{eCtoD} = tabularFactorCreate(reshape([1 100 100 1], 2, 2), [c d]);
    % D-A
    edgePots{eDtoA} = tabularFactorCreate(reshape([100 1 1 100], 2, 2), [a d]);
    
    mrfModel = mrfCreate(G, 'nodePots', nodePots, 'edgePots', edgePots, ...
                      'infEngine', infEngine);
end

