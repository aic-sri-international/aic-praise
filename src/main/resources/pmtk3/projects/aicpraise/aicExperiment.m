function aicExperiment( )
%AICEXPERIMENT Experimental calling code to try out ideas.

    testGeneralFactorGraphToMRF2();
    testMisconceptionMRFtoMRF2();
    testMisconceptionMRF2();
    testMisconceptionMRF();
end

function testGeneralFactorGraphToMRF2()
    a = 1; b = 2; c = 3; d = 4;
    
    cliques = cell(6, 1);
    %
    cliques{1} = tabularFactorCreate(reshape([2 5 3 7],     2, 2), [a b]);
    cliques{2} = tabularFactorCreate(reshape([11 15 14 19], 2, 2), [b c]);
    cliques{3} = tabularFactorCreate(reshape([21 26 23 27], 2, 2), [c d]);
    cliques{4} = tabularFactorCreate(reshape([31 35 33 39], 2, 2), [a d]);
    %
    cliques{5} = tabularFactorCreate(reshape([41 45 43 47 42 46 44 48], 2, 2, 2), [a b c]);
    cliques{6} = tabularFactorCreate(reshape([51 55 53 57 52 56 54 58], 2, 2, 2), [a c d]);
    
    nstates = 2*ones(4, 1);
    
    fg = factorGraphCreate(cliques, nstates);
    method = 'Exact';
    mrf2Model = factorGraphToMrf2(fg, 'method', method);
    
    %clamped = [0 0 0 0]; % i.e. all zeros, no evidence
    [nodeBel, edgeBel, logZ] =  mrf2InferNodesAndEdges(mrf2Model); %, clamped);
    disp('nodeBel')
    nodeBel
    disp('logZ')
    logZ
end

function testMisconceptionMRFtoMRF2()
    [mrfModel a b c d] = makeMisconceptionMRF('enum');
    method = 'Exact';
    mrf2Model = patchedFactorGraphToMrf2(mrfToFactorGraph(mrfModel), 'method', method);
                   
    clamped = [0 0 0 0]; % i.e. all zeros, no evidence
    [nodeBel, edgeBel, logZ] =  mrf2InferNodesAndEdges(mrf2Model, clamped);
    disp('nodeBel')
    nodeBel
    disp('logZ')
    logZ
end

function testMisconceptionMRF2()
    disp('Test Misconception MRF 2');
    
    [a b c d eAtoB eBtoC eCtoD eDtoA nNodes nStates G] = misconceptionGraph();
    
    nodePot = ones(nNodes, max(nStates));
    
    edgePot = zeros(max(nStates), max(nStates), 4);
    % A-B 
    edgePot(:,:,eAtoB) = [30, 5; 1, 10];
    % B-C
    edgePot(:,:,eBtoC) = [100, 1; 1, 100];
    % C-D
    edgePot(:,:,eCtoD) = [1, 100; 100, 1];
    % D-A
    edgePot(:,:,eDtoA) = [100, 1; 1, 100];
    
    method = 'Exact';
    model = mrf2Create(G, nStates, 'nodePot', nodePot, 'edgePot', edgePot, ...
                        'method', method);
                   
    clamped = [0 0 0 0 0]; % i.e. all zeros, no evidence
    [nodeBel, edgeBel, logZ] =  mrf2InferNodesAndEdges(model, clamped);
    disp('nodeBel')
    nodeBel
    disp('logZ')
    logZ
end

function testMisconceptionMRF()
    disp('Test Misconception MRF');

    [mrfModel a b c d] = makeMisconceptionMRF('enum');
    
    [nodeBels, logZ, edgeBels] =  mrfInferNodes(mrfModel);
    disp('A');
    nodeBels{a}.T
    disp('B');
    nodeBels{b}.T
    disp('C');
    nodeBels{c}.T
    disp('D');
    nodeBels{d}.T
    disp('logZ')
    logZ
    
    E = sparsevec([c], [1], 4);
    [bels, logZ] = mrfInferQuery(mrfModel, [b], 'clamped', E);
    bels.T
end

function [mrfModel a b c d] = makeMisconceptionMRF(infEngine)
    [a b c d eAtoB eBtoC eCtoD eDtoA nNodes nStates G] = misconceptionGraph();
    
    nodePots = { tabularFactorCreate(reshape([1 1], 2, 1), [a]); ... 
                 tabularFactorCreate(reshape([1 1], 2, 1), [b]); ...
                 tabularFactorCreate(reshape([1 1], 2, 1), [c]); ...
                 tabularFactorCreate(reshape([1 1], 2, 1), [d]) }
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

function [a b c d eAtoB eBtoC eCtoD eDtoA nNodes nStates G] = misconceptionGraph()
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
    
    nStates = 2*ones(nNodes, 1);
end

