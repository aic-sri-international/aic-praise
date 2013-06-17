function aicPRAiSE()
%AIC PRAiSE Utility class for calling and comparing PRAiSE model output with
%       propositional versions of the algorithm.
    disp('AIC-PRAiSE.');

    %
    % Model to run:
    %
    mkFactorGraphFunctionHandle = @mkExamplePairwiseLPIModelFactorGraph;
    %mkFactorGraphFunctionHandle = @mkExampleGeneralLPIModelFactorGraph;
    %mkFactorGraphFunctionHandle = @mkExperimentLoopyPQFactorGraph;
    %mkFactorGraphFunctionHandle = @mkExperimentLoopyPQWithPriorsFactorGraph;
    %mkFactorGraphFunctionHandle = @mkExperimentLoopyParfactorsExampleFactorGraph;

    %
    % Inference Algorithms to run:
    %
    % only set to true (i.e. non zero) if the model being tested is small enough
    doVarelim         = false;
    doBPBeliefs       = false;
    doLibdaiBPBeliefs = true;
    doLBPBeliefs      = false;
    doLargeIterations = false;
    
    if doVarelim == true
        % 'jtree', 'varelim', 'bp', 'enum'
        % 'libdai*'
        [beliefs, logZ] = testLPIModelToMRF(mkFactorGraphFunctionHandle, 'varelim');
        displayBeliefs('varelim', beliefs);
        disp('logZ =');
        disp(logZ);
    end

    if doLargeIterations == true
        iterations = [10, 500, ...
            1000, 1500, 2000, 2500, 3000, 3500, 4000, ...
            4500, 5000, 5500, 6000, 6500, 7000, 7500, ...
            8000, 8500, 9000, 9500, 10000];
    else
        iterations = [10, 500, 1000];
    end

    bpBeliefs       = cell(1,1);
    libdaiBPBeliefs = cell(1,1);
    lbpBeliefs      = cell(1,1);
    for i = 1:size(iterations, 2)
    	disp(sprintf('On run = %i of %i for %i iterations.', i, size(iterations, 2), iterations(i)));
        if doBPBeliefs == true
            [beliefs, ~] = testLPIModelToMRF(mkFactorGraphFunctionHandle, 'bp', ...
                 'infEngArgs', {'updateProtocol', 'sync', ...
                 'maxIter',        iterations(i)});

            bpBeliefs = extendBeliefs(bpBeliefs, beliefs);
        end
        
        if doLibdaiBPBeliefs == true
            [beliefs, ~] = testLPIModelToMRF(mkFactorGraphFunctionHandle, 'libdaiBP', ...
                'infEngArgs', {'BP', ...
                ['[inference=SUMPROD,updates=SEQMAX,logdomain=0,tol=1e-9,maxiter=' ...
                num2str(iterations(i)) ',damping=0.0]']});

            libdaiBPBeliefs = extendBeliefs(libdaiBPBeliefs, beliefs);
        end

        if doLBPBeliefs == true
            % 'Block_MF' (blocks required), 'Exact', 'LBP', 'MeanField' (maxIter required), 'TRBP'
            % 'Chain' assumes graph is chain.
            % 'Tree' assumes graph is a simple tree.
            [beliefs, ~] = testLPIModelToMRF2(mkFactorGraphFunctionHandle, 'LBP', ...
                'maxIter', iterations(i));

            lbpBeliefs = extendBeliefs(lbpBeliefs, beliefs);
        end
    end

    if doBPBeliefs == true
        displayBeliefs(['bp ' sprintf('%6i', iterations)],       bpBeliefs);
    end
    if doLibdaiBPBeliefs == true
        displayBeliefs(['libdaiBP ' sprintf('%6i', iterations)], libdaiBPBeliefs);
    end
    if doLBPBeliefs == true
        displayBeliefs(['LBP ' sprintf('%6i', iterations)],      lbpBeliefs);
    end
end

function [beliefs, logZ] = testLPIModelToMRF(mkFactorGraphFunctionHandle, infEngine, varargin)
    [fg, varNames] = mkFactorGraphFunctionHandle();

    mrfModel  = factorGraphToMrf(fg, 'infEngine', infEngine, varargin{:});

    [nodeBels, logZ, ~] =  mrfInferNodes(mrfModel);
    beliefs = cell(numel(varNames), 3);
    for i = 1:numel(varNames)
        beliefs{i, 1} = varNames{i};
        beliefs{i, 2} = nodeBels{i}.T(2); % True
        beliefs{i, 3} = nodeBels{i}.T(1); % False
    end
end

function [beliefs, logZ] = testLPIModelToMRF2(mkFactorGraphFunctionHandle, infEngine, varargin)
    [fg, varNames] = mkFactorGraphFunctionHandle();

    mrf2Model = patchedFactorGraphToMrf2(fg, 'method', infEngine, varargin{:});

    [nodeBel, ~, logZ] =  mrf2InferNodesAndEdges(mrf2Model);

    beliefs = cell(numel(varNames), 3);
    for i = 1:numel(varNames)
        beliefs{i, 1} = varNames{i};
        beliefs{i, 2} = nodeBel(i, 2); % True
        beliefs{i, 3} = nodeBel(i, 1); % False
    end
end

function newBeliefs = extendBeliefs(currentBeliefs, nextBeliefs) 
    if 1 == size(currentBeliefs, 2)
        newBeliefs = nextBeliefs;
    else
        newBeliefs = currentBeliefs;
        coffset    = size(newBeliefs, 2);
        for r = 1:size(nextBeliefs, 1)
            newBeliefs(r, coffset+1) = nextBeliefs(r, 2); % True
            newBeliefs(r, coffset+2) = nextBeliefs(r, 3); % False
        end
    end
end

function displayBeliefs(title, beliefs)
    disp('--------------------');
    disp(title);
    disp('--------------------');
    for i = 1:size(beliefs, 1)
        % True
        str = sprintf('%12s  True=', beliefs{i, 1});
        for c = 2:size(beliefs, 2)
            if mod(c, 2) == 0
                str = sprintf('%s\t%1.9f', str, beliefs{i, c});
            end
        end
        disp(str);
        % False
        str = sprintf('%12s False=', '');
        for c = 2:size(beliefs, 2)
            if mod(c, 2) == 1
                str = sprintf('%s\t%1.9f', str, beliefs{i, c});
            end
        end
        disp(str);
    end
    disp('--------------------');
end

